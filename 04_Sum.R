# This script reads the random samples and evaluates progress.
# Coding completed by RAs, Fall 2019 -- Winter 2020
##############################################################

rm(list=ls())
require(tidyverse)
require(data.table)
require(matrixStats)
require(irr)

###################################
######## Read Samples #########
###################################

# read
sample1 <- read.csv("Data/random-samples/01_n10/sample1_merged.csv", stringsAsFactors = F)
sample2 <- read.csv("Data/random-samples/02_n50/sample2_merged.csv", stringsAsFactors = F)
sample3 <- read.csv("Data/random-samples/03_n50/sample3_merged.csv", stringsAsFactors = F)
sample4 <- read.csv("Data/random-samples/04_n50/sample4_merged.csv", stringsAsFactors = F)
sample5 <- read.csv("Data/random-samples/05_n100/sample5_merged.csv", stringsAsFactors = F)
sample6 <- read.csv("Data/random-samples/06_n233/sample6_merged.csv", stringsAsFactors = F)

# un comment to bind
sample <- rbind(sample1, sample2, sample3, sample4, sample5, sample6)

########################
######## Clean #########
#######################

# recode variables (typos, etc)
sample <- sample %>%
  mutate(relevance_1 = as.numeric(recode(relevance_1, "-" = "0", "P" = "1", "NP" = "0")),
         relevance_2 = as.numeric(recode(relevance_2, "-" = "0", "P" = "1", "NP" = "0", "9" = "0")),
         
         relevance_1 = replace_na(relevance_1, 0),
         relevance_2 = replace_na(relevance_2, 0),
         
         sentiment_1 = recode(sentiment_1, "-" = NA_character_, "0" = NA_character_, "1" = "P"),
         sentiment_2 = recode(sentiment_2, " " = NA_character_, "-" = NA_character_, "0" = NA_character_, "H" = NA_character_ , "1" = "P"),
         
         sentiment_1 = na_if(sentiment_1, ""),
         sentiment_2 = na_if(sentiment_2, ""),
         
         confidence_1 = na_if(confidence_1, "-"),
         confidence_2 = na_if(confidence_1, "-")
  )

# test
unique(sample$relevance_1)
unique(sample$relevance_2)
unique(sample$sentiment_1)
unique(sample$sentiment_2)

# merge coder 1 and coder 2
sample <- sample %>%
  # if either coder marked as relevant, make it relevant
  mutate(relevance_m = if_else(relevance_1 + relevance_2 == 0, 0, 1),
         # if either coder marked as relevant, take their sentiment. 
         # if both coders marked as relevant but conflict on sentiment, coder 2 wins. 
         sentiment_m = if_else(sentiment_1 == sentiment_2, sentiment_1, NA_character_),
         sentiment_m = if_else(is.na(sentiment_m), sentiment_1, sentiment_m),
         sentiment_m = if_else(is.na(sentiment_m), sentiment_2, sentiment_m))


##################################
######## Summary Stats  ##########
##################################

# how many documents have we done?
length(unique(sample$UID)) # 260

# how many do we have left?
load("Data/clean-subset/clean-subset.RData")
nrow(subset) - length(unique(sample$UID)) 

# by president
all_by_pres <- subset %>% 
  dplyr::count(speaker) %>%
  select(speaker, "total_docs" = n)

by_pres <- sample %>% 
  group_by(speaker) %>%
  summarise(n_docs_sample = length(unique(UID)),
            n_para = n(),
            n_para_NATO_keyword = sum(keyword_binary),
            n_relevant = sum(relevance_m),
            percent_relevant = n_relevant / n(),
            n_positive = sum(sentiment_m == "P", na.rm = T),
            percent_positive = n_positive / n_relevant,
            n_negative = sum(sentiment_m == "N", na.rm = T),
            percent_negative = n_negative / n_relevant,
            n_ambiv = sum(sentiment_m == "A", na.rm = T),
            percent_ambiv = n_ambiv / n_relevant) %>%
  left_join(all_by_pres) %>%
  select(speaker, total_docs, 2:12)

write.csv(by_pres, "Results/samples1-4_by-pres.csv", row.names = F)

by_pres_long <- sample %>%
  filter(relevance_m == 1, !is.na(sentiment_m)) %>%
  dplyr::count(speaker, sentiment_m) %>%
  group_by(speaker) %>%
  mutate(all = sum(n),
         pc = n / all) %>%
  ungroup() %>%
  mutate(speaker = factor(speaker, levels = unique(subset$speaker)))


# plot -- raw counts
ggplot(by_pres_long, aes(x = fct_reorder(speaker, all, .desc = T), y = n, fill = sentiment_m)) +
  geom_bar(position="stack", stat="identity") +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  xlab("Speaker") +
  ylab("Number of paragraphs") +
  scale_fill_discrete(breaks = c("P", "A", "N"),
                      name="Sentiment",
                      labels=c("Positive", "Ambivalent", "Negative"))

ggsave("Results/samples1-4_by-pres.pdf")

# plot -- percent
ggplot(by_pres_long, aes(x = speaker, y = pc, fill = sentiment_m)) +
  geom_bar(position="stack", stat="identity") +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  xlab("Speaker") +
  ylab("Number of paragraphs") +
  scale_fill_discrete(breaks = c("P", "A", "N"),
                      name="Sentiment",
                      labels=c("Positive", "Ambivalent", "Negative"))

ggsave("Results/samples1-4_by-pres_PC.pdf")


