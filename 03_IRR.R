# This script reads the random samples and evaluates IRR
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

# read samples
sample1 <- read.csv("Data/random-samples/01_n10/sample1_merged.csv", stringsAsFactors = F)
sample2 <- read.csv("Data/random-samples/02_n50/sample2_merged.csv", stringsAsFactors = F)
sample3 <- read.csv("Data/random-samples/03_n50/sample3_merged.csv", stringsAsFactors = F)
sample4 <- read.csv("Data/random-samples/04_n50/sample4_merged.csv", stringsAsFactors = F)
sample5 <- read.csv("Data/random-samples/05_n100/sample5_merged.csv", stringsAsFactors = F)
sample6 <- read.csv("Data/random-samples/06_n233/sample6_merged.csv", stringsAsFactors = F)

# un comment to bind
# full_sample <- rbind(sample1, sample2, sample3, sample4, sample5, sample6)

# assign which sample you want to test
sample <- sample6

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
unique(sample$relevance_1) # should be 0, 1
unique(sample$relevance_2) # should be 0, 1
unique(sample$sentiment_1) # should be NA  "A" "N" "P"
unique(sample$sentiment_2) # should be NA  "A" "N" "P"


##################################
######## Summary Stats  ##########
##################################

# how many documents have we done?
length(unique(sample$UID)) # 260

# how many do we have left?
load("Data/clean-subset/clean-subset.RData")
nrow(subset) - length(unique(sample$UID)) 

# IRR agreement on relevance
sample %>% select(relevance_1, relevance_2) %>% agree()

# IRR kappa2 on relevance
sample %>% select(relevance_1, relevance_2) %>% kappa2()

# IRR agreement on sentiment
sample %>% select(sentiment_1, sentiment_2) %>% agree()

# IRR kappa2 on sentiment
sample %>% select(sentiment_1, sentiment_2) %>% kappa2()

# number of conflicts on relevance
sample <- sample %>%
  mutate(relevance_conflict = if_else(relevance_1 == relevance_2, 0, 1))
table(sample$relevance_conflict)

