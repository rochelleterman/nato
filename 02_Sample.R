# This script extracts a random sample of documents for coding
# Coding completed by RAs, Fall 2019
##############################################################

rm(list=ls())
require(dplyr)
require(ggplot2)
require(data.table)
require(tidyr)
require(matrixStats)

# source extraction function
source('01_Extraction.R')

# read data
load("Data/clean-subset/clean-subset.RData")

# x <- subset %>% filter(grepl("NATO",title))
# write.csv(x, "NATO_titles.csv")


###################################
######## Random Sample 6 ##########
###################################

# read samples 1-4
sample1 <- read.csv("Data/random-samples/01_n10/sample1_merged.csv", stringsAsFactors = F)
sample2 <- read.csv("Data/random-samples/02_n50/sample2_merged.csv", stringsAsFactors = F)
sample3 <- read.csv("Data/random-samples/03_n50/sample3_merged.csv", stringsAsFactors = F)
sample4 <- read.csv("Data/random-samples/04_n50/sample4_merged.csv", stringsAsFactors = F)
sample5 <- read.csv("Data/random-samples/05_n100/sample5_merged.csv", stringsAsFactors = F)

sample <- rbind(sample1, sample2, sample3, sample4, sample5)

# remove those from full sample
subset.uncoded <- subset %>% filter(!UID %in% unique(sample$UID))

# sample and parse
set.seed(411)
sample6 <- sample_n(subset.uncoded, 233)
sample6.parsed <- utterance_df_function(df = sample6)

# write
write.csv(sample6.parsed, file = "Data/random-samples/06_n233/sample6.csv", row.names = F)

                                    