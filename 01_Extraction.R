# This script parses documents to extract utterances about NATO
############################################################

require(tidyverse)
require(ggplot2)
require(data.table)
require(matrixStats)
require(stringr)


# The function takes five arguments: 
# The original dataframe of documents
# A vector of the case sensitive keywords (in regex) for the paragraph
# A vector of the non-case sensitive keywords (in regex) for the paragraph
# A vector of the case sensitive keywords (in regex) for the document title
# A vector of the non-case sensitive keywords (in regex) for the document title

utterance_df_function <- function(df, keywords_text_case = c("NATO", "NAC"), 
                                  keywords_text_no_case = c("Atlantic Treaty", "Atlantic Council", "North Atlantic Alliance"), 
                                  keywords_title_case = c("NATO", "NAC"), 
                                  keywords_title_no_case = c("Atlantic Treaty", "Atlantic Council")) {
  
  dat_tidy <- df %>%
    separate_rows(text, sep = "\n") %>%
    filter(text != '') 
  
  dat_newid <- dat_tidy %>%
    group_by(UID) %>%
    mutate(ID = row_number()) %>%
    select(UID:text, ID, categories) %>%
    unite(PID, UID, ID, remove = FALSE) %>%
    select(-ID)
  
  
  dat_newid_key <- dat_newid %>%
    mutate(keyword_binary = if_else((((str_detect(text, 
                                                  regex(paste(keywords_text_no_case, 
                                                              collapse = '|'), 
                                                        ignore_case = TRUE)))| 
                                        str_detect(text, 
                                                   regex(paste(keywords_text_case, collapse = '|'))))
                                     | ((str_detect(title, 
                                                    regex(paste(keywords_title_no_case, 
                                                                collapse = '|'), 
                                                          ignore_case = TRUE)))| 
                                          str_detect(title, 
                                                     regex(paste(keywords_title_case, collapse = '|'  ))))), 1, 0))
  
  final_df <- dat_newid_key %>%
    select(UID, PID, title, speaker, date, categories, text, keyword_binary) %>%
    mutate(relevance = "-",
           sentiment = "-",
           confidence = "-",
           notes = "-") %>%
    arrange(UID)
  
  return(final_df)
  
}

