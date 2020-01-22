
# AUTHOR: https://twitter.com/Eeysirhc
# DATE WRITTEN: 2019-11-18
# ARTICLE: [PLACEHOLDER]

# DESCRIPTION
# CSV FILES FOR SEO KEYWORD DATA FROM AHREFS DOESN'T PLAY NICE IN R
# USE THIS SCRIPT TO CONSOLIDATE, CLEAN AND OUTPUT A MASTER FILE



library(tidyverse)


ahrefs_raw <- plyr::ldply(list.files(pattern = "\\.com.*.csv"), 
                          read.csv, sep = '\t') %>% 
  as_tibble() %>% 
  select(-X.) 



ahrefs_raw %>% 
  write_csv("organic-keywords.csv")

