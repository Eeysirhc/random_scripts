# AUTHOR: https://twitter.com/Eeysirhc
# DATE WRITTEN: 2020-01-21
# ARTICLE: https://www.christopheryee.org/blog/using-r-gsc-data-identify-stale-content/

# CREDIT: https://twitter.com/JHTScherck
# ORIGINAL IDEA: https://twitter.com/JHTScherck/status/1219713918307692544

# DESCRIPTION
# USE THIS SCRIPT TO IDENTIFY STALE CONTENT USING GOOGLE SEARCH CONSOLE DATA

## LOAD PACKAGES
library(tidyverse)
library(searchConsoleR)

# AUTHENTICATE WITH GSC
scr_auth()

## DOWNLOAD DATA
df <- as_tibble(search_analytics("https://www.christopheryee.org/",
                                 Sys.Date() - 35, # START DATE
                                 Sys.Date() - 3, # END DATE
                                 c("page", "query"),
                                 searchType = "web",
                                 rowLimit = 1e5))

## IDENTIFY KEYWORDS
keywords <- df %>% 
  group_by(query) %>% 
  summarize(impressions = sum(impressions),
            position = mean(position)) %>% 
  filter(!grepl("brand_term", query)) %>% # EXCLUDE BRAND TERMS HERE
  arrange(dsec(impressions)) %>% 
  filter(impressions >= 2000,
         position >= 5 & position < 15) %>% 
  select(query)


## DEDUPE LANDING PAGES
pages <- df %>% 
  inner_join(keywords) %>% # JOIN OUR KEYWORDS DATASET
  group_by(query) %>% 
  arrange(desc(clicks)) %>% 
  mutate(candidate = row_number()) %>% 
  ungroup() %>% 
  filter(candidate == 1) %>% 
  select(page)

# FINAL CANDIDATES
final_candidates <- df %>% 
  inner_join(pages) %>% 
  mutate(ctr = (clicks / impressions) * 100) %>% # STANDARDIZE CTR
  arrange(desc(page, impressions)) %>% 
  distinct(.) %>% 
  filter(ctr < 2) # CHANGE FILTERS HERE

# WRITE RESULTS TO CSV
final_candidates %>% 
  write_csv("~/content_refresh_candidates.csv")

