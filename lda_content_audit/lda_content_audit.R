
# AUTHOR: https://twitter.com/Eeysirhc
# DATE WRITTEN: 2019-11-18
# ARTICLE: [PLACEHOLDER]
# ORIGINAL REFERENCE: https://twitter.com/Eeysirhc/status/1204447958332133376

# DESCRIPTION
## GOAL OF THIS SCRIPT IS TO QUICKLY AUDIT CONTENT FOR A WEBSITE BASED ON AHREFS DATA
## THIS IS A PROOF OF CONCEPT THAT BUILDS AN LDA TOPIC MODEL BY ONLY READING FROM THE RANKING URL
## IT THEN PULLS THE TOP GAMMA TERMS FOR INTERPRETABILITY AND VISUALIZES BY OPPORTUNITY

# ENHANCEMENTS
## CRAWLING EACH URL AND PARSING TITLE, BODY CONTENT, ETC.
## INCORPORATING SENTIMENT
## INDICATION OF CONTENT EFFECTIVENESS (h/t https://twitter.com/corey_northcutt)
## SCALE OVER TIME (h/t https://twitter.com/micahfk)
## INCLUDE MORE THAN ONE COMPETITOR (h/t https://twitter.com/micahfk)

##### LIBRARIES #####
library(tidyverse)
library(scales)
library(tidytext)
library(topicmodels)
library(ggrepel)

##### PARSE DATA #####
# CONSOLIDATE MULTIPLE AHREFS CSVs TO READ INTO R
source("consolidate_csv.R")

ahrefs <- read_csv("organic-keywords.csv")

##### CLEAN DATA #####
# REMOVE UTM PARAMTERS
ahrefs$URL = gsub("\\?.*", "", ahrefs$URL)

# REMOVE ANCHORED LINKS
ahrefs$URL = gsub("\\#.*", "", ahrefs$URL)

# SPEND MOST OF YOUR TIME CLEANING HERE
ahrefs_parsed <- ahrefs %>% 
  select(URL) %>% 
  filter(URL != 'https://moz.com/blog',
         !grepl("/category", URL), 
         !grepl("/author", URL)) %>% 
  mutate(title = str_replace_all(URL , "https://moz.com/blog/", ""),
         title = str_replace_all(title, "-", " "),
         title = str_replace_all(title, "[^a-zA-Z\\s]", " "),
         title = str_to_lower(title),
         title = str_replace_all(title, "[\\s]+", " ")) 

##### BUILD DTM AND LDA #####
# DOCUMENT TERM MATRIX
ahrefs_dtm <- ahrefs_parsed %>% 
  distinct(URL, title) %>% 
  group_by(URL) %>% 
  unnest_tokens(word, title) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE) %>% 
  cast_dtm(URL, word, n)

# LATENT DIRICHLET ALLOCATION
ahrefs_lda <- LDA(ahrefs_dtm, k = 30, control = list(seed = 1210))

##### FINAL TOPIC AND URLS LIST BY MAX GAMMA #####
ahrefs_topics <- tidy(ahrefs_lda, matrix = "gamma") %>% 
  group_by(document) %>% 
  filter(gamma == max(gamma)) %>% 
  ungroup() 

##### PERFORMANCE METRICS #####
ahrefs_data <- ahrefs %>% 
  select(URL, Volume, Traffic..desc., SERP.Features, CPC) %>% 
  group_by(URL, SERP.Features) %>% 
  summarize(volume = sum(Volume),
            traffic = sum(Traffic..desc.),
            n = n(),
            cpc = mean(CPC)) %>% 
  distinct() %>% 
  separate_rows(SERP.Features, sep = ",") %>% 
  mutate(SERP.Features = str_trim(SERP.Features, side = "both")) %>% 
  ungroup()

##### TOPIC NAMES #####
topic_names <- tidy(ahrefs_lda) %>% 
  group_by(topic) %>% 
  top_n(2, beta) %>% 
  ungroup() %>% 
  arrange(topic, -beta) %>% 
  group_by(topic) %>% 
  mutate(topic_name = paste0(term, collapse = " ")) %>% 
  select(topic, topic_name) %>% 
  distinct()

##### FINAL PLOT #####
ahrefs_data %>% 
  left_join(ahrefs_topics, by = c("URL" = "document")) %>% 
  left_join(topic_names) %>% 
  group_by(topic, topic_name) %>% 
  filter(!is.na(volume), !is.na(cpc)) %>% 
  summarize(volume = sum(volume),
            traffic = sum(traffic),
            words_per_topic = sum(n),
            cpc = sum(cpc)) %>% 
  ungroup() %>% 
  mutate(pct_volume = volume / sum(volume),
         pct_traffic = traffic / sum(traffic)) %>% 
  ggplot(aes(pct_volume, pct_traffic, size = words_per_topic, 
             label = topic_name, color = cpc * traffic)) +
  geom_text(check_overlap = TRUE) +
  theme_light(base_size = 15) +
  geom_abline(lty = 2, color = 'grey60') +
  scale_y_log10(labels = percent_format(round(1))) +
  scale_x_log10(labels = percent_format(round(1))) +
  scale_color_gradient(low = "steelblue", 
                       high = "salmon") +
  labs(x = "% Seach Volume", y = "% Estimated Search Traffic",
       title = "Content analysis of Moz blog using LDA",
       caption = "by: @eeysirhc \nSource: Ahrefs via @coreyeulas",
       color = "traffic_value") 
