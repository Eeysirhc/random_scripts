# AUTHOR: https://twitter.com/Eeysirhc
# DATE WRITTEN: 2021-02-15
# ORIGINAL REFERENCE: https://twitter.com/Eeysirhc/status/1361779630374461442
# ARTICLE: [PLACEHOLDER]

# DESCRIPTION
## VISUALIZE DISTIRUBTION OF BACKLINKS BASED ON A NUMBER OF DIMENSIONS
## WHAT IS THE FREQUENCY OF UNIGRAM TERMS BEING TARGETED?
## WHAT IS THE TOTAL NUMBER OF LINKS TO EACH UNIGRAM?
## ARE THESE RECENT OR HISTORICAL ADDITIONS TO THE WEBSITE PROFILE?

# DATA SOURCE
## AHREFS.COM > BACKLINK PROFILE > ANCHORS > FULL CSV EXPORT

##### LIBRARIES #####
library(tidyverse)
library(tidytext)
library(ggwordcloud)
library(scales)

##### AHREFS CSV LOCATION #####
filename <- "~/Desktop/www.wayfair.com-anchors-prefix-live-16-Feb-2021_04-30-04-3c799107d3e86997c925440254fd63b3.csv"

##### LOAD CSV TO DATAFRAME #####
df_raw <- read.csv(filename, sep = "\t", fileEncoding = "utf-16", stringsAsFactors = FALSE) %>% 
  as_tibble() %>% 
  select(Anchor.Text, Referring.Domains.Total, First.Seen) %>% 
  mutate(First.Seen = as.Date(First.Seen)) %>% 
  filter(!is.na(First.Seen)) %>% 
  ### CALCULATE DAYS SINCE FIRST SEEN
  mutate(days_since = Sys.Date() - First.Seen)

##### TIDY DATA #####
df <- df_raw %>% 
  select(anchor_text = Anchor.Text,
         referring_domains = Referring.Domains.Total,
         days_since) %>% 
  group_by(referring_domains, days_since) %>% 
  unnest_tokens(word, anchor_text) %>% 
  count(word, sort = TRUE) %>% 
  ungroup() %>% 
  anti_join(stop_words) %>% 
  group_by(word) %>% 
  summarize(referring_domains = sum(referring_domains),
            n = sum(n),
            days_since = mean(days_since)) %>% 
  arrange(desc(referring_domains)) %>% 
  ungroup() %>% 
  filter(is.na(as.numeric(word)))

##### FILTER OUT IRRELEVANT TERMS #####
df_clean <- df %>% 
  mutate(days_since = as.numeric(days_since)) %>% 
  filter(!grepl("sb0|html|https|http", word)) %>% 
  top_n(100, n)

##### PLOT #####
df_clean %>% 
  ggplot(aes(n, label = word, size = referring_domains, color = days_since)) +
  geom_text_wordcloud(shape = "star") +
  scale_size_area(max_size = 13) +
  scale_x_continuous(labels = comma_format()) +
  scale_color_gradient2(low = "#D55E00",
                        mid = "#999999",
                        midpoint = mean(df_clean$days_since),
                        high = "#56B4E9") +
  labs(title = "Distribution of external text backlinks",
       subtitle = "Size = total links (pairwise) \nColor = days since first seen (orange for recent)",
       x = "Term Frequency",
       caption = "by: @eeysirhc \nsource: Ahrefs") +
  theme_bw()



