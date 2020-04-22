
# AUTHOR: https://twitter.com/Eeysirhc
# DATE WRITTEN: 2020-04-22
# ARTICLE: TBD

# DESCRIPTION
# EXAMPLE ON HOW TO VISUALIZE SYSTEM OUTPUTS WITH DECISION GRIDS

# LOAD LIBRARIES
library(tidyverse)
library(scales)
library(patchwork)


# PLOTTING FUNCTION
decision_plot <- function(df){
  
  decision_tile <- ggplot(data = df, aes(metric_a, metric_b, fill = value)) +
    geom_tile(color = 'white', alpha = 0.8) +
    scale_fill_brewer(palette = 'Set3', direction = -1) +
    scale_x_continuous(labels = percent_format(round(1))) +
    scale_y_continuous(labels = percent_format(round(1))) +
    theme_minimal() +
    theme(legend.position = 'top')
  
  return(decision_tile)
}


# EXAMPLE 1: COMPARING YOY DIFFERENCES
df1 <- as_tibble(expand.grid(metric_a = seq(-.50, .50, .01), 
                            metric_b = seq(-.50, .50, .01)))

df1 <- df1 %>% 
  mutate(value = case_when(metric_a >= .30 ~ 'Output E',
                           metric_a < -.30 ~ 'Output A',
                           metric_a >= -.10 & metric_b >= -.10 
                           & metric_a < .15 & metric_b < .15 ~ 'Output C',
                           metric_a >= 0 & metric_a < 0.30 & metric_b < 0 ~ 'Output E',
                           metric_a >= -.30 & metric_a < 0 & metric_b >= 0 ~ 'Output A',
                           metric_a >= 0 & metric_a < 0.30 & metric_b >= 0 ~ 'Output D',
                           metric_a < 0 & metric_a >= -.30 & metric_b < 0 ~ 'Output B'))

p1 <- df1 %>% 
  decision_plot +
  labs(x = "Metric A", y = "Metric B", fill = NULL, 
       title = "Example 1: YoY differences of two performance metrics") 
  


# EXAMPLE 2: COMPARING RAW % BETWEEN METRICS
df2 <- as_tibble(expand.grid(metric_a = seq(0, 2, .01), 
                           metric_b = seq(0, 2, .01)))

df2 <- df2 %>% 
  mutate(value = case_when(metric_b >= metric_a*1.3 ~ 'Output A',
                           metric_b >= metric_a*1.1 & metric_b < metric_a*1.3 ~ 'Output B',
                           metric_b >= metric_a*0.7 & metric_b < metric_a*0.90 ~ 'Output D',
                           metric_b < metric_a*0.7 ~ 'Output E',
                           TRUE ~ 'Output C'))

p2 <- df2 %>% 
  decision_plot +
  labs(x = "Metric A", y = "Metric B", fill = NULL,
       title = "Example 2: comparing % of two performance metrics",
       caption = "by: @eeysirhc") 



# PLOT CHARTS
p1 + p2



