
# AUTHOR: https://twitter.com/Eeysirhc
# DATE WRITTEN: 2020-04-22
# ARTICLE: TBD

# DESCRIPTION
# EXAMPLE ON HOW TO VISUALIZE SYSTEM OUTPUTS WITH DECISION GRIDS

library(tidyverse)
library(scales)

df <- as_tibble(expand.grid(metric_a = seq(-.50, .50, .01), 
                            metric_b = seq(-.50, .50, .01)))

df <- df %>% 
  mutate(value = case_when(metric_a >= .30 ~ 'Output A',
                           metric_a < -.30 ~ 'Output B',
                           metric_a >= -.10 & metric_b >= -.10 
                           & metric_a < .15 & metric_b < .15 ~ 'Output C',
                           metric_a >= 0 & metric_a < 0.30 & metric_b < 0 ~ 'Output A',
                           metric_a >= -.30 & metric_a < 0 & metric_b >= 0 ~ 'Output B',
                           metric_a >= 0 & metric_a < 0.30 & metric_b >= 0 ~ 'Output D',
                           metric_a < 0 & metric_a >= -.30 & metric_b < 0 ~ 'Output E'))


df %>% 
  ggplot(aes(metric_a, metric_b, fill = value)) +
  geom_tile(color = 'white', alpha = 0.9) +
  scale_fill_brewer(palette = 'Set3', direction = -1) +
  scale_x_continuous(labels = percent_format(round(1))) +
  scale_y_continuous(labels = percent_format(round(1))) +
  labs(x = "Metric A", y = "Metric B", fill = NULL) +
  theme_minimal()






