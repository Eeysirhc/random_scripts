
# AUTHOR: https://twitter.com/Eeysirhc
# DATE WRITTEN: 2020-05-03
# ARTICLE: https://www.christopheryee.org/blog/exploratory-data-analysis-on-covid-19-search-queries/

# SOURCE: https://github.com/microsoft/BingCoronavirusQuerySet

# DESCRIPTION
# SCRIPT TO COMPILE MULTIPLE TSV FILES FROM BING CORONAVIRUS SEARCH QUERY DATA
# CONSOLIDATED DATA IS SAVED TO AS CSV


# LOAD LIBRARIES
library(tidyverse)


# READ TSV FILES
jan <- read_tsv("https://raw.githubusercontent.com/microsoft/BingCoronavirusQuerySet/master/data/2020/QueriesByState_2020-01-01_2020-01-31.tsv")

feb <- read_tsv("https://raw.githubusercontent.com/microsoft/BingCoronavirusQuerySet/master/data/2020/QueriesByState_2020-02-01_2020-02-29.tsv")

mar <- read_tsv("https://raw.githubusercontent.com/microsoft/BingCoronavirusQuerySet/master/data/2020/QueriesByState_2020-03-01_2020-03-31.tsv")

apr <- read_tsv("https://raw.githubusercontent.com/microsoft/BingCoronavirusQuerySet/master/data/2020/QueriesByState_2020-04-01_2020-04-30.tsv")


# JOIN DATA FRAMES
df_compile <- rbind(jan, feb, mar, apr)

# FINAL CSV OUTPUT
df_compile %>% 
  write_csv("coronavirus-queriesbystate.csv")

# CLEAN UP WORKSPACE
rm(df_compile, jan, feb, mar, apr)




