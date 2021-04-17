## Pull data from github

## County level:
# b4:            https://github.com/nytimes/covid-19-data/blob/master/us-counties.csv
# https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv

## State level:
# https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv

## Country level:
# https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv

## Mask use by county
# https://raw.githubusercontent.com/nytimes/covid-19-data/master/mask-use/mask-use-by-county.csv

## Excess deaths
# -- NOTE USED
# https://raw.githubusercontent.com/nytimes/covid-19-data/master/excess-deaths/deaths.csv

data_dir <- "~/Dropbox-NAS/STAT_7520/Project/Data/SP_2021/raw_data/"

library(tidyverse)
library(RCurl)
library(stringr)

# this updates daily
x <- getURL("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
covid_data <- read.csv(text = x)
rm(x)  # remove chonkey raw text that we don't need anymore

population_data <- read_csv(paste0(data_dir,"co-est2019-alldata.csv"))
population_data$STATE <- str_remove(as.character(population_data$STATE), "^0+")
population_data$COUNTY <- str_pad(population_data$COUNTY,
                                  width = 3, pad = "0",side = "left")

population_data$fips <- as.integer(paste0(
                                      population_data$STATE,
                                      population_data$COUNTY))

covid_data <- covid_data %>% left_join(population_data, by = "fips")
rm(population_data)




demographic_data <- read_csv(paste0(data_dir,"cc-est2019-alldata.csv")) %>% 
  filter(YEAR == 12)  # most recent estimates

