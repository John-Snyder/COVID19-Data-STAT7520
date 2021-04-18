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

# this updates daily, getURL pulls raw text from this location
x <- getURL("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
covid_data <- read.csv(text = x)
rm(x)

covid_data <- covid_data %>% filter(county != "Unknown")

population_data <- read_csv(paste0(data_dir,"co-est2019-alldata.csv"))
population_data$STATE <- str_remove(as.character(population_data$STATE), "^0+")
population_data$COUNTY <- str_pad(population_data$COUNTY,
                                  width = 3, pad = "0",side = "left")

# make FIPS code for joining to covid_data
population_data$fips <- as.integer(paste0(
  population_data$STATE,
  population_data$COUNTY))


population_data <- population_data %>% mutate(
  fips = as.integer( paste0( STATE, COUNTY ) ),
  Pop_2019 = POPESTIMATE2019,
  avg_pop_chg = rowMeans(select(population_data, starts_with("NPOPCHG_20"))),
  avg_bth_num = rowMeans(select(population_data, starts_with("BIRTHS20"))),
  avg_dth_num = rowMeans(select(population_data, starts_with("DEATHS20"))),
  avg_itl_mig = rowMeans(select(population_data, starts_with("INTERNATIONALMIG20"))),
  avg_dom_mig = rowMeans(select(population_data, starts_with("DOMESTICMIG20"))),
  num_Grp_quarters = GQESTIMATES2019,
) %>% select(fips,
             CTYNAME,
             Pop_2019,
             avg_pop_chg,
             avg_bth_num,
             avg_dth_num,
             avg_itl_mig,
             avg_dom_mig,
             num_Grp_quarters)

covid_data <- covid_data %>% left_join(population_data, by = "fips")
rm(population_data)

sapply(1:nrow(covid_data), function(i) {
  grepl(pattern = covid_data$county[i],x = covid_data$CTYNAME[i])
})



demographic_data <- read_csv(paste0(data_dir,"cc-est2019-alldata.csv")) %>% 
  filter(YEAR == 12)  # most recent estimates

