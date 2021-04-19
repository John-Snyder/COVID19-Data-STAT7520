library(tidyverse)
library(RCurl)
library(stringr)
## Pull data from github

## County level:
# https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv

## State level:
# https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv

## Country level:
# https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv

## Mask use by county
# https://raw.githubusercontent.com/nytimes/covid-19-data/master/mask-use/mask-use-by-county.csv

## Excess deaths
# -- NOT USED
# https://raw.githubusercontent.com/nytimes/covid-19-data/master/excess-deaths/deaths.csv

# 
data_dir <- "~/Dropbox-NAS/STAT_7520/Project/Data/SP_2021/raw_data/"
####################################################################################################
# Pull raw COVID count data                                                                        #
####################################################################################################
# this updates daily, getURL so pull raw text from this location
x <- getURL("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
covid_data <- read.csv(text = x)
rm(x)

# Some areas do not have demographic data from the census bureau, so we will remove them here.
covid_data <- covid_data %>% filter(county != "Unknown")
covid_data <- covid_data %>% filter(state != "Puerto Rico")
covid_data <- covid_data %>% filter(state != "Virgin Islands")
covid_data <- covid_data %>% filter(state != "Northern Mariana Islands")

# The 5 boroughs of NYC are aggregated to "New York City", and no
# fips code provided, so we add a dummy one.
covid_data$fips[covid_data$county == "New York City"] <- 99999

####################################################################################################
# Population demographic data                                                                      #
####################################################################################################
population_data <- read_csv(paste0(data_dir,"co-est2019-alldata.csv"))
####################################################################################################
#
# Note:  The FIPS code is a unique identifier for state/county combinations and is needed for 
# joining to other files, one can merge STATE and COUNTY while ensuring county is always 3 digits to
# accomplish this
#
# e.g. 
# Autauga County, Alabama has STATE=1 and COUNTY=1, so this would be fips = 1001
# Bullock County, Alabama has STATE=1 and COUNTY=11, so this would be fips = 1011
# Anderson County, Texas has STATE=48 and COUNTY=1, so this would be fips = 48001
# DeWitt County, Texas has STATE=48 and COUNTY=123, so this would be fips = 48123
#
####################################################################################################

population_data$STATE <- str_remove(as.character(population_data$STATE), "^0+")
population_data$COUNTY <- str_pad(population_data$COUNTY,
                                  width = 3, pad = "0",side = "left")

# make FIPS code for joining to covid_data
population_data$fips <- as.integer(paste0(
  population_data$STATE,
  population_data$COUNTY))

# Manipulate variables:
# There would be more possibilities one could work with, see 
# co-est2019-alldata_Column_Descriptions.pdf for details
population_data <- population_data %>% mutate(
  Pop_2019 = POPESTIMATE2019,
  # Compute 10yr average changes for each county
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

# Now deal with NYC: 
# Merge the 5 boroughs
population_data$CTYNAME[
  population_data$STNAME == "New York" & 
    population_data$CTYNAME %in% c("Bronx County",
                                   "Kings County",
                                   "New York County",
                                   "Queens County",
                                   "Richmond County")] <- "New York City"

# add everything up and get it ready for merging back in
NYC_AGG <- 
  population_data %>% 
  filter(CTYNAME == "New York City") %>% 
  select(-fips,-CTYNAME) %>% 
  colSums %>% t %>%
  as.data.frame %>% 
  mutate(fips = 99999,
         CTYNAME = "New York City")

# Remove borough level data and bring in aggregated data
population_data <- 
  population_data %>% 
  filter(CTYNAME != "New York City") %>%
  bind_rows(NYC_AGG)

covid_data <- covid_data %>% 
  left_join(population_data, by = "fips") %>%
  na.omit %>% # this removes some counties that the NYtimes aggregated, e.g. in Alaska
  select(-CTYNAME)

rm("population_data","NYC_AGG")

#############################################################################################
demographic_data <- read_csv(paste0(data_dir,"cc-est2019-alldata.csv")) %>% 
  filter(YEAR == 12, AGEGRP != 0)  # most recent estimates, rem overall totals

# similar logic to before
demographic_data$STATE <- str_remove(as.character(demographic_data$STATE), "^0+")
demographic_data$COUNTY <- str_pad(demographic_data$COUNTY,
                                   width = 3, pad = "0",side = "left")

demographic_data <-demographic_data %>% 
  mutate(fips = as.integer( paste0( STATE, COUNTY ) )) %>% 
  select(-STATE,-COUNTY)

demographic_data$CTYNAME[
  demographic_data$STNAME == "New York" & 
    demographic_data$CTYNAME %in% c("Bronx County",
                                    "Kings County",
                                    "New York County",
                                    "Queens County",
                                    "Richmond County")] <- "New York City"

NYC_AGG <- 
  demographic_data %>% 
  filter(CTYNAME == "New York City") %>% 
  group_by(CTYNAME,SUMLEV,STNAME,YEAR,AGEGRP) %>% 
  summarise_all(sum) %>%
  as.data.frame %>% 
  mutate(fips = 99999,
         CTYNAME = "New York City")

demographic_data <- 
  demographic_data %>% 
  filter(CTYNAME != "New York City") %>%
  bind_rows(NYC_AGG)

# Group ages together into covid related ones.
demographic_data$AGE_GRP_AGG <- ""
demographic_data$AGE_GRP_AGG[demographic_data$AGEGRP >= 1 & demographic_data$AGEGRP <= 4 ] <- "AGE_le19"
demographic_data$AGE_GRP_AGG[demographic_data$AGEGRP >= 5 & demographic_data$AGEGRP <= 13 ] <- "AGE_ge20le64"
demographic_data$AGE_GRP_AGG[demographic_data$AGEGRP >= 14] <- "AGE_ge65"


demographic_data <- demographic_data %>%
  group_by(AGE_GRP_AGG, fips) %>% 
  summarise(ppl = sum(TOT_POP)) %>%
  spread(AGE_GRP_AGG, ppl)

covid_data <- covid_data %>% 
  left_join(demographic_data, by = "fips") %>%
  na.omit

rm("demographic_data","NYC_AGG")

####################################################################################################
# Mask use data
####################################################################################################
x <- getURL("https://raw.githubusercontent.com/nytimes/covid-19-data/master/mask-use/mask-use-by-county.csv")
maskuse_data <- read.csv(text = x)
rm(x)
names(maskuse_data) <- paste0("maskprop_", names(maskuse_data))

NYC_AGG <- 
  maskuse_data %>% 
  filter( maskprop_COUNTYFP %in% c( 36005, 36047, 36061, 36081, 36085 ) ) %>% 
  select(-maskprop_COUNTYFP) %>% 
  colMeans() %>% t %>%
  as.data.frame %>% 
  mutate(maskprop_COUNTYFP = 99999)

maskuse_data <- 
  maskuse_data %>% 
  filter( !(maskprop_COUNTYFP %in% c( 36005, 36047, 36061, 36081, 36085 )) ) %>%
  bind_rows(NYC_AGG) %>% 
  rename(fips = maskprop_COUNTYFP)

covid_data <- covid_data %>% 
  left_join(maskuse_data, by = "fips") %>%
  na.omit

rm("maskuse_data", "NYC_AGG")

####################################################################################################
## Create state level data
####################################################################################################
covid_data_state <- covid_data %>% 
  group_by(date,state) %>%
  summarise(cases = sum(cases),
            deaths = sum(deaths),
            Pop_2019 = sum(Pop_2019),
            avg_pop_chg = sum(avg_pop_chg),
            avg_bth_num = sum(avg_bth_num),
            avg_dth_num = sum(avg_dth_num),
            avg_itl_mig = sum(avg_itl_mig),
            avg_dom_mig = sum(avg_dom_mig),
            num_Grp_quarters = sum(num_Grp_quarters),
            AGE_ge20le64 = sum(AGE_ge20le64),
            AGE_ge65 = sum(AGE_ge65),
            AGE_le19 = sum(AGE_le19),
            maskprop_NEVER = mean(maskprop_NEVER),
            maskprop_RARELY = mean(maskprop_RARELY),
            maskprop_SOMETIMES = mean(maskprop_SOMETIMES),
            maskprop_FREQUENTLY = mean(maskprop_FREQUENTLY),
            maskprop_ALWAYS = mean(maskprop_ALWAYS)
  )

####################################################################################################
## write data files
####################################################################################################
write.csv(covid_data, 
          file = paste0(data_dir,"curated_data_county.csv"),
          row.names=FALSE)

write.csv(covid_data_state, 
          file = paste0(data_dir,"curated_data_state.csv"),
          row.names=FALSE)
















