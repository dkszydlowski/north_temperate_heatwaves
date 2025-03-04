#### Archive data prior to re-submission of paper to L+O letters #####


# goal of this script is to do final formatting and remove unused columns from 
# dataframes, keeping intact all data that was used in the final version of the manuscript

if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

##### 1. Manual chl and sonde temperature #####
chl = read.csv("./formatted data/full raw data manual and sonde chlorophyll.csv")

chl = chl %>% select(year, lake, date, doyCat, manual_chl, mean_temp) %>% 
  rename(chlorophyll = manual_chl, temperature = mean_temp, doy = doyCat, lakeid = lake) %>% 
  mutate(lake = NA) %>% 
  mutate(lake = replace(lake, lakeid == "L", "Paul")) %>% 
  mutate(lake = replace(lake, lakeid == "R", "Peter")) %>% 
  mutate(lake = replace(lake, lakeid == "T", "Tuesday")) %>% 
  select(year, lake, lakeid, date, doy, chlorophyll, temperature)

chl = chl %>% arrange(lake, year, date)

write.csv(chl, "./formatted data/FINAL DATA FOR PUBLICATION/chlorophyll and temperature 2008-2019.csv", row.names = FALSE)

##### 2. Heatwaves results and explanatory variables #####
hw.exp = read.csv("./formatted data/master explanatory dataset/heatwaves explained var6.csv")

# these are the explanatory variables that were used
# in analysis in our supplement
exp.vars = c("duration", "intensity_mean", "intensity_max", "intensity_var", "intensity_cumulative", 
             "intensity_mean_relThresh", "intensity_max_relThresh", "intensity_var_relThresh", "intensity_cumulative_relThresh", 
             "intensity_mean_abs", "intensity_max_abs", "intensity_var_abs", "intensity_cumulative_abs", 
             "rate_onset", "rate_decline", "precip", "PML.g440", "biomass", "biomass.during", "cumulative.load", "daily.load", 
             "stability.before", "stability.during", "stability.after", "daphnia.biomass.before", 
             "daphnia.biomass.during", "daphnia.biomass.after", "total.biomass.before", "total.biomass.during", "total.biomass.after", 
             "manual.chl.before", "manual.chl.during", "zoop.days.before", 
             "daphnia.length.before", "daphnia.length.during", 
             "daphnia.length.after","pchange.total.zoop", "abschange.total.zoop", "pchange.total.zoop.during.to.after", 
             "pchange.daphnia.length", "abschange.daphnia.zoop", "abschange.daphnia.zoop.during.to.after", "tp_ugL.before", 
             "tp_ugL.after", "tp_ugL.during","tn_ugL.before", 
             "tn_ugL.after", "tn_ugL.during")

cols.to.keep = c("lake", "year", "doy", "percentChange", "SE.percent", "date_start","date_peak", "date_end", exp.vars)

hw.exp = hw.exp %>% select(cols.to.keep)

hw.exp = hw.exp %>% rename(lakeid = lake) %>% 
  mutate(lake = NA) %>% 
  mutate(lake = replace(lake, lakeid == "L", "Paul")) %>% 
  mutate(lake = replace(lake, lakeid == "R", "Peter")) %>% 
  mutate(lake = replace(lake, lakeid == "T", "Tuesday"))

hw.exp = hw.exp %>% select(year, lake, lakeid, doy, everything())

ggplot(hw.exp, aes(x = biomass, y = daphnia.biomass.after))+
  geom_point()

write.csv(hw.exp, "./formatted data/FINAL DATA FOR PUBLICATION/heatwave results and covariates 2008-2019.csv", row.names = FALSE)


##### 3. Woodruff Airport and Sparkling Lake water temperature #####

# Sparkling Lake temperature
SP.temp = read.csv("./formatted data/LTER daily temperature/Sparkling Lake daily temperature all depths.csv")
# 
# SP.temp.1 = SP.temp %>% filter(depth == 0 | depth == 0.01 | (depth == 0.25 & year4 == 2013)) %>% 
#   rename(year = year4, date = sampledate, doy = daynum) %>% select(-depth, -flag_wtemp) %>% 
#   rename(SP.temp.1 = wtemp) %>% 
#   mutate(date = as.Date(date))

SP.temp.1 = SP.temp %>% filter(depth == 1) %>%
  dplyr::rename(year = year4, date = sampledate, doy = daynum) %>% select(-depth, -flag_wtemp) %>%
  dplyr::rename(SP.temp.1 = wtemp) %>%
  mutate(date = as.Date(date))


# Woodruff airport temperature
woodruff = read.csv("./formatted data/LTER daily temperature/woodruff airport temperature LTER.csv")

woodruff = woodruff %>% dplyr::rename(year = year4, date = sampledate, doy = daynum)

# make a daily woodruff dataframe
woodruff = woodruff %>% group_by(year, date, doy) %>% 
  dplyr::summarize(woodruff.temp = mean(avg_air_temp, na.rm = TRUE)) %>% 
  mutate(date = as.Date(date))


# combine woodruff, Sparkling, and LRT sonde temp
SP.woodruff = SP.temp.1 %>% left_join(woodruff, by = c("doy", "date", "year"))

write.csv(SP.woodruff, "./formatted data/FINAL DATA FOR PUBLICATION/water temp modeling Sparkling Lake and Woodruuf Airport 1989-2022.csv", row.names = FALSE)

