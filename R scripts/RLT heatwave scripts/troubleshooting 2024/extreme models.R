#' How well do we model the 90th percentile of events?
#' Are the baseline temperatures modeled from Sparkling Lake
#' and air temperature any good at modeling the extremes?
#' 

# load packages
library(tidyverse)


# read in the modeled temp data from Sparkling Lake and air temperature, along with 
# the heatwaves identified
# Sparkling Lake modeled climatology
peterHW = readRDS(file = "./results/heatwave modeled outputs/peter heatwave outputs modeled.rds")
paulHW = readRDS(file = "./results/heatwave modeled outputs/paul heatwave outputs modeled.rds")
tuesdayHW = readRDS(file = "./results/heatwave modeled outputs/tuesday heatwave outputs modeled.rds")

climatology.R = peterHW$climatology %>% mutate(lake = "peter")
climatology.L = paulHW$climatology %>% mutate(lake = "paul")
climatology.T = tuesdayHW$climatology %>% mutate(lake = "tuesday")

climatology.all = rbind(climatology.R, climatology.L, climatology.T) %>% mutate(model = "Sparkling Lake temp 1m model")

# read in the actual temp data
allSonde = read.csv("./formatted data/CombinedData.csv")
allSonde$date = as.Date(allSonde$date)


names(allSonde)
names(climatology.all)

climatology.all = climatology.all %>% rename(doyCat = doy, date = t) %>% 
  mutate(year = year(date))

climatology.all = climatology.all %>% 
  mutate(lake = replace(lake, lake == "peter", "R")) %>% 
  mutate(lake = replace(lake, lake == "paul", "L")) %>% 
  mutate(lake = replace(lake, lake == "tuesday", "T"))

allSonde = allSonde %>% rename(sonde.temp = mean_temp)

allSonde = allSonde %>% select(lake, year, date, doyCat, sonde.temp)

allSonde = allSonde %>% full_join(climatology.all, by = c("lake", "year", "date", "doyCat"))
