### Try other definitions of heatwaves ###

### Heatwave calculation with different thresholds ####


### 95th percentile ###


#####Calculate and Plot Heatwaves for Peter, Paul, and Tuesday Lakes#####

## this script produces three outputs:
# saveRDS(paulHW, file = "./results/heatwave modeled outputs/paul heatwave outputs actual data.rds")
# saveRDS(peterHW, file = "./results/heatwave modeled outputs/peter heatwave outputs actual data.rds")
# saveRDS(tuesdayHW, file = "./results/heatwave modeled outputs/tuesday heatwave outputs actual data.rds")

# load in packages
library("tidyverse")
library("ggridges")
library("lubridate")
library("zoo")
library(heatwaveR)

# CombinedData comes from "step0_DataCleaning.R", which combines all sonde data
# we are just using temperature data from this set, which is from the hydrolabs
allSonde = read.csv("./formatted data/CombinedData.csv")
allSonde$date = as.Date(allSonde$date)

paul = allSonde %>%
  filter(lake == "L") %>% 
  rename(t = date, temp = mean_temp)

peter = allSonde %>%
  filter(lake == "R") %>% 
  rename(t = date, temp = mean_temp)

tuesday = allSonde %>%
  filter(lake == "T") %>% 
  rename(t = date, temp = mean_temp)


paulHWinput = paul %>% select(t, temp)

peterHWinput = peter %>% select(t, temp)

tuesdayHWinput = tuesday %>% select(t, temp)


##### Calculate heatwave events #####
climOutputL = ts2clm(paulHWinput, climatologyPeriod = c(min(paulHWinput$t), max(paulHWinput$t)))
paulHW = detect_event(climOutputL)

# check the output from the heatwave code
event_line(paulHW, metric = "intensity_max", start_date = "2010-06-01", end_date = "2010-09-15")+
  geom_point()+
  xlim(xmin = as.Date("2010-06-01"), xmax = as.Date("2010-09-15"))+
  theme_classic()
# geom_density_line(aes(stat = "identity", size = 0.5, fill = "steelblue3", alpha = 0.3))


climOutputR = ts2clm(peterHWinput, climatologyPeriod = c(min(peterHWinput$t), max(peterHWinput$t)))
peterHW = detect_event(climOutputR)

event_line(peterHW, metric = "intensity_max", start_date = "2018-06-01", end_date = "2018-09-15")+
  geom_point()+
  xlim(as.Date("2018-05-01"), as.Date("2018-09-01"))

# the Tuesday data throws an error because technically there aren't three years of data
# just add a dummy day that is NA to bypass that error-- We have three summers of data

tuesdayHWinput[nrow(tuesdayHWinput)+1, ] = NA
tuesdayHWinput[nrow(tuesdayHWinput), 1] = as.Date("2016-06-01")


climOutputT = ts2clm(tuesdayHWinput, climatologyPeriod = c(min(tuesdayHWinput$t), max(tuesdayHWinput$t)))
tuesdayHW = detect_event(climOutputT)

event_line(tuesdayHW, metric = "intensity_max", start_date = "2013-06-01", end_date = "2013-09-15")+
  geom_point()+
  xlim(as.Date("2013-05-01"), as.Date("2013-09-01"))


##### Clean up the output #####
peterHW$event$lake = "R" 
paulHW$event$lake = "L"
tuesdayHW$event$lake = "T"
heatwaves = rbind(peterHW$event, paulHW$event, tuesdayHW$event)

# create a year column
heatwaves = heatwaves %>%
  mutate(year = year(date_peak)) 

# create a lake_year column
heatwaves = heatwaves %>%
  mutate(lake_year = paste(lake, year, sep = "_")) 

##### Save the final heatwave data #####
#write.csv(heatwaves, "./formatted data/heatwavesdata.csv", row.names = FALSE)


# assess which lakes and years we currently have sonde temp data for
allSonde = allSonde %>% mutate(lake.year = paste(lake, year, sep = "_"))

# 
# ##### Save climatology from actual data #####
# saveRDS(paulHW, file = "./results/heatwave modeled outputs/paul heatwave outputs actual data.rds")
# saveRDS(peterHW, file = "./results/heatwave modeled outputs/peter heatwave outputs actual data.rds")
# saveRDS(tuesdayHW, file = "./results/heatwave modeled outputs/tuesday heatwave outputs actual data.rds")









# Ensure date columns are in Date format
heatwaves$date_start <- as.Date(heatwaves$date_start)
heatwaves$date_end <- as.Date(heatwaves$date_end)
heatwaves.exp$date_start <- as.Date(heatwaves.exp$date_start)
heatwaves.exp$date_end <- as.Date(heatwaves.exp$date_end)

# Function to check if two date ranges overlap
check_overlap <- function(start1, end1, start2, end2) {
  !(end1 < start2 || end2 < start1)
}

# Identify heatwaves in `heatwaves` that do not overlap with any in `heatwaves.exp`
non_overlapping_hw <- heatwaves[!apply(heatwaves, 1, function(hw) {
  any(apply(heatwaves.exp, 1, function(hw_exp) {
    check_overlap(hw["date_start"], hw["date_end"], hw_exp["date_start"], hw_exp["date_end"])
  }))
}), ]

# Identify heatwaves in `heatwaves.exp` that do not overlap with any in `heatwaves`
non_overlapping_hw_exp <- heatwaves.exp[!apply(heatwaves.exp, 1, function(hw_exp) {
  any(apply(heatwaves, 1, function(hw) {
    check_overlap(hw_exp["date_start"], hw_exp["date_end"], hw["date_start"], hw["date_end"])
  }))
}), ]

# Results
list(
  heatwaves_non_overlapping = non_overlapping_hw,
  heatwaves_exp_non_overlapping = non_overlapping_hw_exp
)













# Ensure date columns are in Date format
heatwaves$date_start <- as.Date(heatwaves$date_start)
heatwaves$date_end <- as.Date(heatwaves$date_end)
heatwaves.exp$date_start <- as.Date(heatwaves.exp$date_start)
heatwaves.exp$date_end <- as.Date(heatwaves.exp$date_end)


# Function to check if two date ranges overlap
check_overlap <- function(start1, end1, start2, end2) {
  !(end1 < start2 || end2 < start1)
}

# Identify non-overlapping heatwaves in `heatwaves`
non_overlapping_hw <- heatwaves[!apply(heatwaves, 1, function(hw) {
  any(apply(heatwaves.exp, 1, function(hw_exp) {
    check_overlap(hw["date_start"], hw["date_end"], hw_exp["date_start"], hw_exp["date_end"])
  }))
}), ]

# Identify non-overlapping heatwaves in `heatwaves.exp`
non_overlapping_hw_exp <- heatwaves.exp[!apply(heatwaves.exp, 1, function(hw_exp) {
  any(apply(heatwaves, 1, function(hw) {
    check_overlap(hw_exp["date_start"], hw_exp["date_end"], hw["date_start"], hw["date_end"])
  }))
}), ]

# Combine results into a single dataframe with a source column
non_overlapping_combined <- rbind(
  cbind(non_overlapping_hw, source = "heatwaves"),
  cbind(non_overlapping_hw_exp, source = "heatwaves.exp")
)

# View results
non_overlapping_combined

