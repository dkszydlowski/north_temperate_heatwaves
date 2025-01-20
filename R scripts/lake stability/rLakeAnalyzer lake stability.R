# calculate Lake Number over time

# https://rdrr.io/cran/rLakeAnalyzer/man/ts.lake.number.html

# going to try to use the ts.lake.number function
# need wtr, a dataframe of water temperatures in C, load.ts
    # needs columns that are datetime and wtr_##.# where ##.# is depth in m
# wnd is a dataframe of wind speeds in m/s, using load.ts
# wnd.height is the height of the anemometer above the lake surface
# bathy contains hypsometric data

library(tidyverse)
library(rLakeAnalyzer)
library(ggpubr)
library(corrplot)
library(zoo)


##### Bathymetry #####
bathy = load.bathy("./formatted data/bathymetric data/Tuesday_bathy_formatted_for_rLakeAnalyzer_underc_data.csv")
bathy = bathy %>% filter(!is.na(depths))

bathyR = load.bathy("./formatted data/bathymetric data/Peter_bathy_formatted_for_rLakeAnalyzer_underc_data.csv")
bathyR = bathyR %>% filter(!is.na(depths))

bathyL = load.bathy("./formatted data/bathymetric data/Paul_bathy_formatted_for_rLakeAnalyzer_underc_data.csv")
bathyL = bathyL %>% filter(!is.na(depths))

bathyT = load.bathy("./formatted data/bathymetric data/Tuesday_bathy_formatted_for_rLakeAnalyzer_underc_data.csv")
bathyT = bathyT %>% filter(!is.na(depths))


# ##### Wind #####
# # wnd.height --> Need to ask NEON, but for now the height of the buoy is about 2 m
# wnd.height = 2
# 
# # load in the wind data
# wind22 = read.csv("./formatted data/met data/Crampton_Wind_2022.csv")
# 
# # function to process the NEON wind data into the format we want
# 
# NEON2lakeAnalyzer = function(wind, wnd.height, year){
# 
# # Need to match up the timescales of the temperature and the wind data, so
# # will take hourly mean because wind is every half hour
# wind = wind %>% select(startDateTime, endDateTime, windSpeedMean)
# 
# # convert startDateTime and endDateTime to CST by subtracting 6 hours
# wind = wind %>% mutate(startDateTime = ymd_hms(startDateTime), endDateTime = ymd_hms(endDateTime))
# wind = wind %>% mutate(startDateTime = startDateTime - hours(6), endDateTime = endDateTime - hours(6))
# 
# # create doy, year, and hour columns
# wind = wind %>% mutate(doy = yday(startDateTime), hour = hour(startDateTime), year = year(startDateTime))
# 
# # summarize the data by hour to match the temperature data
# wind_hourly = wind %>% group_by(doy, hour) %>% summarize(mean = mean(windSpeedMean))
# wind_hourly = wind_hourly %>% mutate(datetime =  make_datetime(year, 1, 1, hour, 0, 0) + days(doy - 1))
# 
# wind_hourly = wind_hourly %>% ungroup %>% 
#   select(datetime, mean) %>% 
#   rename(wnd = mean)
# 
# # make sure the datetime is formatted properly
# wind_hourly = wind_hourly %>% mutate(datetime = format(datetime, "%Y-%m-%d %H:%M:%S"))
# 
# return(wind_hourly)
# }

# 
# # save the hourly wind data for 2022
# wind_hourly22 = NEON2lakeAnalyzer(wind22, wnd.height, 2022)
# write.table(wind_hourly22, "./formatted data/met data/Crampton_wind_for_rLakeAnalyzer_2022.csv", sep = "\t", row.names = FALSE)
# 
# # format 2023 wind data
# wind23 = read.csv("./formatted data/met data/Crampton_Wind_2023.csv")
# 
# # save hourly wind data for 2023
# wind_hourly23 = NEON2lakeAnalyzer(wind23, wnd.height, 2023)
# write.table(wind_hourly23, "./formatted data/met data/Crampton_wind_for_rLakeAnalyzer_2023.csv", sep = "\t", row.names = FALSE)




### Stability for 2013-2015 ####

# make a function which calculates stability for LRT using the data between 2013 and 2015
# along with Sparkling Lake temperature 

 # Tuesday 2013
Ao = bathyT$areas[1]
wtr = load.ts("./formatted data/LRT temp chains rLakeAnalyzer/Tuesday_temperature_2013_rLakeAnalyzer.csv", tz = "America/Chicago")

schmidt_T13 = ts.schmidt.stability(wtr, bathy, na.rm = FALSE)

ggplot(schmidt_T13, aes(x = datetime, y = (schmidt.stability)))+
  geom_line(linewidth = 1, color = "darkorchid4")+
  theme_classic()+
  labs(title = "Tuesday 2013 Schmidt Stability")+
  ylab('Schmidt Stability')+
  xlab('date')

# # compare schmidt stability to water temp
# schmidt_T13 = schmidt_T13 %>% full_join(wtr, by = "datetime")
# 
# ggplot(schmidt_T13, aes(x = wtr_2.0, y = schmidt.stability))+
#          geom_point()


# Tuesday 2014
Ao = bathyT$areas[1]
wtr = load.ts("./formatted data/LRT temp chains rLakeAnalyzer/Tuesday_temperature_2014_rLakeAnalyzer.csv", tz = "America/Chicago")

schmidt_T14 = ts.schmidt.stability(wtr, bathy, na.rm = FALSE)

ggplot(schmidt_T14, aes(x = datetime, y = (schmidt.stability)))+
  geom_line(linewidth = 1, color = "darkorchid4")+
  theme_classic()+
  labs(title = "Tuesday 2014 Schmidt Stability")+
  ylab('Schmidt Stability')+
  xlab('date')





# Tuesday 2015
Ao = bathyT$areas[1]
wtr = load.ts("./formatted data/LRT temp chains rLakeAnalyzer/Tuesday_temperature_2015_rLakeAnalyzer.csv", tz = "America/Chicago")

# approximate missing values in wtr
wtr = wtr %>% mutate(across(where(~ !inherits(., "POSIXct")), ~ na.approx(., na.rm = FALSE, rule = 2)))


schmidt_T15 = ts.schmidt.stability(wtr, bathy, na.rm = FALSE)

# replace with NA where date was initially missing
schmidt_T15 = schmidt_T15 %>%
  mutate(schmidt.stability = replace(schmidt.stability, datetime >= "2015-07-15 04:00:00" & datetime <= "2015-07-17 13:00:00", NA))


ggplot(schmidt_T15, aes(x = datetime, y = (schmidt.stability)))+
  geom_line(linewidth = 1, color = "darkorchid4")+
  theme_classic()+
  labs(title = "Tuesday 2015 Schmidt Stability")+
  ylab('Schmidt Stability')+
  xlab('date')




# Paul 2013
Ao = bathyL$areas[1]
wtr = load.ts("./formatted data/LRT temp chains rLakeAnalyzer/Paul_temperature_2013_rLakeAnalyzer.csv", tz = "America/Chicago")

# approximate missing values in wtr
wtr = wtr %>% mutate(across(where(~ !inherits(., "POSIXct")), ~ na.approx(., na.rm = FALSE, rule = 2)))

schmidt_L13 = ts.schmidt.stability(wtr, bathy, na.rm = FALSE)


ggplot(schmidt_L13, aes(x = datetime, y = (schmidt.stability)))+
  geom_line(linewidth = 1, color = "darkorchid4")+
  theme_classic()+
  labs(title = "Paul 2013 Schmidt Stability")+
  ylab('Schmidt Stability')+
  xlab('date')


# Paul 2014
Ao = bathyL$areas[1]
wtr = load.ts("./formatted data/LRT temp chains rLakeAnalyzer/Paul_temperature_2014_rLakeAnalyzer.csv", tz = "America/Chicago")

# approximate missing values in wtr
wtr = wtr %>% mutate(across(where(~ !inherits(., "POSIXct")), ~ na.approx(., na.rm = FALSE, rule = 2)))

schmidt_L14 = ts.schmidt.stability(wtr, bathy, na.rm = FALSE)


ggplot(schmidt_L14, aes(x = datetime, y = (schmidt.stability)))+
  geom_line(linewidth = 1, color = "darkorchid4")+
  theme_classic()+
  labs(title = "Paul 2014 Schmidt Stability")+
  ylab('Schmidt Stability')+
  xlab('date')



# Paul 2015
Ao = bathyL$areas[1]
wtr = load.ts("./formatted data/LRT temp chains rLakeAnalyzer/Paul_temperature_2015_rLakeAnalyzer.csv", tz = "America/Chicago")

# approximate missing values in wtr
wtr = wtr %>% mutate(across(where(~ !inherits(., "POSIXct")), ~ na.approx(., na.rm = FALSE, rule = 2)))

schmidt_L15 = ts.schmidt.stability(wtr, bathy, na.rm = FALSE)


ggplot(schmidt_L15, aes(x = datetime, y = (schmidt.stability)))+
  geom_line(linewidth = 1, color = "darkorchid4")+
  theme_classic()+
  labs(title = "Paul 2015 Schmidt Stability")+
  ylab('Schmidt Stability')+
  xlab('date')



# Paul 2018
Ao = bathyL$areas[1]
wtr = load.ts("./formatted data/LRT temp chains rLakeAnalyzer/Paul_temperature_2018_rLakeAnalyzer.csv", tz = "America/Chicago")

# approximate missing values in wtr
wtr = wtr %>% mutate(across(where(~ !inherits(., "POSIXct")), ~ na.approx(., na.rm = FALSE, rule = 2)))

schmidt_L18 = ts.schmidt.stability(wtr, bathy, na.rm = FALSE)


ggplot(schmidt_L18, aes(x = datetime, y = (schmidt.stability)))+
  geom_line(linewidth = 1, color = "darkorchid4")+
  theme_classic()+
  labs(title = "Paul 2018 Schmidt Stability")+
  ylab('Schmidt Stability')+
  xlab('date')






# Peter 2013
Ao = bathyR$areas[1]
wtr = load.ts("./formatted data/LRT temp chains rLakeAnalyzer/Peter_temperature_2013_rLakeAnalyzer.csv", tz = "America/Chicago")

# approximate missing values in wtr
wtr = wtr %>% mutate(across(where(~ !inherits(., "POSIXct")), ~ na.approx(., na.rm = FALSE, rule = 2)))

schmidt_R13 = ts.schmidt.stability(wtr, bathy, na.rm = FALSE)


ggplot(schmidt_R13, aes(x = datetime, y = (schmidt.stability)))+
  geom_line(linewidth = 1, color = "darkorchid4")+
  theme_classic()+
  labs(title = "Peter 2013 Schmidt Stability")+
  ylab('Schmidt Stability')+
  xlab('date')


# Peter 2014
Ao = bathyR$areas[1]
wtr = load.ts("./formatted data/LRT temp chains rLakeAnalyzer/Peter_temperature_2014_rLakeAnalyzer.csv", tz = "America/Chicago")

# approximate missing values in wtr
wtr = wtr %>% mutate(across(where(~ !inherits(., "POSIXct")), ~ na.approx(., na.rm = FALSE, rule = 2)))

schmidt_R14 = ts.schmidt.stability(wtr, bathy, na.rm = FALSE)


ggplot(schmidt_R14, aes(x = datetime, y = (schmidt.stability)))+
  geom_line(linewidth = 1, color = "darkorchid4")+
  theme_classic()+
  labs(title = "Peter 2014 Schmidt Stability")+
  ylab('Schmidt Stability')+
  xlab('date')



# Peter 2015
Ao = bathyR$areas[1]
wtr = load.ts("./formatted data/LRT temp chains rLakeAnalyzer/Peter_temperature_2015_rLakeAnalyzer.csv", tz = "America/Chicago")

# approximate missing values in wtr
wtr = wtr %>% mutate(across(where(~ !inherits(., "POSIXct")), ~ na.approx(., na.rm = FALSE, rule = 2)))

schmidt_R15 = ts.schmidt.stability(wtr, bathy, na.rm = FALSE)


ggplot(schmidt_R15, aes(x = datetime, y = (schmidt.stability)))+
  geom_line(linewidth = 1, color = "darkorchid4")+
  theme_classic()+
  labs(title = "Peter 2015 Schmidt Stability")+
  ylab('Schmidt Stability')+
  xlab('date')



# Peter 2018
Ao = bathyR$areas[1]
wtr = load.ts("./formatted data/LRT temp chains rLakeAnalyzer/Peter_temperature_2018_rLakeAnalyzer.csv", tz = "America/Chicago")

# approximate missing values in wtr
wtr = wtr %>% mutate(across(where(~ !inherits(., "POSIXct")), ~ na.approx(., na.rm = FALSE, rule = 2)))

schmidt_R18 = ts.schmidt.stability(wtr, bathy, na.rm = FALSE)


ggplot(schmidt_R18, aes(x = datetime, y = (schmidt.stability)))+
  geom_line(linewidth = 1, color = "darkorchid4")+
  theme_classic()+
  labs(title = "Peter 2018 Schmidt Stability")+
  ylab('Schmidt Stability')+
  xlab('date')





# Peter 2019
Ao = bathyR$areas[1]
wtr = load.ts("./formatted data/LRT temp chains rLakeAnalyzer/Peter_temperature_2019_rLakeAnalyzer.csv", tz = "America/Chicago")

# approximate missing values in wtr
wtr = wtr %>% mutate(across(where(~ !inherits(., "POSIXct")), ~ na.approx(., na.rm = FALSE, rule = 2)))

schmidt_R19 = ts.schmidt.stability(wtr, bathy, na.rm = FALSE)


ggplot(schmidt_R19, aes(x = datetime, y = (schmidt.stability)))+
  geom_line(linewidth = 1, color = "darkorchid4")+
  theme_classic()+
  labs(title = "Peter 2019 Schmidt Stability")+
  ylab('Schmidt Stability')+
  xlab('date')




