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


##### Bathymetry #####
bathy = load.bathy("./bathymetric data/Tuesday_bathy_formatted_for_rLakeAnalyzer_underc_data.csv")
bathy = bathy %>% filter(!is.na(depths))

bathyR = load.bathy("./bathymetric data/Peter_bathy_formatted_for_rLakeAnalyzer_underc_data.csv")
bathyR = bathyR %>% filter(!is.na(depths))

bathyL = load.bathy("./bathymetric data/Paul_bathy_formatted_for_rLakeAnalyzer_underc_data.csv")
bathyL = bathyL %>% filter(!is.na(depths))

bathyT = load.bathy("./bathymetric data/Tuesday_bathy_formatted_for_rLakeAnalyzer_underc_data.csv")
bathyT = bathyT %>% filter(!is.na(depths))


##### Wind #####
# wnd.height --> Need to ask NEON, but for now the height of the buoy is about 2 m
wnd.height = 2

# load in the wind data
wind22 = read.csv("./formatted data/met data/Crampton_Wind_2022.csv")

# function to process the NEON wind data into the format we want

NEON2lakeAnalyzer = function(wind, wnd.height, year){

# Need to match up the timescales of the temperature and the wind data, so
# will take hourly mean because wind is every half hour
wind = wind %>% select(startDateTime, endDateTime, windSpeedMean)

# convert startDateTime and endDateTime to CST by subtracting 6 hours
wind = wind %>% mutate(startDateTime = ymd_hms(startDateTime), endDateTime = ymd_hms(endDateTime))
wind = wind %>% mutate(startDateTime = startDateTime - hours(6), endDateTime = endDateTime - hours(6))

# create doy, year, and hour columns
wind = wind %>% mutate(doy = yday(startDateTime), hour = hour(startDateTime), year = year(startDateTime))

# summarize the data by hour to match the temperature data
wind_hourly = wind %>% group_by(doy, hour) %>% summarize(mean = mean(windSpeedMean))
wind_hourly = wind_hourly %>% mutate(datetime =  make_datetime(year, 1, 1, hour, 0, 0) + days(doy - 1))

wind_hourly = wind_hourly %>% ungroup %>% 
  select(datetime, mean) %>% 
  rename(wnd = mean)

# make sure the datetime is formatted properly
wind_hourly = wind_hourly %>% mutate(datetime = format(datetime, "%Y-%m-%d %H:%M:%S"))

return(wind_hourly)
}


# save the hourly wind data for 2022
wind_hourly22 = NEON2lakeAnalyzer(wind22, wnd.height, 2022)
write.table(wind_hourly22, "./formatted data/met data/Crampton_wind_for_rLakeAnalyzer_2022.csv", sep = "\t", row.names = FALSE)

# format 2023 wind data
wind23 = read.csv("./formatted data/met data/Crampton_Wind_2023.csv")

# save hourly wind data for 2023
wind_hourly23 = NEON2lakeAnalyzer(wind23, wnd.height, 2023)
write.table(wind_hourly23, "./formatted data/met data/Crampton_wind_for_rLakeAnalyzer_2023.csv", sep = "\t", row.names = FALSE)


##### Temperature2022 format #####
tempT = read.csv("./formatted data/tuesday temperature/tuesday_hourly_unclean.csv")
tempL = read.csv("./formatted data/temperature 2022/paul 2022 temperature/paul 2022 temperature hourly.csv")

# reformat so that the depths have decimal places
tempT$depth = as.character(format(tempT$depth, nsmall = 1))
tempL$depth = as.character(format(tempL$depth, nsmall = 1))

# need to pivot_wider by depth
tempT = tempT %>% 
  pivot_wider(names_from = depth, values_from = temp, names_prefix = "wtr_")

tempT = tempT %>% rename(datetime = date_time)

tempL = tempL %>% 
  pivot_wider(names_from = depth, values_from = temp, names_prefix = "wtr_")

tempL = tempL %>% rename(datetime = date.time)


# fix issue where hour 1 does not show up in date time
tempT$month = month(tempT$datetime)
tempT = tempT %>% mutate(datetime = make_datetime(2022, 1, 1, hour, 0, 0) + days(doy - 1))

tempL$month = month(tempL$datetime)
tempL = tempL %>% mutate(datetime = make_datetime(2022, 1, 1, hour, 0, 0) + days(doy - 1))

# remove doy, month, and hour columns
tempT = tempT %>% select(-doy, -hour, -month)
tempT = tempT %>% mutate(datetime = format(datetime, "%Y-%m-%d %H:%M:%S"))

tempL = tempL %>% select(-doy, -hour, -month)
tempL = tempL %>% mutate(datetime = format(datetime, "%Y-%m-%d %H:%M:%S"))

write.table(tempT, "./formatted data/tuesday temperature/tuesday_temp_rLakeAnalyzer.csv", row.names = FALSE, sep = "\t")
write.table(tempT, "./formatted data/temperature rLakeAnalyzer/Tuesday_temperature_2022_rLakeAnalyzer.csv", row.names = FALSE, sep = "\t")
write.table(tempL, "./formatted data/temperature 2022/paul 2022 temperature/paul_temp_rLakeAnalyzer.csv", row.names = FALSE, sep = "\t")

### calculate Lake Number and stability ###
# loading and formatting for rLakeAnalyzer #

# A0 = area of the lake
Ao = bathyL$areas[1]

wnd = load.ts("./formatted data/met data/Crampton_wind_for_rLakeAnalyzer_2022.csv", tz = "America/Chicago")
wtr = load.ts("./formatted data/temperature 2022/paul 2022 temperature/paul_temp_rLakeAnalyzer.csv", tz = "America/Chicago")

ts.lake.number(wtr, wnd, wnd.height, bathy, seasonal = TRUE)

LN = ts.lake.number(wtr, wnd, wnd.height, bathy, seasonal = FALSE)

schmidt = ts.schmidt.stability(wtr, bathy, na.rm = FALSE)

wedder = ts.wedderburn.number(wtr, wnd, wnd.height, bathy, Ao, seasonal = TRUE)

# calculate wedderburn number for the last week or so of the dataset, when 3.0 m is not available
#wtrLateSeason = wtr %>% filter(is.na(wtr_3.0) & !(datetime == "2022-06-06 14:00:00")) %>% select(-wtr_3.0)
#write.table(wtrLateSeason, "./formatted data/tuesday temperature/tuesday_temp_rLakeAnalyzer_lateSeason.csv", row.names = FALSE, sep = "\t")
#wtrLateSeason = load.ts("./formatted data/tuesday temperature/tuesday_temp_rLakeAnalyzer_lateSeason.csv", tz = "America/Chicago")

wedderLate = ts.wedderburn.number(wtrLateSeason, wnd, wnd.height, bathy, Ao, seasonal = TRUE)

wedder = wedder %>% filter(!is.na(wedderburn.number))
wedder = rbind(wedder, wedderLate)

#### Plot the stability measures over time
ggplot(LN, aes(x = datetime, y = log10(lake.number)))+
  geom_line()

ggplot(schmidt, aes(x = datetime, y = schmidt.stability))+
  geom_line()

#png("./figures/lake stability/wedderburn_Paul.png", width = 11, height = 6, units = "in", res = 600)
ggplot(wedder, aes(x = datetime, y = log10(wedderburn.number)))+
  geom_line(size = 1, color = "darkorchid4")+
  geom_hline(yintercept = 0.69897, color = "steelblue4", size = 1, linetype = "dashed")+
  theme_classic()+
  labs(title = "Paul Wedderburn number")+
  ylab('log(Wedderburn Number)')+
  xlab('date')

#dev.off()

### Format the wedder data more
wedder = wedder %>% mutate(doy = yday(datetime))

# add the wind so I can compare
wedder = wedder %>% left_join(wnd, by = "datetime")
wedderEvents = wedder %>% filter(wedderburn.number <= 5)

# create a table with the number of entrainment events, how many hours they lasted




#### calculate wedderburn number for 2023 for LRT ####

##### Temperature 2023 format #####
tempT23 = read.csv("./formatted data/temperature 2023/Tuesday/hourly_temperature_Tuesday_2023.csv")
tempL23 = read.csv("./formatted data/temperature 2023/Paul/hourly_temperature_Paul_2023.csv")
tempR23 =  read.csv("./formatted data/temperature 2023/Peter/hourly_temperature_Peter_2023.csv")

# reformat so that the depths have decimal places
tempT23$depth = as.character(format(tempT23$depth, nsmall = 1))
tempL23$depth = as.character(format(tempL23$depth, nsmall = 1))
tempR23$depth = as.character(format(tempR23$depth, nsmall = 1))

# need to pivot_wider by depth
tempT23 = tempT23 %>% 
  pivot_wider(names_from = depth, values_from = temp, names_prefix = "wtr_")

tempT23 = tempT23 %>% rename(datetime = date_time)

tempL23 = tempL23 %>% 
  pivot_wider(names_from = depth, values_from = temp, names_prefix = "wtr_")

tempL23 = tempL23 %>% rename(datetime = date_time)

tempR23 = tempR23 %>% 
  pivot_wider(names_from = depth, values_from = temp, names_prefix = "wtr_")

tempR23 = tempR23 %>% rename(datetime = date_time)

# fix issue where hour 1 does not show up in date time
tempT23$month = month(tempT23$datetime)
tempT23 = tempT23 %>% mutate(datetime = make_datetime(2023, 1, 1, hour, 0, 0) + days(doy - 1))

tempL23$month = month(tempL23$datetime)
tempL23 = tempL23 %>% mutate(datetime = make_datetime(2023, 1, 1, hour, 0, 0) + days(doy - 1))

tempR23$month = month(tempR23$datetime)
tempR23 = tempR23 %>% mutate(datetime = make_datetime(2023, 1, 1, hour, 0, 0) + days(doy - 1))

# remove doy, month, and hour columns
tempT23 = tempT23 %>% select(-doy, -hour, -month)
tempT23 = tempT23 %>% mutate(datetime = format(datetime, "%Y-%m-%d %H:%M:%S"))

tempL23 = tempL23 %>% select(-doy, -hour, -month)
tempL23 = tempL23 %>% mutate(datetime = format(datetime, "%Y-%m-%d %H:%M:%S"))

tempR23 = tempR23 %>% select(-doy, -hour, -month)
tempR23 = tempR23 %>% mutate(datetime = format(datetime, "%Y-%m-%d %H:%M:%S"))

write.table(tempT23, "./formatted data/temperature 2023/Tuesday/tuesday_temp_rLakeAnalyzer_2023.csv", row.names = FALSE, sep = "\t")
write.table(tempL23, "./formatted data/temperature 2023/Paul/paul_temp_rLakeAnalyzer_2023.csv", row.names = FALSE, sep = "\t")
write.table(tempR23, "./formatted data/temperature 2023/Peter/peter_temp_rLakeAnalyzer_2023.csv", row.names = FALSE, sep = "\t")

write.table(tempT23, "./formatted data/temperature rLakeAnalyzer/Tuesday_temperature_2023_rLakeAnalyzer.csv", row.names = FALSE, sep = "\t")
write.table(tempL23, "./formatted data/temperature rLakeAnalyzer/Paul_temperature_2023_rLakeAnalyzer.csv", row.names = FALSE, sep = "\t")
write.table(tempR23, "./formatted data/temperature rLakeAnalyzer/Peter_temperature_2023_rLakeAnalyzer.csv", row.names = FALSE, sep = "\t")


#### Peter 2023 stability ####
Ao = bathyR$areas[1]
wnd = load.ts("./formatted data/met data/Crampton_wind_for_rLakeAnalyzer_2023.csv")
wtr = load.ts("./formatted data/temperature 2023/Peter/peter_temp_rLakeAnalyzer_2023.csv", tz = "America/Chicago")

wnd <- wnd %>%
  filter(datetime %in% wtr$datetime)

wtr <- wtr %>%
  filter(datetime %in% wnd$datetime)

LN_R23 = ts.lake.number(wtr, wnd, wnd.height, bathy, seasonal = FALSE)

schmidt_R23 = ts.schmidt.stability(wtr, bathy, na.rm = FALSE)

wedder_R23 = ts.wedderburn.number(wtr, wnd, wnd.height, bathy, Ao, seasonal = FALSE)

r23 = ggplot(wedder_R23, aes(x = datetime, y = log10(wedderburn.number)))+
  geom_line(size = 0.7, color = "darkorchid4")+
  geom_hline(yintercept = 0.69897, color = "steelblue4", size = 1, linetype = "dashed")+
  theme_classic()+
  labs(title = "Peter Wedderburn number 2023")+
  ylab('log(Wedderburn Number)')+
  xlab('date')+ylim(0,5)

#### Paul 2023 stability ####
Ao = bathyL$areas[1]
wnd = load.ts("./formatted data/met data/Crampton_wind_for_rLakeAnalyzer_2023.csv")
wtr = load.ts("./formatted data/temperature 2023/Paul/paul_temp_rLakeAnalyzer_2023.csv", tz = "America/Chicago")
wtr = wtr %>% select(-wtr_4.5)

wnd <- wnd %>%
  filter(datetime %in% wtr$datetime)

wtr <- wtr %>%
  filter(datetime %in% wnd$datetime)

LN_L23 = ts.lake.number(wtr, wnd, wnd.height, bathy, seasonal = FALSE)

schmidt_L23 = ts.schmidt.stability(wtr, bathy, na.rm = FALSE)

wedder_L23 = ts.wedderburn.number(wtr, wnd, wnd.height, bathy, Ao, seasonal = FALSE)

l23 = ggplot(wedder_L23, aes(x = datetime, y = log10(wedderburn.number)))+
  geom_line(size = 0.7, color = "darkorchid4")+
  geom_hline(yintercept = 0.69897, color = "steelblue4", size = 1, linetype = "dashed")+
  theme_classic()+
  labs(title = "Paul Wedderburn number 2023")+
  ylab('log(Wedderburn Number)')+
  xlab('date')+
  ylim(0, 5)

#### Tuesday 2023 stability ####
Ao = bathyT$areas[1]
wnd = load.ts("./formatted data/met data/Crampton_wind_for_rLakeAnalyzer_2023.csv")
wtr = load.ts("./formatted data/temperature 2023/Tuesday/tuesday_temp_rLakeAnalyzer_2023.csv", tz = "America/Chicago")

wnd <- wnd %>%
  filter(datetime %in% wtr$datetime)

wtr <- wtr %>%
  filter(datetime %in% wnd$datetime)

LN_T23 = ts.lake.number(wtr, wnd, wnd.height, bathy, seasonal = FALSE)

schmidt_T23 = ts.schmidt.stability(wtr, bathy, na.rm = FALSE)

wedder_T23 = ts.wedderburn.number(wtr, wnd, wnd.height, bathy, Ao, seasonal = FALSE)

t23 = ggplot(wedder_T23, aes(x = datetime, y = log10(wedderburn.number)))+
  geom_line(size = 0.7, color = "darkorchid4")+
  geom_hline(yintercept = 0.69897, color = "steelblue4", size = 1, linetype = "dashed")+
  theme_classic()+
  labs(title = "Tuesday Wedderburn number 2023")+
  ylab('log(Wedderburn Number)')+
  xlab('date')+
  ylim(0, 5)

png("./figures/lake stability/2023 wedderburn.png", width = 6, height = 10, units = "in", res = 600)
ggarrange(r23, l23, t23, ncol = 1, nrow = 3)
dev.off()

### combine the wedderburn numbers for 2023 into a single dataframe
wedder23 = full_join(wedder_L23, wedder_R23, by = "datetime")
wedder23 = full_join(wedder23, wedder_T23, by = "datetime")

wedder23 = wedder23 %>% rename(wedderT = wedderburn.number, wedderR = wedderburn.number.y, wedderL = wedderburn.number.x)

cor(wedder23[,c("wedderL", "wedderR", "wedderT")], use = "complete.obs", method = "spearman")




### Stability for 2013-2015 ####

# make a function which calculates stability for LRT using the data between 2013 and 2015
# along with Sparkling Lake temperature 

 # Tuesday 2013
Ao = bathyT$areas[1]
wnd = load.ts("./formatted data/met data/wind_for_rLakeAnalyzer_2013.csv")
wtr = load.ts("./formatted data/temperature 2013-2021/Tuesday_temperature_2013_rLakeAnalyzer.csv", tz = "America/Chicago")

wnd <- wnd %>%
  filter(datetime %in% wtr$datetime)

wtr <- wtr %>%
  filter(datetime %in% wnd$datetime)

LN_T13 = ts.lake.number(wtr, wnd, wnd.height, bathy, seasonal = FALSE)

schmidt_T13 = ts.schmidt.stability(wtr, bathy, na.rm = FALSE)

wedder_T13 = ts.wedderburn.number(wtr, wnd, wnd.height, bathy, Ao, seasonal = FALSE)

png("./figures/lake stability/Tuesday 2013 wedderburn.png", width = 6, height = 4, units = "in", res = 600)
ggplot(wedder_T13, aes(x = datetime, y = log10(wedderburn.number)))+
  geom_line(linewidth = 0.7, color = "darkorchid4")+
  geom_hline(yintercept = 0.69897, color = "steelblue4", size = 1, linetype = "dashed")+
  theme_classic()+
  labs(title = "Tuesday Wedderburn number 2013")+
  ylab('log(Wedderburn Number)')+
  xlab('date')+
  ylim(0, 5)

dev.off()



# Tuesday 2014
Ao = bathyT$areas[1]
wnd = load.ts("./formatted data/met data/wind_for_rLakeAnalyzer_2014.csv")
wtr = load.ts("./formatted data/temperature 2013-2021/Tuesday_temperature_2014_rLakeAnalyzer.csv", tz = "America/Chicago")

wnd <- wnd %>%
  filter(datetime %in% wtr$datetime)

wtr <- wtr %>%
  filter(datetime %in% wnd$datetime)

LN_T14 = ts.lake.number(wtr, wnd, wnd.height, bathy, seasonal = FALSE)

schmidt_T14 = ts.schmidt.stability(wtr, bathy, na.rm = FALSE)

wedder_T14 = ts.wedderburn.number(wtr, wnd, wnd.height, bathy, Ao, seasonal = FALSE)

png("./figures/lake stability/Tuesday 2014 wedderburn.png", width = 6, height = 4, units = "in", res = 600)
ggplot(wedder_T14, aes(x = datetime, y = log10(wedderburn.number)))+
  geom_line(linewidth = 0.7, color = "darkorchid4")+
  geom_hline(yintercept = 0.69897, color = "steelblue4", size = 1, linetype = "dashed")+
  theme_classic()+
  labs(title = "Tuesday Wedderburn number 2014")+
  ylab('log(Wedderburn Number)')+
  xlab('date')+
  ylim(0, 5)

dev.off()



# Tuesday 2015
Ao = bathyT$areas[1]
wnd = load.ts("./formatted data/met data/wind_for_rLakeAnalyzer_2015.csv")
wtr = load.ts("./formatted data/temperature 2013-2021/Tuesday_temperature_2015_rLakeAnalyzer.csv", tz = "America/Chicago")

wnd <- wnd %>%
  filter(datetime %in% wtr$datetime)

wtr <- wtr %>%
  filter(datetime %in% wnd$datetime)

LN_T15 = ts.lake.number(wtr, wnd, wnd.height, bathy, seasonal = FALSE)

schmidt_T15 = ts.schmidt.stability(wtr, bathy, na.rm = FALSE)

wedder_T15 = ts.wedderburn.number(wtr, wnd, wnd.height, bathy, Ao, seasonal = FALSE)

png("./figures/lake stability/Tuesday 2015 wedderburn.png", width = 6, height = 4, units = "in", res = 600)
ggplot(wedder_T15, aes(x = datetime, y = log10(wedderburn.number)))+
  geom_line(linewidth = 0.7, color = "darkorchid4")+
  geom_hline(yintercept = 0.69897, color = "steelblue4", size = 1, linetype = "dashed")+
  theme_classic()+
  labs(title = "Tuesday Wedderburn number 2015")+
  ylab('log(Wedderburn Number)')+
  xlab('date')+
  ylim(0, 5)

dev.off()




# Peter 2013
Ao = bathyR$areas[1]
wnd = load.ts("./formatted data/met data/wind_for_rLakeAnalyzer_2013.csv")
wtr = load.ts("./formatted data/temperature 2013-2021/Peter_temperature_2013_rLakeAnalyzer.csv", tz = "America/Chicago")

wnd <- wnd %>%
  filter(datetime %in% wtr$datetime)

wtr <- wtr %>%
  filter(datetime %in% wnd$datetime)

LN_R13 = ts.lake.number(wtr, wnd, wnd.height, bathy, seasonal = FALSE)

schmidt_R13 = ts.schmidt.stability(wtr, bathy, na.rm = FALSE)

wedder_R13 = ts.wedderburn.number(wtr, wnd, wnd.height, bathy, Ao, seasonal = FALSE)

png("./figures/lake stability/Peter 2013 wedderburn.png", width = 6, height = 4, units = "in", res = 600)
ggplot(wedder_R13, aes(x = datetime, y = log10(wedderburn.number)))+
  geom_line(linewidth = 0.7, color = "darkorchid4")+
  geom_hline(yintercept = 0.69897, color = "steelblue4", size = 1, linetype = "dashed")+
  theme_classic()+
  labs(title = "Peter Wedderburn number 2013")+
  ylab('log(Wedderburn Number)')+
  xlab('date')+
  ylim(0, 5)

dev.off()




# Peter 2014
Ao = bathyR$areas[1]
wnd = load.ts("./formatted data/met data/wind_for_rLakeAnalyzer_2014.csv")
wtr = load.ts("./formatted data/temperature 2013-2021/Peter_temperature_2014_rLakeAnalyzer.csv", tz = "America/Chicago")

wnd <- wnd %>%
  filter(datetime %in% wtr$datetime)

wtr <- wtr %>%
  filter(datetime %in% wnd$datetime)

LN_R14 = ts.lake.number(wtr, wnd, wnd.height, bathy, seasonal = FALSE)

schmidt_R14 = ts.schmidt.stability(wtr, bathy, na.rm = FALSE)

wedder_R14 = ts.wedderburn.number(wtr, wnd, wnd.height, bathy, Ao, seasonal = FALSE)

png("./figures/lake stability/Peter 2014 wedderburn.png", width = 6, height = 4, units = "in", res = 600)
ggplot(wedder_R14, aes(x = datetime, y = log10(wedderburn.number)))+
  geom_line(linewidth = 0.7, color = "darkorchid4")+
  geom_hline(yintercept = 0.69897, color = "steelblue4", size = 1, linetype = "dashed")+
  theme_classic()+
  labs(title = "Peter Wedderburn number 2014")+
  ylab('log(Wedderburn Number)')+
  xlab('date')+
  ylim(0, 5)

dev.off()



# Peter 2015
Ao = bathyR$areas[1]
wnd = load.ts("./formatted data/met data/wind_for_rLakeAnalyzer_2015.csv")
wtr = load.ts("./formatted data/temperature 2013-2021/Peter_temperature_2015_rLakeAnalyzer.csv", tz = "America/Chicago")

wnd <- wnd %>%
  filter(datetime %in% wtr$datetime)

wtr <- wtr %>%
  filter(datetime %in% wnd$datetime)

LN_R15 = ts.lake.number(wtr, wnd, wnd.height, bathy, seasonal = FALSE)

schmidt_R15 = ts.schmidt.stability(wtr, bathy, na.rm = FALSE)

wedder_R15 = ts.wedderburn.number(wtr, wnd, wnd.height, bathy, Ao, seasonal = FALSE)

png("./figures/lake stability/Peter 2015 wedderburn.png", width = 6, height = 4, units = "in", res = 600)
ggplot(wedder_R15, aes(x = datetime, y = log10(wedderburn.number)))+
  geom_line(linewidth = 0.7, color = "darkorchid4")+
  geom_hline(yintercept = 0.69897, color = "steelblue4", size = 1, linetype = "dashed")+
  theme_classic()+
  labs(title = "Peter Wedderburn number 2015")+
  ylab('log(Wedderburn Number)')+
  xlab('date')+
  ylim(0, 5)

dev.off()






# Paul 2013
Ao = bathyL$areas[1]
wnd = load.ts("./formatted data/met data/wind_for_rLakeAnalyzer_2013.csv")
wtr = load.ts("./formatted data/temperature 2013-2021/Paul_temperature_2013_rLakeAnalyzer.csv", tz = "America/Chicago")

wnd <- wnd %>%
  filter(datetime %in% wtr$datetime)

wtr <- wtr %>%
  filter(datetime %in% wnd$datetime)

LN_l13 = ts.lake.number(wtr, wnd, wnd.height, bathy, seasonal = FALSE)

schmidt_l13 = ts.schmidt.stability(wtr, bathy, na.rm = FALSE)

wedder_l13 = ts.wedderburn.number(wtr, wnd, wnd.height, bathy, Ao, seasonal = FALSE)

png("./figures/lake stability/Paul 2013 wedderburn.png", width = 6, height = 4, units = "in", res = 600)
ggplot(wedder_l13, aes(x = datetime, y = log10(wedderburn.number)))+
  geom_line(linewidth = 0.7, color = "darkorchid4")+
  geom_hline(yintercept = 0.69897, color = "steelblue4", size = 1, linetype = "dashed")+
  theme_classic()+
  labs(title = "Paul Wedderburn number 2013")+
  ylab('log(Wedderburn Number)')+
  xlab('date')+
  ylim(0, 5)

dev.off()




# Paul 2014
Ao = bathyL$areas[1]
wnd = load.ts("./formatted data/met data/wind_for_rLakeAnalyzer_2014.csv")
wtr = load.ts("./formatted data/temperature 2013-2021/Paul_temperature_2014_rLakeAnalyzer.csv", tz = "America/Chicago")

wnd <- wnd %>%
  filter(datetime %in% wtr$datetime)

wtr <- wtr %>%
  filter(datetime %in% wnd$datetime)

LN_l14 = ts.lake.number(wtr, wnd, wnd.height, bathy, seasonal = FALSE)

schmidt_l14 = ts.schmidt.stability(wtr, bathy, na.rm = FALSE)

wedder_l14 = ts.wedderburn.number(wtr, wnd, wnd.height, bathy, Ao, seasonal = FALSE)

png("./figures/lake stability/Paul 2014 wedderburn.png", width = 6, height = 4, units = "in", res = 600)
ggplot(wedder_l14, aes(x = datetime, y = log10(wedderburn.number)))+
  geom_line(linewidth = 0.7, color = "darkorchid4")+
  geom_hline(yintercept = 0.69897, color = "steelblue4", size = 1, linetype = "dashed")+
  theme_classic()+
  labs(title = "Paul Wedderburn number 2014")+
  ylab('log(Wedderburn Number)')+
  xlab('date')+
  ylim(0, 5)

dev.off()



# Paul 2015
Ao = bathyL$areas[1]
wnd = load.ts("./formatted data/met data/Sparkling_wind_for_rLakeAnalyzer_2015.csv")
wtr = load.ts("./formatted data/temperature 2013-2021/Paul_temperature_2015_rLakeAnalyzer.csv", tz = "America/Chicago")

wnd <- wnd %>%
  filter(datetime %in% wtr$datetime)

wtr <- wtr %>%
  filter(datetime %in% wnd$datetime)

LN_l15 = ts.lake.number(wtr, wnd, wnd.height, bathy, seasonal = FALSE)

schmidt_l15 = ts.schmidt.stability(wtr, bathy, na.rm = FALSE)

wedder_l15 = ts.wedderburn.number(wtr, wnd, wnd.height, bathy, Ao, seasonal = TRUE)

png("./figures/lake stability/Paul 2015 wedderburn.png", width = 6, height = 4, units = "in", res = 600)
ggplot(wedder_l15, aes(x = datetime, y = log10(wedderburn.number)))+
  geom_line(linewidth = 0.7, color = "darkorchid4")+
  geom_hline(yintercept = 0.69897, color = "steelblue4", size = 1, linetype = "dashed")+
  theme_classic()+
  labs(title = "Paul Wedderburn number 2015")+
  ylab('log(Wedderburn Number)')+
  xlab('date')+
  ylim(0, 5)

dev.off()




#### STABILITY FUNCTION ####
##### Create a function which takes the stability metric, lake, year, and wind source, and calculates it
# also plots the stability metric over time. Data need to be in the rLakeAnalyzer format

# metric can be wedderburn.number, lake.number, or schmidt.stability
# lake can be Peter, Paul, or Tuesday, capitalized
# year can be 2013, 2014, 2015, 2016, 2018, or 2019
# wind.source is either Crampton or Sparkling
# seasonal says whether seasonal thermocline is used, usually false?

lake = "Paul"
year = 2018
wind.source = "Crampton"
wnd.height = 2
seasonal = TRUE


stability <- function(lake, year, wind.source, wnd.height, seasonal){
  
  lake_year = paste(lake, year, sep = "_")
  
  if(lake_year == "Paul_2023" | lake_year == "Tuesday_2022" | lake_year == "Tuesday_2015"){

    print(lake_year)
    
    # first, read in bathymetric information for the target lake
    bathy = read.csv(paste("./bathymetric data/", lake, "_bathy_formatted_for_rLakeAnalyzer_underc_data.csv", sep = ""))
    
    # remove empty rows of bathy
    bathy = bathy %>% filter(!is.na(depths))
    
    # get the area of the lake
    A0 = bathy$areas_m2[1] 
    
    # read in the wind data
    wind.file = paste("./formatted data/met data/", wind.source, "_wind_for_rLakeanalyzer_", year, ".csv", sep = "")
    wnd = load.ts(wind.file, tz = "America/Chicago")
    
    # read in the temperature data
    temp.file = paste("./formatted data/temperature rLakeAnalyzer/", lake, "_temperature_", year, "_rLakeAnalyzer_1.csv", sep = "")
    wtr1 = load.ts(temp.file, tz = "America/Chicago")
    
    temp.file = paste("./formatted data/temperature rLakeAnalyzer/", lake, "_temperature_", year, "_rLakeAnalyzer_2.csv", sep = "")
    wtr2 = load.ts(temp.file, tz = "America/Chicago")
    
    # get rid of extra rows
    wnd1 <- wnd %>%
      filter(datetime %in% wtr1$datetime)
    
    wtr1 <- wtr1 %>%
      filter(datetime %in% wnd$datetime)
    
    wnd2 <- wnd %>%
      filter(datetime %in% wtr2$datetime)
    
    wtr2 <- wtr2 %>%
      filter(datetime %in% wnd$datetime)
    
    wedder1 = ts.wedderburn.number(wtr1, wnd1, wnd.height, bathy, Ao, seasonal = seasonal)
    lake.number1 = ts.lake.number(wtr1, wnd1, wnd.height, bathy, seasonal = seasonal)
    
    wedder2 = ts.wedderburn.number(wtr2, wnd2, wnd.height, bathy, Ao, seasonal = seasonal)
    lake.number2 = ts.lake.number(wtr2, wnd2, wnd.height, bathy, seasonal = seasonal)
    
    wedder = rbind(wedder1, wedder2)
    
    lake.number = rbind(lake.number1, lake.number2)
    
    # remove rows with NA
    wedder = wedder %>% filter(!is.na(datetime))
    lake.number = lake.number %>% filter(!is.na(datetime))
    
    stability.output = full_join(wedder, lake.number, by = "datetime")
    
    stability.output$lake = lake
    stability.output$year = year
    stability.output$wind.source = wind.source
    stability.output$seasonal = seasonal
    
    
    
  } else{
  
  
  # first, read in bathymetric information for the target lake
  bathy = read.csv(paste("./bathymetric data/", lake, "_bathy_formatted_for_rLakeAnalyzer_underc_data.csv", sep = ""))
 
  # remove empty rows of bathy
  bathy = bathy %>% filter(!is.na(depths))
  
   # get the area of the lake
  A0 = bathy$areas_m2[1] 
  
  # read in the wind data
  wind.file = paste("./formatted data/met data/", wind.source, "_wind_for_rLakeanalyzer_", year, ".csv", sep = "")
  wnd = load.ts(wind.file, tz = "America/Chicago")
  
  # read in the temperature data
  temp.file = paste("./formatted data/temperature rLakeAnalyzer/", lake, "_temperature_", year, "_rLakeAnalyzer.csv", sep = "")
  wtr = load.ts(temp.file, tz = "America/Chicago")
  
  # get rid of extra rows
  wnd <- wnd %>%
    filter(datetime %in% wtr$datetime)
  
  wtr <- wtr %>%
    filter(datetime %in% wnd$datetime)
  
wedder = ts.wedderburn.number(wtr, wnd, wnd.height, bathy, Ao, seasonal = seasonal)
lake.number = ts.lake.number(wtr, wnd, wnd.height, bathy, seasonal = seasonal)

# remove rows with NA
wedder = wedder %>% filter(!is.na(datetime))
lake.number = lake.number %>% filter(!is.na(datetime))

  stability.output = full_join(wedder, lake.number, by = "datetime")
  
  stability.output$lake = lake
  stability.output$year = year
  stability.output$wind.source = wind.source
  stability.output$seasonal = seasonal

  print(lake_year)
  }
  
  return(stability.output)
  
}

test = stability(lake, year,
                 wind.source, wnd.height = 2, seasonal = TRUE)


ggplot(test, aes(x = datetime, y = log10(lake.number)))+
  geom_line(linewidth = 0.7, color = "darkorchid4")+
  geom_hline(yintercept = 0.69897, color = "steelblue4", size = 1, linetype = "dashed")+
  theme_classic()+
  labs(title = "Paul Wedderburn number")+
  ylab('log(Lake Number)')+
  xlab('date')+
  ylim(0, 5)


ggplot(test, aes(x = wedderburn.number, y = lake.number))+
  geom_point( color = "darkorchid4")+
  geom_hline(yintercept = 0.69897, color = "steelblue4", size = 1, linetype = "dashed")+
  theme_classic()+
  labs(title = "Paul Wedderburn number")+
  ylab('log(Wedderburn Number)')+
  xlab('date')

# run through every combination and save the wedderburn number and lake number for all



### Calculate stability measures in a for loop ###
combos = read.csv("./formatted data/profiles lake physics combinations.csv")

for(i in 1:nrow(combos)){
  lake = combos$lake[i]
  year = combos$year[i]
  wind.source = combos$wind[i]
  wnd.height = 2
  
  stability.measures = stability(lake, year, wind.source, wnd.height = 2, seasonal = TRUE)
  if(i == 1){stability.all = stability.measures}
  if(i > 1){stability.all = rbind(stability.all, stability.measures)}
  
}


#save the stability measures dataframe

##### Assess relationships of stability metrics under different conditions and lakes
write.csv(stability.all, "./formatted data/stability results/stability results all combinations.csv", row.names = FALSE)

### Start by making plots by year and by wind metric

pdf("./figures/lake stability/wedderburn all combinations.pdf", width = 14)

plot_list <- list()

for(i in 1:nrow(combos)){
  lake.targ = combos$lake[i]
  year.targ = combos$year[i]
  wind.source.targ = combos$wind[i]

  current = stability.all %>% filter(lake == lake.targ, year == year.targ, wind.source == wind.source.targ)
  
print(ggplot(current, aes(x = datetime, y = log10(wedderburn.number)))+
    geom_line(linewidth = 0.7, color = "darkorchid4")+
    geom_hline(yintercept = 0.69897, color = "steelblue4", size = 1, linetype = "dashed")+
      geom_hline(yintercept = 0, color = "steelblue4", size = 1, linetype = "dashed")+
    theme_classic()+
    labs(title = paste(lake.targ, year.targ, " Wedderburn number", " (", wind.source.targ,  " wind)", sep = ""))+
    ylab('log(Wedderburn Number)')+
    xlab('date')+
    ylim(-1, 5)+
    xlim(as.POSIXct(paste(year.targ, "-05-31 23:00:00", sep = "")), as.POSIXct(paste(year.targ, "-08-08 23:00:00", sep = ""))))
  
  # Add each plot to the list
  plot_list[[i]] <- p
  
}
dev.off()

#### PLOTS OF LAKE NUMBER ####

pdf("./figures/lake stability/lake number all combinations.pdf", width = 14)

plot_list <- list()

for(i in 1:nrow(combos)){
  lake.targ = combos$lake[i]
  year.targ = combos$year[i]
  wind.source.targ = combos$wind[i]
  
  current = stability.all %>% filter(lake == lake.targ, year == year.targ, wind.source == wind.source.targ)
  
  print(ggplot(current, aes(x = datetime, y = log10(lake.number)))+
          geom_line(linewidth = 0.7, color = "darkorchid4")+
          geom_hline(yintercept = 0.69897, color = "steelblue4", size = 1, linetype = "dashed")+
          geom_hline(yintercept = 0, color = "steelblue4", size = 1, linetype = "dashed")+
          theme_classic()+
          labs(title = paste(lake.targ, year.targ, " lake number", " (", wind.source.targ,  " wind)", sep = ""))+
          ylab('log(lake number)')+
          xlab('date')+
          ylim(-1, 5)+
          xlim(as.POSIXct(paste(year.targ, "-05-31 23:00:00", sep = "")), as.POSIXct(paste(year.targ, "-08-08 23:00:00", sep = ""))))
  
  # Add each plot to the list
  plot_list[[i]] <- p
  
}
dev.off()





# pivot_wider so that I can calculate correlations between combinations of years and wind
stab.all = stability.all %>% pivot_wider(names_from = c("lake", "year", "wind.source"), values_from = wedderburn.number)

stability.all.cramp = stability.all %>% filter(wind.source == "Crampton")
stability.all.spark = stability.all %>% filter(wind.source == "Sparkling")

stability.wider <- stability.all %>%
  pivot_wider(
    id_cols = datetime,
    names_from = c(lake, year, wind.source),
    values_from = wedderburn.number,
    names_glue = "{lake}_{year}_{wind.source}"
  ) %>%
  group_by(datetime) %>%
  summarize_all(list(~ sum(., na.rm = TRUE)))


stability.wider.cramp <- stability.all.cramp %>%
  pivot_wider(
    id_cols = datetime,
    names_from = c(lake, year, wind.source),
    values_from = wedderburn.number,
    names_glue = "{lake}_{year}_{wind.source}"
  ) %>%
  group_by(datetime) %>%
  summarize_all(list(~ sum(., na.rm = TRUE)))


stability.wider.spark <- stability.all.spark %>%
  pivot_wider(
    id_cols = datetime,
    names_from = c(lake, year, wind.source),
    values_from = wedderburn.number,
    names_glue = "{lake}_{year}_{wind.source}"
  ) %>%
  group_by(datetime) %>%
  summarize_all(list(~ sum(., na.rm = TRUE)))

stability.wider = stability.wider %>% replace(., . == 0, NA)
stability.wider = stability.wider %>% replace(., . == Inf, NA)

stability.wider.cramp = stability.wider.cramp %>% replace(., . == 0, NA)
stability.wider.spark = stability.wider.spark %>% replace(., . == 0, NA)

stability.wider.cramp = stability.wider.cramp %>% replace(., . == Inf, NA)
stability.wider.spark = stability.wider.spark %>% replace(., . == Inf, NA)

wedd.corr = cor(stability.wider[, -1], use = "pairwise.complete.obs")
corrplot(wedd.corr)

wedd.corr.cramp = cor(stability.wider.cramp[, -1], use = "pairwise.complete.obs")
corrplot(wedd.corr.cramp)

wedd.corr.spark = cor(stability.wider.spark[, -1], use = "pairwise.complete.obs")
corrplot(wedd.corr.spark)




## FIGURE OUT WHY SPARK 2022 IS NOT WORKING WITH COR
corrTesting = stability.wider.spark
corrTesting = corrTesting %>% select(datetime, Paul_2022_Sparkling, Tuesday_2022_Sparkling)

corrTesting = corrTesting %>% filter(!is.na(Paul_2022_Sparkling))

corrTesting = corrTesting %>% filter(log10(Paul_2022_Sparkling) < 3)

cor(corrTesting[, -1], use = "pairwise.complete.obs")

ggplot(corrTesting, aes(x = datetime, y = log10(Paul_2022_Sparkling)))+
  geom_line()+
  geom_line(aes(x = datetime, y = log10(Tuesday_2022_Sparkling)))+
  theme_classic()

# Gather the columns into a long format
stability_long <- stability.wider %>%
  gather(Column_Name, Value, -datetime) 




stability_long <- stability_long %>%
  separate(Column_Name, into = c("lake", "year", "wind.source"), sep = "_") %>% 
  mutate(Column_Name = paste(lake, year, wind.source, sep = "_"))


stability_long <- stability_long %>%
  mutate(order_var = paste(year, lake, wind.source, sep = "_"))

# Reorder the dataframe by the custom ordering variable
stability_long <- stability_long %>%
  arrange(order_var)

# Create a boxplot with reordered x-axis for Sparkling wind
png("./figures/lake stability/avg wedder box plots sparkling wind.png", height = 6, width = 10, res = 300, units = "in")
stability_long %>% filter(wind.source == "Sparkling") %>% 
ggplot(aes(x = order_var, y = log10(Value), fill = lake)) +
  geom_boxplot() +
  labs(
    x = "",
    y = "log10(Wedderburn Number)",
    title = "Average wedderburn number from Sparkling Lake wind data"
  ) +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_hline(yintercept = 0.69897, color = "black", size = 1, linetype = "dashed")+
  geom_hline(yintercept = 0, color = "black", size = 1, linetype = "dashed")+
  scale_fill_manual(values = c("Paul" = "#ADDAE3", "Peter"=  "#4AB5C4", "Tuesday"=  "#BAAD8D"))+
  ylim(-1, 6)
dev.off()



png("./figures/lake stability/avg wedder box plots crampton wind.png", height = 6, width = 7.5, res = 300, units = "in")
stability_long %>% filter(wind.source == "Crampton") %>% 
  ggplot(aes(x = order_var, y = log10(Value), fill = lake)) +
  geom_boxplot() +
  labs(
    x = "",
    y = "log10(Wedderburn Number)",
    title = "Average wedderburn number from Crampton Lake wind data"
  ) +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_hline(yintercept = 0.69897, color = "black", size = 1, linetype = "dashed")+
  geom_hline(yintercept = 0, color = "black", size = 1, linetype = "dashed")+
  scale_fill_manual(values = c("Paul" = "#ADDAE3", "Peter"=  "#4AB5C4", "Tuesday"=  "#BAAD8D"))+
  ylim(-1, 6)
  
dev.off()


# Plot only of years where we have both Crampton wind and Sparkling wind
# The only years when I have both wind datasets are 2019 and 2022
png("./figures/lake stability/avg wedder box plots wind comparison.png", height = 6, width = 7.5, res = 300, units = "in")

stability_long %>% filter(year == 2019 | year == 2022) %>% 
  ggplot(aes(x = order_var, y = log10(Value), fill = lake)) +
  geom_boxplot() +
  labs(
    x = "",
    y = "log10(Wedderburn Number)",
    title = "Crampton vs. Sparkling wind"
  ) +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_hline(yintercept = 0.69897, color = "black", size = 1, linetype = "dashed")+
  geom_hline(yintercept = 0, color = "black", size = 1, linetype = "dashed")+
  scale_fill_manual(values = c("Paul" = "#ADDAE3", "Peter"=  "#4AB5C4", "Tuesday"=  "#BAAD8D"))+
  ylim(-1, 6)

dev.off()
