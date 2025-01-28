### calculate heatwaves throughout the water column to show that the
# heatwaves at 0.5 m depth are representative of the epilimnion


if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

if (!require(heatwaveR)) install.packages('heatwaveR')
library(heatwaveR)


### read in the temperature tchain data ###

# read in the sonde data to get the lake_years
allData = read.csv("./formatted data/interpolated_manual_chl_for_slopes.csv")

lake_years = unique(allData$lake_year)

# remove 2008, 2009, 2010, and 2011 because we don't have tchain data for those years
# lake_years = lake_years[-which(grepl("2009", lake_years))]
lake_years = lake_years[-which(grepl("2010", lake_years))]
# lake_years = lake_years[-which(grepl("2011", lake_years))]
lake_years = lake_years[-which(grepl("2008", lake_years))]
lake_years = lake_years[-which(grepl("L_2019", lake_years))]
# lake_years = lake_years[-which(grepl("T_2009", lake_years))]
# lake_years = lake_years[-which(grepl("T_2010", lake_years))]
# lake_years = lake_years[-which(grepl("T_2011", lake_years))]
# lake_years = lake_years[-which(grepl("NA", lake_years))]



for(i in 1:length(lake_years)){
  
  # get current iteration's lake and year
  cur.lake = strsplit(lake_years[i], "_")[[1]][1]
  cur.year = strsplit(lake_years[i], "_")[[1]][2]
  
  
  if(cur.lake == "L"){
    lake = "Paul"
  }
  if(cur.lake == "R"){
    
    lake = "Peter"
  }
  
  if(cur.lake == "T"){
    lake = "Tuesday"
  }
  
  wtr = load.ts(paste0("./formatted data/LRT temp chains rLakeAnalyzer/", lake, "_temperature_", cur.year, "_rLakeAnalyzer.csv" ))

  wtr = wtr %>% mutate(lake = cur.lake, year = cur.year)
  
  # remove two depths that were measured in 2018 and 2019 but not other years
  if(cur.year %in% c(2018, 2019)){
  wtr = wtr %>% select(-wtr_4.5, -wtr_6.0)
  }
  
  if(i == 1){wtr.all = wtr}
  if(i > 1){wtr.all = rbind(wtr.all, wtr)}
  
}


## create a daily average dataset ##
wtr.all = wtr.all %>% mutate(date = as.Date(datetime))

daily.tchain <- wtr.all %>%
  group_by(lake, date) %>%
  summarize(across(starts_with("wtr_"), mean, na.rm = TRUE)) %>% 
  filter(year != 2010)


### calculate heatwaves throughout the water column


### 0.5 m ###
r.daily.tchain0.5 = daily.tchain %>%
  filter(lake == "R") %>% 
  rename(t = date, temp = wtr_0.5) %>% 
  ungroup() %>% 
  select(t, temp) %>% 
  filter(!is.na(t))


##### Calculate heatwave events #####
climOutput = ts2clm(r.daily.tchain0.5, climatologyPeriod = c(min(r.daily.tchain0.5$t), max(r.daily.tchain0.5$t)))
rhw0.5 = detect_event(climOutput)$event %>% mutate(depth = 0.5)

### 1 m ###
r.daily.tchain1.0 = daily.tchain %>%
  filter(lake == "R") %>% 
  rename(t = date, temp = wtr_1.0) %>% 
  ungroup() %>% 
  select(t, temp) %>% 
  filter(!is.na(t))


##### Calculate heatwave events #####
climOutput = ts2clm(r.daily.tchain1.0, climatologyPeriod = c(min(r.daily.tchain1.0$t), max(r.daily.tchain1.0$t)))
rhw1.0 = detect_event(climOutput)$event %>% mutate(depth = 1.0)




### 1.5 m ###
r.daily.tchain1.5 = daily.tchain %>%
  filter(lake == "R") %>% 
  rename(t = date, temp = wtr_1.5) %>% 
  ungroup() %>% 
  select(t, temp) %>% 
  filter(!is.na(t))


##### Calculate heatwave events #####
climOutput = ts2clm(r.daily.tchain1.5, climatologyPeriod = c(min(r.daily.tchain1.5$t), max(r.daily.tchain1.5$t)))
rhw1.5 = detect_event(climOutput)$event %>% mutate(depth = 1.5)



### 2.0 m ###
r.daily.tchain2.0 = daily.tchain %>%
  filter(lake == "R") %>% 
  rename(t = date, temp = wtr_2.0) %>% 
  ungroup() %>% 
  select(t, temp) %>% 
  filter(!is.na(t))


##### Calculate heatwave events #####
climOutput = ts2clm(r.daily.tchain2.0, climatologyPeriod = c(min(r.daily.tchain2.0$t), max(r.daily.tchain2.0$t)))
rhw2.0 = detect_event(climOutput)$event %>% mutate(depth = 2.0)



### 2.5 m ###
r.daily.tchain2.5 = daily.tchain %>%
  filter(lake == "R") %>% 
  rename(t = date, temp = wtr_2.5) %>% 
  ungroup() %>% 
  select(t, temp) %>% 
  filter(!is.na(t))


##### Calculate heatwave events #####
climOutput = ts2clm(r.daily.tchain2.5, climatologyPeriod = c(min(r.daily.tchain2.5$t), max(r.daily.tchain2.5$t)))
rhw2.5 = detect_event(climOutput)$event %>% mutate(depth = 2.5)




### 3.0 m ###
r.daily.tchain3.0 = daily.tchain %>%
  filter(lake == "R") %>% 
  rename(t = date, temp = wtr_3.0) %>% 
  ungroup() %>% 
  select(t, temp) %>% 
  filter(!is.na(t))


##### Calculate heatwave events #####
climOutput = ts2clm(r.daily.tchain3.0, climatologyPeriod = c(min(r.daily.tchain3.0$t), max(r.daily.tchain3.0$t)))
rhw3.0 = detect_event(climOutput)$event %>% mutate(depth = 3.0)




### 3.5 m ###
r.daily.tchain3.5 = daily.tchain %>%
  filter(lake == "R") %>% 
  rename(t = date, temp = wtr_3.5) %>% 
  ungroup() %>% 
  select(t, temp) %>% 
  filter(!is.na(t))


##### Calculate heatwave events #####
climOutput = ts2clm(r.daily.tchain3.5, climatologyPeriod = c(min(r.daily.tchain3.5$t), max(r.daily.tchain3.5$t)))
rhw3.5 = detect_event(climOutput)$event %>% mutate(depth = 3.5)




### 4.0 m ###
r.daily.tchain4.0 = daily.tchain %>%
  filter(lake == "R") %>% 
  rename(t = date, temp = wtr_4.0) %>% 
  ungroup() %>% 
  select(t, temp) %>% 
  filter(!is.na(t))


##### Calculate heatwave events #####
climOutput = ts2clm(r.daily.tchain4.0, climatologyPeriod = c(min(r.daily.tchain4.0$t), max(r.daily.tchain4.0$t)))
rhw4.0 = detect_event(climOutput)$event %>% mutate(depth = 4.0)



### 5.0 m ###
r.daily.tchain5.0 = daily.tchain %>%
  filter(lake == "R") %>% 
  rename(t = date, temp = wtr_5.0) %>% 
  ungroup() %>% 
  select(t, temp) %>% 
  filter(!is.na(t))


##### Calculate heatwave events #####
climOutput = ts2clm(r.daily.tchain5.0, climatologyPeriod = c(min(r.daily.tchain5.0$t), max(r.daily.tchain5.0$t)))
rhw5.0 = detect_event(climOutput)$event %>% mutate(depth = 5.0)


## combine all of the heatwave events
r.hw.all.depths = rbind(rhw0.5, rhw1.0, rhw1.5, rhw2.0, rhw2.5, rhw3.0, rhw3.5, rhw4.0, rhw5.0)

# want a dataframe that is long, where the columns are
# lake, depth, year, doy, heatwave

# make a dataframe that has all the dates that temp data were available for R
r.hw.all.depths.plotting = daily.tchain %>% filter(lake == "R") %>%
  select(date) %>% mutate(year = year(date), doy = yday(date), heatwave = "no heatwave", depth = NA)


depths = c(0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 5.0)


for(i in 1:length(depths)){
  
  cur.depth = depths[i]
  
  cur.hws = r.hw.all.depths %>% filter(depth == cur.depth)
  
  for(j in 1:nrow(cur.hws)){
    cur.start.date = cur.hws$date_start[j]
    cur.end.date = cur.hws$date_end[j]
    
    r.hw.all.depths.plotting = r.hw.all.depths.plotting %>% 
      mutate(heatwave = replace(heatwave, date >= cur.start.date & date <= cur.end.date, "heatwave")) %>% 
      mutate(depth = cur.depth)
    
  }
  
  if(i == 1){
    r.hw.final = r.hw.all.depths.plotting
  }
  if(i > 1){
    r.hw.final = rbind(r.hw.final, r.hw.all.depths.plotting)
  }
  
  # reset the loop
  r.hw.all.depths.plotting$heatwave = "no heatwave"
  r.hw.all.depths.plotting$depth = NA
}



ggplot(r.hw.final, aes(x = doy, y = depth, fill = heatwave))+
  facet_wrap(~year)+
  geom_tile()+
  scale_y_reverse()






#### Paul heatwaves #####


### 0.5 m ###
l.daily.tchain0.5 = daily.tchain %>%
  filter(lake == "L") %>% 
  rename(t = date, temp = wtr_0.5) %>% 
  ungroup() %>% 
  select(t, temp) %>% 
  filter(!is.na(t))


##### Calculate heatwave events #####
climOutput = ts2clm(l.daily.tchain0.5, climatologyPeriod = c(min(l.daily.tchain0.5$t), max(l.daily.tchain0.5$t)))
lhw0.5 = detect_event(climOutput)$event %>% mutate(depth = 0.5)

### 1 m ###
l.daily.tchain1.0 = daily.tchain %>%
  filter(lake == "L") %>% 
  rename(t = date, temp = wtr_1.0) %>% 
  ungroup() %>% 
  select(t, temp) %>% 
  filter(!is.na(t))


##### Calculate heatwave events #####
climOutput = ts2clm(l.daily.tchain1.0, climatologyPeriod = c(min(l.daily.tchain1.0$t), max(l.daily.tchain1.0$t)))
lhw1.0 = detect_event(climOutput)$event %>% mutate(depth = 1.0)




### 1.5 m ###
l.daily.tchain1.5 = daily.tchain %>%
  filter(lake == "L") %>% 
  rename(t = date, temp = wtr_1.5) %>% 
  ungroup() %>% 
  select(t, temp) %>% 
  filter(!is.na(t))


##### Calculate heatwave events #####
climOutput = ts2clm(l.daily.tchain1.5, climatologyPeriod = c(min(l.daily.tchain1.5$t), max(l.daily.tchain1.5$t)))
lhw1.5 = detect_event(climOutput)$event %>% mutate(depth = 1.5)



### 2.0 m ###
l.daily.tchain2.0 = daily.tchain %>%
  filter(lake == "L") %>% 
  rename(t = date, temp = wtr_2.0) %>% 
  ungroup() %>% 
  select(t, temp) %>% 
  filter(!is.na(t))


##### Calculate heatwave events #####
climOutput = ts2clm(l.daily.tchain2.0, climatologyPeriod = c(min(l.daily.tchain2.0$t), max(l.daily.tchain2.0$t)))
lhw2.0 = detect_event(climOutput)$event %>% mutate(depth = 2.0)



### 2.5 m ###
l.daily.tchain2.5 = daily.tchain %>%
  filter(lake == "L") %>% 
  rename(t = date, temp = wtr_2.5) %>% 
  ungroup() %>% 
  select(t, temp) %>% 
  filter(!is.na(t))


##### Calculate heatwave events #####
climOutput = ts2clm(l.daily.tchain2.5, climatologyPeriod = c(min(l.daily.tchain2.5$t), max(l.daily.tchain2.5$t)))
lhw2.5 = detect_event(climOutput)$event %>% mutate(depth = 2.5)




### 3.0 m ###
l.daily.tchain3.0 = daily.tchain %>%
  filter(lake == "L") %>% 
  rename(t = date, temp = wtr_3.0) %>% 
  ungroup() %>% 
  select(t, temp) %>% 
  filter(!is.na(t))


##### Calculate heatwave events #####
climOutput = ts2clm(l.daily.tchain3.0, climatologyPeriod = c(min(l.daily.tchain3.0$t), max(l.daily.tchain3.0$t)))
lhw3.0 = detect_event(climOutput)$event %>% mutate(depth = 3.0)




### 3.5 m ###
l.daily.tchain3.5 = daily.tchain %>%
  filter(lake == "L") %>% 
  rename(t = date, temp = wtr_3.5) %>% 
  ungroup() %>% 
  select(t, temp) %>% 
  filter(!is.na(t))


##### Calculate heatwave events #####
climOutput = ts2clm(l.daily.tchain3.5, climatologyPeriod = c(min(l.daily.tchain3.5$t), max(l.daily.tchain3.5$t)))
lhw3.5 = detect_event(climOutput)$event %>% mutate(depth = 3.5)




### 4.0 m ###
l.daily.tchain4.0 = daily.tchain %>%
  filter(lake == "L") %>% 
  rename(t = date, temp = wtr_4.0) %>% 
  ungroup() %>% 
  select(t, temp) %>% 
  filter(!is.na(t))


##### Calculate heatwave events #####
climOutput = ts2clm(l.daily.tchain4.0, climatologyPeriod = c(min(l.daily.tchain4.0$t), max(l.daily.tchain4.0$t)))
lhw4.0 = detect_event(climOutput)$event %>% mutate(depth = 4.0)



### 5.0 m ###
l.daily.tchain5.0 = daily.tchain %>%
  filter(lake == "L") %>% 
  rename(t = date, temp = wtr_5.0) %>% 
  ungroup() %>% 
  select(t, temp) %>% 
  filter(!is.na(t))


##### Calculate heatwave events #####
climOutput = ts2clm(l.daily.tchain5.0, climatologyPeriod = c(min(l.daily.tchain5.0$t), max(l.daily.tchain5.0$t)))
lhw5.0 = detect_event(climOutput)$event %>% mutate(depth = 5.0)


## combine all of the heatwave events
l.hw.all.depths = rbind(lhw0.5, lhw1.0, lhw1.5, lhw2.0, lhw2.5, lhw3.0, lhw3.5, lhw4.0, lhw5.0)

# want a dataframe that is long, where the columns are
# lake, depth, year, doy, heatwave

# make a dataframe that has all the dates that temp data were available for R
l.hw.all.depths.plotting = daily.tchain %>% filter(lake == "L") %>%
  select(date) %>% mutate(year = year(date), doy = yday(date), heatwave = "no heatwave", depth = NA)


depths = c(0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 5.0)


for(i in 1:length(depths)){
  
  cur.depth = depths[i]
  
  cur.hws = l.hw.all.depths %>% filter(depth == cur.depth)
  
  for(j in 1:nrow(cur.hws)){
    cur.start.date = cur.hws$date_start[j]
    cur.end.date = cur.hws$date_end[j]
    
    l.hw.all.depths.plotting = l.hw.all.depths.plotting %>% 
      mutate(heatwave = replace(heatwave, date >= cur.start.date & date <= cur.end.date, "heatwave")) %>% 
      mutate(depth = cur.depth)
    
  }
  
  if(i == 1){
    l.hw.final = l.hw.all.depths.plotting
  }
  if(i > 1){
    l.hw.final = rbind(l.hw.final, l.hw.all.depths.plotting)
  }
  
  # reset the loop
  l.hw.all.depths.plotting$heatwave = "no heatwave"
  l.hw.all.depths.plotting$depth = NA
}



ggplot(l.hw.final, aes(x = doy, y = depth, fill = heatwave))+
  facet_wrap(~year)+
  geom_tile()+
  scale_y_reverse()

