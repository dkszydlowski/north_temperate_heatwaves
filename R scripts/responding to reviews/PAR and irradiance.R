### combine all of the different Cascade PAR datasets ###

library(tidyverse)

# investigate whether PAR is higher during heatwaves--is it sunnier? ####

# read in the datasets and format

#### 2008-2011 ####

# Package ID: knb-lter-ntl.360.2 Cataloging System:https://pasta.edirepository.org.
# Data set title: Cascade Project at North Temperate Lakes LTER High Frequency Sonde Data from Food Web Resilience Experiment 2008 - 2011.
# Data set creator:  Stephen Carpenter - University of Wisconsin 
# Data set creator:  Mike Pace - University of Virginia 
# Data set creator:  Jon Cole - Cary Institute of Ecosystem Studies 
# Data set creator:  Ryan Batt - Rutgers University 
# Data set creator:  Cal Buelo - University of Virginia 
# Data set creator:  Jason Kurzweil - University of Wisconsin 
# Contact:  Stephen Carpenter -  University of Wisconsin  - steve.carpenter@wisc.edu
# Contact:    -  NTL LTER  - ntl.infomgr@gmail.com
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/360/2/592195c90b43a36ba3f09cd99021225e" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "lake",     
                 "year",     
                 "doy",     
                 "datetime",     
                 "wind",     
                 "par",     
                 "zmix",     
                 "stemp",     
                 "chl",     
                 "pH",     
                 "doSat"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$lake)!="factor") dt1$lake<- as.factor(dt1$lake)
if (class(dt1$year)=="factor") dt1$year <-as.numeric(levels(dt1$year))[as.integer(dt1$year) ]               
if (class(dt1$year)=="character") dt1$year <-as.numeric(dt1$year)
if (class(dt1$doy)=="factor") dt1$doy <-as.numeric(levels(dt1$doy))[as.integer(dt1$doy) ]               
if (class(dt1$doy)=="character") dt1$doy <-as.numeric(dt1$doy)                                   
# attempting to convert dt1$datetime dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d %H:%M:%S" 
tmp1datetime<-as.POSIXct(dt1$datetime,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt1[dt1$datetime != "",]) == length(tmp1datetime[!is.na(tmp1datetime)])){dt1$datetime <- tmp1datetime } else {print("Date conversion failed for dt1$datetime. Please inspect the data and do the date conversion yourself.")}                                                                    

if (class(dt1$wind)=="factor") dt1$wind <-as.numeric(levels(dt1$wind))[as.integer(dt1$wind) ]               
if (class(dt1$wind)=="character") dt1$wind <-as.numeric(dt1$wind)
if (class(dt1$par)=="factor") dt1$par <-as.numeric(levels(dt1$par))[as.integer(dt1$par) ]               
if (class(dt1$par)=="character") dt1$par <-as.numeric(dt1$par)
if (class(dt1$zmix)=="factor") dt1$zmix <-as.numeric(levels(dt1$zmix))[as.integer(dt1$zmix) ]               
if (class(dt1$zmix)=="character") dt1$zmix <-as.numeric(dt1$zmix)
if (class(dt1$stemp)=="factor") dt1$stemp <-as.numeric(levels(dt1$stemp))[as.integer(dt1$stemp) ]               
if (class(dt1$stemp)=="character") dt1$stemp <-as.numeric(dt1$stemp)
if (class(dt1$chl)=="factor") dt1$chl <-as.numeric(levels(dt1$chl))[as.integer(dt1$chl) ]               
if (class(dt1$chl)=="character") dt1$chl <-as.numeric(dt1$chl)
if (class(dt1$pH)=="factor") dt1$pH <-as.numeric(levels(dt1$pH))[as.integer(dt1$pH) ]               
if (class(dt1$pH)=="character") dt1$pH <-as.numeric(dt1$pH)
if (class(dt1$doSat)=="factor") dt1$doSat <-as.numeric(levels(dt1$doSat))[as.integer(dt1$doSat) ]               
if (class(dt1$doSat)=="character") dt1$doSat <-as.numeric(dt1$doSat)

# Convert Missing Values to NA for non-dates

dt1$wind <- ifelse((trimws(as.character(dt1$wind))==trimws("NA")),NA,dt1$wind)               
suppressWarnings(dt1$wind <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$wind))==as.character(as.numeric("NA"))),NA,dt1$wind))
dt1$par <- ifelse((trimws(as.character(dt1$par))==trimws("NA")),NA,dt1$par)               
suppressWarnings(dt1$par <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$par))==as.character(as.numeric("NA"))),NA,dt1$par))
dt1$zmix <- ifelse((trimws(as.character(dt1$zmix))==trimws("NA")),NA,dt1$zmix)               
suppressWarnings(dt1$zmix <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$zmix))==as.character(as.numeric("NA"))),NA,dt1$zmix))
dt1$stemp <- ifelse((trimws(as.character(dt1$stemp))==trimws("NA")),NA,dt1$stemp)               
suppressWarnings(dt1$stemp <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$stemp))==as.character(as.numeric("NA"))),NA,dt1$stemp))
dt1$chl <- ifelse((trimws(as.character(dt1$chl))==trimws("NA")),NA,dt1$chl)               
suppressWarnings(dt1$chl <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$chl))==as.character(as.numeric("NA"))),NA,dt1$chl))
dt1$pH <- ifelse((trimws(as.character(dt1$pH))==trimws("NA")),NA,dt1$pH)               
suppressWarnings(dt1$pH <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$pH))==as.character(as.numeric("NA"))),NA,dt1$pH))
dt1$doSat <- ifelse((trimws(as.character(dt1$doSat))==trimws("NA")),NA,dt1$doSat)               
suppressWarnings(dt1$doSat <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$doSat))==as.character(as.numeric("NA"))),NA,dt1$doSat))


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(lake)
summary(year)
summary(doy)
summary(datetime)
summary(wind)
summary(par)
summary(zmix)
summary(stemp)
summary(chl)
summary(pH)
summary(doSat) 
# Get more details on character variables

summary(as.factor(dt1$lake))
detach(dt1)               

par1 = dt1

# select relevant columns
par1 = par1 %>% select(lake, year, doy, datetime, par)

# format dateimte
par1 = par1 %>% mutate(datetime = as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S"))

par1 = par1 %>% filter(lake == "Peter") %>% 
  select(-lake)

par1 = par1 %>% mutate(doy = yday(datetime))

#### 2013-2015 ####
par2 = read.csv("./formatted data/PAR/weather_and_tchain_2013_2014_2015.csv")

# select relevant columns and rename
par2 = par2 %>% rename(datetime = DateTime, year = Year, lake = Lake, doy = RoundDoY, par = PAR) %>% 
  select(lake, year, doy, datetime, par)

# format datetime
par2 = par2 %>% mutate(datetime = as.POSIXct(datetime, format = "%m/%d/%Y %H:%M"))

# all the PAR data is from Peter Lake, so filter to Peter
par2 = par2 %>% filter(lake == "R") %>% 
  select(-lake)


#### 2018 ####
par18 = read.csv("./formatted data/PAR/UNDERC_22August2018_Peter_Lake_2018.csv", header = FALSE)

par18 = par18 %>% select(-V1)

par18 = par18 %>% select(V2, V3)

names(par18) = c("datetime", "par")

par18 = par18[3:nrow(par18), ]

# format columns
par18 = par18 %>% mutate(datetime = as.POSIXct(datetime, format = "%m/%d/%y %I:%M:%S %p"),
                         par = as.numeric(par))

par18 = par18 %>% mutate(year = year(datetime), doy = yday(datetime)) %>% 
  select(year, doy, datetime, par)


#### 2019 ####
par19 = read.csv("./formatted data/PAR/UNDERC_-_Peter_Lake_06September2019.csv", header = FALSE)

par19 = par19 %>% select(-V1)

par19 = par19 %>% select(V2, V3)

names(par19) = c("datetime", "par")

par19 = par19[3:nrow(par19), ]

# format columns
par19 = par19 %>% mutate(datetime = as.POSIXct(datetime, format = "%m/%d/%y %I:%M:%S %p"),
                         par = as.numeric(par))

par19 = par19 %>% mutate(year = year(datetime), doy = yday(datetime)) %>% 
  select(year, doy, datetime, par)



par.all = rbind(par1, par2, par18, par19)


## take the average of datetime par
daily.par = par.all %>% mutate(hour = hour(datetime)) %>% 
  filter(hour >= 8 & hour <= 16) %>% 
  group_by(doy, year) %>% 
  summarize(mean.par = mean(par, na.rm = TRUE)) %>% 
  ungroup()


ggplot(daily.par, aes(x = doy, y = mean.par, color = as.factor(year)))+
  geom_line(size = 1)+
  facet_wrap(~year)+
  theme_bw()



rw.par <- daily.par %>%
  group_by(year) %>%  # Group by year
  arrange(doy, .by_group = TRUE) %>%  # Ensure data is ordered by day of year within each year
  mutate(rw.avg.par = rollmean(mean.par, k = 7, fill = NA, align = "right")) %>%  # 3-day rolling average
  ungroup()



ggplot(rw.par, aes(x = doy, y = rw.avg.par, color = as.factor(year)))+
  geom_line(size = 1)+
  facet_wrap(~year)+
  theme_bw()


## plot the heatwaves onto PAR ##
# use the heatwaves from Peter lake #

hw.par = read.csv("./formatted data/master explanatory dataset/heatwaves explained var5.csv")


hw.par = hw.par %>% filter(lake == "R")

hw.par <- hw.par %>%
  mutate(doy_start = yday(date_start), doy_end = yday(date_end)) %>%
  select(year, doy_start, doy_end)

ggplot(rw.par, aes(x = doy, y = rw.avg.par, color = as.factor(year))) +
  geom_line(size = 1) +
  geom_rect(
    data = hw.par,
    aes(xmin = doy_start, xmax = doy_end, ymin = -Inf, ymax = Inf, fill = as.factor(year)),
    inherit.aes = FALSE,
    alpha = 0.5
  ) +
  facet_wrap(~year) +
  theme_bw() +
  labs(fill = "Year") 

hw.par = read.csv("./formatted data/master explanatory dataset/heatwaves explained var5.csv")


### get the mean par before, during, and after each heatwave ###
# add the the master explanatory variables dataset #
# hw.par = read.csv("./results/heatwave modeled outputs/heatwave events LRT.csv")


hw.par = hw.par %>% mutate(par.before.hw = NA, par.during.hw = NA, par.after.hw = NA)

hw.par = hw.par %>% mutate(doy = yday(date_start))

for(i in 1:nrow(hw.par)){
  
  # save the start date of current heatwave
  start.date = hw.par$date_start[i]
  start.doy = hw.par$doy[i]
  end.doy = yday(hw.par$date_end[i])
  print(start.date)
  
  # save the lake of the current heatwave
  targ.year = hw.par$year[i]
  
  # filter the explanatory dataframe to match
  # make sure doy is <= the heatwave day
  daily.par.cur = daily.par %>% filter(year == targ.year)
  
  
  daily.par.before = daily.par.cur %>% filter(doy >= start.doy - 6 & doy <= start.doy)
  daily.par.during = daily.par.cur %>% filter(doy >= start.doy & doy <= end.doy)
  daily.par.after = daily.par.cur %>% filter(doy > end.doy & doy <= end.doy + 6)
  
  
  if(nrow(daily.par.during) > 0){
    par.during.hw = mean(daily.par.during$mean.par, na.rm = TRUE)
    hw.par$mean.par.during[i] = par.during.hw
    
    par.during.hw = mean(daily.par.during$mean.par, na.rm = TRUE)
    hw.par$mean.par.during[i] = par.during.hw
  }
  
  if(nrow(daily.par.after) > 0){
    par.after.hw = mean(daily.par.after$mean.par, na.rm = TRUE)
    hw.par$mean.par.after[i] = par.after.hw
    
    par.after.hw = mean(daily.par.after$mean.par, na.rm = TRUE)
    hw.par$mean.par.after[i] = par.after.hw
    
  }
  
  if(nrow(daily.par.before) > 0){
    par.before.hw = mean(daily.par.before$mean.par, na.rm = TRUE)
    hw.par$mean.par.before[i] = par.before.hw
    
    par.before.hw = mean(daily.par.before$mean.par, na.rm = TRUE)
    hw.par$mean.par.before[i] = par.before.hw
  }
  
  
  
  
}





ggplot(hw.par, aes(x = mean.par.before, y = percentChange, color = lake))+
  geom_point(size = 3)+
  theme_classic()

write.csv(hw.par, "./formatted data/master explanatory dataset/heatwaves explained var6.csv", row.names = FALSE)








### make a longer version and plot ####

hw.par.long = hw.par %>% pivot_longer(cols = c("mean.par.before", "mean.par.during", "mean.par.after"),
                                            values_to = "par", names_to = "period")


ggplot(hw.par.long, aes(x = period, y = par))+
  geom_boxplot()+
  facet_wrap(~year)




#### figure out if there is a lag between PAR and lake temperature
# read in the lake temperature data

# read in the temperature data
allSonde = read.csv("./formatted data/CombinedData.csv") %>% 
  mutate(doy = yday(date))

# join the par to the allSonde

allSonde = allSonde %>% left_join(rw.par, by = c("year", "doy"))

ggplot(allSonde %>% filter(lake == "R"), aes(x = mean_temp, y = rw.avg.par))+
  geom_point()



ccf(na.approx(allSonde$mean_temp), na.approx(allSonde$rw.avg.par))


