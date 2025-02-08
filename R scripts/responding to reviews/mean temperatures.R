### identify mean temps during heatwaves ###

library(tidyverse)

# read in the heatwaves
heatwaves = read.csv("./results/heatwave modeled outputs/heatwave events LRT.csv")

heatwaves$date_end = ymd(heatwaves$date_end)
heatwaves$date_start = ymd(heatwaves$date_start)

# read in the manual chl data
allData = read.csv("./formatted data/interpolated_manual_chl_for_slopes.csv")
i = 1

allData$heatwave = "no heatwave"
allData$heatwave_index = 100

for(i in 1:nrow(heatwaves)){
  cur.lake = heatwaves$lake[i]
  cur.year = heatwaves$year[i]
  
  end.date = ymd(heatwaves$date_end[i])
  start.date = ymd(heatwaves$date_start[i])
  
  dates = seq(start.date, end.date, 1)
  
  allData = allData %>% mutate(heatwave = replace(heatwave, lake == cur.lake & year == cur.year & date %in% dates, "heatwave"))
  allData = allData %>% mutate(heatwave_index = replace(heatwave_index, lake == cur.lake & year == cur.year & date %in% dates, i))
  
}

# get the mean temperature by heatwave
mean.temp = allData %>% filter(heatwave_index != 100) %>% 
  group_by(lake, year, heatwave_index) %>% 
  summarize(mean.temp = mean(mean_temp, na.rm = TRUE),
            min.temp = min(mean_temp, na.rm = TRUE))




### get mean temperatures as profiles ###
### read in the routines temperature ####


# Package ID: knb-lter-ntl.352.4 Cataloging System:https://pasta.edirepository.org.
# Data set title: Cascade Project at North Temperate Lakes LTER Core Data Physical and Chemical Limnology 1984 - 2016.
# Data set creator:  Stephen Carpenter - University of Wisconsin 
# Data set creator:  Jim Kitchell - University of Wisconsin 
# Data set creator:  Jon Cole - Cary Institute of Ecosystem Studies 
# Data set creator:  Mike Pace - University of Virginia 
# Contact:  Stephen Carpenter -  University of Wisconsin  - steve.carpenter@wisc.edu
# Contact:  Mike Pace -  University of Virginia  - pacem@virginia.edu
# Contact:    -  NTL LTER  - ntl.infomgr@gmail.com
# Stylesheet v2.14 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu      
# Uncomment the following lines to have R clear previous work, or set a working directory
# rm(list=ls())      

# setwd("C:/users/my_name/my_dir")       



options(HTTPUserAgent="EDI_CodeGen")


inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/352/4/3f928dbd3989c95bc7146ee8363d69bd" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "lakeid",     
                 "lakename",     
                 "year4",     
                 "daynum",     
                 "sampledate",     
                 "depth",     
                 "temperature_C",     
                 "dissolvedOxygen",     
                 "irradianceWater",     
                 "irradianceDeck",     
                 "comments"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$lakeid)!="factor") dt1$lakeid<- as.factor(dt1$lakeid)
if (class(dt1$lakename)!="factor") dt1$lakename<- as.factor(dt1$lakename)
if (class(dt1$year4)=="factor") dt1$year4 <-as.numeric(levels(dt1$year4))[as.integer(dt1$year4) ]               
if (class(dt1$year4)=="character") dt1$year4 <-as.numeric(dt1$year4)
if (class(dt1$daynum)=="factor") dt1$daynum <-as.numeric(levels(dt1$daynum))[as.integer(dt1$daynum) ]               
if (class(dt1$daynum)=="character") dt1$daynum <-as.numeric(dt1$daynum)                                   
# attempting to convert dt1$sampledate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1sampledate<-as.Date(dt1$sampledate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt1[dt1$sampledate != "",]) == length(tmp1sampledate[!is.na(tmp1sampledate)])){dt1$sampledate <- tmp1sampledate } else {print("Date conversion failed for dt1$sampledate. Please inspect the data and do the date conversion yourself.")}                                                                    

if (class(dt1$depth)=="factor") dt1$depth <-as.numeric(levels(dt1$depth))[as.integer(dt1$depth) ]               
if (class(dt1$depth)=="character") dt1$depth <-as.numeric(dt1$depth)
if (class(dt1$temperature_C)=="factor") dt1$temperature_C <-as.numeric(levels(dt1$temperature_C))[as.integer(dt1$temperature_C) ]               
if (class(dt1$temperature_C)=="character") dt1$temperature_C <-as.numeric(dt1$temperature_C)
if (class(dt1$dissolvedOxygen)=="factor") dt1$dissolvedOxygen <-as.numeric(levels(dt1$dissolvedOxygen))[as.integer(dt1$dissolvedOxygen) ]               
if (class(dt1$dissolvedOxygen)=="character") dt1$dissolvedOxygen <-as.numeric(dt1$dissolvedOxygen)
if (class(dt1$irradianceWater)=="factor") dt1$irradianceWater <-as.numeric(levels(dt1$irradianceWater))[as.integer(dt1$irradianceWater) ]               
if (class(dt1$irradianceWater)=="character") dt1$irradianceWater <-as.numeric(dt1$irradianceWater)
if (class(dt1$irradianceDeck)=="factor") dt1$irradianceDeck <-as.numeric(levels(dt1$irradianceDeck))[as.integer(dt1$irradianceDeck) ]               
if (class(dt1$irradianceDeck)=="character") dt1$irradianceDeck <-as.numeric(dt1$irradianceDeck)
if (class(dt1$comments)!="factor") dt1$comments<- as.factor(dt1$comments)

# Convert Missing Values to NA for non-dates

dt1$year4 <- ifelse((trimws(as.character(dt1$year4))==trimws("NA")),NA,dt1$year4)               
suppressWarnings(dt1$year4 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$year4))==as.character(as.numeric("NA"))),NA,dt1$year4))
dt1$daynum <- ifelse((trimws(as.character(dt1$daynum))==trimws("NA")),NA,dt1$daynum)               
suppressWarnings(dt1$daynum <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$daynum))==as.character(as.numeric("NA"))),NA,dt1$daynum))
dt1$depth <- ifelse((trimws(as.character(dt1$depth))==trimws("NA")),NA,dt1$depth)               
suppressWarnings(dt1$depth <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$depth))==as.character(as.numeric("NA"))),NA,dt1$depth))
dt1$temperature_C <- ifelse((trimws(as.character(dt1$temperature_C))==trimws("NA")),NA,dt1$temperature_C)               
suppressWarnings(dt1$temperature_C <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$temperature_C))==as.character(as.numeric("NA"))),NA,dt1$temperature_C))
dt1$dissolvedOxygen <- ifelse((trimws(as.character(dt1$dissolvedOxygen))==trimws("NA")),NA,dt1$dissolvedOxygen)               
suppressWarnings(dt1$dissolvedOxygen <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$dissolvedOxygen))==as.character(as.numeric("NA"))),NA,dt1$dissolvedOxygen))
dt1$irradianceWater <- ifelse((trimws(as.character(dt1$irradianceWater))==trimws("NA")),NA,dt1$irradianceWater)               
suppressWarnings(dt1$irradianceWater <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$irradianceWater))==as.character(as.numeric("NA"))),NA,dt1$irradianceWater))
dt1$irradianceDeck <- ifelse((trimws(as.character(dt1$irradianceDeck))==trimws("NA")),NA,dt1$irradianceDeck)               
suppressWarnings(dt1$irradianceDeck <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$irradianceDeck))==as.character(as.numeric("NA"))),NA,dt1$irradianceDeck))


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(lakeid)
summary(lakename)
summary(year4)
summary(daynum)
summary(sampledate)
summary(depth)
summary(temperature_C)
summary(dissolvedOxygen)
summary(irradianceWater)
summary(irradianceDeck)
summary(comments) 
# Get more details on character variables

summary(as.factor(dt1$lakeid)) 
summary(as.factor(dt1$lakename)) 
summary(as.factor(dt1$comments))
detach(dt1)               

rout.temp = dt1

rout.temp = rout.temp %>% filter(lakeid %in% c("L", "R", "T"))

# take the mean by depth
mean.rout.temp = rout.temp %>% group_by(lakeid, depth) %>% 
  summarize(temp = mean(temperature_C, na.rm = TRUE)) %>% filter(!is.na(temp)) %>% ungroup()

ggplot(mean.rout.temp %>% filter(depth <= 8), aes(x = temp, y = depth, color = lakeid))+
  geom_point()+
  geom_path()+
  scale_y_reverse()+
  theme_classic()

