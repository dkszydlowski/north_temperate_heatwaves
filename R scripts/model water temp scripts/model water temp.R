# modeling water temperature data for Peter, Paul, and Tuesday lakes
# based off of PRISM air temperature data

library(tidyverse)
library(heatwaveR)

library(lme4)
library(lmerTest)
library(MuMIn)

# read in the PRISM temperature data

prism = read.csv("./formatted data/PRISM_tmin_tmean_tmax_tdmean_provisional_4km_19810101_20240101_46.2514_-89.4958.csv")

# rename prism columns
prism = prism %>% rename(tmin = tmin..degrees.C., tmean = tmean..degrees.C.,
                         tmax = tmax..degrees.C., tdmean = tdmean..degrees.C.,
                         date = Date)

prism = prism %>% mutate(date = as.Date(date, format="%m/%d/%Y")) %>% 
  mutate(year = year(date), month = month(date), doy = yday(date))

ggplot(prism %>% filter(month >= 5 & month <= 9 & year == 2013), aes(x = doy, y = tmean))+
  geom_point()+
  geom_line()






#### Read in the Cascade routine temperature data ####

# Package ID: knb-lter-ntl.352.4 Cataloging System:https://pasta.edirepository.org.
# Data set title: Cascade Project at North Temperate Lakes LTER Core Data Physical and Chemical Limnology 1984 - 2016.
# Data set creator:  Stephen Carpenter - University of Wisconsin 
# Data set creator:  Jim Kitchell - University of Wisconsin 
# Data set creator:  Jon Cole - Cary Institute of Ecosystem Studies 
# Data set creator:  Mike Pace - University of Virginia 
# Contact:  Stephen Carpenter -  University of Wisconsin  - steve.carpenter@wisc.edu
# Contact:  Mike Pace -  University of Virginia  - pacem@virginia.edu
# Contact:    -  NTL LTER  - ntl.infomgr@gmail.com
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/352/4/3f928dbd3989c95bc7146ee8363d69bd" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
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
if(length(tmp1sampledate) == length(tmp1sampledate[!is.na(tmp1sampledate)])){dt1$sampledate <- tmp1sampledate } else {print("Date conversion failed for dt1$sampledate. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1sampledate) 
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

casc.temp = dt1


#### view and clean cascade temp data ####

# filter so only L, R, T
casc.temp = casc.temp %>% filter(lakeid %in% c("L", "R", "T"))

# select only the columns we need
casc.temp = casc.temp %>% select(lakeid, year4, daynum, sampledate, depth, temperature_C, irradianceWater, irradianceDeck) %>% 
  rename(year = year4, doy = daynum, date = sampledate, temp = temperature_C, lake = lakeid)

casc.temp.0 = casc.temp %>% filter(depth == 0)
casc.temp.0.5 = casc.temp %>% filter(depth == 0.5)

ggplot(data = casc.temp.0, aes(x = date, y = temp, color = lake))+
  geom_point()+
  facet_wrap(~lake)+
  scale_color_manual(values = c("L" = "#ADDAE3", "R"=  "#4AB5C4", "T"=  "#BAAD8D"))+
  theme_bw()



# add the PRISM data to the Cascade temperature dataframe
casc.temp = casc.temp %>% left_join(prism, by = c("date", "year", "doy"))

casc.temp.0 = casc.temp %>% filter(depth == 0)
casc.temp.0.5 = casc.temp %>% filter(depth == 0.5)

ggplot(casc.temp.0, aes(x = temp, y = tmean, color = lake))+
  geom_point()+
  facet_wrap(~lake)+
  scale_color_manual(values = c("L" = "#ADDAE3", "R"=  "#4AB5C4", "T"=  "#BAAD8D"))
  
# restrict dates between May 1st and September 30th
casc.temp = casc.temp %>% filter(doy >= 121 & doy <= 372)

casc.temp.0 = casc.temp %>% filter(depth == 0)
casc.temp.0.5 = casc.temp %>% filter(depth == 0.5)

test = lmer(temp~tmean+doy+year + (1|lake), data = casc.temp.0)

summary(test)
r.squaredGLMM(test)





#### bring in the carbon ####
# Package ID: knb-lter-ntl.350.7 Cataloging System:https://pasta.edirepository.org.
# Data set title: Cascade Project at North Temperate Lakes LTER Core Data Carbon 1984 - 2016.
# Data set creator:  Stephen Carpenter - University of Wisconsin 
# Data set creator:  Jim Kitchell - University of Wisconsin 
# Data set creator:  Jon Cole - Cary Institute of Ecosystem Studies 
# Data set creator:  Mike Pace - University of Virginia 
# Contact:  Stephen Carpenter -  University of Wisconsin  - steve.carpenter@wisc.edu
# Contact:  Mike Pace -  University of Virginia  - pacem@virginia.edu
# Contact:    -  NTL LTER  - ntl.infomgr@gmail.com
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/350/7/9fc12d6790cdc2271a18334e17e2cc89" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               , col.names=c(
                 "lakeid",     
                 "lakename",     
                 "year4",     
                 "daynum",     
                 "sampledate",     
                 "depth",     
                 "depth_id",     
                 "tpc",     
                 "tpn",     
                 "DIC_mg",     
                 "DIC_uM",     
                 "air_pco2",     
                 "water_pco2",     
                 "doc",     
                 "absorbance"    ), check.names=TRUE)

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
if(length(tmp1sampledate) == length(tmp1sampledate[!is.na(tmp1sampledate)])){dt1$sampledate <- tmp1sampledate } else {print("Date conversion failed for dt1$sampledate. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1sampledate) 
if (class(dt1$depth)!="factor") dt1$depth<- as.factor(dt1$depth)
if (class(dt1$depth_id)!="factor") dt1$depth_id<- as.factor(dt1$depth_id)
if (class(dt1$tpc)=="factor") dt1$tpc <-as.numeric(levels(dt1$tpc))[as.integer(dt1$tpc) ]               
if (class(dt1$tpc)=="character") dt1$tpc <-as.numeric(dt1$tpc)
if (class(dt1$tpn)=="factor") dt1$tpn <-as.numeric(levels(dt1$tpn))[as.integer(dt1$tpn) ]               
if (class(dt1$tpn)=="character") dt1$tpn <-as.numeric(dt1$tpn)
if (class(dt1$DIC_mg)=="factor") dt1$DIC_mg <-as.numeric(levels(dt1$DIC_mg))[as.integer(dt1$DIC_mg) ]               
if (class(dt1$DIC_mg)=="character") dt1$DIC_mg <-as.numeric(dt1$DIC_mg)
if (class(dt1$DIC_uM)=="factor") dt1$DIC_uM <-as.numeric(levels(dt1$DIC_uM))[as.integer(dt1$DIC_uM) ]               
if (class(dt1$DIC_uM)=="character") dt1$DIC_uM <-as.numeric(dt1$DIC_uM)
if (class(dt1$air_pco2)=="factor") dt1$air_pco2 <-as.numeric(levels(dt1$air_pco2))[as.integer(dt1$air_pco2) ]               
if (class(dt1$air_pco2)=="character") dt1$air_pco2 <-as.numeric(dt1$air_pco2)
if (class(dt1$water_pco2)=="factor") dt1$water_pco2 <-as.numeric(levels(dt1$water_pco2))[as.integer(dt1$water_pco2) ]               
if (class(dt1$water_pco2)=="character") dt1$water_pco2 <-as.numeric(dt1$water_pco2)
if (class(dt1$doc)=="factor") dt1$doc <-as.numeric(levels(dt1$doc))[as.integer(dt1$doc) ]               
if (class(dt1$doc)=="character") dt1$doc <-as.numeric(dt1$doc)
if (class(dt1$absorbance)=="factor") dt1$absorbance <-as.numeric(levels(dt1$absorbance))[as.integer(dt1$absorbance) ]               
if (class(dt1$absorbance)=="character") dt1$absorbance <-as.numeric(dt1$absorbance)

# Convert Missing Values to NA for non-dates

dt1$tpc <- ifelse((trimws(as.character(dt1$tpc))==trimws("NA")),NA,dt1$tpc)               
suppressWarnings(dt1$tpc <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$tpc))==as.character(as.numeric("NA"))),NA,dt1$tpc))
dt1$tpn <- ifelse((trimws(as.character(dt1$tpn))==trimws("NA")),NA,dt1$tpn)               
suppressWarnings(dt1$tpn <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$tpn))==as.character(as.numeric("NA"))),NA,dt1$tpn))
dt1$DIC_mg <- ifelse((trimws(as.character(dt1$DIC_mg))==trimws("NA")),NA,dt1$DIC_mg)               
suppressWarnings(dt1$DIC_mg <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DIC_mg))==as.character(as.numeric("NA"))),NA,dt1$DIC_mg))
dt1$DIC_uM <- ifelse((trimws(as.character(dt1$DIC_uM))==trimws("NA")),NA,dt1$DIC_uM)               
suppressWarnings(dt1$DIC_uM <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DIC_uM))==as.character(as.numeric("NA"))),NA,dt1$DIC_uM))
dt1$air_pco2 <- ifelse((trimws(as.character(dt1$air_pco2))==trimws("NA")),NA,dt1$air_pco2)               
suppressWarnings(dt1$air_pco2 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$air_pco2))==as.character(as.numeric("NA"))),NA,dt1$air_pco2))
dt1$water_pco2 <- ifelse((trimws(as.character(dt1$water_pco2))==trimws("NA")),NA,dt1$water_pco2)               
suppressWarnings(dt1$water_pco2 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$water_pco2))==as.character(as.numeric("NA"))),NA,dt1$water_pco2))
dt1$doc <- ifelse((trimws(as.character(dt1$doc))==trimws("NA")),NA,dt1$doc)               
suppressWarnings(dt1$doc <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$doc))==as.character(as.numeric("NA"))),NA,dt1$doc))
dt1$absorbance <- ifelse((trimws(as.character(dt1$absorbance))==trimws("NA")),NA,dt1$absorbance)               
suppressWarnings(dt1$absorbance <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$absorbance))==as.character(as.numeric("NA"))),NA,dt1$absorbance))


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
summary(depth_id)
summary(tpc)
summary(tpn)
summary(DIC_mg)
summary(DIC_uM)
summary(air_pco2)
summary(water_pco2)
summary(doc)
summary(absorbance) 
# Get more details on character variables

summary(as.factor(dt1$lakeid)) 
summary(as.factor(dt1$lakename)) 
summary(as.factor(dt1$depth)) 
summary(as.factor(dt1$depth_id))
detach(dt1)               


carbon = dt1

carbon = carbon %>% filter(!is.na(absorbance)) %>% 
  filter(depth == "Epilimnion" | depth == "PML")


carbon = carbon %>% rename(date = sampledate, lake = lakeid, year = year4, doy = daynum)

casc.temp$depth = as.numeric(casc.temp$depth)
carbon$depth = as.numeric(carbon$depth)


all.cascade = casc.temp %>% left_join(carbon, by = c("date", "year", "doy", "lake"))

all.cascade.0 = all.cascade %>% filter(depth.x == 0)

test = lmer(temp~tmean+doy+year + (1|lake), data = all.cascade.0)
summary(test)
r.squaredGLMM(test)

test = lmer(temp~tmean+doy+doc + (1|lake), data = all.cascade.0)
summary(test)
r.squaredGLMM(test)










####### Try making  a model with the daily sonde temperature data #########

temp.sonde = read.csv("./formatted data/allSonde_interpolated.csv")

temp.sonde = temp.sonde %>% select(lake, year, date, doyCat, mean_temp) %>% 
  rename(doy = doyCat)

temp.sonde = temp.sonde %>% mutate(date = as.Date(date))

# add the PRISM data to temp.sonde
temp.sonde.prism = temp.sonde %>% left_join(prism, by = c("date", "year", "doy"))


library(ggpubr)

ggplot(temp.sonde.prism %>% filter(year == 2015), aes(x = mean_temp, y = tmean, color = lake))+
  geom_point()+
  facet_wrap(~lake)+
  labs(y = "prism temperature", x = "sonde temperature")+
  scale_color_manual(values = c("L" = "#ADDAE3", "R"=  "#4AB5C4", "T"=  "#BAAD8D"))+
  geom_smooth(method = "lm", se = TRUE, color = "black", size = 1)# Add linear regression lines
  

temp.sonde.prism.L = temp.sonde.prism %>% filter(lake == "L")
summary(lm(temp.sonde.prism.L$mean_temp~temp.sonde.prism.L$tmean))

# temp.sonde.prism.carbon
temp.sonde.prism.carbon = temp.sonde.prism %>% left_join(carbon, by = c("date", "doy", "lake"))

temp.sonde.prism.carbon = temp.sonde.prism.carbon %>% filter(doy >= 152)

temp.sonde.prism.carbon = temp.sonde.prism.carbon %>% mutate(year.x = as.factor(year.x))

test = lmer(mean_temp~tmean*lag(tmean, 1)*lag(tmean, 2)*lag(tmean, 3)*lag(tmean, 4)*lag(tmean, 5)*doy + (1|lake), data = temp.sonde.prism.carbon)
summary(test)
r.squaredGLMM(test)

temp.sonde.prism.carbon$modeled = predict(test, temp.sonde.prism.carbon)

ggplot(temp.sonde.prism.carbon %>% filter(year.x == 2015), aes(x = doy, y = mean_temp, color = lake))+
  geom_line(linewidth = 0.8)+
  #geom_point(aes(x = doy, y = mean_temp), size = 1, color = "red", alpha = 0.5)+
  geom_line(aes(x = doy, y = modeled), size = 1, color = "red", alpha = 0.3)+
 # geom_line(aes(x = doy, y = tmean), size = 0.3, color = "purple", alpha = 0.5)+
  facet_wrap(lake~year.x)+
  labs(y = "predicted temperature", x = "doy")+
  scale_color_manual(values = c("L" = "#ADDAE3", "R"=  "#4AB5C4", "T"=  "#BAAD8D"))
 # geom_smooth(method = "lm", se = TRUE, color = "black", size = 1)# Add linear regression lines





ggplot(temp.sonde.prism %>% filter(year == 2014), aes(x = date, y = tmean, color = lake))+
#  geom_point()+
  geom_line()+
  geom_line(aes(x = date, y = mean_temp, color = "black"), size = 1)+
  facet_wrap(~lake)+
  labs(y = "prism temperature", x = "sonde temperature")+
  scale_color_manual(values = c("L" = "#ADDAE3", "R"=  "#4AB5C4", "T"=  "#BAAD8D"))


ccf(temp.sonde.prism$mean_temp, temp.sonde.prism$tmean)




##### model with woodruff air temperature from LTER ######
woodruff = read.csv("./formatted data/LTER daily temperature/woodruff airport temperature LTER.csv")

woodruff = woodruff %>% rename(year = year4, date = sampledate, doy = daynum)

# read in the Cascade temperature sensor data 
# this data was formatted in my PROFILES chapter
lrt.temp.18.19 = read.csv("./formatted data/Cascade hourly temp sensor data/LR_hourly_wide_temperature_2018_2019.csv")
lrt.temp.13.15 = read.csv("./formatted data/Cascade hourly temp sensor data/LRT_hourly_wide_temperature_2013_to_2015.csv")

# right now, only interested in the top 0.5 m of the water column
lrt.temp.18.19 = lrt.temp.18.19 %>% select(lake, year, datetime, wtr_0.5)
lrt.temp.13.15 = lrt.temp.13.15 %>% select(lake, year, datetime, wtr_0.5)


woodruff.matched.years = woodruff %>% filter(year %in% c(2013:2019)) %>% 
  select(year, date, doy, hour, avg_air_temp, flag_avg_air_temp)

lrt.temp.all = rbind(lrt.temp.18.19, lrt.temp.13.15)

# add a datetime column to woodruff.matched.years
woodruff.matched.years <- woodruff.matched.years %>%
  mutate(datetime = lubridate::ymd_hm(paste(date, sprintf("%04d", hour), sep=" ")))

# format datetime as a datetime in lrt.temp.all
lrt.temp.all <- lrt.temp.all %>%
  mutate(datetime = ifelse(nchar(datetime) == 10, paste(datetime, "00:00:00"), datetime),
         datetime = ymd_hms(datetime))

# add the woodruff temp to lrt.temp.all based on datetime
lrt.temp.all = lrt.temp.all %>% left_join(woodruff.matched.years, by = c("datetime", "year"))


ggplot(lrt.temp.all, aes(x = wtr_0.5, y = avg_air_temp, fill = lake))+
  geom_point(pch = 21)+
 # geom_line()+
#  geom_line(aes(x = date, y = mean_temp, color = "black"), size = 1)+
  facet_wrap(~lake)+
  labs(y = "prism temperature", x = "temp sensor 0.5 m temperature")+
  scale_fill_manual(values = c("Paul" = "#ADDAE3", "Peter"=  "#4AB5C4", "Tuesday"=  "#BAAD8D"))   


ggplot(lrt.temp.all %>% year == 2014, aes(x = datetime, y = avg_air_temp, color = lake))+
  geom_point(pch = 21)+
  # geom_line()+
  geom_line(aes(x = datetime, y = wtr_0.5, color = "black"), size = 1)+
  facet_wrap(~lake)+
  labs(y = "prism temperature", x = "temp sensor 0.5 m temperature")+
  scale_color_manual(values = c("Paul" = "#ADDAE3", "Peter"=  "#4AB5C4", "Tuesday"=  "#BAAD8D"))   


test = lmer(wtr_0.5~avg_air_temp*doy*hour*year + (1|lake), data = lrt.temp.all)
summary(test)
r.squaredGLMM(test)



lrt.temp.all$modeled = predict(test, lrt.temp.all)

ggplot(lrt.temp.all, aes(x = doy, y = modeled, color = lake))+
  geom_line(linewidth = 0.8)+
  geom_point(aes(x = doy, y = wtr_0.5), size = 0.3, color = "red", alpha = 0.5)+
  geom_line(aes(x = doy, y = avg_air_temp), size = 0.3, color = "purple", alpha = 0.5)+
  facet_wrap(lake~year)+
  labs(y = "predicted temperature", x = "doy")+
  scale_color_manual(values = c("L" = "#ADDAE3", "R"=  "#4AB5C4", "T"=  "#BAAD8D"))
# geom_smooth(method = "lm", se = TRUE, color = "black", size = 1)# Add linear regression lines




l.lrt.temp.all = lrt.temp.all %>% filter(lake == "Paul")

ccf(l.lrt.temp.all$wtr_0.5, l.lrt.temp.all$avg_air_temp, na.action = na.rm)





##### Model with Sparkling Lake temperature and woodruff air temperature as predictors ####
SP.temp = read.csv("./formatted data/LTER daily temperature/Sparkling Lake daily temperature all depths.csv")

SP.temp.1 = SP.temp %>% filter(depth == 1) %>% 
  rename(year = year4, date = sampledate, doy = daynum) %>% select(-depth, -flag_wtemp) %>% 
  rename(SP.temp.1 = wtemp)

lrt.temp.all.SP = lrt.temp.all %>% left_join(SP.temp.1, by = c("doy", "date", "year"))

ggplot(lrt.temp.all.SP, aes(x = wtr_0.5, y = SP.temp.1, fill = lake))+
  geom_point(pch = 21)+
  # geom_line()+
  #  geom_line(aes(x = date, y = mean_temp, color = "black"), size = 1)+
  facet_wrap(~lake)+
  labs(y = "prism temperature", x = "temp sensor 0.5 m temperature")+
  scale_fill_manual(values = c("Paul" = "#ADDAE3", "Peter"=  "#4AB5C4", "Tuesday"=  "#BAAD8D"))  

summary(lm(lrt.temp.all.SP$wtr_0.5~lrt.temp.all.SP$SP.temp.1))



test = lmer(wtr_0.5~SP.temp.1*avg_air_temp*doy*lag(avg_air_temp, 1)*lag(avg_air_temp, 2)+lag(avg_air_temp, 3) + (1|lake), data = lrt.temp.all.SP)
summary(test)
r.squaredGLMM(test)



#### Try this on a daily timescale ####

# modeling dataset
fitting = daily.lrt.temp.all.SP %>% mutate(lake_year = paste(lake, year, sep = "_")) %>%
  filter(lake_year != "Peter_2013")

daily.lrt.temp.all.SP = daily.lrt.temp.all.SP %>% mutate(lake_year = paste(lake, year, sep = "_"))

daily.lrt.temp.all.SP = lrt.temp.all.SP %>% group_by(lake, year, date, doy) %>%
  summarize(daily.sonde.temp = mean(wtr_0.5), daily.air.temp = mean(avg_air_temp), daily.sp.temp = mean(SP.temp.1))


test = lmer(daily.sonde.temp~daily.sp.temp*daily.air.temp*doy+lag(daily.air.temp, 1)+lag(daily.air.temp, 2)+lag(daily.sp.temp, 1) + (1|lake), data = fitting)
summary(test)
r.squaredGLMM(test)

daily.lrt.temp.all.SP$modeled = predict(test, daily.lrt.temp.all.SP)

daily.lrt.temp.all.SP = daily.lrt.temp.all.SP %>% mutate(lake_year = paste(lake, year, sep = "_"))


ggplot(daily.lrt.temp.all.SP %>% filter(lake_year == "Tuesday_2013"), aes(x = doy, y = modeled, color = lake))+
  geom_line(linewidth = 1, color = "black")+
  geom_line(aes(x = doy, y = daily.sonde.temp, color = lake), size = 1, color = "steelblue4", alpha = 0.5)+
  geom_point(aes(x = doy, y = daily.sonde.temp, color = lake), size = 1, color = "steelblue4", alpha = 0.5)+
  facet_wrap(lake~year)+
  labs(y = "predicted temperature", x = "doy")+
  scale_color_manual(values = c("L" = "#ADDAE3", "R"=  "#4AB5C4", "T"=  "#BAAD8D"))+
  theme_classic()
# geom_smooth(method = "lm", se = TRUE, color = "black", size = 1)# Add linear regression lines


ggplot(daily.lrt.temp.all.SP %>% filter(year == 2013), aes(x = doy, y = modeled, color = lake))+
  geom_line(size = 1)+
  geom_point(aes(x = doy, y = daily.sonde.temp, color = lake), size = 1, alpha = 0.5)+
  #geom_point(aes(x = doy, y = daily.sonde.temp, color = lake), size = 1, color = "steelblue4", alpha = 0.5)+
 # facet_wrap(lake~year)+
  labs(y = "predicted temperature", x = "doy")+
  scale_color_manual(values = c("Paul" = "#ADDAE3", "Peter"=  "#4AB5C4", "Tuesday"=  "#BAAD8D"))+
  theme_classic()
# geom_smooth(method = "lm", se = TRUE, color = "black", size = 1)# Add linear regression lines

ggplot(daily.lrt.temp.all.SP, aes(x = daily.sonde.temp, y = daily.sp.temp, fill = lake))+
  geom_point(pch = 21)+
  # geom_line()+
  #  geom_line(aes(x = date, y = mean_temp, color = "black"), size = 1)+
  facet_wrap(~lake)+
  labs(y = "prism temperature", x = "temp sensor 0.5 m temperature")+
  scale_fill_manual(values = c("Paul" = "#ADDAE3", "Peter"=  "#4AB5C4", "Tuesday"=  "#BAAD8D"))  




##### Make a model for all years of Sparkling temperature data #####
# add the Woodruff data to the Sparkling Lake data

# get daily woodruff temperature data

# add daily woodruff temperature data to Sparkling
SP.temp.1 = SP.temp.1 %>% left_join()

















######### Model water temp of RLT #########

# use sonde temperature data, woodruff airport data, and Sparkling Lake data

# sonde temperature
temp.sonde = read.csv("./formatted data/allSonde_interpolated.csv")

temp.sonde = temp.sonde %>% select(lake, year, date, doyCat, mean_temp) %>% 
  rename(doy = doyCat)

temp.sonde = temp.sonde %>% mutate(date = as.Date(date))


# Sparkling Lake temperature
SP.temp = read.csv("./formatted data/LTER daily temperature/Sparkling Lake daily temperature all depths.csv")
# 
# SP.temp.1 = SP.temp %>% filter(depth == 0 | depth == 0.01 | (depth == 0.25 & year4 == 2013)) %>% 
#   rename(year = year4, date = sampledate, doy = daynum) %>% select(-depth, -flag_wtemp) %>% 
#   rename(SP.temp.1 = wtemp) %>% 
#   mutate(date = as.Date(date))

SP.temp.1 = SP.temp %>% filter(depth == 1) %>%
  rename(year = year4, date = sampledate, doy = daynum) %>% select(-depth, -flag_wtemp) %>%
  rename(SP.temp.1 = wtemp) %>%
  mutate(date = as.Date(date))


# Woodruff airport temperature
woodruff = read.csv("./formatted data/LTER daily temperature/woodruff airport temperature LTER.csv")

woodruff = woodruff %>% rename(year = year4, date = sampledate, doy = daynum)

# make a daily woodruff dataframe
woodruff = woodruff %>% group_by(year, date, doy) %>% 
  summarize(woodruff.temp = mean(avg_air_temp, na.rm = TRUE)) %>% 
  mutate(date = as.Date(date))


# combine woodruff, Sparkling, and LRT sonde temp
sonde.SP.woodruff = temp.sonde %>% left_join(SP.temp.1, by = c("doy", "date", "year"))
sonde.SP.woodruff = sonde.SP.woodruff %>% left_join(woodruff, by = c("doy", "date", "year"))

# add new lag columns in case we want to include time lags
sonde.SP.woodruff = sonde.SP.woodruff %>% mutate(woodruff.temp.lag.1 = lead(woodruff.temp, 1))

# run the model
test = lmer(mean_temp~SP.temp.1* woodruff.temp +doy + (1|lake), data = sonde.SP.woodruff)
summary(test)
r.squaredGLMM(test)

# model with just sparkling lake temperature and air temperature
# summary(lm(sonde.SP.woodruff$mean_temp~sonde.SP.woodruff$SP.temp.1))

### make a fitting dataset and a testing dataset





##### Use the model to predict temperature of RLT for every year of Sparkling data ######
SP.woodruff = SP.temp.1 %>% left_join(woodruff, by = c("date", "doy", "year"))

# make columns of lake that can be input to the model
SP.woodruffL = SP.woodruff %>% mutate(lake = "L")
SP.woodruffR = SP.woodruff %>% mutate(lake = "R")
SP.woodruffT = SP.woodruff %>% mutate(lake = "T")

# combine into one for predicting temp
SP.woodruff = rbind(SP.woodruffL, SP.woodruffR, SP.woodruffT)

SP.woodruff$modeled.temp = predict(test, SP.woodruff)

pdf("./figures/modeled temperature/modeled temperature daily sp 1 m.pdf", width = 12, height = 18)


ggplot(SP.woodruff, aes(x = doy, y = modeled.temp, color = lake))+
   geom_line()+
  #geom_line(aes(x = doy, y = SP.temp.1, color = "black"), size = 1)+
  facet_wrap(~year)+
  labs(y = "modeled temperature", x = "day of year")+
  scale_color_manual(values = c("L" = "#ADDAE3", "R"=  "#4AB5C4", "T"=  "#BAAD8D"))  +
  xlim(152, 259)+
  ylim(15, 30)+
  theme_classic()

dev.off()

# compare the predicted temperature to the actual temperature
SP.woodruff.cascade = temp.sonde %>% left_join(SP.woodruff, by = c("year", "date", "doy", "lake"))

SP.woodruff.cascade.real.data = SP.woodruff.cascade %>% filter(!is.na(mean_temp))

# pivot longer for plotting
SP.woodruff.cascade.real.data = SP.woodruff.cascade.real.data %>% pivot_longer()


SP.woodruff.cascade.real.data_long <- SP.woodruff.cascade.real.data %>%
  pivot_longer(
    cols = c("modeled.temp", "mean_temp"),
    names_to = "temp.type",
    values_to = "temperature"
  )


pdf("./figures/modeled temperature/modeled vs real temperature daily sp 1 m.pdf", width = 12, height = 12)

ggplot(SP.woodruff.cascade.real.data_long, aes(x = doy, y = temperature, color = temp.type))+
  geom_line(size = 1, alpha = 0.8)+
  #geom_point()+
  #geom_line(aes(x = doy, y = SP.temp.1, color = "black"), size = 1)+
  facet_wrap(lake~year)+
  labs( x = "day of year")+
 # scale_color_manual(values = c("L" = "#ADDAE3", "R"=  "#4AB5C4", "T"=  "#BAAD8D"))  +
 # strip.background = element_rect(fill = c(L = "#ADDAE3", R = "#4AB5C4", T = "#BAAD8D"), strip.text = element_text(color = "white"))+
  xlim(152, 259)+
  ylim(15, 30)+
  theme_classic()


dev.off()

##### Calculate heatwaves from the modeled data #####
SP.woodruff.subset = SP.woodruff %>% filter(year %in% c(1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999,
                                                        2002, 2003, 2009, 2010, 2011, 2012, 2014,
                                                        2016, 2019, 2020, 2021, 2022)) %>% 
                                                        filter(doy > 120 & doy < 275)

# format for heatwaveR
modeled.heatwaveR.L = SP.woodruff.subset %>% filter(lake == "L") %>% mutate(t = date, temp = modeled.temp) %>% 
  select(t, temp)

modeled.heatwaveR.R = SP.woodruff.subset %>% filter(lake == "R") %>% mutate(t = date, temp = modeled.temp) %>% 
  select(t, temp)

modeled.heatwaveR.T = SP.woodruff.subset %>% filter(lake == "T") %>% mutate(t = date, temp = modeled.temp) %>% 
  select(t, temp)

# calculate climatology and thresholds
modeled.climOutputL = ts2clm(modeled.heatwaveR.L, climatologyPeriod = c(min(modeled.heatwaveR.L$t), max(modeled.heatwaveR.L$t)))
paulHW = detect_event(modeled.climOutputL)

modeled.climOutputR = ts2clm(modeled.heatwaveR.R, climatologyPeriod = c(min(modeled.heatwaveR.R$t), max(modeled.heatwaveR.R$t)))
peterHW = detect_event(modeled.climOutputR)

modeled.climOutputT = ts2clm(modeled.heatwaveR.T, climatologyPeriod = c(min(modeled.heatwaveR.T$t), max(modeled.heatwaveR.T$t)))
tuesdayHW = detect_event(modeled.climOutputT)

View(paulHW$event)


# apply the new climatology and thresholds to the old data
# Paul
L.temp.sonde = temp.sonde %>% filter(lake == "L") %>% 
  rename(temp = mean_temp, t = date) %>% 
  select(doy, t, temp)

climatology = modeled.climOutputL %>% filter(year(t) == 1990) %>%  select(doy, seas, thresh)

L.climatology = left_join(L.temp.sonde, climatology, by = c("doy"), relationship = "many-to-many")

paulHW = detect_event(L.climatology)

View(paulHW$event)

event_line(paulHW, category = TRUE, start_date = "2019-05-15", end_date= "2019-09-15")+geom_point()+
  theme_classic()

# Peter
R.temp.sonde = temp.sonde %>% filter(lake == "R") %>% 
  rename(temp = mean_temp, t = date) %>% 
  select(doy, t, temp)

climatology = modeled.climOutputR %>% filter(year(t) == 1990) %>%  select(doy, seas, thresh)

R.climatology = left_join(R.temp.sonde, climatology, by = c("doy"), relationship = "many-to-many")

peterHW = detect_event(R.climatology)

View(peterHW$event)

event_line(peterHW, category = TRUE, start_date = "2019-05-15", end_date= "2019-09-15")+geom_point()+
  theme_classic()


# Tuesday
T.temp.sonde = temp.sonde %>% filter(lake == "T") %>% 
  rename(temp = mean_temp, t = date) %>% 
  select(doy, t, temp)

climatology = modeled.climOutputT %>% filter(year(t) == 1990) %>%  select(doy, seas, thresh)

T.climatology = left_join(T.temp.sonde, climatology, by = c("doy"), relationship = "many-to-many")

tuesdayHW = detect_event(T.climatology)

View(tuesdayHW$event)

event_line(tuesdayHW, category = TRUE, start_date = "2013-05-15", end_date= "2013-09-15")+geom_point()+
  theme_classic()

saveRDS(paulHW, file = "./results/heatwave modeled outputs/paul heatwave outputs modeled.rds")
saveRDS(peterHW, file = "./results/heatwave modeled outputs/peter heatwave outputs modeled.rds")
saveRDS(tuesdayHW, file = "./results/heatwave modeled outputs/tuesday heatwave outputs modeled.rds")








####### Model temperature with hourly data #########
cascade.sonde.temp.all = read.csv("./formatted data/Cascade hourly sonde data/Cascade hourly sonde data 2008-2015.csv")

# Add in the woodruff hourly and Sparkling Lake hourly data
woodruff = read.csv("./formatted data/LTER daily temperature/woodruff airport temperature LTER.csv")
woodruff = woodruff %>% rename(year = year4, date = sampledate, doy = daynum) %>% 
  select(year, date, doy, hour, avg_air_temp) %>% 
  rename(woodruff.temp = avg_air_temp)

sp.hourly = read.csv("./formatted data/LTER hourly temperature/Sparkling Lake hourly temperature.csv")

# sp.hourly = sp.hourly %>% rename(date = sampledate, year = year4) %>% filter(depth == 0 | depth == 0.01)
sp.hourly = sp.hourly %>% rename(date = sampledate, year = year4) %>% filter(depth == 1)

sp.woodruff.hourly = left_join(woodruff, sp.hourly, by = c("year", "date", "hour")) %>% rename(sp.temp = wtemp)

# Need to make the Cascade data hourly now
cascade.sonde.temp.all = cascade.sonde.temp.all %>% mutate(hour = hour(ymd_hms(datetime))*100)

cascade.sonde.temp.all = cascade.sonde.temp.all %>% group_by(lake, year, doy, hour) %>% summarize(temp = mean(temp, na.rm = TRUE))


# add hourly sparkling woodruff data to Cascade
cascade.sonde.temp.all = cascade.sonde.temp.all %>% left_join(sp.woodruff.hourly, by = c("year", "doy", "hour"))

cascade.sonde.temp.all = cascade.sonde.temp.all %>% mutate(lead.woodruff.temp.5 = lead(woodruff.temp, 5))

hourly.model = lmer(temp~sp.temp +woodruff.temp*lead.woodruff.temp.5 + doy + (1|lake), data = cascade.sonde.temp.all)
summary(hourly.model)
r.squaredGLMM(hourly.model)

