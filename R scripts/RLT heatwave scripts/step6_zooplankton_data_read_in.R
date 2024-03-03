
library(tidyverse)

# read in and format the daily zooplankton data

# Package ID: knb-lter-ntl.374.2 Cataloging System:https://pasta.edirepository.org.
# Data set title: Cascade project at North Temperate Lakes LTER - Daily data for key variables in whole lake experiments on early warnings of critical transitions, Paul and Peter Lakes, 2008-2011.
# Data set creator:  Stephen Carpenter - University of Wisconsin 
# Data set creator:  Mike Pace - University of Virginia 
# Data set creator:  Jon Cole - Cary Institute of Ecosystem Studies 
# Data set creator:  James Kitchell -  
# Data set creator:  James Hodgson - St. Norbert College 
# Contact:  Stephen Carpenter -  University of Wisconsin  - steve.carpenter@wisc.edu
# Contact:    -  NTL LTER  - ntl.infomgr@gmail.com
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/374/2/33e43147adbf696d0512a5e8f2904d8d" 
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
                 "DOY",     
                 "Chla",     
                 "ZBgrav",     
                 "LMinnCPE"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$lake)!="factor") dt1$lake<- as.factor(dt1$lake)
if (class(dt1$year)=="factor") dt1$year <-as.numeric(levels(dt1$year))[as.integer(dt1$year) ]               
if (class(dt1$year)=="character") dt1$year <-as.numeric(dt1$year)
if (class(dt1$DOY)=="factor") dt1$DOY <-as.numeric(levels(dt1$DOY))[as.integer(dt1$DOY) ]               
if (class(dt1$DOY)=="character") dt1$DOY <-as.numeric(dt1$DOY)
if (class(dt1$Chla)=="factor") dt1$Chla <-as.numeric(levels(dt1$Chla))[as.integer(dt1$Chla) ]               
if (class(dt1$Chla)=="character") dt1$Chla <-as.numeric(dt1$Chla)
if (class(dt1$ZBgrav)=="factor") dt1$ZBgrav <-as.numeric(levels(dt1$ZBgrav))[as.integer(dt1$ZBgrav) ]               
if (class(dt1$ZBgrav)=="character") dt1$ZBgrav <-as.numeric(dt1$ZBgrav)
if (class(dt1$LMinnCPE)=="factor") dt1$LMinnCPE <-as.numeric(levels(dt1$LMinnCPE))[as.integer(dt1$LMinnCPE) ]               
if (class(dt1$LMinnCPE)=="character") dt1$LMinnCPE <-as.numeric(dt1$LMinnCPE)

# Convert Missing Values to NA for non-dates

dt1$year <- ifelse((trimws(as.character(dt1$year))==trimws("NA")),NA,dt1$year)               
suppressWarnings(dt1$year <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$year))==as.character(as.numeric("NA"))),NA,dt1$year))
dt1$DOY <- ifelse((trimws(as.character(dt1$DOY))==trimws("NA")),NA,dt1$DOY)               
suppressWarnings(dt1$DOY <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DOY))==as.character(as.numeric("NA"))),NA,dt1$DOY))
dt1$Chla <- ifelse((trimws(as.character(dt1$Chla))==trimws("NA")),NA,dt1$Chla)               
suppressWarnings(dt1$Chla <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Chla))==as.character(as.numeric("NA"))),NA,dt1$Chla))
dt1$ZBgrav <- ifelse((trimws(as.character(dt1$ZBgrav))==trimws("NA")),NA,dt1$ZBgrav)               
suppressWarnings(dt1$ZBgrav <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$ZBgrav))==as.character(as.numeric("NA"))),NA,dt1$ZBgrav))
dt1$LMinnCPE <- ifelse((trimws(as.character(dt1$LMinnCPE))==trimws("NA")),NA,dt1$LMinnCPE)               
suppressWarnings(dt1$LMinnCPE <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$LMinnCPE))==as.character(as.numeric("NA"))),NA,dt1$LMinnCPE))


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(lake)
summary(year)
summary(DOY)
summary(Chla)
summary(ZBgrav)
summary(LMinnCPE) 
# Get more details on character variables

summary(as.factor(dt1$lake))
detach(dt1)               


zoop08.11 = dt1

ggplot(data = zoop08.11, aes(x = DOY, y = ZBgrav, color = lake))+
  geom_point()+
  geom_line()+
  facet_wrap(~year)


