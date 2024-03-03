#### Download the Cascade sonde data on hourly timescales for
# making the temperature model

library(tidyverse)


#### 2008 to 2011 ####
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
if(length(tmp1datetime) == length(tmp1datetime[!is.na(tmp1datetime)])){dt1$datetime <- tmp1datetime } else {print("Date conversion failed for dt1$datetime. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1datetime) 
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





