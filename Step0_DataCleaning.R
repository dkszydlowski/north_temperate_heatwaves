#Initial Data Download and Cleaning for Heat Waves Project
library("tidyverse")
library("ggridges")
library("lubridate")
library("zoo")
library(heatwaveR)
##### Cascade Sonde Data 2008-2011 #####

# Package ID: knb-lter-ntl.360.1 Cataloging System:https://pasta.edirepository.org.
# Data set title: Cascade Project at North Temperate Lakes LTER High Frequency Sonde Data from Food Web Resilience Experiment 2008 - 2011.
# Data set creator:  Stephen Carpenter - University of Wisconsin 
# Data set creator:  Mike Pace - University of Virginia 
# Data set creator:  Jon Cole - Cary Institute of Ecosystem Studies 
# Data set creator:  Ryan Batt - Rutgers University 
# Data set creator:  Cal Buelo - University of Virginia 
# Data set creator:  Jason Kurzweil - University of Wisconsin 
# Contact:  Stephen Carpenter -  University of Wisconsin  - steve.carpenter@wisc.edu
# Contact:  NTL Information Manager -  University of Wisconsin  - ntl.infomgr@gmail.com
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/360/1/592195c90b43a36ba3f09cd99021225e" 
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

#####Cleaning Cascade Sonde 2008-2011#####
sonde08_11 = dt1
sonde08_11=sonde08_11 %>%
  mutate(doyCat=yday(datetime))

sonde08_11mean=sonde08_11 %>%
  group_by(lake, year, doyCat) %>%
  summarize(mean_temp=mean(stemp, na.remove = TRUE),
            mean_chl=mean(chl, na.remove = TRUE),
            mean_pH=mean(pH),
            mean_doSat=mean(doSat),
            mean_zmix=mean(zmix),
            mean_par=mean(par),
            mean_wind=mean(wind))

sonde08_11mean=sonde08_11mean %>%
  mutate(date=as.Date(doyCat, origin=paste(year-1, "-12-31", sep="")))

sonde08_11_mean_L = sonde08_11mean %>%
  filter(lake == "Paul", year == "2010")

sonde08_11_mean_L$normChl = sonde08_11_mean_L$mean_chl/sonde08_11_mean_L$mean_chl[1]*100
sonde08_11_mean_L$normTemp = sonde08_11_mean_L$mean_temp/sonde08_11_mean_L$mean_temp[1]*100

sonde08_11_mean_L$normChl = na.approx(sonde08_11_mean_L$normChl)


# make a plot for 2010
ggplot(data=sonde08_11_mean_L, aes(x=date, y=normChl)) + 
  geom_point() +
  geom_line(aes(x = date, y = normTemp), size = 1.5)+
  geom_density_line(aes(x = date, y = normTemp), stat = "identity", size = 0.5, fill = "steelblue3", alpha = 0.3)+
  geom_line(size = 1.5) +
  geom_density_line(stat = "identity", size = 1.5, fill = "forestgreen", alpha = 0.3)+
  theme_classic()+
  labs(title = paste(sonde08_11_mean_L$lake[1], "Lake", sonde08_11_mean_L$year[1]), y = "% of initial value")+
  theme(title = element_text(size = 20))+
  annotate("rect", xmin = as.Date("2010-05-24"), xmax = as.Date("2010-06-03"), ymin = 0, ymax = Inf,
           fill = "red", alpha = 0.3)+
  annotate("rect", xmin = as.Date("2010-08-09"), xmax = as.Date("2010-08-15"), ymin = 0, ymax = Inf,
           fill = "red", alpha = 0.3)+
  theme(text = element_text(size = 20))








#scale_y_continuous(name = "Mean temperature", sec.axis = sec_axis(~.*1/2, name = "mean chl"))
#  geom_line(aes(x = date, y = mean_chl))
# geom_rect(xmin = "2009-06-15", xmax = "2009-06-30", ymin = 16, ymax = 16)

sonde08_11mean = sonde08_11mean %>% ungroup()


##### Cascade Sonde Data 2013-2015#####

# Package ID: knb-lter-ntl.371.2 Cataloging System:https://pasta.edirepository.org.
# Data set title: Cascade project at North Temperate Lakes LTER - High Frequency Data for Whole Lake Nutrient Additions 2013-2015.
# Data set creator:  Mike Pace - University of Virginia 
# Data set creator:  Jon Cole - Cary Institute of Ecosystem Studies 
# Data set creator:  Stephen Carpenter - University of Wisconsin 
# Contact:  Mike Pace -  University of Virginia  - pacem@virginia.edu
# Contact:  NTL Information Manager -  University of Wisconsin  - ntl.infomgr@gmail.com
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/371/2/db9752988bbf4eb7d5a5491c0b642b94" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "Year",     
                 "Lake",     
                 "DoY",     
                 "BGA_YSI",     
                 "BGA_HYLB",     
                 "BGA_logged_YSI",     
                 "BGA_logged_HYLB",     
                 "DO_YSI",     
                 "DO_HYLB",     
                 "DOsat_calc_YSI",     
                 "DOsat_calc_HYLB",     
                 "PH_YSI",     
                 "PH_HYLB",     
                 "Chl_YSI",     
                 "Chl_HYLB",     
                 "Temp_YSI",     
                 "Temp_HYLB"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$Year)=="factor") dt1$Year <-as.numeric(levels(dt1$Year))[as.integer(dt1$Year) ]               
if (class(dt1$Year)=="character") dt1$Year <-as.numeric(dt1$Year)
if (class(dt1$Lake)!="factor") dt1$Lake<- as.factor(dt1$Lake)
if (class(dt1$DoY)=="factor") dt1$DoY <-as.numeric(levels(dt1$DoY))[as.integer(dt1$DoY) ]               
if (class(dt1$DoY)=="character") dt1$DoY <-as.numeric(dt1$DoY)
if (class(dt1$BGA_YSI)=="factor") dt1$BGA_YSI <-as.numeric(levels(dt1$BGA_YSI))[as.integer(dt1$BGA_YSI) ]               
if (class(dt1$BGA_YSI)=="character") dt1$BGA_YSI <-as.numeric(dt1$BGA_YSI)
if (class(dt1$BGA_HYLB)=="factor") dt1$BGA_HYLB <-as.numeric(levels(dt1$BGA_HYLB))[as.integer(dt1$BGA_HYLB) ]               
if (class(dt1$BGA_HYLB)=="character") dt1$BGA_HYLB <-as.numeric(dt1$BGA_HYLB)
if (class(dt1$BGA_logged_YSI)=="factor") dt1$BGA_logged_YSI <-as.numeric(levels(dt1$BGA_logged_YSI))[as.integer(dt1$BGA_logged_YSI) ]               
if (class(dt1$BGA_logged_YSI)=="character") dt1$BGA_logged_YSI <-as.numeric(dt1$BGA_logged_YSI)
if (class(dt1$BGA_logged_HYLB)=="factor") dt1$BGA_logged_HYLB <-as.numeric(levels(dt1$BGA_logged_HYLB))[as.integer(dt1$BGA_logged_HYLB) ]               
if (class(dt1$BGA_logged_HYLB)=="character") dt1$BGA_logged_HYLB <-as.numeric(dt1$BGA_logged_HYLB)
if (class(dt1$DO_YSI)=="factor") dt1$DO_YSI <-as.numeric(levels(dt1$DO_YSI))[as.integer(dt1$DO_YSI) ]               
if (class(dt1$DO_YSI)=="character") dt1$DO_YSI <-as.numeric(dt1$DO_YSI)
if (class(dt1$DO_HYLB)=="factor") dt1$DO_HYLB <-as.numeric(levels(dt1$DO_HYLB))[as.integer(dt1$DO_HYLB) ]               
if (class(dt1$DO_HYLB)=="character") dt1$DO_HYLB <-as.numeric(dt1$DO_HYLB)
if (class(dt1$DOsat_calc_YSI)=="factor") dt1$DOsat_calc_YSI <-as.numeric(levels(dt1$DOsat_calc_YSI))[as.integer(dt1$DOsat_calc_YSI) ]               
if (class(dt1$DOsat_calc_YSI)=="character") dt1$DOsat_calc_YSI <-as.numeric(dt1$DOsat_calc_YSI)
if (class(dt1$DOsat_calc_HYLB)=="factor") dt1$DOsat_calc_HYLB <-as.numeric(levels(dt1$DOsat_calc_HYLB))[as.integer(dt1$DOsat_calc_HYLB) ]               
if (class(dt1$DOsat_calc_HYLB)=="character") dt1$DOsat_calc_HYLB <-as.numeric(dt1$DOsat_calc_HYLB)
if (class(dt1$PH_YSI)=="factor") dt1$PH_YSI <-as.numeric(levels(dt1$PH_YSI))[as.integer(dt1$PH_YSI) ]               
if (class(dt1$PH_YSI)=="character") dt1$PH_YSI <-as.numeric(dt1$PH_YSI)
if (class(dt1$PH_HYLB)=="factor") dt1$PH_HYLB <-as.numeric(levels(dt1$PH_HYLB))[as.integer(dt1$PH_HYLB) ]               
if (class(dt1$PH_HYLB)=="character") dt1$PH_HYLB <-as.numeric(dt1$PH_HYLB)
if (class(dt1$Chl_YSI)=="factor") dt1$Chl_YSI <-as.numeric(levels(dt1$Chl_YSI))[as.integer(dt1$Chl_YSI) ]               
if (class(dt1$Chl_YSI)=="character") dt1$Chl_YSI <-as.numeric(dt1$Chl_YSI)
if (class(dt1$Chl_HYLB)=="factor") dt1$Chl_HYLB <-as.numeric(levels(dt1$Chl_HYLB))[as.integer(dt1$Chl_HYLB) ]               
if (class(dt1$Chl_HYLB)=="character") dt1$Chl_HYLB <-as.numeric(dt1$Chl_HYLB)
if (class(dt1$Temp_YSI)=="factor") dt1$Temp_YSI <-as.numeric(levels(dt1$Temp_YSI))[as.integer(dt1$Temp_YSI) ]               
if (class(dt1$Temp_YSI)=="character") dt1$Temp_YSI <-as.numeric(dt1$Temp_YSI)
if (class(dt1$Temp_HYLB)=="factor") dt1$Temp_HYLB <-as.numeric(levels(dt1$Temp_HYLB))[as.integer(dt1$Temp_HYLB) ]               
if (class(dt1$Temp_HYLB)=="character") dt1$Temp_HYLB <-as.numeric(dt1$Temp_HYLB)

# Convert Missing Values to NA for non-dates

dt1$Year <- ifelse((trimws(as.character(dt1$Year))==trimws("NA")),NA,dt1$Year)               
suppressWarnings(dt1$Year <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Year))==as.character(as.numeric("NA"))),NA,dt1$Year))
dt1$DoY <- ifelse((trimws(as.character(dt1$DoY))==trimws("NA")),NA,dt1$DoY)               
suppressWarnings(dt1$DoY <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DoY))==as.character(as.numeric("NA"))),NA,dt1$DoY))
dt1$BGA_YSI <- ifelse((trimws(as.character(dt1$BGA_YSI))==trimws("NA")),NA,dt1$BGA_YSI)               
suppressWarnings(dt1$BGA_YSI <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$BGA_YSI))==as.character(as.numeric("NA"))),NA,dt1$BGA_YSI))
dt1$BGA_HYLB <- ifelse((trimws(as.character(dt1$BGA_HYLB))==trimws("NA")),NA,dt1$BGA_HYLB)               
suppressWarnings(dt1$BGA_HYLB <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$BGA_HYLB))==as.character(as.numeric("NA"))),NA,dt1$BGA_HYLB))
dt1$BGA_logged_YSI <- ifelse((trimws(as.character(dt1$BGA_logged_YSI))==trimws("NA")),NA,dt1$BGA_logged_YSI)               
suppressWarnings(dt1$BGA_logged_YSI <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$BGA_logged_YSI))==as.character(as.numeric("NA"))),NA,dt1$BGA_logged_YSI))
dt1$BGA_logged_HYLB <- ifelse((trimws(as.character(dt1$BGA_logged_HYLB))==trimws("NA")),NA,dt1$BGA_logged_HYLB)               
suppressWarnings(dt1$BGA_logged_HYLB <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$BGA_logged_HYLB))==as.character(as.numeric("NA"))),NA,dt1$BGA_logged_HYLB))
dt1$DO_YSI <- ifelse((trimws(as.character(dt1$DO_YSI))==trimws("NA")),NA,dt1$DO_YSI)               
suppressWarnings(dt1$DO_YSI <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DO_YSI))==as.character(as.numeric("NA"))),NA,dt1$DO_YSI))
dt1$DO_HYLB <- ifelse((trimws(as.character(dt1$DO_HYLB))==trimws("NA")),NA,dt1$DO_HYLB)               
suppressWarnings(dt1$DO_HYLB <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DO_HYLB))==as.character(as.numeric("NA"))),NA,dt1$DO_HYLB))
dt1$DOsat_calc_YSI <- ifelse((trimws(as.character(dt1$DOsat_calc_YSI))==trimws("NA")),NA,dt1$DOsat_calc_YSI)               
suppressWarnings(dt1$DOsat_calc_YSI <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DOsat_calc_YSI))==as.character(as.numeric("NA"))),NA,dt1$DOsat_calc_YSI))
dt1$DOsat_calc_HYLB <- ifelse((trimws(as.character(dt1$DOsat_calc_HYLB))==trimws("NA")),NA,dt1$DOsat_calc_HYLB)               
suppressWarnings(dt1$DOsat_calc_HYLB <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DOsat_calc_HYLB))==as.character(as.numeric("NA"))),NA,dt1$DOsat_calc_HYLB))
dt1$PH_YSI <- ifelse((trimws(as.character(dt1$PH_YSI))==trimws("NA")),NA,dt1$PH_YSI)               
suppressWarnings(dt1$PH_YSI <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$PH_YSI))==as.character(as.numeric("NA"))),NA,dt1$PH_YSI))
dt1$PH_HYLB <- ifelse((trimws(as.character(dt1$PH_HYLB))==trimws("NA")),NA,dt1$PH_HYLB)               
suppressWarnings(dt1$PH_HYLB <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$PH_HYLB))==as.character(as.numeric("NA"))),NA,dt1$PH_HYLB))
dt1$Chl_YSI <- ifelse((trimws(as.character(dt1$Chl_YSI))==trimws("NA")),NA,dt1$Chl_YSI)               
suppressWarnings(dt1$Chl_YSI <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Chl_YSI))==as.character(as.numeric("NA"))),NA,dt1$Chl_YSI))
dt1$Chl_HYLB <- ifelse((trimws(as.character(dt1$Chl_HYLB))==trimws("NA")),NA,dt1$Chl_HYLB)               
suppressWarnings(dt1$Chl_HYLB <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Chl_HYLB))==as.character(as.numeric("NA"))),NA,dt1$Chl_HYLB))
dt1$Temp_YSI <- ifelse((trimws(as.character(dt1$Temp_YSI))==trimws("NA")),NA,dt1$Temp_YSI)               
suppressWarnings(dt1$Temp_YSI <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Temp_YSI))==as.character(as.numeric("NA"))),NA,dt1$Temp_YSI))
dt1$Temp_HYLB <- ifelse((trimws(as.character(dt1$Temp_HYLB))==trimws("NA")),NA,dt1$Temp_HYLB)               
suppressWarnings(dt1$Temp_HYLB <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Temp_HYLB))==as.character(as.numeric("NA"))),NA,dt1$Temp_HYLB))


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(Year)
summary(Lake)
summary(DoY)
summary(BGA_YSI)
summary(BGA_HYLB)
summary(BGA_logged_YSI)
summary(BGA_logged_HYLB)
summary(DO_YSI)
summary(DO_HYLB)
summary(DOsat_calc_YSI)
summary(DOsat_calc_HYLB)
summary(PH_YSI)
summary(PH_HYLB)
summary(Chl_YSI)
summary(Chl_HYLB)
summary(Temp_YSI)
summary(Temp_HYLB) 
# Get more details on character variables

summary(as.factor(dt1$Lake))
detach(dt1)               



sonde13_15 = dt1





##### Testing calculating heatwaves #####
hwTest <- sonde08_11mean %>% rename(t = date, temp = mean_temp) %>% filter(lake == "Paul") %>% select(t, temp)

start = "2008-06-01"
end = "2011-08-15"

climOutput = ts2clm(hwTest, climatologyPeriod = c(start, end))
heatwaves = detect_event(climOutput)

event_line(heatwaves, metric = "intensity_max", start_date = start, end_date = end)+
  geom_point()


