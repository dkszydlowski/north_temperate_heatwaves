#### Woodruff airport meteorlogical data #####

# Package ID: knb-lter-ntl.17.38 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER: Meteorological Data - Woodruff Airport 1989 - current.
# Data set creator:  John Magnuson - University of Wisconsin-Madison 
# Data set creator:  Stephen Carpenter - University of Wisconsin-Madison 
# Data set creator:  Emily Stanley - University of Wisconsin-Madison 
# Metadata Provider:  NTL Information Manager - University of Wisconsin-Madison 
# Contact:    -  NTL LTER  - ntl.infomgr@gmail.com
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/17/38/9e39437554e8ebb54843b18e0ad27110" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "year4",     
                 "sampledate",     
                 "daynum",     
                 "avg_air_temp",     
                 "flag_avg_air_temp",     
                 "max_air_temp",     
                 "flag_max_air_temp",     
                 "min_air_temp",     
                 "flag_min_air_temp",     
                 "avg_dewpoint_temp",     
                 "flag_avg_dewpoint_temp",     
                 "max_dewpoint_temp",     
                 "flag_max_dewpoint_temp",     
                 "min_dewpoint_temp",     
                 "flag_min_dewpoint_temp",     
                 "avg_rel_hum",     
                 "flag_avg_rel_hum",     
                 "max_rel_hum",     
                 "flag_max_rel_hum",     
                 "min_rel_hum",     
                 "flag_min_rel_hum",     
                 "avg_barom_pres_mbar",     
                 "flag_avg_barom_pres_mbar",     
                 "avg_wind_speed",     
                 "flag_avg_wind_speed",     
                 "max_wind_speed_1min",     
                 "flag_max_wind_speed_1min",     
                 "direction_peak_wind",     
                 "flag_direction_peak_wind",     
                 "avg_par",     
                 "flag_avg_par",     
                 "avg_shortwave_rad",     
                 "flag_avg_shortwave_rad",     
                 "avg_sol_rad",     
                 "flag_avg_sol_rad",     
                 "avg_longwave_rad",     
                 "flag_avg_longwave_rad",     
                 "tot_precip",     
                 "flag_tot_precip"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$year4)=="factor") dt1$year4 <-as.numeric(levels(dt1$year4))[as.integer(dt1$year4) ]               
if (class(dt1$year4)=="character") dt1$year4 <-as.numeric(dt1$year4)                                   
# attempting to convert dt1$sampledate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1sampledate<-as.Date(dt1$sampledate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1sampledate) == length(tmp1sampledate[!is.na(tmp1sampledate)])){dt1$sampledate <- tmp1sampledate } else {print("Date conversion failed for dt1$sampledate. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1sampledate) 
if (class(dt1$daynum)=="factor") dt1$daynum <-as.numeric(levels(dt1$daynum))[as.integer(dt1$daynum) ]               
if (class(dt1$daynum)=="character") dt1$daynum <-as.numeric(dt1$daynum)
if (class(dt1$avg_air_temp)=="factor") dt1$avg_air_temp <-as.numeric(levels(dt1$avg_air_temp))[as.integer(dt1$avg_air_temp) ]               
if (class(dt1$avg_air_temp)=="character") dt1$avg_air_temp <-as.numeric(dt1$avg_air_temp)
if (class(dt1$flag_avg_air_temp)!="factor") dt1$flag_avg_air_temp<- as.factor(dt1$flag_avg_air_temp)
if (class(dt1$max_air_temp)=="factor") dt1$max_air_temp <-as.numeric(levels(dt1$max_air_temp))[as.integer(dt1$max_air_temp) ]               
if (class(dt1$max_air_temp)=="character") dt1$max_air_temp <-as.numeric(dt1$max_air_temp)
if (class(dt1$flag_max_air_temp)!="factor") dt1$flag_max_air_temp<- as.factor(dt1$flag_max_air_temp)
if (class(dt1$min_air_temp)=="factor") dt1$min_air_temp <-as.numeric(levels(dt1$min_air_temp))[as.integer(dt1$min_air_temp) ]               
if (class(dt1$min_air_temp)=="character") dt1$min_air_temp <-as.numeric(dt1$min_air_temp)
if (class(dt1$flag_min_air_temp)!="factor") dt1$flag_min_air_temp<- as.factor(dt1$flag_min_air_temp)
if (class(dt1$avg_dewpoint_temp)=="factor") dt1$avg_dewpoint_temp <-as.numeric(levels(dt1$avg_dewpoint_temp))[as.integer(dt1$avg_dewpoint_temp) ]               
if (class(dt1$avg_dewpoint_temp)=="character") dt1$avg_dewpoint_temp <-as.numeric(dt1$avg_dewpoint_temp)
if (class(dt1$flag_avg_dewpoint_temp)!="factor") dt1$flag_avg_dewpoint_temp<- as.factor(dt1$flag_avg_dewpoint_temp)
if (class(dt1$max_dewpoint_temp)=="factor") dt1$max_dewpoint_temp <-as.numeric(levels(dt1$max_dewpoint_temp))[as.integer(dt1$max_dewpoint_temp) ]               
if (class(dt1$max_dewpoint_temp)=="character") dt1$max_dewpoint_temp <-as.numeric(dt1$max_dewpoint_temp)
if (class(dt1$flag_max_dewpoint_temp)!="factor") dt1$flag_max_dewpoint_temp<- as.factor(dt1$flag_max_dewpoint_temp)
if (class(dt1$min_dewpoint_temp)=="factor") dt1$min_dewpoint_temp <-as.numeric(levels(dt1$min_dewpoint_temp))[as.integer(dt1$min_dewpoint_temp) ]               
if (class(dt1$min_dewpoint_temp)=="character") dt1$min_dewpoint_temp <-as.numeric(dt1$min_dewpoint_temp)
if (class(dt1$flag_min_dewpoint_temp)!="factor") dt1$flag_min_dewpoint_temp<- as.factor(dt1$flag_min_dewpoint_temp)
if (class(dt1$avg_rel_hum)=="factor") dt1$avg_rel_hum <-as.numeric(levels(dt1$avg_rel_hum))[as.integer(dt1$avg_rel_hum) ]               
if (class(dt1$avg_rel_hum)=="character") dt1$avg_rel_hum <-as.numeric(dt1$avg_rel_hum)
if (class(dt1$flag_avg_rel_hum)!="factor") dt1$flag_avg_rel_hum<- as.factor(dt1$flag_avg_rel_hum)
if (class(dt1$max_rel_hum)=="factor") dt1$max_rel_hum <-as.numeric(levels(dt1$max_rel_hum))[as.integer(dt1$max_rel_hum) ]               
if (class(dt1$max_rel_hum)=="character") dt1$max_rel_hum <-as.numeric(dt1$max_rel_hum)
if (class(dt1$flag_max_rel_hum)!="factor") dt1$flag_max_rel_hum<- as.factor(dt1$flag_max_rel_hum)
if (class(dt1$min_rel_hum)=="factor") dt1$min_rel_hum <-as.numeric(levels(dt1$min_rel_hum))[as.integer(dt1$min_rel_hum) ]               
if (class(dt1$min_rel_hum)=="character") dt1$min_rel_hum <-as.numeric(dt1$min_rel_hum)
if (class(dt1$flag_min_rel_hum)!="factor") dt1$flag_min_rel_hum<- as.factor(dt1$flag_min_rel_hum)
if (class(dt1$avg_barom_pres_mbar)=="factor") dt1$avg_barom_pres_mbar <-as.numeric(levels(dt1$avg_barom_pres_mbar))[as.integer(dt1$avg_barom_pres_mbar) ]               
if (class(dt1$avg_barom_pres_mbar)=="character") dt1$avg_barom_pres_mbar <-as.numeric(dt1$avg_barom_pres_mbar)
if (class(dt1$flag_avg_barom_pres_mbar)!="factor") dt1$flag_avg_barom_pres_mbar<- as.factor(dt1$flag_avg_barom_pres_mbar)
if (class(dt1$avg_wind_speed)=="factor") dt1$avg_wind_speed <-as.numeric(levels(dt1$avg_wind_speed))[as.integer(dt1$avg_wind_speed) ]               
if (class(dt1$avg_wind_speed)=="character") dt1$avg_wind_speed <-as.numeric(dt1$avg_wind_speed)
if (class(dt1$flag_avg_wind_speed)!="factor") dt1$flag_avg_wind_speed<- as.factor(dt1$flag_avg_wind_speed)
if (class(dt1$max_wind_speed_1min)=="factor") dt1$max_wind_speed_1min <-as.numeric(levels(dt1$max_wind_speed_1min))[as.integer(dt1$max_wind_speed_1min) ]               
if (class(dt1$max_wind_speed_1min)=="character") dt1$max_wind_speed_1min <-as.numeric(dt1$max_wind_speed_1min)
if (class(dt1$flag_max_wind_speed_1min)!="factor") dt1$flag_max_wind_speed_1min<- as.factor(dt1$flag_max_wind_speed_1min)
if (class(dt1$direction_peak_wind)=="factor") dt1$direction_peak_wind <-as.numeric(levels(dt1$direction_peak_wind))[as.integer(dt1$direction_peak_wind) ]               
if (class(dt1$direction_peak_wind)=="character") dt1$direction_peak_wind <-as.numeric(dt1$direction_peak_wind)
if (class(dt1$flag_direction_peak_wind)!="factor") dt1$flag_direction_peak_wind<- as.factor(dt1$flag_direction_peak_wind)
if (class(dt1$avg_par)=="factor") dt1$avg_par <-as.numeric(levels(dt1$avg_par))[as.integer(dt1$avg_par) ]               
if (class(dt1$avg_par)=="character") dt1$avg_par <-as.numeric(dt1$avg_par)
if (class(dt1$flag_avg_par)!="factor") dt1$flag_avg_par<- as.factor(dt1$flag_avg_par)
if (class(dt1$avg_shortwave_rad)=="factor") dt1$avg_shortwave_rad <-as.numeric(levels(dt1$avg_shortwave_rad))[as.integer(dt1$avg_shortwave_rad) ]               
if (class(dt1$avg_shortwave_rad)=="character") dt1$avg_shortwave_rad <-as.numeric(dt1$avg_shortwave_rad)
if (class(dt1$flag_avg_shortwave_rad)!="factor") dt1$flag_avg_shortwave_rad<- as.factor(dt1$flag_avg_shortwave_rad)
if (class(dt1$avg_sol_rad)=="factor") dt1$avg_sol_rad <-as.numeric(levels(dt1$avg_sol_rad))[as.integer(dt1$avg_sol_rad) ]               
if (class(dt1$avg_sol_rad)=="character") dt1$avg_sol_rad <-as.numeric(dt1$avg_sol_rad)
if (class(dt1$flag_avg_sol_rad)!="factor") dt1$flag_avg_sol_rad<- as.factor(dt1$flag_avg_sol_rad)
if (class(dt1$avg_longwave_rad)=="factor") dt1$avg_longwave_rad <-as.numeric(levels(dt1$avg_longwave_rad))[as.integer(dt1$avg_longwave_rad) ]               
if (class(dt1$avg_longwave_rad)=="character") dt1$avg_longwave_rad <-as.numeric(dt1$avg_longwave_rad)
if (class(dt1$flag_avg_longwave_rad)!="factor") dt1$flag_avg_longwave_rad<- as.factor(dt1$flag_avg_longwave_rad)
if (class(dt1$tot_precip)=="factor") dt1$tot_precip <-as.numeric(levels(dt1$tot_precip))[as.integer(dt1$tot_precip) ]               
if (class(dt1$tot_precip)=="character") dt1$tot_precip <-as.numeric(dt1$tot_precip)
if (class(dt1$flag_tot_precip)!="factor") dt1$flag_tot_precip<- as.factor(dt1$flag_tot_precip)

# Convert Missing Values to NA for non-dates



# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(year4)
summary(sampledate)
summary(daynum)
summary(avg_air_temp)
summary(flag_avg_air_temp)
summary(max_air_temp)
summary(flag_max_air_temp)
summary(min_air_temp)
summary(flag_min_air_temp)
summary(avg_dewpoint_temp)
summary(flag_avg_dewpoint_temp)
summary(max_dewpoint_temp)
summary(flag_max_dewpoint_temp)
summary(min_dewpoint_temp)
summary(flag_min_dewpoint_temp)
summary(avg_rel_hum)
summary(flag_avg_rel_hum)
summary(max_rel_hum)
summary(flag_max_rel_hum)
summary(min_rel_hum)
summary(flag_min_rel_hum)
summary(avg_barom_pres_mbar)
summary(flag_avg_barom_pres_mbar)
summary(avg_wind_speed)
summary(flag_avg_wind_speed)
summary(max_wind_speed_1min)
summary(flag_max_wind_speed_1min)
summary(direction_peak_wind)
summary(flag_direction_peak_wind)
summary(avg_par)
summary(flag_avg_par)
summary(avg_shortwave_rad)
summary(flag_avg_shortwave_rad)
summary(avg_sol_rad)
summary(flag_avg_sol_rad)
summary(avg_longwave_rad)
summary(flag_avg_longwave_rad)
summary(tot_precip)
summary(flag_tot_precip) 
# Get more details on character variables

summary(as.factor(dt1$flag_avg_air_temp)) 
summary(as.factor(dt1$flag_max_air_temp)) 
summary(as.factor(dt1$flag_min_air_temp)) 
summary(as.factor(dt1$flag_avg_dewpoint_temp)) 
summary(as.factor(dt1$flag_max_dewpoint_temp)) 
summary(as.factor(dt1$flag_min_dewpoint_temp)) 
summary(as.factor(dt1$flag_avg_rel_hum)) 
summary(as.factor(dt1$flag_max_rel_hum)) 
summary(as.factor(dt1$flag_min_rel_hum)) 
summary(as.factor(dt1$flag_avg_barom_pres_mbar)) 
summary(as.factor(dt1$flag_avg_wind_speed)) 
summary(as.factor(dt1$flag_max_wind_speed_1min)) 
summary(as.factor(dt1$flag_direction_peak_wind)) 
summary(as.factor(dt1$flag_avg_par)) 
summary(as.factor(dt1$flag_avg_shortwave_rad)) 
summary(as.factor(dt1$flag_avg_sol_rad)) 
summary(as.factor(dt1$flag_avg_longwave_rad)) 
summary(as.factor(dt1$flag_tot_precip))
detach(dt1)               


inUrl2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/17/38/243910e4bc06665975139a06082e64b9" 
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl"))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")


dt2 <-read.csv(infile2,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "year4",     
                 "sampledate",     
                 "daynum",     
                 "hour",     
                 "avg_air_temp",     
                 "flag_avg_air_temp",     
                 "avg_dewpoint_temp",     
                 "flag_avg_dewpoint_temp",     
                 "avg_rel_hum",     
                 "flag_avg_rel_hum",     
                 "avg_barom_pres_mbar",     
                 "flag_avg_barom_pres_mbar",     
                 "avg_wind_speed",     
                 "flag_avg_wind_speed",     
                 "max_wind_speed_1min",     
                 "flag_max_wind_speed_1min",     
                 "std_dev_wind_direction",     
                 "flag_std_dev_wind_direction",     
                 "avg_par",     
                 "flag_avg_par",     
                 "avg_shortwave_rad_licor",     
                 "flag_avg_sw_rad_licor",     
                 "avg_sw_sol_rad_eppley",     
                 "flag_avg_sw_rad_eppley",     
                 "avg_longwave_rad_eppley",     
                 "flag_avg_longwave_rad_eppley",     
                 "tot_precip",     
                 "flag_tot_precip"    ), check.names=TRUE)

unlink(infile2)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt2$year4)=="factor") dt2$year4 <-as.numeric(levels(dt2$year4))[as.integer(dt2$year4) ]               
if (class(dt2$year4)=="character") dt2$year4 <-as.numeric(dt2$year4)                                   
# attempting to convert dt2$sampledate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp2sampledate<-as.Date(dt2$sampledate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp2sampledate) == length(tmp2sampledate[!is.na(tmp2sampledate)])){dt2$sampledate <- tmp2sampledate } else {print("Date conversion failed for dt2$sampledate. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp2sampledate) 
if (class(dt2$daynum)=="factor") dt2$daynum <-as.numeric(levels(dt2$daynum))[as.integer(dt2$daynum) ]               
if (class(dt2$daynum)=="character") dt2$daynum <-as.numeric(dt2$daynum)
if (class(dt2$hour)=="factor") dt2$hour <-as.numeric(levels(dt2$hour))[as.integer(dt2$hour) ]               
if (class(dt2$hour)=="character") dt2$hour <-as.numeric(dt2$hour)
if (class(dt2$avg_air_temp)=="factor") dt2$avg_air_temp <-as.numeric(levels(dt2$avg_air_temp))[as.integer(dt2$avg_air_temp) ]               
if (class(dt2$avg_air_temp)=="character") dt2$avg_air_temp <-as.numeric(dt2$avg_air_temp)
if (class(dt2$flag_avg_air_temp)!="factor") dt2$flag_avg_air_temp<- as.factor(dt2$flag_avg_air_temp)
if (class(dt2$avg_dewpoint_temp)=="factor") dt2$avg_dewpoint_temp <-as.numeric(levels(dt2$avg_dewpoint_temp))[as.integer(dt2$avg_dewpoint_temp) ]               
if (class(dt2$avg_dewpoint_temp)=="character") dt2$avg_dewpoint_temp <-as.numeric(dt2$avg_dewpoint_temp)
if (class(dt2$flag_avg_dewpoint_temp)!="factor") dt2$flag_avg_dewpoint_temp<- as.factor(dt2$flag_avg_dewpoint_temp)
if (class(dt2$avg_rel_hum)=="factor") dt2$avg_rel_hum <-as.numeric(levels(dt2$avg_rel_hum))[as.integer(dt2$avg_rel_hum) ]               
if (class(dt2$avg_rel_hum)=="character") dt2$avg_rel_hum <-as.numeric(dt2$avg_rel_hum)
if (class(dt2$flag_avg_rel_hum)!="factor") dt2$flag_avg_rel_hum<- as.factor(dt2$flag_avg_rel_hum)
if (class(dt2$avg_barom_pres_mbar)=="factor") dt2$avg_barom_pres_mbar <-as.numeric(levels(dt2$avg_barom_pres_mbar))[as.integer(dt2$avg_barom_pres_mbar) ]               
if (class(dt2$avg_barom_pres_mbar)=="character") dt2$avg_barom_pres_mbar <-as.numeric(dt2$avg_barom_pres_mbar)
if (class(dt2$flag_avg_barom_pres_mbar)!="factor") dt2$flag_avg_barom_pres_mbar<- as.factor(dt2$flag_avg_barom_pres_mbar)
if (class(dt2$avg_wind_speed)=="factor") dt2$avg_wind_speed <-as.numeric(levels(dt2$avg_wind_speed))[as.integer(dt2$avg_wind_speed) ]               
if (class(dt2$avg_wind_speed)=="character") dt2$avg_wind_speed <-as.numeric(dt2$avg_wind_speed)
if (class(dt2$flag_avg_wind_speed)!="factor") dt2$flag_avg_wind_speed<- as.factor(dt2$flag_avg_wind_speed)
if (class(dt2$max_wind_speed_1min)=="factor") dt2$max_wind_speed_1min <-as.numeric(levels(dt2$max_wind_speed_1min))[as.integer(dt2$max_wind_speed_1min) ]               
if (class(dt2$max_wind_speed_1min)=="character") dt2$max_wind_speed_1min <-as.numeric(dt2$max_wind_speed_1min)
if (class(dt2$flag_max_wind_speed_1min)!="factor") dt2$flag_max_wind_speed_1min<- as.factor(dt2$flag_max_wind_speed_1min)
if (class(dt2$std_dev_wind_direction)=="factor") dt2$std_dev_wind_direction <-as.numeric(levels(dt2$std_dev_wind_direction))[as.integer(dt2$std_dev_wind_direction) ]               
if (class(dt2$std_dev_wind_direction)=="character") dt2$std_dev_wind_direction <-as.numeric(dt2$std_dev_wind_direction)
if (class(dt2$flag_std_dev_wind_direction)!="factor") dt2$flag_std_dev_wind_direction<- as.factor(dt2$flag_std_dev_wind_direction)
if (class(dt2$avg_par)=="factor") dt2$avg_par <-as.numeric(levels(dt2$avg_par))[as.integer(dt2$avg_par) ]               
if (class(dt2$avg_par)=="character") dt2$avg_par <-as.numeric(dt2$avg_par)
if (class(dt2$flag_avg_par)!="factor") dt2$flag_avg_par<- as.factor(dt2$flag_avg_par)
if (class(dt2$avg_shortwave_rad_licor)=="factor") dt2$avg_shortwave_rad_licor <-as.numeric(levels(dt2$avg_shortwave_rad_licor))[as.integer(dt2$avg_shortwave_rad_licor) ]               
if (class(dt2$avg_shortwave_rad_licor)=="character") dt2$avg_shortwave_rad_licor <-as.numeric(dt2$avg_shortwave_rad_licor)
if (class(dt2$flag_avg_sw_rad_licor)!="factor") dt2$flag_avg_sw_rad_licor<- as.factor(dt2$flag_avg_sw_rad_licor)
if (class(dt2$avg_sw_sol_rad_eppley)=="factor") dt2$avg_sw_sol_rad_eppley <-as.numeric(levels(dt2$avg_sw_sol_rad_eppley))[as.integer(dt2$avg_sw_sol_rad_eppley) ]               
if (class(dt2$avg_sw_sol_rad_eppley)=="character") dt2$avg_sw_sol_rad_eppley <-as.numeric(dt2$avg_sw_sol_rad_eppley)
if (class(dt2$flag_avg_sw_rad_eppley)!="factor") dt2$flag_avg_sw_rad_eppley<- as.factor(dt2$flag_avg_sw_rad_eppley)
if (class(dt2$avg_longwave_rad_eppley)=="factor") dt2$avg_longwave_rad_eppley <-as.numeric(levels(dt2$avg_longwave_rad_eppley))[as.integer(dt2$avg_longwave_rad_eppley) ]               
if (class(dt2$avg_longwave_rad_eppley)=="character") dt2$avg_longwave_rad_eppley <-as.numeric(dt2$avg_longwave_rad_eppley)
if (class(dt2$flag_avg_longwave_rad_eppley)!="factor") dt2$flag_avg_longwave_rad_eppley<- as.factor(dt2$flag_avg_longwave_rad_eppley)
if (class(dt2$tot_precip)=="factor") dt2$tot_precip <-as.numeric(levels(dt2$tot_precip))[as.integer(dt2$tot_precip) ]               
if (class(dt2$tot_precip)=="character") dt2$tot_precip <-as.numeric(dt2$tot_precip)
if (class(dt2$flag_tot_precip)!="factor") dt2$flag_tot_precip<- as.factor(dt2$flag_tot_precip)

# Convert Missing Values to NA for non-dates



# Here is the structure of the input data frame:
str(dt2)                            
attach(dt2)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(year4)
summary(sampledate)
summary(daynum)
summary(hour)
summary(avg_air_temp)
summary(flag_avg_air_temp)
summary(avg_dewpoint_temp)
summary(flag_avg_dewpoint_temp)
summary(avg_rel_hum)
summary(flag_avg_rel_hum)
summary(avg_barom_pres_mbar)
summary(flag_avg_barom_pres_mbar)
summary(avg_wind_speed)
summary(flag_avg_wind_speed)
summary(max_wind_speed_1min)
summary(flag_max_wind_speed_1min)
summary(std_dev_wind_direction)
summary(flag_std_dev_wind_direction)
summary(avg_par)
summary(flag_avg_par)
summary(avg_shortwave_rad_licor)
summary(flag_avg_sw_rad_licor)
summary(avg_sw_sol_rad_eppley)
summary(flag_avg_sw_rad_eppley)
summary(avg_longwave_rad_eppley)
summary(flag_avg_longwave_rad_eppley)
summary(tot_precip)
summary(flag_tot_precip) 
# Get more details on character variables

summary(as.factor(dt2$flag_avg_air_temp)) 
summary(as.factor(dt2$flag_avg_dewpoint_temp)) 
summary(as.factor(dt2$flag_avg_rel_hum)) 
summary(as.factor(dt2$flag_avg_barom_pres_mbar)) 
summary(as.factor(dt2$flag_avg_wind_speed)) 
summary(as.factor(dt2$flag_max_wind_speed_1min)) 
summary(as.factor(dt2$flag_std_dev_wind_direction)) 
summary(as.factor(dt2$flag_avg_par)) 
summary(as.factor(dt2$flag_avg_sw_rad_licor)) 
summary(as.factor(dt2$flag_avg_sw_rad_eppley)) 
summary(as.factor(dt2$flag_avg_longwave_rad_eppley)) 
summary(as.factor(dt2$flag_tot_precip))
detach(dt2)               


inUrl3  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/17/38/fa72ddcbd255790b68cf31a5a629fdf7" 
infile3 <- tempfile()
try(download.file(inUrl3,infile3,method="curl"))
if (is.na(file.size(infile3))) download.file(inUrl3,infile3,method="auto")


dt3 <-read.csv(infile3,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "year4",     
                 "sampledate",     
                 "daynum",     
                 "sampletime",     
                 "avg_par",     
                 "flag_avg_par",     
                 "avg_shortwave_rad_licor",     
                 "flag_avg_sw_rad_licor",     
                 "avg_sw_sol_rad_eppley",     
                 "flag_avg_sw_rad_eppley"    ), check.names=TRUE)

unlink(infile3)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt3$year4)=="factor") dt3$year4 <-as.numeric(levels(dt3$year4))[as.integer(dt3$year4) ]               
if (class(dt3$year4)=="character") dt3$year4 <-as.numeric(dt3$year4)                                   
# attempting to convert dt3$sampledate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp3sampledate<-as.Date(dt3$sampledate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp3sampledate) == length(tmp3sampledate[!is.na(tmp3sampledate)])){dt3$sampledate <- tmp3sampledate } else {print("Date conversion failed for dt3$sampledate. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp3sampledate) 
if (class(dt3$daynum)=="factor") dt3$daynum <-as.numeric(levels(dt3$daynum))[as.integer(dt3$daynum) ]               
if (class(dt3$daynum)=="character") dt3$daynum <-as.numeric(dt3$daynum)
if (class(dt3$avg_par)=="factor") dt3$avg_par <-as.numeric(levels(dt3$avg_par))[as.integer(dt3$avg_par) ]               
if (class(dt3$avg_par)=="character") dt3$avg_par <-as.numeric(dt3$avg_par)
if (class(dt3$flag_avg_par)!="factor") dt3$flag_avg_par<- as.factor(dt3$flag_avg_par)
if (class(dt3$avg_shortwave_rad_licor)=="factor") dt3$avg_shortwave_rad_licor <-as.numeric(levels(dt3$avg_shortwave_rad_licor))[as.integer(dt3$avg_shortwave_rad_licor) ]               
if (class(dt3$avg_shortwave_rad_licor)=="character") dt3$avg_shortwave_rad_licor <-as.numeric(dt3$avg_shortwave_rad_licor)
if (class(dt3$flag_avg_sw_rad_licor)!="factor") dt3$flag_avg_sw_rad_licor<- as.factor(dt3$flag_avg_sw_rad_licor)
if (class(dt3$avg_sw_sol_rad_eppley)=="factor") dt3$avg_sw_sol_rad_eppley <-as.numeric(levels(dt3$avg_sw_sol_rad_eppley))[as.integer(dt3$avg_sw_sol_rad_eppley) ]               
if (class(dt3$avg_sw_sol_rad_eppley)=="character") dt3$avg_sw_sol_rad_eppley <-as.numeric(dt3$avg_sw_sol_rad_eppley)
if (class(dt3$flag_avg_sw_rad_eppley)!="factor") dt3$flag_avg_sw_rad_eppley<- as.factor(dt3$flag_avg_sw_rad_eppley)

# Convert Missing Values to NA for non-dates



# Here is the structure of the input data frame:
str(dt3)                            
attach(dt3)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(year4)
summary(sampledate)
summary(daynum)
summary(sampletime)
summary(avg_par)
summary(flag_avg_par)
summary(avg_shortwave_rad_licor)
summary(flag_avg_sw_rad_licor)
summary(avg_sw_sol_rad_eppley)
summary(flag_avg_sw_rad_eppley) 
# Get more details on character variables

summary(as.factor(dt3$flag_avg_par)) 
summary(as.factor(dt3$flag_avg_sw_rad_licor)) 
summary(as.factor(dt3$flag_avg_sw_rad_eppley))
detach(dt3)               


inUrl4  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/17/38/dd95edb804b0f222424b2efe5547ce68" 
infile4 <- tempfile()
try(download.file(inUrl4,infile4,method="curl"))
if (is.na(file.size(infile4))) download.file(inUrl4,infile4,method="auto")


dt4 <-read.csv(infile4,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "year4",     
                 "sampledate",     
                 "sampletime",     
                 "month",     
                 "daynum",     
                 "data_freq",     
                 "air_temp",     
                 "flag_air_temp",     
                 "dewpoint_temp",     
                 "flag_dewpoint_temp",     
                 "rel_hum",     
                 "flag_rel_hum",     
                 "barom_pres",     
                 "flag_barom_pres",     
                 "wind_speed",     
                 "flag_wind_speed",     
                 "resultant_wind_speed",     
                 "flag_resultant_wind_speed",     
                 "max_wind_speed_1min",     
                 "flag_max_wind_speed_1min",     
                 "wind_direction",     
                 "flag_wind_direction",     
                 "resultant_wind_direction",     
                 "flag_resultant_wind_direction",     
                 "std_dev_wind_direction",     
                 "flag_std_dev_wind_direction",     
                 "par",     
                 "flag_par",     
                 "shortwave_rad_licor",     
                 "flag_shortwave_rad_licor",     
                 "sw_sol_rad_eppley",     
                 "flag_sw_sol_rad_eppley",     
                 "longwave_rad_eppley",     
                 "flag_longwave_rad_eppley",     
                 "tot_precip",     
                 "flag_tot_precip"    ), check.names=TRUE)

unlink(infile4)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt4$year4)=="factor") dt4$year4 <-as.numeric(levels(dt4$year4))[as.integer(dt4$year4) ]               
if (class(dt4$year4)=="character") dt4$year4 <-as.numeric(dt4$year4)                                   
# attempting to convert dt4$sampledate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp4sampledate<-as.Date(dt4$sampledate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp4sampledate) == length(tmp4sampledate[!is.na(tmp4sampledate)])){dt4$sampledate <- tmp4sampledate } else {print("Date conversion failed for dt4$sampledate. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp4sampledate) 
if (class(dt4$month)=="factor") dt4$month <-as.numeric(levels(dt4$month))[as.integer(dt4$month) ]               
if (class(dt4$month)=="character") dt4$month <-as.numeric(dt4$month)
if (class(dt4$daynum)=="factor") dt4$daynum <-as.numeric(levels(dt4$daynum))[as.integer(dt4$daynum) ]               
if (class(dt4$daynum)=="character") dt4$daynum <-as.numeric(dt4$daynum)
if (class(dt4$data_freq)!="factor") dt4$data_freq<- as.factor(dt4$data_freq)
if (class(dt4$air_temp)=="factor") dt4$air_temp <-as.numeric(levels(dt4$air_temp))[as.integer(dt4$air_temp) ]               
if (class(dt4$air_temp)=="character") dt4$air_temp <-as.numeric(dt4$air_temp)
if (class(dt4$flag_air_temp)!="factor") dt4$flag_air_temp<- as.factor(dt4$flag_air_temp)
if (class(dt4$dewpoint_temp)=="factor") dt4$dewpoint_temp <-as.numeric(levels(dt4$dewpoint_temp))[as.integer(dt4$dewpoint_temp) ]               
if (class(dt4$dewpoint_temp)=="character") dt4$dewpoint_temp <-as.numeric(dt4$dewpoint_temp)
if (class(dt4$flag_dewpoint_temp)!="factor") dt4$flag_dewpoint_temp<- as.factor(dt4$flag_dewpoint_temp)
if (class(dt4$rel_hum)=="factor") dt4$rel_hum <-as.numeric(levels(dt4$rel_hum))[as.integer(dt4$rel_hum) ]               
if (class(dt4$rel_hum)=="character") dt4$rel_hum <-as.numeric(dt4$rel_hum)
if (class(dt4$flag_rel_hum)!="factor") dt4$flag_rel_hum<- as.factor(dt4$flag_rel_hum)
if (class(dt4$barom_pres)=="factor") dt4$barom_pres <-as.numeric(levels(dt4$barom_pres))[as.integer(dt4$barom_pres) ]               
if (class(dt4$barom_pres)=="character") dt4$barom_pres <-as.numeric(dt4$barom_pres)
if (class(dt4$flag_barom_pres)!="factor") dt4$flag_barom_pres<- as.factor(dt4$flag_barom_pres)
if (class(dt4$wind_speed)=="factor") dt4$wind_speed <-as.numeric(levels(dt4$wind_speed))[as.integer(dt4$wind_speed) ]               
if (class(dt4$wind_speed)=="character") dt4$wind_speed <-as.numeric(dt4$wind_speed)
if (class(dt4$flag_wind_speed)!="factor") dt4$flag_wind_speed<- as.factor(dt4$flag_wind_speed)
if (class(dt4$resultant_wind_speed)=="factor") dt4$resultant_wind_speed <-as.numeric(levels(dt4$resultant_wind_speed))[as.integer(dt4$resultant_wind_speed) ]               
if (class(dt4$resultant_wind_speed)=="character") dt4$resultant_wind_speed <-as.numeric(dt4$resultant_wind_speed)
if (class(dt4$flag_resultant_wind_speed)!="factor") dt4$flag_resultant_wind_speed<- as.factor(dt4$flag_resultant_wind_speed)
if (class(dt4$max_wind_speed_1min)=="factor") dt4$max_wind_speed_1min <-as.numeric(levels(dt4$max_wind_speed_1min))[as.integer(dt4$max_wind_speed_1min) ]               
if (class(dt4$max_wind_speed_1min)=="character") dt4$max_wind_speed_1min <-as.numeric(dt4$max_wind_speed_1min)
if (class(dt4$flag_max_wind_speed_1min)!="factor") dt4$flag_max_wind_speed_1min<- as.factor(dt4$flag_max_wind_speed_1min)
if (class(dt4$wind_direction)=="factor") dt4$wind_direction <-as.numeric(levels(dt4$wind_direction))[as.integer(dt4$wind_direction) ]               
if (class(dt4$wind_direction)=="character") dt4$wind_direction <-as.numeric(dt4$wind_direction)
if (class(dt4$flag_wind_direction)!="factor") dt4$flag_wind_direction<- as.factor(dt4$flag_wind_direction)
if (class(dt4$resultant_wind_direction)=="factor") dt4$resultant_wind_direction <-as.numeric(levels(dt4$resultant_wind_direction))[as.integer(dt4$resultant_wind_direction) ]               
if (class(dt4$resultant_wind_direction)=="character") dt4$resultant_wind_direction <-as.numeric(dt4$resultant_wind_direction)
if (class(dt4$flag_resultant_wind_direction)!="factor") dt4$flag_resultant_wind_direction<- as.factor(dt4$flag_resultant_wind_direction)
if (class(dt4$std_dev_wind_direction)=="factor") dt4$std_dev_wind_direction <-as.numeric(levels(dt4$std_dev_wind_direction))[as.integer(dt4$std_dev_wind_direction) ]               
if (class(dt4$std_dev_wind_direction)=="character") dt4$std_dev_wind_direction <-as.numeric(dt4$std_dev_wind_direction)
if (class(dt4$flag_std_dev_wind_direction)!="factor") dt4$flag_std_dev_wind_direction<- as.factor(dt4$flag_std_dev_wind_direction)
if (class(dt4$par)=="factor") dt4$par <-as.numeric(levels(dt4$par))[as.integer(dt4$par) ]               
if (class(dt4$par)=="character") dt4$par <-as.numeric(dt4$par)
if (class(dt4$flag_par)!="factor") dt4$flag_par<- as.factor(dt4$flag_par)
if (class(dt4$shortwave_rad_licor)=="factor") dt4$shortwave_rad_licor <-as.numeric(levels(dt4$shortwave_rad_licor))[as.integer(dt4$shortwave_rad_licor) ]               
if (class(dt4$shortwave_rad_licor)=="character") dt4$shortwave_rad_licor <-as.numeric(dt4$shortwave_rad_licor)
if (class(dt4$flag_shortwave_rad_licor)!="factor") dt4$flag_shortwave_rad_licor<- as.factor(dt4$flag_shortwave_rad_licor)
if (class(dt4$sw_sol_rad_eppley)=="factor") dt4$sw_sol_rad_eppley <-as.numeric(levels(dt4$sw_sol_rad_eppley))[as.integer(dt4$sw_sol_rad_eppley) ]               
if (class(dt4$sw_sol_rad_eppley)=="character") dt4$sw_sol_rad_eppley <-as.numeric(dt4$sw_sol_rad_eppley)
if (class(dt4$flag_sw_sol_rad_eppley)!="factor") dt4$flag_sw_sol_rad_eppley<- as.factor(dt4$flag_sw_sol_rad_eppley)
if (class(dt4$longwave_rad_eppley)=="factor") dt4$longwave_rad_eppley <-as.numeric(levels(dt4$longwave_rad_eppley))[as.integer(dt4$longwave_rad_eppley) ]               
if (class(dt4$longwave_rad_eppley)=="character") dt4$longwave_rad_eppley <-as.numeric(dt4$longwave_rad_eppley)
if (class(dt4$flag_longwave_rad_eppley)!="factor") dt4$flag_longwave_rad_eppley<- as.factor(dt4$flag_longwave_rad_eppley)
if (class(dt4$tot_precip)=="factor") dt4$tot_precip <-as.numeric(levels(dt4$tot_precip))[as.integer(dt4$tot_precip) ]               
if (class(dt4$tot_precip)=="character") dt4$tot_precip <-as.numeric(dt4$tot_precip)
if (class(dt4$flag_tot_precip)!="factor") dt4$flag_tot_precip<- as.factor(dt4$flag_tot_precip)

# Convert Missing Values to NA for non-dates



# Here is the structure of the input data frame:
str(dt4)                            
attach(dt4)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(year4)
summary(sampledate)
summary(sampletime)
summary(month)
summary(daynum)
summary(data_freq)
summary(air_temp)
summary(flag_air_temp)
summary(dewpoint_temp)
summary(flag_dewpoint_temp)
summary(rel_hum)
summary(flag_rel_hum)
summary(barom_pres)
summary(flag_barom_pres)
summary(wind_speed)
summary(flag_wind_speed)
summary(resultant_wind_speed)
summary(flag_resultant_wind_speed)
summary(max_wind_speed_1min)
summary(flag_max_wind_speed_1min)
summary(wind_direction)
summary(flag_wind_direction)
summary(resultant_wind_direction)
summary(flag_resultant_wind_direction)
summary(std_dev_wind_direction)
summary(flag_std_dev_wind_direction)
summary(par)
summary(flag_par)
summary(shortwave_rad_licor)
summary(flag_shortwave_rad_licor)
summary(sw_sol_rad_eppley)
summary(flag_sw_sol_rad_eppley)
summary(longwave_rad_eppley)
summary(flag_longwave_rad_eppley)
summary(tot_precip)
summary(flag_tot_precip) 
# Get more details on character variables

summary(as.factor(dt4$data_freq)) 
summary(as.factor(dt4$flag_air_temp)) 
summary(as.factor(dt4$flag_dewpoint_temp)) 
summary(as.factor(dt4$flag_rel_hum)) 
summary(as.factor(dt4$flag_barom_pres)) 
summary(as.factor(dt4$flag_wind_speed)) 
summary(as.factor(dt4$flag_resultant_wind_speed)) 
summary(as.factor(dt4$flag_max_wind_speed_1min)) 
summary(as.factor(dt4$flag_wind_direction)) 
summary(as.factor(dt4$flag_resultant_wind_direction)) 
summary(as.factor(dt4$flag_std_dev_wind_direction)) 
summary(as.factor(dt4$flag_par)) 
summary(as.factor(dt4$flag_shortwave_rad_licor)) 
summary(as.factor(dt4$flag_sw_sol_rad_eppley)) 
summary(as.factor(dt4$flag_longwave_rad_eppley)) 
summary(as.factor(dt4$flag_tot_precip))
detach(dt4)               




woodruff.temp = dt2

woodruff.temp$sampledate = as.Date(woodruff.temp$sampledate)

ggplot(woodruff.temp, aes(x = sampledate, y = avg_air_temp))+
  geom_line(alpha = 0.5)

write.csv(woodruff.temp, "./formatted data/LTER daily temperature/woodruff airport temperature LTER.csv", row.names = FALSE)


