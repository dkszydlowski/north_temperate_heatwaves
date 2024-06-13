#### Examine synchrony of heatwaves in LTER lakes

# This code does the following:
# 1. Downloads the LTER daily temperature data
# 2. Formats the temps so it is compatible with heatwaveR, which needs files with t,
#    the date formatted as "yyyy-mm-dd", and temp, the water temp

# Sparkling Lake

# Package ID: knb-lter-ntl.5.24 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER: High Frequency Water Temperature Data - Sparkling Lake Raft 1989 - current.
# Data set creator:  John Magnuson - University of Wisconsin-Madison 
# Data set creator:  Stephen Carpenter - University of Wisconsin-Madison 
# Data set creator:  Emily Stanley - University of Wisconsin-Madison 
# Metadata Provider:  NTL Information Manager - University of Wisconsin-Madison 
# Contact:    -  NTL LTER  - ntl.infomgr@gmail.com
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

#

##### DOWNLOAD DATA ####

## Sparkling Lake
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/5/24/8d02edd842ec6aaa444ae0948e6c7383" 
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
                 "depth",     
                 "wtemp",     
                 "flag_wtemp"    ), check.names=TRUE)

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
if (class(dt1$depth)=="factor") dt1$depth <-as.numeric(levels(dt1$depth))[as.integer(dt1$depth) ]               
if (class(dt1$depth)=="character") dt1$depth <-as.numeric(dt1$depth)
if (class(dt1$wtemp)=="factor") dt1$wtemp <-as.numeric(levels(dt1$wtemp))[as.integer(dt1$wtemp) ]               
if (class(dt1$wtemp)=="character") dt1$wtemp <-as.numeric(dt1$wtemp)
if (class(dt1$flag_wtemp)!="factor") dt1$flag_wtemp<- as.factor(dt1$flag_wtemp)

# Convert Missing Values to NA for non-dates



# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(year4)
summary(sampledate)
summary(daynum)
summary(depth)
summary(wtemp)
summary(flag_wtemp) 
# Get more details on character variables

summary(as.factor(dt1$flag_wtemp))
detach(dt1)       

SP.temp = dt1

SP.surface = SP.temp %>%
  filter(depth == 0.5) %>% 
  rename(year = year4, doy = daynum) %>% 
  mutate(year = as.factor(year))


ggplot(data = SP.surface, aes(x =doy, y = wtemp, color = year))+
  geom_line(size = 1, alpha = 0.5)+
  theme_classic()

SP.surface.HWR = SP.surface %>% select(sampledate, wtemp) %>% 
  rename(t = sampledate, temp = wtemp)





