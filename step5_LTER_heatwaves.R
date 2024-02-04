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




## Trout Lake 

# Package ID: knb-lter-ntl.116.28 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER: High Frequency Water Temperature Data - Trout Lake Buoy 2004 - current.
# Data set creator:  John Magnuson - University of Wisconsin-Madison 
# Data set creator:  Stephen Carpenter - University of Wisconsin-Madison 
# Data set creator:  Emily Stanley - University of Wisconsin-Madison 
# Metadata Provider:  NTL Information Manager - University of Wisconsin-Madison 
# Contact:    -  NTL LTER  - ntl.infomgr@gmail.com
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/116/28/3e434ea36377b35693e33dbce346c879" 
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

TR.temp = dt1


TR.surface = TR.temp %>%
  filter(depth == 1) %>% 
  rename(year = year4, doy = daynum) %>% 
  mutate(year = as.factor(year))


ggplot(data = TR.surface, aes(x =doy, y = wtemp, color = year))+
  geom_line(size = 1, alpha = 0.5)+
  theme_classic()

TR.surface.HWR = TR.surface %>% select(sampledate, wtemp) %>% 
  rename(t = sampledate, temp = wtemp)


climOutputTR = ts2clm(TR.surface.HWR, climatologyPeriod = c(min(TR.surface.HWR$t), max(TR.surface.HWR$t)))
TRHW = detect_event(climOutputTR)

TRHW = TRHW$event

TRHW = TRHW %>% mutate(year = year(date_start))


#### Crystal Bog ####
#  Package ID: knb-lter-ntl.119.10 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER: High Frequency Water Temperature Data - Crystal Bog Buoy 2005 - current.
# Data set creator:  John Magnuson - University of Wisconsin-Madison 
# Data set creator:  Stephen Carpenter - University of Wisconsin-Madison 
# Data set creator:  Emily Stanley - University of Wisconsin-Madison 
# Metadata Provider:  NTL Information Manager - University of Wisconsin-Madison 
# Contact:    -  NTL LTER  - ntl.infomgr@gmail.com
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/119/10/bd6c71aa58e266dc3be6dbf53aa8005b" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "sampledate",     
                 "depth",     
                 "wtemp",     
                 "flag_wtemp"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

# attempting to convert dt1$sampledate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1sampledate<-as.Date(dt1$sampledate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1sampledate) == length(tmp1sampledate[!is.na(tmp1sampledate)])){dt1$sampledate <- tmp1sampledate } else {print("Date conversion failed for dt1$sampledate. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1sampledate) 
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

summary(sampledate)
summary(depth)
summary(wtemp)
summary(flag_wtemp) 
# Get more details on character variables

summary(as.factor(dt1$flag_wtemp))
detach(dt1)             


CB.temp = dt1

CB.temp = CB.temp %>% mutate(year4 = year(sampledate), daynum = yday(sampledate))

CB.surface = CB.temp %>%
  filter(depth == 1) %>% 
  rename(year = year4, doy = daynum) %>% 
  mutate(year = as.factor(year))


ggplot(data = CB.surface, aes(x =doy, y = wtemp, color = year))+
  geom_line(size = 1, alpha = 0.5)+
  theme_classic()

CB.surface.HWR = CB.surface %>% select(sampledate, wtemp) %>% 
  rename(t = sampledate, temp = wtemp)


climOutputCB = ts2clm(CB.surface.HWR, climatologyPeriod = c(min(CB.surface.HWR$t), max(CB.surface.HWR$t)))
CBHW = detect_event(climOutputCB)

CBHW = CBHW$event

CBHW = CBHW %>% mutate(year = year(date_start))




##### Trout Bog #####
# Package ID: knb-lter-ntl.70.29 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER: High Frequency Water Temperature Data - Trout Bog Buoy 2003 - current.
# Data set creator:  John Magnuson - University of Wisconsin-Madison 
# Data set creator:  Stephen Carpenter - University of Wisconsin-Madison 
# Data set creator:  Emily Stanley - University of Wisconsin-Madison 
# Metadata Provider:  NTL Information Manager - University of Wisconsin-Madison 
# Contact:    -  NTL LTER  - ntl.infomgr@gmail.com
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/70/29/786bc0771a31f418c0ef9f7762328134" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "sampledate",     
                 "depth",     
                 "wtemp",     
                 "flag_wtemp"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

# attempting to convert dt1$sampledate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1sampledate<-as.Date(dt1$sampledate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1sampledate) == length(tmp1sampledate[!is.na(tmp1sampledate)])){dt1$sampledate <- tmp1sampledate } else {print("Date conversion failed for dt1$sampledate. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1sampledate) 
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

summary(sampledate)
summary(depth)
summary(wtemp)
summary(flag_wtemp) 
# Get more details on character variables

summary(as.factor(dt1$flag_wtemp))
detach(dt1)               



TB.temp = dt1

TB.temp = TB.temp %>% mutate(year4 = year(sampledate), daynum = yday(sampledate))

TB.surface = TB.temp %>%
  filter(depth == 1) %>% 
  rename(year = year4, doy = daynum) %>% 
  mutate(year = as.factor(year))


ggplot(data = TB.surface, aes(x =doy, y = wtemp, color = year))+
  geom_line(size = 1, alpha = 0.5)+
  theme_classic()

TB.surface.HWR = TB.surface %>% select(sampledate, wtemp) %>% 
  rename(t = sampledate, temp = wtemp)


climOutputTB = ts2clm(TB.surface.HWR, climatologyPeriod = c(min(TB.surface.HWR$t), max(TB.surface.HWR$t)))
TBHW = detect_event(climOutputTB)

TBHW = TBHW$event

TBHW = TBHW %>% mutate(year = year(date_start))






##### FORMAT DATA ######
SP.surface = SP.temp %>%
  filter(depth == 1) %>% 
  rename(year = year4, doy = daynum) %>% 
  mutate(year = as.factor(year))


ggplot(data = SP.surface, aes(x =doy, y = wtemp, color = year))+
  geom_line(size = 1, alpha = 0.5)+
  theme_classic()

SP.surface.HWR = SP.surface %>% select(sampledate, wtemp) %>% 
  rename(t = sampledate, temp = wtemp)


climOutputSP = ts2clm(SP.surface.HWR, climatologyPeriod = c(min(SP.surface.HWR$t), max(SP.surface.HWR$t)))
SPHW = detect_event(climOutputSP)

SPHW = SPHW$event

SPHW = SPHW %>% mutate(year = year(date_start))


# summarize how many heatwaves per year
sparklingHW.perYear = sparklingHW %>% group_by(year) %>% 
  summarize(num.hw = n())


ggplot(sparklingHW.perYear, aes(x = year, y = num.hw))+
  geom_point()







#### Combine the heatwaves dataframes #####
TRHW = TRHW %>% mutate(lake = "TR") %>% mutate(year = year(date_start))
SPHW = SPHW %>% mutate(lake = "SP")
TBHW = TBHW %>% mutate(lake = "TB")
CBHW = CBHW %>% mutate(lake = "CB")
HW.all = rbind(TRHW, SPHW, TBHW, CBHW)


# combine the temperature dataframes
SP.surface = SP.surface %>% mutate(lake = "SP")
TR.surface = TR.surface %>% mutate(lake = "TR")
TB.surface = TB.surface %>% mutate(lake = "TB")
CB.surface = CB.surface %>% mutate(lake = "CB")
all.surface = rbind(SP.surface, TR.surface, TB.surface, CB.surface)



years = sort(unique(HW.all$year))
HW.all = HW.all %>% mutate(doy = yday(date_start))

all.surface = all.surface %>% mutate(sampledate = as.Date(sampledate))

for(i in 1:length(years)){
  
  cur.year = years[i]
  
  current = HW.all %>% filter(year == cur.year, doy > 145 & doy < 260)
  temp.current = all.surface %>% filter(year == cur.year)
  
  # restrict the heatwave and temp dates so we are just investigating summer
  temp.current = temp.current %>% filter(doy > 145 & doy < 260)
  

   print(ggplot(data = temp.current, aes(x = sampledate, y = wtemp, color = lake)) +
     geom_line(size = 0.9, alpha = 0.7) +
     theme_classic() +
     scale_color_manual(values = c("TR" = "#AA4499", "SP" = "#117733", "TB" = "#88CCEE", CB = "#DDCC77")) +
     labs(title = cur.year) +
      geom_rect(data = current, inherit.aes = FALSE, aes(xmin = date_start, xmax = date_end, ymin = 0, ymax = Inf, fill = lake), alpha = 0.5) +
     scale_fill_manual(values = c("TR" = "#AA4499", "SP" = "#117733", "TB" = "#88CCEE", CB = "#DDCC77")) +
     guides(fill = FALSE))
   
  
}


ggplot(all.surface, aes(x = doy, y = wtemp, color = lake))+
  geom_line(size = 0.8)+
  facet_wrap(~year)+
  theme_bw()





# get a summary of the sampling depths available by year
