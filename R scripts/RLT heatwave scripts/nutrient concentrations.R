### read in the actual nutrient concentration data ###

library(tidyverse)

# Package ID: knb-lter-ntl.351.5 Cataloging System:https://pasta.edirepository.org.
# Data set title: Cascade Project at North Temperate Lakes LTER Core Data Nutrients 1991 - 2019.
# Data set creator:  Stephen Carpenter - University of Wisconsin 
# Data set creator:  Jim Kitchell - University of Wisconsin 
# Data set creator:  Jonathan Cole - Cary Institute of Ecosystem Studies 
# Data set creator:  Michael Pace - University of Virginia 
# Contact:  Stephen Carpenter -  University of Wisconsin  - steve.carpenter@wisc.edu
# Contact:  Michael Pace -  University of Virginia  - pacem@virginia.edu
# Contact:    -  NTL LTER  - ntl.infomgr@gmail.com
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/351/5/f6becb1654de782d8e6274f7b789182c" 
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
                 "depth_id",     
                 "depth",     
                 "tn_ug",     
                 "tp_ug",     
                 "nh34",     
                 "no23",     
                 "po4",     
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

if (class(dt1$depth_id)!="factor") dt1$depth_id<- as.factor(dt1$depth_id)
if (class(dt1$depth)=="factor") dt1$depth <-as.numeric(levels(dt1$depth))[as.integer(dt1$depth) ]               
if (class(dt1$depth)=="character") dt1$depth <-as.numeric(dt1$depth)
if (class(dt1$tn_ug)=="factor") dt1$tn_ug <-as.numeric(levels(dt1$tn_ug))[as.integer(dt1$tn_ug) ]               
if (class(dt1$tn_ug)=="character") dt1$tn_ug <-as.numeric(dt1$tn_ug)
if (class(dt1$tp_ug)=="factor") dt1$tp_ug <-as.numeric(levels(dt1$tp_ug))[as.integer(dt1$tp_ug) ]               
if (class(dt1$tp_ug)=="character") dt1$tp_ug <-as.numeric(dt1$tp_ug)
if (class(dt1$nh34)=="factor") dt1$nh34 <-as.numeric(levels(dt1$nh34))[as.integer(dt1$nh34) ]               
if (class(dt1$nh34)=="character") dt1$nh34 <-as.numeric(dt1$nh34)
if (class(dt1$no23)=="factor") dt1$no23 <-as.numeric(levels(dt1$no23))[as.integer(dt1$no23) ]               
if (class(dt1$no23)=="character") dt1$no23 <-as.numeric(dt1$no23)
if (class(dt1$po4)=="factor") dt1$po4 <-as.numeric(levels(dt1$po4))[as.integer(dt1$po4) ]               
if (class(dt1$po4)=="character") dt1$po4 <-as.numeric(dt1$po4)
if (class(dt1$comments)!="factor") dt1$comments<- as.factor(dt1$comments)

# Convert Missing Values to NA for non-dates

dt1$depth_id <- as.factor(ifelse((trimws(as.character(dt1$depth_id))==trimws("NA")),NA,as.character(dt1$depth_id)))
dt1$depth <- ifelse((trimws(as.character(dt1$depth))==trimws("NA")),NA,dt1$depth)               
suppressWarnings(dt1$depth <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$depth))==as.character(as.numeric("NA"))),NA,dt1$depth))
dt1$tn_ug <- ifelse((trimws(as.character(dt1$tn_ug))==trimws("NA")),NA,dt1$tn_ug)               
suppressWarnings(dt1$tn_ug <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$tn_ug))==as.character(as.numeric("NA"))),NA,dt1$tn_ug))
dt1$tp_ug <- ifelse((trimws(as.character(dt1$tp_ug))==trimws("NA")),NA,dt1$tp_ug)               
suppressWarnings(dt1$tp_ug <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$tp_ug))==as.character(as.numeric("NA"))),NA,dt1$tp_ug))
dt1$nh34 <- ifelse((trimws(as.character(dt1$nh34))==trimws("NA")),NA,dt1$nh34)               
suppressWarnings(dt1$nh34 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$nh34))==as.character(as.numeric("NA"))),NA,dt1$nh34))
dt1$no23 <- ifelse((trimws(as.character(dt1$no23))==trimws("NA")),NA,dt1$no23)               
suppressWarnings(dt1$no23 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$no23))==as.character(as.numeric("NA"))),NA,dt1$no23))
dt1$po4 <- ifelse((trimws(as.character(dt1$po4))==trimws("NA")),NA,dt1$po4)               
suppressWarnings(dt1$po4 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$po4))==as.character(as.numeric("NA"))),NA,dt1$po4))


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(lakeid)
summary(lakename)
summary(year4)
summary(daynum)
summary(sampledate)
summary(depth_id)
summary(depth)
summary(tn_ug)
summary(tp_ug)
summary(nh34)
summary(no23)
summary(po4)
summary(comments) 
# Get more details on character variables

summary(as.factor(dt1$lakeid)) 
summary(as.factor(dt1$lakename)) 
summary(as.factor(dt1$depth_id)) 
summary(as.factor(dt1$comments))
detach(dt1)               



# make a new dataframe called routine.nuts with the nutrient data
# filter just for heatwave years and just nutrients from the PML/epilimnion, 
# a value where depth_id == -1
routine.nuts = dt1 %>% 
  filter(year4 >= 2008 & depth_id %in% c(-1, 1) & lakeid %in% c("L", "R", "T") & year4 != 2012)

# plot to check which data are available
ggplot(routine.nuts, aes(x = daynum, y = tp_ug, color = lakeid))+
  geom_point()+
  geom_line()+
  facet_wrap(~year4)+
  scale_color_manual(values = c("R" = '#1F78B4', "L" = '#FED976', "T" = '#8C510A'))+
  theme_classic()
  
# we only have dissolved nutrient data for 2013, 2018, and 2019. Going to focus on TN and TP for now

# format so columns match code
routine.nuts = routine.nuts %>% rename(lake = lakeid, doy = daynum, year = year4)


#==================================================================================#
##### combine actual nutrient concentrations with the explanatory dataset #####


heatwaves.exp = read.csv("./formatted data/master explanatory dataset/heatwaves explained var4.csv")


heatwaves.exp = heatwaves.exp %>% mutate(doy = yday(date_start))

#heatwaves.exp = heatwaves.exp %>% group_by(lake, year) %>% join_by(casc.color.nut.zoops, date_start > date)


# create new columns of heatwaves.exp for TP and TN
heatwaves.exp = heatwaves.exp %>% mutate(tp_ugL.before = NA, tp_ugL.during = NA, tp_ugL.after = NA, tp_ugL.analysis = NA,
                                         tn_ugL.before = NA, tn_ugL.during = NA, tn_ugL.after = NA, tn_ugL.analysis = NA)


for(i in 1:nrow(heatwaves.exp)){
  
  # save the start date of current heatwave
  start.date = heatwaves.exp$date_start[i]
  start.doy = heatwaves.exp$doy[i]
  end.doy = yday(heatwaves.exp$date_end[i])
  print(start.date)
  
  # save the lake of the current heatwave
  targ.lake = heatwaves.exp$lake[i]
  targ.year = heatwaves.exp$year[i]
  
  # filter the explanatory dataframe to match
  # make sure doy is <= the heatwave day
  routine.nuts.cur = routine.nuts %>% filter(lake == targ.lake, year == targ.year)
  
  # week before heatwave
  routine.nuts.before = routine.nuts.cur %>% filter(doy >= start.doy - 6 & doy <= start.doy)
  
  # during heatwave
  routine.nuts.during = routine.nuts.cur %>% filter(doy >= start.doy & doy <= end.doy)
  
  # after heatwave
  routine.nuts.after = routine.nuts.cur %>% filter(doy > end.doy & doy <= end.doy + 6)
  
  # get nutrients strictly during the heatwave analysis period
  if(targ.lake == "T"){
  routine.nuts.analysis = routine.nuts.cur %>% filter(doy >= end.doy - 6 & doy <= end.doy + 2)
  }
  
  
  if(targ.lake != "T"){
    routine.nuts.analysis = routine.nuts.cur %>% filter(doy >= end.doy - 6 & doy <= end.doy + 3)
  }
  
  if(nrow(routine.nuts.during) > 0){
    nuts.during.hw = mean(routine.nuts.during$tp_ug, na.rm = TRUE)
    heatwaves.exp$tp_ugL.during[i] = nuts.during.hw
    
    nuts.during.hw = mean(routine.nuts.during$tn_ug, na.rm = TRUE)
    heatwaves.exp$tn_ugL.during[i] = nuts.during.hw
  }
  
  if(nrow(routine.nuts.after) > 0){
    nuts.after.hw = mean(routine.nuts.after$tp_ug, na.rm = TRUE)
    heatwaves.exp$tp_ugL.after[i] = nuts.after.hw
    
    nuts.after.hw = mean(routine.nuts.after$tn_ug, na.rm = TRUE)
    heatwaves.exp$tn_ugL.after[i] = nuts.after.hw
    
  }
  
  if(nrow(routine.nuts.before) > 0){
    nuts.before.hw = mean(routine.nuts.before$tp_ug, na.rm = TRUE)
    heatwaves.exp$tp_ugL.before[i] = nuts.before.hw
    
    nuts.before.hw = mean(routine.nuts.before$tn_ug, na.rm = TRUE)
    heatwaves.exp$tn_ugL.before[i] = nuts.before.hw
  }
  
  
  if(nrow(routine.nuts.analysis) > 0){
    nuts.analysis.hw = mean(routine.nuts.analysis$tp_ug, na.rm = TRUE)
    heatwaves.exp$tp_ugL.analysis[i] = nuts.analysis.hw
    
    nuts.analysis.hw = mean(routine.nuts.analysis$tn_ug, na.rm = TRUE)
    heatwaves.exp$tn_ugL.analysis[i] = nuts.analysis.hw
  }
  
  
  
  
}




ggplot(heatwaves.exp, aes(x = tp_ugL.after, y = percentChange, color = lake))+
  geom_point(size = 3)+
  theme_classic()


ggplot(heatwaves.exp, aes(x = tp_ugL.analysis, y = percentChange, color = lake))+
  geom_point(size = 3)+
  theme_classic()

ggplot(heatwaves.exp, aes(x = tn_ugL.during, y = percentChange, color = lake))+
  geom_point(size = 3)+
  theme_classic()


summary(lm(heatwaves.exp$percentChange~heatwaves.exp$tp_ugL.before))


## save the dataframe with the new nutrient variables added
write.csv(heatwaves.exp, "./formatted data/master explanatory dataset/heatwaves explained var5.csv", row.names = FALSE)


