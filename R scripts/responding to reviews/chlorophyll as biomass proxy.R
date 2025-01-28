# code for responding to reviewer concerns about the validity of chl as a metric of
# phytoplankton biomass, and how representative it is of the whole water column

### demonstrating that chlorophyll is consistent down to about 10% light
## 

library(tidyverse)
library(ggborderline)


### daily surface grabs vs PML ###

# read in the manual daily chlorophyll data
allData = read.csv("./formatted data/interpolated_manual_chl_for_slopes.csv")








# Package ID: knb-lter-ntl.354.5 Cataloging System:https://pasta.edirepository.org.
# Data set title: Cascade Project at North Temperate Lakes LTER Core Data Process Data 1984 - 2016.
# Data set creator:  Stephen Carpenter - University of Wisconsin 
# Data set creator:  Jim Kitchell - University of Wisconsin 
# Data set creator:  Jon Cole - Cary Institute of Ecosystem Studies 
# Data set creator:  Mike Pace - University of Virginia 
# Contact:  Stephen Carpenter -  University of Wisconsin  - steve.carpenter@wisc.edu
# Contact:  Mike Pace -  University of Virginia  - pacem@virginia.edu
# Contact:    -  NTL LTER  - ntl.infomgr@gmail.com
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/354/5/f8d6f5308b4d0f5437ffa62725b6fc8e" 
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
                 "depth_id",     
                 "DIC_mg",     
                 "primary_prod_rep1",     
                 "primary_prod_rep2",     
                 "chla",     
                 "phaeo",     
                 "chla_lt_35um",     
                 "phaeo_lt_35um",     
                 "alk_phosphatase",     
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
if (class(dt1$depth_id)!="factor") dt1$depth_id<- as.factor(dt1$depth_id)
if (class(dt1$DIC_mg)=="factor") dt1$DIC_mg <-as.numeric(levels(dt1$DIC_mg))[as.integer(dt1$DIC_mg) ]               
if (class(dt1$DIC_mg)=="character") dt1$DIC_mg <-as.numeric(dt1$DIC_mg)
if (class(dt1$primary_prod_rep1)=="factor") dt1$primary_prod_rep1 <-as.numeric(levels(dt1$primary_prod_rep1))[as.integer(dt1$primary_prod_rep1) ]               
if (class(dt1$primary_prod_rep1)=="character") dt1$primary_prod_rep1 <-as.numeric(dt1$primary_prod_rep1)
if (class(dt1$primary_prod_rep2)=="factor") dt1$primary_prod_rep2 <-as.numeric(levels(dt1$primary_prod_rep2))[as.integer(dt1$primary_prod_rep2) ]               
if (class(dt1$primary_prod_rep2)=="character") dt1$primary_prod_rep2 <-as.numeric(dt1$primary_prod_rep2)
if (class(dt1$chla)=="factor") dt1$chla <-as.numeric(levels(dt1$chla))[as.integer(dt1$chla) ]               
if (class(dt1$chla)=="character") dt1$chla <-as.numeric(dt1$chla)
if (class(dt1$phaeo)=="factor") dt1$phaeo <-as.numeric(levels(dt1$phaeo))[as.integer(dt1$phaeo) ]               
if (class(dt1$phaeo)=="character") dt1$phaeo <-as.numeric(dt1$phaeo)
if (class(dt1$chla_lt_35um)=="factor") dt1$chla_lt_35um <-as.numeric(levels(dt1$chla_lt_35um))[as.integer(dt1$chla_lt_35um) ]               
if (class(dt1$chla_lt_35um)=="character") dt1$chla_lt_35um <-as.numeric(dt1$chla_lt_35um)
if (class(dt1$phaeo_lt_35um)=="factor") dt1$phaeo_lt_35um <-as.numeric(levels(dt1$phaeo_lt_35um))[as.integer(dt1$phaeo_lt_35um) ]               
if (class(dt1$phaeo_lt_35um)=="character") dt1$phaeo_lt_35um <-as.numeric(dt1$phaeo_lt_35um)
if (class(dt1$alk_phosphatase)=="factor") dt1$alk_phosphatase <-as.numeric(levels(dt1$alk_phosphatase))[as.integer(dt1$alk_phosphatase) ]               
if (class(dt1$alk_phosphatase)=="character") dt1$alk_phosphatase <-as.numeric(dt1$alk_phosphatase)
if (class(dt1$comments)!="factor") dt1$comments<- as.factor(dt1$comments)

# Convert Missing Values to NA for non-dates

dt1$depth <- ifelse((trimws(as.character(dt1$depth))==trimws("NA")),NA,dt1$depth)               
suppressWarnings(dt1$depth <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$depth))==as.character(as.numeric("NA"))),NA,dt1$depth))
dt1$DIC_mg <- ifelse((trimws(as.character(dt1$DIC_mg))==trimws("NA")),NA,dt1$DIC_mg)               
suppressWarnings(dt1$DIC_mg <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DIC_mg))==as.character(as.numeric("NA"))),NA,dt1$DIC_mg))
dt1$primary_prod_rep1 <- ifelse((trimws(as.character(dt1$primary_prod_rep1))==trimws("NA")),NA,dt1$primary_prod_rep1)               
suppressWarnings(dt1$primary_prod_rep1 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$primary_prod_rep1))==as.character(as.numeric("NA"))),NA,dt1$primary_prod_rep1))
dt1$primary_prod_rep2 <- ifelse((trimws(as.character(dt1$primary_prod_rep2))==trimws("NA")),NA,dt1$primary_prod_rep2)               
suppressWarnings(dt1$primary_prod_rep2 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$primary_prod_rep2))==as.character(as.numeric("NA"))),NA,dt1$primary_prod_rep2))
dt1$chla <- ifelse((trimws(as.character(dt1$chla))==trimws("NA")),NA,dt1$chla)               
suppressWarnings(dt1$chla <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$chla))==as.character(as.numeric("NA"))),NA,dt1$chla))
dt1$phaeo <- ifelse((trimws(as.character(dt1$phaeo))==trimws("NA")),NA,dt1$phaeo)               
suppressWarnings(dt1$phaeo <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$phaeo))==as.character(as.numeric("NA"))),NA,dt1$phaeo))
dt1$chla_lt_35um <- ifelse((trimws(as.character(dt1$chla_lt_35um))==trimws("NA")),NA,dt1$chla_lt_35um)               
suppressWarnings(dt1$chla_lt_35um <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$chla_lt_35um))==as.character(as.numeric("NA"))),NA,dt1$chla_lt_35um))
dt1$phaeo_lt_35um <- ifelse((trimws(as.character(dt1$phaeo_lt_35um))==trimws("NA")),NA,dt1$phaeo_lt_35um)               
suppressWarnings(dt1$phaeo_lt_35um <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$phaeo_lt_35um))==as.character(as.numeric("NA"))),NA,dt1$phaeo_lt_35um))
dt1$alk_phosphatase <- ifelse((trimws(as.character(dt1$alk_phosphatase))==trimws("NA")),NA,dt1$alk_phosphatase)               
suppressWarnings(dt1$alk_phosphatase <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$alk_phosphatase))==as.character(as.numeric("NA"))),NA,dt1$alk_phosphatase))


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
summary(DIC_mg)
summary(primary_prod_rep1)
summary(primary_prod_rep2)
summary(chla)
summary(phaeo)
summary(chla_lt_35um)
summary(phaeo_lt_35um)
summary(alk_phosphatase)
summary(comments) 
# Get more details on character variables

summary(as.factor(dt1$lakeid)) 
summary(as.factor(dt1$lakename)) 
summary(as.factor(dt1$depth_id)) 
summary(as.factor(dt1$comments))
detach(dt1)               

routines.chl = dt1

routines.chl = routines.chl %>% filter(lakeid %in% c("L", "R", "T") & depth_id %in% c(1:6))

# %>% filter(year4 %in% c(2008, 2009, 2010, 2011, 2013, 2014, 2015))
  # filter(depth_id %in% c(1, 2, 3, 4))

ggplot(routines.chl, aes(x = as.factor(doy), y = chla, fill = lakeid))+
  geom_boxplot()+
  facet_wrap(~year4)


### make a plot that is number of depths included (x) vs standard deviation of chlorophyll (y) by lake

routines.chl = routines.chl %>% group_by(daynum, lakeid, year4) %>% 
  mutate(depth_id = as.numeric(depth_id))

for(i in 1:6){
  
  cur.chl = routines.chl %>% filter(depth_id <= i)
  
  chl.results = cur.chl %>% group_by(lakeid, daynum, year4) %>% 
    summarize(sd.chl = sd(chla, na.rm = TRUE))
  
  chl.results$depth.included = i
  if(i == 1){
    chl.all.results =  chl.results
  }
  if(i > 1){
  chl.all.results = rbind(chl.all.results, chl.results)
  }
  
}


ggplot(chl.all.results, aes(x = depth.included, y = sd.chl, color = lakeid))+
  geom_point()

# get mean sd by depth for each lake

chl.all.results.mean = chl.all.results %>% group_by(lakeid, depth.included) %>% 
  summarize(mean.sd = mean(sd.chl, na.rm = TRUE))


ggplot(chl.all.results.mean, aes(x = depth.included, y = mean.sd, fill = lakeid, color = lakeid))+
  geom_point(size = 5, pch = 21, color = "black")+
  geom_borderline(size = 1, bordercolour = "black") +
  theme_classic()+
  scale_fill_manual(values = c("R"=  "#60BFCC", "L" = "#D9EEF3", "T"=  "#544C34"),
                     labels = c("R" = "Peter", "L" = "Paul", "T" = "Tuesday"))+
  scale_color_manual(values = c("R"=  "#60BFCC", "L" = "#D9EEF3", "T"=  "#544C34"),
                    labels = c("R" = "Peter", "L" = "Paul", "T" = "Tuesday"))+
  scale_x_continuous(breaks = 1:6, 
                     labels = c("100", "50", "25", "10", "5", "1")) +
  labs(x = "percent light", y = "mean standard deviation (ug/L)")+
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14))



#### individual replicate standard deviation calculation ####

mean.chl = routines.chl %>% group_by(lakeid, depth_id) %>% 
  summarize(chl.mean = mean(chla, na.rm = TRUE))

ggplot(mean.chl, aes(x = chl.mean, y = depth_id, color = lakeid))+
  geom_point(size = 3.5, pch = 21, color = "black", aes(fill = lakeid))+
  geom_borderline(size = 1, bordercolour = "black")+
  scale_fill_manual(values = c("R"=  "#60BFCC", "L" = "#D9EEF3", "T"=  "#544C34"),
                    labels = c("R" = "Peter", "L" = "Paul", "T" = "Tuesday"))+
  scale_color_manual(values = c("R"=  "#60BFCC", "L" = "#D9EEF3", "T"=  "#544C34"),
                     labels = c("R" = "Peter", "L" = "Paul", "T" = "Tuesday"))+
  scale_y_reverse(breaks = 1:6,
                  labels = c("100", "50", "25", "10", "5", "1"))+
  labs(y = "percent light", x = "mean chlorophyll (ug/L), 1984 to 2016")+
  theme_bw()+
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14))


