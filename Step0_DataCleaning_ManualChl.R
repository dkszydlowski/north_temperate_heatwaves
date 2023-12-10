# data cleaning for the MANUAL chlorophyll data instead of the sonde data

##### 2008-2009 LR #####

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


##### Cleaning 2008-2009 LR #####
LR_08_09 = dt1

# rename columns
LR_08_09 = LR_08_09 %>% rename(doyCat = DOY, mean_chl = Chla)

LR_08_09 = LR_08_09 %>% select(year, lake, doyCat, mean_chl)
#===============================================================================#
##### 2013-2015 LRT #####
# 
# # Package ID: knb-lter-ntl.372.3 Cataloging System:https://pasta.edirepository.org.
# # Data set title: Cascade project at North Temperate Lakes LTER - Daily Chlorophyll Data for Whole Lake Nutrient Additions 2013-2015.
# # Data set creator:  Mike Pace - University of Virginia 
# # Data set creator:  Jon Cole - Cary Institute of Ecosystem Studies 
# # Data set creator:  Stephen Carpenter - University of Wisconsin 
# # Contact:  Mike Pace -  University of Virginia  - pacem@virginia.edu
# # Contact:    -  NTL LTER  - ntl.infomgr@gmail.com
# # Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 
# 
# inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/372/3/a50718a421790c9a5fc68082bb7af45e" 
# infile1 <- tempfile()
# try(download.file(inUrl1,infile1,method="curl"))
# if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")
# 
# 
# dt1 <-read.csv(infile1,header=F 
#                ,skip=1
#                ,sep=","  
#                ,quot='"' 
#                , col.names=c(
#                  "Year",     
#                  "Lake",     
#                  "DoY",     
#                  "Manual_Chl"    ), check.names=TRUE)
# 
# unlink(infile1)
# 
# # Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
# 
# if (class(dt1$Year)=="factor") dt1$Year <-as.numeric(levels(dt1$Year))[as.integer(dt1$Year) ]               
# if (class(dt1$Year)=="character") dt1$Year <-as.numeric(dt1$Year)
# if (class(dt1$Lake)!="factor") dt1$Lake<- as.factor(dt1$Lake)
# if (class(dt1$DoY)=="factor") dt1$DoY <-as.numeric(levels(dt1$DoY))[as.integer(dt1$DoY) ]               
# if (class(dt1$DoY)=="character") dt1$DoY <-as.numeric(dt1$DoY)
# if (class(dt1$Manual_Chl)=="factor") dt1$Manual_Chl <-as.numeric(levels(dt1$Manual_Chl))[as.integer(dt1$Manual_Chl) ]               
# if (class(dt1$Manual_Chl)=="character") dt1$Manual_Chl <-as.numeric(dt1$Manual_Chl)
# 
# # Convert Missing Values to NA for non-dates
# 
# dt1$Year <- ifelse((trimws(as.character(dt1$Year))==trimws("NA")),NA,dt1$Year)               
# suppressWarnings(dt1$Year <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Year))==as.character(as.numeric("NA"))),NA,dt1$Year))
# dt1$Lake <- as.factor(ifelse((trimws(as.character(dt1$Lake))==trimws("NA")),NA,as.character(dt1$Lake)))
# dt1$DoY <- ifelse((trimws(as.character(dt1$DoY))==trimws("NA")),NA,dt1$DoY)               
# suppressWarnings(dt1$DoY <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DoY))==as.character(as.numeric("NA"))),NA,dt1$DoY))
# dt1$Manual_Chl <- ifelse((trimws(as.character(dt1$Manual_Chl))==trimws("NA")),NA,dt1$Manual_Chl)               
# suppressWarnings(dt1$Manual_Chl <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Manual_Chl))==as.character(as.numeric("NA"))),NA,dt1$Manual_Chl))
# 
# 
# # Here is the structure of the input data frame:
# str(dt1)                            
# attach(dt1)                            
# # The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 
# 
# summary(Year)
# summary(Lake)
# summary(DoY)
# summary(Manual_Chl) 
# # Get more details on character variables
# 
# summary(as.factor(dt1$Lake))
# detach(dt1)               
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# LR_13_15 
# 


##### Cleaning 2013-2015 LRT #####

#===============================================================================#
##### 2011-2019 LRT #####

# Package ID: knb-lter-ntl.413.2 Cataloging System:https://pasta.edirepository.org.
# Data set title: Cascade Project at North Temperate Lakes LTER â Daily Bloom Data for Whole Lake             Experiments 2011 - 2019.
# Data set creator:  Cal Buelo - University of Wisconsin 
# Data set creator:  Michael Pace - University of Virginia 
# Data set creator:  Stephen Carpenter - University of Wisconsin 
# Data set creator:  Emily Stanley - University of Wisconsin 
# Data set creator:  David Ortiz - University of Wisconsin 
# Data set creator:  Dat Ha - University of Virginia 
# Contact:  Cal Buelo -  University of Wisconsin  - cbuelo@wisc.edu
# Contact:    -  NTL LTER  - ntl.infomgr@gmail.com
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/413/2/9d812efccf4107979d11e16316000b2e" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "Lake",     
                 "Year",     
                 "DOY",     
                 "BGA_HYLB",     
                 "Manual_Chl",     
                 "DO_Sat",     
                 "pH"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$Lake)!="factor") dt1$Lake<- as.factor(dt1$Lake)
if (class(dt1$Year)=="factor") dt1$Year <-as.numeric(levels(dt1$Year))[as.integer(dt1$Year) ]               
if (class(dt1$Year)=="character") dt1$Year <-as.numeric(dt1$Year)
if (class(dt1$DOY)=="factor") dt1$DOY <-as.numeric(levels(dt1$DOY))[as.integer(dt1$DOY) ]               
if (class(dt1$DOY)=="character") dt1$DOY <-as.numeric(dt1$DOY)
if (class(dt1$BGA_HYLB)=="factor") dt1$BGA_HYLB <-as.numeric(levels(dt1$BGA_HYLB))[as.integer(dt1$BGA_HYLB) ]               
if (class(dt1$BGA_HYLB)=="character") dt1$BGA_HYLB <-as.numeric(dt1$BGA_HYLB)
if (class(dt1$Manual_Chl)=="factor") dt1$Manual_Chl <-as.numeric(levels(dt1$Manual_Chl))[as.integer(dt1$Manual_Chl) ]               
if (class(dt1$Manual_Chl)=="character") dt1$Manual_Chl <-as.numeric(dt1$Manual_Chl)
if (class(dt1$DO_Sat)=="factor") dt1$DO_Sat <-as.numeric(levels(dt1$DO_Sat))[as.integer(dt1$DO_Sat) ]               
if (class(dt1$DO_Sat)=="character") dt1$DO_Sat <-as.numeric(dt1$DO_Sat)
if (class(dt1$pH)=="factor") dt1$pH <-as.numeric(levels(dt1$pH))[as.integer(dt1$pH) ]               
if (class(dt1$pH)=="character") dt1$pH <-as.numeric(dt1$pH)

# Convert Missing Values to NA for non-dates

dt1$BGA_HYLB <- ifelse((trimws(as.character(dt1$BGA_HYLB))==trimws("NA")),NA,dt1$BGA_HYLB)               
suppressWarnings(dt1$BGA_HYLB <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$BGA_HYLB))==as.character(as.numeric("NA"))),NA,dt1$BGA_HYLB))
dt1$Manual_Chl <- ifelse((trimws(as.character(dt1$Manual_Chl))==trimws("NA")),NA,dt1$Manual_Chl)               
suppressWarnings(dt1$Manual_Chl <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Manual_Chl))==as.character(as.numeric("NA"))),NA,dt1$Manual_Chl))
dt1$DO_Sat <- ifelse((trimws(as.character(dt1$DO_Sat))==trimws("NA")),NA,dt1$DO_Sat)               
suppressWarnings(dt1$DO_Sat <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DO_Sat))==as.character(as.numeric("NA"))),NA,dt1$DO_Sat))
dt1$pH <- ifelse((trimws(as.character(dt1$pH))==trimws("NA")),NA,dt1$pH)               
suppressWarnings(dt1$pH <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$pH))==as.character(as.numeric("NA"))),NA,dt1$pH))


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(Lake)
summary(Year)
summary(DOY)
summary(BGA_HYLB)
summary(Manual_Chl)
summary(DO_Sat)
summary(pH) 
# Get more details on character variables

summary(as.factor(dt1$Lake))
detach(dt1)               




##### Cleaning 2011-2019 LRT #####
LRT_11_19 = dt1

# rename columns
LRT_11_19 = LRT_11_19 %>% rename(year = Year, lake = Lake, doyCat = DOY, mean_chl = Manual_Chl)

# just the columns we need
LRT_11_19 = LRT_11_19 %>% select(year, lake, doyCat, mean_chl)



##### Finalize dataframe #####
manual_chl = rbind(LR_08_09, LRT_11_19)

# remove 2016 data because it is empty
manual_chl = manual_chl %>% filter(year != 2016)

# replace the lake names with their letter codes
manual_chl = manual_chl %>%
  mutate(lake = replace(lake, lake == "Peter", "R")) %>%
  mutate(lake = replace(lake, lake == "Paul", "L")) %>% 
  mutate(lake = replace(lake, lake == "Tuesday", "T"))

# save the manual chlorophyll data
write.csv(manual_chl, "./formatted data/manual_chlorophyll.csv", row.names = FALSE)

## Finally, combine the manual chlorophyll with the temperature data to make a combined dataframe

all = read.csv("./formatted data/CombinedData.csv")

manual_chl = manual_chl %>% rename(manual_chl = mean_chl)
full_dataset = full_join(manual_chl, all, by = c("lake", "year", "doyCat"))

write.csv(full_dataset, "./formatted data/full raw data manual and sonde chlorophyll.csv", row.names = FALSE)
