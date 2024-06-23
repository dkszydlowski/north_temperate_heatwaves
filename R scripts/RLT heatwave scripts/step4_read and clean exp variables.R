#### Read in the explanatory variables from Cascade to be matched with the heatwaves dataframe #####

library(readxl)


# Package ID: knb-lter-ntl.351.4 Cataloging System:https://pasta.edirepository.org.
# Data set title: Cascade Project at North Temperate Lakes LTER Core Data Nutrients 1991 - 2016.
# Data set creator:  Stephen Carpenter - University of Wisconsin 
# Data set creator:  Jim Kitchell - University of Wisconsin 
# Data set creator:  Jon Cole - Cary Institute of Ecosystem Studies 
# Data set creator:  Mike Pace - University of Virginia 
# Contact:  Stephen Carpenter -  University of Wisconsin  - steve.carpenter@wisc.edu
# Contact:  Mike Pace -  University of Virginia  - pacem@virginia.edu
# Contact:    -  NTL LTER  - ntl.infomgr@gmail.com
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/351/4/99c3d1eb81740bcea565e09f1d56df2b" 
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
if(length(tmp1sampledate) == length(tmp1sampledate[!is.na(tmp1sampledate)])){dt1$sampledate <- tmp1sampledate } else {print("Date conversion failed for dt1$sampledate. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1sampledate) 
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

casc.nut = dt1

casc.nut = casc.nut %>% filter(lakeid %in% c("L", "R", "T")) %>% filter(year4 > 2007)
#filter(depth_id == 1 | depth_id == -1)

ggplot(casc.nut, aes(x = daynum, y = tn_ug, color = lakename))+
  geom_line()+
  facet_wrap(~year4)





##### Make a dataframe of cumulative nutrient loading by year and lake for each experiment #######

###### Read in the Cascade water color data from EDI #######
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


casc.color = dt1




##### format water color #####
wc.18 = read_xls("./formatted data/Routines 2018 2019/LimnoData_2018.xls", sheet = "Color")
wc.19 = read_xlsx("./formatted data/Routines 2018 2019/LimnoData_2019.xlsx", sheet = "Color")

# rename columns to match my standard format
wc.18 = wc.18 %>% dplyr::rename(lake = Lake, date = `Date`, year = Year, doy = DoY, PML.g440 = `PML_g440_m-1`)
wc.19 = wc.19 %>% dplyr::rename(lake = Lake, date = `Sample Date`, year = Year, doy = DoY, PML.g440 = `PML_g440_m-1`)

# select columns and filter out NA values
wc.18 = wc.18 %>% select(lake, date, year, doy, PML, PML.g440) %>% filter(!is.na(lake)) %>% filter(!is.na(PML))
wc.19 = wc.19 %>% select(lake, date, year, doy, PML, PML.g440) %>% filter(!is.na(lake)) %>% filter(!is.na(PML))

# combine dataframes
wc.18.19 = rbind(wc.18, wc.19)

# plot the PML color over time
ggplot(data = wc.18.19, aes(x = doy, y = PML.g440, color = as.factor(year)))+
  geom_point()+
  geom_line()+
  facet_wrap(~lake)


# 2008-2016 cascade color
# rename columns to match my standard format
casc.color = casc.color %>% dplyr::rename(lake = lakeid, year = year4, doy = daynum, date = sampledate)

# filter to just PML color and years 2008-2016
casc.color = casc.color %>% 
  filter(year <= 2016 & year >= 2008) %>% 
  filter(lake == "R" | lake == "L" | lake == "T") %>% 
  filter(depth == "PML")

ggplot(data = casc.color, aes(x = doy, y = absorbance, color = as.factor(year)))+
  geom_line()+
  facet_wrap(~lake)

# calculate g440 from absorbance
# 10 cm cuvette
casc.color = casc.color %>% mutate(PML.g440 = absorbance *2.303/0.1) %>% mutate(PML = absorbance)

casc.color = casc.color %>% select(lake, date, year, doy, PML, PML.g440) %>% filter(!is.na(lake)) %>% filter(!is.na(PML))

casc.color = rbind(casc.color, wc.18.19)

ggplot(data = casc.color, aes(x = doy, y = PML.g440, color = as.factor(year)))+
  geom_line()+
  facet_wrap(~lake)

# remove one suspected outlier from 2014 in Tuesday Lake (ask Grace, Mike)
casc.color = casc.color %>% filter(PML.g440 < 8.5)

# calculate mean color
mean.lake.color = casc.color %>% group_by(lake) %>% summarize(mean.color = mean(PML.g440, na.rm = TRUE), sd.color = sd(PML.g440, na.rm = TRUE))



#### calculate mean chlorophyll ####
chl = read.csv("./formatted data/full raw data manual and sonde chlorophyll.csv")

ggplot(data = chl, aes(x = doyCat, y = manual_chl, color = as.factor(year)))+
  geom_point()+
  geom_line()+
  facet_wrap(~lake)

mean.chl = chl %>% group_by(lake) %>% summarize(mean.chl = mean(manual_chl, na.rm = TRUE), sd.chl = sd(manual_chl, na.rm = TRUE))











### Plot slopes rank ####
# slopes.rank = slopes %>% mutate(rank = rank(percent_change))
# 
# ggplot(slopes.rank, aes(x = rank, y = percent_change, fill = period))+
#   geom_point(alpha = 0.5, pch = 21)+
#   facet_wrap(~lake)+
#   scale_fill_manual(values = c("black", "white"))+
#   geom_hline(yintercept = 0, linetype = "dashed")
# 
# ggplot(slopes.rank, aes(x = period, y = percent_change, fill = lake))+
#   geom_boxplot(size = 0.8)+
#   facet_wrap(~lake)+
#   scale_fill_manual(values = c("black", "white"))+
#   geom_hline(yintercept = 0, linetype = "dashed")+
#   scale_fill_manual(values = c("R"=  "#4AB5C4", "L" = "#ADDAE3", "T"=  "#BAAD8D"))
#   











#### format color for teleconnections ####
casc.color = dt1

# 2008-2016 cascade color
# rename columns to match my standard format
casc.color = casc.color %>% rename(lake = lakeid, year = year4, doy = daynum, date = sampledate)

# filter to just PML color and years 2008-2016
casc.color = casc.color %>% 
  filter(lake == "R" | lake == "L" | lake == "T") %>% 
  filter(depth == "PML")

ggplot(data = casc.color, aes(x = doy, y = absorbance, color = as.factor(year)))+
  geom_line()+
  facet_wrap(~lake)

# calculate g440 from absorbance
# 10 cm cuvette
casc.color = casc.color %>% mutate(PML.g440 = absorbance *2.303/0.1) %>% mutate(PML = absorbance)

casc.color = casc.color %>% select(lake, date, year, doy, PML, PML.g440) %>% filter(!is.na(lake)) %>% filter(!is.na(PML))

casc.color = rbind(casc.color, wc.18.19)

ggplot(data = casc.color, aes(x = doy, y = PML.g440, color = as.factor(year)))+
  geom_line()+
  facet_wrap(~lake)

# remove one suspected outlier from 2014 in Tuesday Lake (ask Grace, Mike)
casc.color = casc.color %>% filter(PML.g440 < 8.5)

casc.color = casc.color %>% mutate(month = month(date))

# calculate mean monthly color
mean.lake.color = casc.color %>% group_by(lake) %>% summarize(mean.color = mean(PML.g440, na.rm = TRUE), sd.color = sd(PML.g440, na.rm = TRUE))

mean.monthly.color = casc.color %>% group_by(lake, year, month) %>% summarize(mean.color = mean(PML.g440, na.rm = TRUE))

monthly.color.L = mean.monthly.color %>% filter(lake == "L")

# Create all combinations of years and months
all_combinations <- expand.grid(year = seq(1999, 2019), month = seq(1, 12))

monthly.values <- merge(all_combinations, data.frame(lake = "L"), all.x = TRUE)

monthly.color.L = monthly.color.L %>% mutate(lake = as.character(lake)) %>% ungroup()

monthly.values = monthly.values %>% left_join(monthly.color.L, by = c("year", "month", "lake"))

#monthly.values = monthly.values %>% mutate(mean.color = replace(mean.color, is.na(mean.color), -666))

# pivot wider
# monthly.values = monthly.values %>%  select(-lake) %>% 
#   pivot_wider(id_cols = c("year"), values_from = "mean.color", 
#               names_from = month, names_prefix = "xm")
# 


monthly.values %>%
  group_by(year, month) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1L)

monthly.values <- monthly.values %>%
  pivot_wider(
    id_cols = year,
    names_from = month,
    values_from = mean.color,
    names_prefix = "xm"
  )

monthly.values = monthly.values %>% mutate(replace(., is.na(.), -666))

write.csv(monthly.values, "./formatted data/teleconnections data/Paul_color_monthly.csv", row.names = FALSE)






#### Weekly daphnia data at time of heatwave #####

zoops.18.19 = read.csv("./formatted data/explanatory variables heatwaves/cascade_zoops_2018_2019_lterFormat_v1.csv")

zoops.18.19 = zoops.18.19 %>% filter(taxon_name == "Daphnia")

ggplot(zoops.18.19, aes(x = daynum, y = abundance, color = lakeid))+
  geom_line()+
  geom_point()+
  facet_wrap(~year4)+
  theme_classic()











#### Routine zooplankton 2008-2016 

# Package ID: knb-lter-ntl.355.5 Cataloging System:https://pasta.edirepository.org.
# Data set title: Cascade Project at North Temperate Lakes LTER Core Data Zooplankton 1984 - 2016.
# Data set creator:  Stephen Carpenter - University of Wisconsin 
# Data set creator:  Jim Kitchell - University of Wisconsin 
# Data set creator:  Jon Cole - Cary Institute of Ecosystem Studies 
# Data set creator:  Mike Pace - University of Virginia 
# Contact:  Stephen Carpenter -  University of Wisconsin  - steve.carpenter@wisc.edu
# Contact:  Mike Pace -  University of Virginia  - pacem@virginia.edu
# Contact:    -  NTL LTER  - ntl.infomgr@gmail.com
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/355/5/8084d8a30424cbf3feb4f69621e6c0a1" 
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
                 "standard_taxon_code",     
                 "group_code",     
                 "taxon_name",     
                 "number_per_net",     
                 "mean_length",     
                 "mean_individ_biomass",     
                 "net_efficiency_measured",     
                 "net_efficiency_mean",     
                 "abundance_raw",     
                 "abundance",     
                 "biomass"    ), check.names=TRUE)

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
if (class(dt1$standard_taxon_code)!="factor") dt1$standard_taxon_code<- as.factor(dt1$standard_taxon_code)
if (class(dt1$group_code)!="factor") dt1$group_code<- as.factor(dt1$group_code)
if (class(dt1$taxon_name)!="factor") dt1$taxon_name<- as.factor(dt1$taxon_name)
if (class(dt1$number_per_net)=="factor") dt1$number_per_net <-as.numeric(levels(dt1$number_per_net))[as.integer(dt1$number_per_net) ]               
if (class(dt1$number_per_net)=="character") dt1$number_per_net <-as.numeric(dt1$number_per_net)
if (class(dt1$mean_length)=="factor") dt1$mean_length <-as.numeric(levels(dt1$mean_length))[as.integer(dt1$mean_length) ]               
if (class(dt1$mean_length)=="character") dt1$mean_length <-as.numeric(dt1$mean_length)
if (class(dt1$mean_individ_biomass)=="factor") dt1$mean_individ_biomass <-as.numeric(levels(dt1$mean_individ_biomass))[as.integer(dt1$mean_individ_biomass) ]               
if (class(dt1$mean_individ_biomass)=="character") dt1$mean_individ_biomass <-as.numeric(dt1$mean_individ_biomass)
if (class(dt1$net_efficiency_measured)=="factor") dt1$net_efficiency_measured <-as.numeric(levels(dt1$net_efficiency_measured))[as.integer(dt1$net_efficiency_measured) ]               
if (class(dt1$net_efficiency_measured)=="character") dt1$net_efficiency_measured <-as.numeric(dt1$net_efficiency_measured)
if (class(dt1$net_efficiency_mean)=="factor") dt1$net_efficiency_mean <-as.numeric(levels(dt1$net_efficiency_mean))[as.integer(dt1$net_efficiency_mean) ]               
if (class(dt1$net_efficiency_mean)=="character") dt1$net_efficiency_mean <-as.numeric(dt1$net_efficiency_mean)
if (class(dt1$abundance_raw)=="factor") dt1$abundance_raw <-as.numeric(levels(dt1$abundance_raw))[as.integer(dt1$abundance_raw) ]               
if (class(dt1$abundance_raw)=="character") dt1$abundance_raw <-as.numeric(dt1$abundance_raw)
if (class(dt1$abundance)=="factor") dt1$abundance <-as.numeric(levels(dt1$abundance))[as.integer(dt1$abundance) ]               
if (class(dt1$abundance)=="character") dt1$abundance <-as.numeric(dt1$abundance)
if (class(dt1$biomass)=="factor") dt1$biomass <-as.numeric(levels(dt1$biomass))[as.integer(dt1$biomass) ]               
if (class(dt1$biomass)=="character") dt1$biomass <-as.numeric(dt1$biomass)

# Convert Missing Values to NA for non-dates

dt1$number_per_net <- ifelse((trimws(as.character(dt1$number_per_net))==trimws("NA")),NA,dt1$number_per_net)               
suppressWarnings(dt1$number_per_net <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$number_per_net))==as.character(as.numeric("NA"))),NA,dt1$number_per_net))
dt1$mean_length <- ifelse((trimws(as.character(dt1$mean_length))==trimws("NA")),NA,dt1$mean_length)               
suppressWarnings(dt1$mean_length <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$mean_length))==as.character(as.numeric("NA"))),NA,dt1$mean_length))
dt1$mean_individ_biomass <- ifelse((trimws(as.character(dt1$mean_individ_biomass))==trimws("NA")),NA,dt1$mean_individ_biomass)               
suppressWarnings(dt1$mean_individ_biomass <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$mean_individ_biomass))==as.character(as.numeric("NA"))),NA,dt1$mean_individ_biomass))
dt1$net_efficiency_measured <- ifelse((trimws(as.character(dt1$net_efficiency_measured))==trimws("NA")),NA,dt1$net_efficiency_measured)               
suppressWarnings(dt1$net_efficiency_measured <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$net_efficiency_measured))==as.character(as.numeric("NA"))),NA,dt1$net_efficiency_measured))
dt1$net_efficiency_mean <- ifelse((trimws(as.character(dt1$net_efficiency_mean))==trimws("NA")),NA,dt1$net_efficiency_mean)               
suppressWarnings(dt1$net_efficiency_mean <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$net_efficiency_mean))==as.character(as.numeric("NA"))),NA,dt1$net_efficiency_mean))
dt1$abundance_raw <- ifelse((trimws(as.character(dt1$abundance_raw))==trimws("NA")),NA,dt1$abundance_raw)               
suppressWarnings(dt1$abundance_raw <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$abundance_raw))==as.character(as.numeric("NA"))),NA,dt1$abundance_raw))
dt1$abundance <- ifelse((trimws(as.character(dt1$abundance))==trimws("NA")),NA,dt1$abundance)               
suppressWarnings(dt1$abundance <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$abundance))==as.character(as.numeric("NA"))),NA,dt1$abundance))
dt1$biomass <- ifelse((trimws(as.character(dt1$biomass))==trimws("NA")),NA,dt1$biomass)               
suppressWarnings(dt1$biomass <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$biomass))==as.character(as.numeric("NA"))),NA,dt1$biomass))


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(lakeid)
summary(lakename)
summary(year4)
summary(daynum)
summary(sampledate)
summary(standard_taxon_code)
summary(group_code)
summary(taxon_name)
summary(number_per_net)
summary(mean_length)
summary(mean_individ_biomass)
summary(net_efficiency_measured)
summary(net_efficiency_mean)
summary(abundance_raw)
summary(abundance)
summary(biomass) 
# Get more details on character variables

summary(as.factor(dt1$lakeid)) 
summary(as.factor(dt1$lakename)) 
summary(as.factor(dt1$standard_taxon_code)) 
summary(as.factor(dt1$group_code)) 
summary(as.factor(dt1$taxon_name))
detach(dt1)               


zoop.08.16 = dt1

zoop.08.16 = zoop.08.16 %>% filter(year4 > 2007) %>% 
  filter(lakeid != "Ward") %>% 
  filter(taxon_name == "Daphnia")


ggplot(zoop.08.16, aes(x = daynum, y = abundance, color = lakeid))+
  facet_wrap(~year4)+
  geom_point()+
  geom_line()+
  theme_classic()




##### Combine zooplankton dataframes
zoops.18.19 = zoops.18.19 %>% select(lakeid, year4, daynum, sampledate, taxon_name, number_per_net, mean_length, mean_individ_biomass,
                                   net_efficiency_mean, abundance_raw, abundance, biomass)

zoops.08.16 = zoop.08.16 %>% select(lakeid, year4, daynum, sampledate, taxon_name, number_per_net, mean_length, mean_individ_biomass,
                                    net_efficiency_mean, abundance_raw, abundance, biomass)

casc.weekly.zoops = rbind(zoops.08.16, zoops.18.19)

ggplot(casc.weekly.zoops, aes(x = daynum, y = abundance, color = lakeid))+
  facet_wrap(~year4)+
  geom_point()+
  geom_line()+
  theme_classic()


ggplot(casc.weekly.zoops, aes(x = daynum, y = biomass, color = lakeid))+
  facet_wrap(~year4)+
  geom_point()+
  geom_line()+
  theme_classic()








###### combine all of the different Cascade datasets ########

casc.weekly.zoops = casc.weekly.zoops %>% dplyr::rename(lake = lakeid, year = year4, doy = daynum,
                                                        date = sampledate)

casc.weekly.zoops = casc.weekly.zoops %>% mutate(lake = as.character(lake))
casc.color = casc.color %>% mutate(lake = as.character(lake))

casc.color.zoops = casc.color %>% full_join(casc.weekly.zoops, by = c("lake", "date", "year", "doy"))


nut.load = read.csv("./formatted data/explanatory variables heatwaves.csv")

# make a date column in nut.load
nut.load = nut.load %>% mutate(date = as.Date(paste(year, doy), format = "%Y %j"))

casc.color.nut.zoops = casc.color.zoops %>% full_join(nut.load, by = c("lake", "date", "year", "doy"))

#write.csv(casc.color.nut.zoops, "./formatted data/explanatory variables heatwaves/color nutrient additions and zooplankton.csv", row.names = FALSE)



## function which combines heatwaves output with the explanatory variables ##
heatwaves.exp = read.csv("./formatted data/explanatory variables heatwaves/heatwaves with percent.csv")

heatwaves.exp = heatwaves.exp %>% mutate(doy = yday(date_start))

#heatwaves.exp = heatwaves.exp %>% group_by(lake, year) %>% join_by(casc.color.nut.zoops, date_start > date)


# create new columns of heatwaves.exp for PML.g440, biomass, cumulative.load, daily.load
heatwaves.exp = heatwaves.exp %>% mutate(PML.g440 = NA, biomass = NA, biomass.during = NA, cumulative.load = NA, daily.load = NA)
i = 1

casc.color.nut.zoops = casc.color.nut.zoops %>% distinct()

for(i in 1:nrow(heatwaves.exp)){
  
  # save the start date of current heatwave
  targ.date = heatwaves.exp$date_start[i]
  targ.doy = heatwaves.exp$doy[i]
  print(targ.date)
  
  # save the lake of the current heatwave
  targ.lake = heatwaves.exp$lake[i]
  targ.year = heatwaves.exp$year[i]
  
  # filter the explanatory dataframe to match
  # make sure doy is <= the heatwave day
  cur.casc.exp = casc.color.nut.zoops %>% filter(lake == targ.lake, year == targ.year, doy <= heatwaves.exp$date_end[i])
  
  cur.casc.exp.zoop = cur.casc.exp %>% filter(!is.na(biomass))
  cur.casc.exp.color = cur.casc.exp %>% filter(!is.na(PML.g440))
  cur.casc.exp.nut = cur.casc.exp %>% filter(!is.na(cumulative.load))
  
  # either before or during heatwave
  j = which(abs(cur.casc.exp.zoop$doy - targ.doy) == min(abs(cur.casc.exp.zoop$doy - targ.doy)))
  if(nrow(cur.casc.exp.zoop) > 0){ heatwaves.exp$biomass[i] = cur.casc.exp.zoop$biomass[j]}
  
  if(nrow(cur.casc.exp.zoop) > 0){
  cur.casc.exp.zoop = cur.casc.exp %>% filter(date >= targ.date & date <= heatwaves.exp$date_end[i])
  biomass.during.hw = mean(cur.casc.exp.zoop$biomass, na.rm = TRUE)}
  
  if(nrow(cur.casc.exp.zoop) > 0){heatwaves.exp$biomass.during[i] = biomass.during.hw}
  
  k = which(abs(cur.casc.exp.color$doy - targ.doy) == min(abs(cur.casc.exp.color$doy - targ.doy)))
  if(nrow(cur.casc.exp.color) > 0){ heatwaves.exp$PML.g440[i] = cur.casc.exp.color$PML.g440[k]}
  
  l = which(abs(cur.casc.exp.nut$doy - targ.doy) == min(abs(cur.casc.exp.nut$doy - targ.doy)))
  if(nrow(cur.casc.exp.nut) > 0){ heatwaves.exp$cumulative.load[i] = cur.casc.exp.nut$cumulative.load[l]
                                  heatwaves.exp$daily.load[i] = cur.casc.exp.nut$daily.load[l]
  }
  
}

# there are certain cases when routines started after the heatwave
# or when there were no daphnia in Tuesday Lake


write.csv(heatwaves.exp, "./formatted data/explanatory variables heatwaves/heatwaves with percent zoop color nutrients.csv", row.names = FALSE)



ggplot(heatwaves.exp, aes(x = log10(biomass.during), y = percentChange, fill = lake))+
  geom_point(size = 5, color = "black", shape = 21, stroke = 1, alpha = 0.9)+
  labs(x = "log10(Daphnia biomass)", y = "")+
  theme_classic()+
  scale_fill_manual(values = c("R" = "#4AB5C4", "L" = "#ADDAE3", "T" = "#BAAD8D"))+
  ylim(0, 250)

