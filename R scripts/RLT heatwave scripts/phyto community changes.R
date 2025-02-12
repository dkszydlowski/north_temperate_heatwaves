### community changes in phytoplankton ater heatwaves

library(tidyverse)
library(vegan)

# Package ID: knb-lter-ntl.353.5 Cataloging System:https://pasta.edirepository.org.
# Data set title: Cascade Project at North Temperate Lakes LTER Core Data Phytoplankton 1984 - 2015.
# Data set creator:  Stephen Carpenter - University of Wisconsin 
# Data set creator:  Jim Kitchell - University of Wisconsin 
# Data set creator:  Jon Cole - Cary Institute of Ecosystem Studies 
# Data set creator:  Mike Pace - University of Virginia 
# Contact:  Stephen Carpenter -  University of Wisconsin  - steve.carpenter@wisc.edu
# Contact:  Mike Pace -  University of Virginia  - pacem@virginia.edu
# Contact:    -  NTL LTER  - ntl.infomgr@gmail.com
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/353/5/c36bcc062259fe68fb719996eb470ce6" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "lakename",     
                 "lakeid",     
                 "year4",     
                 "daynum",     
                 "sampledate",     
                 "division",     
                 "genus",     
                 "species",     
                 "description",     
                 "concentration",     
                 "gal_dimension",     
                 "mean_individ_vol",     
                 "mean_individ_biovol",     
                 "total_vol",     
                 "total_biovol"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$lakename)!="factor") dt1$lakename<- as.factor(dt1$lakename)
if (class(dt1$lakeid)!="factor") dt1$lakeid<- as.factor(dt1$lakeid)
if (class(dt1$year4)=="factor") dt1$year4 <-as.numeric(levels(dt1$year4))[as.integer(dt1$year4) ]               
if (class(dt1$year4)=="character") dt1$year4 <-as.numeric(dt1$year4)
if (class(dt1$daynum)=="factor") dt1$daynum <-as.numeric(levels(dt1$daynum))[as.integer(dt1$daynum) ]               
if (class(dt1$daynum)=="character") dt1$daynum <-as.numeric(dt1$daynum)                                   
# attempting to convert dt1$sampledate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1sampledate<-as.Date(dt1$sampledate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt1[dt1$sampledate != "",]) == length(tmp1sampledate[!is.na(tmp1sampledate)])){dt1$sampledate <- tmp1sampledate } else {print("Date conversion failed for dt1$sampledate. Please inspect the data and do the date conversion yourself.")}                                                                    

if (class(dt1$division)!="factor") dt1$division<- as.factor(dt1$division)
if (class(dt1$genus)!="factor") dt1$genus<- as.factor(dt1$genus)
if (class(dt1$species)!="factor") dt1$species<- as.factor(dt1$species)
if (class(dt1$description)!="factor") dt1$description<- as.factor(dt1$description)
if (class(dt1$concentration)=="factor") dt1$concentration <-as.numeric(levels(dt1$concentration))[as.integer(dt1$concentration) ]               
if (class(dt1$concentration)=="character") dt1$concentration <-as.numeric(dt1$concentration)
if (class(dt1$gal_dimension)=="factor") dt1$gal_dimension <-as.numeric(levels(dt1$gal_dimension))[as.integer(dt1$gal_dimension) ]               
if (class(dt1$gal_dimension)=="character") dt1$gal_dimension <-as.numeric(dt1$gal_dimension)
if (class(dt1$mean_individ_vol)=="factor") dt1$mean_individ_vol <-as.numeric(levels(dt1$mean_individ_vol))[as.integer(dt1$mean_individ_vol) ]               
if (class(dt1$mean_individ_vol)=="character") dt1$mean_individ_vol <-as.numeric(dt1$mean_individ_vol)
if (class(dt1$mean_individ_biovol)=="factor") dt1$mean_individ_biovol <-as.numeric(levels(dt1$mean_individ_biovol))[as.integer(dt1$mean_individ_biovol) ]               
if (class(dt1$mean_individ_biovol)=="character") dt1$mean_individ_biovol <-as.numeric(dt1$mean_individ_biovol)
if (class(dt1$total_vol)=="factor") dt1$total_vol <-as.numeric(levels(dt1$total_vol))[as.integer(dt1$total_vol) ]               
if (class(dt1$total_vol)=="character") dt1$total_vol <-as.numeric(dt1$total_vol)
if (class(dt1$total_biovol)=="factor") dt1$total_biovol <-as.numeric(levels(dt1$total_biovol))[as.integer(dt1$total_biovol) ]               
if (class(dt1$total_biovol)=="character") dt1$total_biovol <-as.numeric(dt1$total_biovol)

# Convert Missing Values to NA for non-dates

dt1$year4 <- ifelse((trimws(as.character(dt1$year4))==trimws("NA")),NA,dt1$year4)               
suppressWarnings(dt1$year4 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$year4))==as.character(as.numeric("NA"))),NA,dt1$year4))
dt1$daynum <- ifelse((trimws(as.character(dt1$daynum))==trimws("NA")),NA,dt1$daynum)               
suppressWarnings(dt1$daynum <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$daynum))==as.character(as.numeric("NA"))),NA,dt1$daynum))
dt1$concentration <- ifelse((trimws(as.character(dt1$concentration))==trimws("NA")),NA,dt1$concentration)               
suppressWarnings(dt1$concentration <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$concentration))==as.character(as.numeric("NA"))),NA,dt1$concentration))
dt1$gal_dimension <- ifelse((trimws(as.character(dt1$gal_dimension))==trimws("NA")),NA,dt1$gal_dimension)               
suppressWarnings(dt1$gal_dimension <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$gal_dimension))==as.character(as.numeric("NA"))),NA,dt1$gal_dimension))
dt1$mean_individ_vol <- ifelse((trimws(as.character(dt1$mean_individ_vol))==trimws("NA")),NA,dt1$mean_individ_vol)               
suppressWarnings(dt1$mean_individ_vol <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$mean_individ_vol))==as.character(as.numeric("NA"))),NA,dt1$mean_individ_vol))
dt1$mean_individ_biovol <- ifelse((trimws(as.character(dt1$mean_individ_biovol))==trimws("NA")),NA,dt1$mean_individ_biovol)               
suppressWarnings(dt1$mean_individ_biovol <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$mean_individ_biovol))==as.character(as.numeric("NA"))),NA,dt1$mean_individ_biovol))
dt1$total_vol <- ifelse((trimws(as.character(dt1$total_vol))==trimws("NA")),NA,dt1$total_vol)               
suppressWarnings(dt1$total_vol <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$total_vol))==as.character(as.numeric("NA"))),NA,dt1$total_vol))
dt1$total_biovol <- ifelse((trimws(as.character(dt1$total_biovol))==trimws("NA")),NA,dt1$total_biovol)               
suppressWarnings(dt1$total_biovol <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$total_biovol))==as.character(as.numeric("NA"))),NA,dt1$total_biovol))


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(lakename)
summary(lakeid)
summary(year4)
summary(daynum)
summary(sampledate)
summary(division)
summary(genus)
summary(species)
summary(description)
summary(concentration)
summary(gal_dimension)
summary(mean_individ_vol)
summary(mean_individ_biovol)
summary(total_vol)
summary(total_biovol) 
# Get more details on character variables

summary(as.factor(dt1$lakename)) 
summary(as.factor(dt1$lakeid)) 
summary(as.factor(dt1$division)) 
summary(as.factor(dt1$genus)) 
summary(as.factor(dt1$species)) 
summary(as.factor(dt1$description))
detach(dt1)               



phyto = dt1

phyto.all = dt1


phyto = phyto %>% filter(year4 >= 2008)


phyto.wide = phyto %>% pivot_wider(names_from = genus, values_from = concentration, id_cols = c("lakeid", "year4", "daynum"))


phyto.wide = phyto.wide %>% select(-"NA")






# Install and load necessary packages
install.packages("vegan")
install.packages("tidyverse")
library(vegan)
library(tidyverse)


# Convert list elements to numeric, replace NULL with NA
phyto.wide <- phyto.wide %>% 
  mutate(across(where(is.list), ~map_dbl(.x, ~ifelse(is.null(.x), NA, .x))))

# Drop non-numeric columns (lakeid, year4, daynum)
phyto_matrix <- phyto.wide %>% 
  select(-lakeid, -year4, -daynum) %>% 
  as.data.frame() %>% 
  as.matrix()

# Replace NAs with zeros (if appropriate for your analysis)
phyto_matrix[is.na(phyto_matrix)] <- 0

# Calculate the Distance Matrix
phyto_dist <- vegdist(phyto_matrix, method = "bray")

# Perform NMDS
phyto_nmds <- metaMDS(phyto_dist, k = 2, trymax = 100)

# Basic plot
plot(phyto_nmds)

# Enhanced plot with ggplot2
phyto_nmds_scores <- as.data.frame(scores(phyto_nmds))
phyto_nmds_scores <- cbind(phyto.wide[, c("lakeid", "year4", "daynum")], phyto_nmds_scores)

ggplot(phyto_nmds_scores, aes(x = NMDS1, y = NMDS2, fill = lakeid)) +
  geom_point(size = 4, pch = 21) +
  theme_minimal() +
  labs(title = "NMDS of Phytoplankton Community Data 2013-2015",
       x = "NMDS1",
       y = "NMDS2") +
  scale_fill_manual(values = c("R" = "#4AB5C4", "L" = "#ADDAE3", "T" = "#BAAD8D")) +
  theme(legend.position = "right")


slide


##### identify dominant species of phytoplankton during heatwaves when we have the data #####
phyto = dt1

phyto = phyto %>% filter(year4 >= 2008 & lakeid %in% c("L", "R", "T"))

## get total volume by species ##
phyto.total = phyto %>% group_by(division, lakeid, year4, daynum) %>% 
  summarize(total.volume = sum(total_vol, na.rm = TRUE))

png("./figures/revisions draft 2025-01-27/phytoplankton community over time.png", height = 6, width = 7, res = 300, units = "in")
ggplot(phyto.total %>% filter(division != "Miscellaneous"), aes(x = daynum, y = percent, fill = division)) +
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Percent of total", fill = "Division") +
  facet_wrap(year4~lakeid)+
  scale_fill_manual(values = c("Bacillariophyta" = "black", "Chlorophyta" = "#009292", "Chrysophyta" = "#ffff6d", "Cryptophyta" = "#db6d00",
                               "Cyanophyta" = "#004949", "Euglenophyta" = "#b66dff", "Rhodophyta" = "#920000", "Pyrrhophyta" = "#924900",
                               "Xanthophyta" = "#ffb6db", "Haptophyta" = "#006ddb"))+
  theme_bw()

dev.off()
# color-blind friendly colors
# "#000000","#004949","#009292","#ff6db6","#ffb6db",
# "#490092","#006ddb","#b66dff","#6db6ff","#b6dbff",
# "#920000","#924900","#db6d00","#24ff24","#ffff6d"

## create a column that has the percent of total volume ##
phyto.total = phyto.total %>% group_by(lakeid, year4, daynum) %>% 
  mutate(day.total = sum(total.volume, na.rm = TRUE)) %>% 
  mutate(percent = 100*total.volume/day.total)





#### relate phytoplankton biovolume to chlorophyll concentrations during hw years #####

phyto.total = phyto %>% group_by(lakeid, year4, daynum) %>% 
  summarize(total.phyto = sum(total_biovol, na.rm = TRUE))

chl = read.csv("./formatted data/interpolated_manual_chl_for_slopes.csv") %>% 
  rename(doy = doyCat)

phyto.total = phyto.total %>% rename(doy = daynum, year = year4, lake = lakeid)

phyto.total = phyto.total %>% left_join(chl, by = c("lake", "year", "doy"))

ggplot(phyto.total, aes(x = total.phyto, y = mean_chl, fill = lake))+
  geom_point(size = 4, pch = 21, alpha = 0.8)+
  scale_fill_manual(values = c("R"=  "#60BFCC", "L" = "#D9EEF3", "T"=  "#544C34"),
                    labels = c("R" = "Peter", "L" = "Paul", "T" = "Tuesday"))+
  theme_classic()+
  labs(x = expression("total phytoplankton biovolume (mm"^3*"/L)"),
       y = expression("mean filtered chlorophyll (  "*mu*"g/L)"))+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))






###### calculate total phytoplankton biovolume for all available years #####

phyto.all.sum = phyto.all %>% group_by(lakeid, year4, daynum, sampledate) %>% 
  summarize(total.phyto = sum(total_biovol, na.rm = TRUE))

phyto.all.sum = phyto.all.sum %>% rename(doy = daynum, year = year4, lake = lakeid)

phyto.all.sum = phyto.all.sum %>% left_join(chl, by = c("lake", "year", "doy")) %>% filter(lake %in% c("R", "L", "T"))


ggplot(phyto.all.sum, aes( x = doy, y = total.phyto, color = lake))+
  geom_line(size = 1)+
  geom_point()+
  facet_wrap(~year, scales = "free")+
  scale_color_manual(values = c("R"=  "#60BFCC", "L" = "#D9EEF3", "T"=  "#544C34"))+
  theme_classic()
                    
  
## read in the routines chlorophyll data ###

# Package ID: knb-lter-ntl.354.5 Cataloging System:https://pasta.edirepository.org.
# Data set title: Cascade Project at North Temperate Lakes LTER Core Data Process Data 1984 - 2016.
# Data set creator:  Stephen Carpenter - University of Wisconsin 
# Data set creator:  Jim Kitchell - University of Wisconsin 
# Data set creator:  Jon Cole - Cary Institute of Ecosystem Studies 
# Data set creator:  Mike Pace - University of Virginia 
# Contact:  Stephen Carpenter -  University of Wisconsin  - steve.carpenter@wisc.edu
# Contact:  Mike Pace -  University of Virginia  - pacem@virginia.edu
# Contact:    -  NTL LTER  - ntl.infomgr@gmail.com
# Stylesheet v2.14 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu      
# Uncomment the following lines to have R clear previous work, or set a working directory
# rm(list=ls())      

# setwd("C:/users/my_name/my_dir")       



options(HTTPUserAgent="EDI_CodeGen")


inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/354/5/f8d6f5308b4d0f5437ffa62725b6fc8e" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
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


rout.chl = dt1 %>% filter(depth == 0) %>% 
  filter(lakeid %in% c("L", "R", "T")) %>% 
  rename(lake = lakeid, year = year4, doy = daynum, date = sampledate) %>% 
  select(lake, year, doy, date, chla) %>% 
  mutate(week = week(date))

### add in the phyto total biovolume dataset

phyto.and.rout.chl = phyto.all.sum %>% select(lake, year, doy, sampledate, total.phyto) %>% 
  mutate(week = week(sampledate)) %>% 
  left_join(rout.chl, by = c("lake", "year", "week"))


ggplot(phyto.and.rout.chl, aes(x = total.phyto, y = chla, color = lake))+
  geom_point()+
  facet_wrap(~year, scales = "free")

## relationship is not always great, but these are at a weekly timescale. Could also be taken at different depths
# would have needed to average samples...
# need to look at the daily values from 2013-2015, which were the heatwave years
# the phytoplankton were pooled samples at 100%, 50%, and 25% of surface irradiance



