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

