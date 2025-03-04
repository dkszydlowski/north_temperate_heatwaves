### investigate changes in phycocyanin following heatwaves ####

library(tidyverse)

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
# Stylesheet v2.14 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu      
# Uncomment the following lines to have R clear previous work, or set a working directory
# rm(list=ls())      

# setwd("C:/users/my_name/my_dir")       



options(HTTPUserAgent="EDI_CodeGen")


inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/413/2/9d812efccf4107979d11e16316000b2e" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
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

phyco = dt1


ggplot(phyco %>% filter(Year != 2011 & Year != 2016), aes(x = DOY, y = BGA_HYLB, color = Lake))+
 # geom_point()+
  geom_line(size = 1)+
  facet_wrap(Lake~Year, scales = "free")+
  theme_bw()+
  scale_color_manual(values = c("R"=  "#60BFCC", "L" = "#D9EEF3", "T"=  "#544C34"),
                     labels = c("R" = "Peter", "L" = "Paul", "T" = "Tuesday"))+
  geom_rect(data = hw.exp, aes(xmin = doy.start, xmax = doy.end, ymin = -Inf, ymax = Inf), 
            fill = "gray", alpha = 0.3, inherit.aes = FALSE)
                    



### read in the heatwave data ###
hw.exp = read.csv("./formatted data/master explanatory dataset/heatwaves explained var6.csv")

hw.exp =hw.exp %>% mutate(doy.start = yday(date_start), doy.end = yday(date_end)) %>% 
  select(lake, year, doy.start, doy.end) %>% 
  rename(Lake = lake, Year = year) %>% 
  filter(Year > 2011) %>% 
  mutate(Lake = replace(Lake, Lake == "R", "Peter")) %>% 
  mutate(Lake = replace(Lake, Lake == "L", "Paul")) %>% 
  mutate(Lake = replace(Lake, Lake == "T", "Tuesday"))

phyco = phyco %>% 
  mutate(Lake = as.character(Lake)) %>% 
  mutate(Lake = replace(Lake, Lake == "R", "Peter")) %>% 
  mutate(Lake = replace(Lake, Lake == "L", "Paul")) %>% 
  mutate(Lake = replace(Lake, Lake == "T", "Tuesday"))

ggplot(phyco %>% filter(Year != 2011 & Year != 2016), aes(x = DOY, y = BGA_HYLB, color = Lake))+
  # geom_point()+
  geom_line(size = 1)+
  facet_wrap(Lake~Year, scales = "free")+
  theme_bw()+
  scale_color_manual(values = c("R"=  "#60BFCC", "L" = "#D9EEF3", "T"=  "#544C34"),
                     labels = c("R" = "Peter", "L" = "Paul", "T" = "Tuesday"))+
  geom_rect(data = hw.exp, aes(xmin = doy.start, xmax = doy.end, ymin = -Inf, ymax = Inf), 
            fill = "gray", alpha = 0.3, inherit.aes = FALSE)



ggplot(phyco %>% filter(Year != 2011 & Year != 2016), aes(x = DOY, y = BGA_HYLB, color = Lake)) +
  geom_line(size = 1) +
  facet_wrap(Lake ~ Year, scales = "free") +
  theme_bw() +
  scale_color_manual(values = c("R" = "#60BFCC", "L" = "#D9EEF3", "T" = "#544C34"),
                     labels = c("R" = "Peter", "L" = "Paul", "T" = "Tuesday")) +
  geom_rect(data = hw.exp, aes(xmin = doy.start, xmax = doy.end, ymin = -Inf, ymax = Inf, 
                               fill = Lake), alpha = 0.3, inherit.aes = FALSE) +
  scale_fill_manual(values = c("R" = "#60BFCC", "L" = "#D9EEF3", "T" = "#544C34"))


ggplot(phyco %>% filter(Year != 2011 & Year != 2016), aes(x = DOY, y = BGA_HYLB, color = Lake)) +
  geom_line(size = 1) +
  facet_wrap(Lake ~ Year, scales = "free") +
  theme_bw() +
  scale_color_manual(values = c("R" = "#60BFCC", "L" = "#D9EEF3", "T" = "#544C34"),
                     labels = c("R" = "Peter", "L" = "Paul", "T" = "Tuesday")) +
  # Add multiple rectangles per Lake-Year
  geom_rect(data = hw.exp, aes(xmin = doy.start, xmax = doy.end, ymin = -Inf, ymax = Inf, fill = Lake),
            alpha = 0.3, inherit.aes = FALSE) +
  scale_fill_manual(values = c("R" = "#60BFCC", "L" = "#D9EEF3", "T" = "#544C34"))


ggplot(phyco %>% filter(Year != 2011 & Year != 2016) %>% arrange(year),  
         aes(x = DOY, y = BGA_HYLB)) +
  geom_rect(data = hw.exp, 
            aes(xmin = doy.start, xmax = doy.end, ymin = -Inf, ymax = Inf, fill = Lake), 
            alpha = 1, inherit.aes = FALSE) +
  geom_line(size = 0.8) +
  facet_wrap(Lake ~ Year, scales = "free") +
  theme_bw() +
 # scale_color_manual(values = c("R" = "#60BFCC", "L" = "#D9EEF3", "T" = "#544C34"),
  #                   labels = c("R" = "Peter", "L" = "Paul", "T" = "Tuesday")) +
  # Add rectangles per Lake-Year from hw.exp
  scale_fill_manual(values = c("Peter" = "#60BFCC", "Paul" = "#D9EEF3", "Tuesday" = "#544C34"))




ggplot(phyco %>% filter(Year == 2016) %>% arrange(year),  
       aes(x = DOY, y = BGA_HYLB, color = Lake)) +
  #geom_rect(data = hw.exp, 
   #         aes(xmin = doy.start, xmax = doy.end, ymin = -Inf, ymax = Inf, fill = Lake), 
    #        alpha = 1, inherit.aes = FALSE) +
  geom_line(size = 2) +
  labs(title = "2016 phycocyanin")+
  #facet_wrap(Lake ~ Year) +
  theme_bw() +
  # scale_color_manual(values = c("R" = "#60BFCC", "L" = "#D9EEF3", "T" = "#544C34"),
  #                   labels = c("R" = "Peter", "L" = "Paul", "T" = "Tuesday")) +
  # Add rectangles per Lake-Year from hw.exp
  scale_color_manual(values = c("Peter" = "#60BFCC", "Paul" = "#D9EEF3", "Tuesday" = "#544C34"))
