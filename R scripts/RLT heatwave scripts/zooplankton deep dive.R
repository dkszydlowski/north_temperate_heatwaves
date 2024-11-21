
library(tidyverse)
library(DescTools)


zoops.18.19 = read.csv("./formatted data/explanatory variables heatwaves/cascade_zoops_2018_2019_lterFormat_v1.csv")

#zoops.18.19 = zoops.18.19 %>% filter(taxon_name == "Daphnia")

ggplot(zoops.18.19, aes(x = daynum, y = abundance, color = lakeid))+
  geom_line()+
  geom_point()+
  facet_wrap(~year4)+
  theme_classic()











#### Routine zooplankton 2008-2016 ####

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


zoop.08.16 = dt1

zoop.08.16 = zoop.08.16 %>% 
  filter(year4 > 2007) %>% 
  filter(lakeid != "Ward") 


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


ggplot(casc.weekly.zoops %>% filter(grepl("holopedium", taxon_name, ignore.case = TRUE)), aes(x = daynum, y = biomass, color = lakeid))+
  facet_wrap(~year4, scales = "free")+
  geom_point()+
  geom_line()+
  theme_classic()



ggplot(casc.weekly.zoops %>% filter(grepl("Daphnia", taxon_name, ignore.case = TRUE)), aes(x = daynum, y = biomass, color = lakeid))+
  facet_wrap(~year4, scales = "free")+
  geom_point()+
  geom_line()+
  theme_classic()



casc.weekly.zoops.total = casc.weekly.zoops %>% group_by(lakeid, year4, daynum) %>% 
  summarize(biomass.total = sum(biomass, na.rm = TRUE))


ggplot(casc.weekly.zoops.total %>% filter(year4 != 2016), aes(x = daynum, y = biomass.total, color = lakeid))+
  facet_wrap(~year4, scales = "free")+
  geom_point()+
  geom_line()+
  theme_classic()

##### compare zooplankton changes before and after heatwaves #######
# get zooplankton biomass before, during, and after heatwaves
# do this for holopedium, Daphnia, and total biomass

#hw.exp = read.csv("./formatted data/explanatory variables heatwaves/heatwaves with percent zoop color nutrients.csv")

hw.exp = read.csv("./formatted data/master explanatory dataset/heatwaves explained var3.csv")

# get rid of rows with no biomass
cur.casc.weekly.zoops = casc.weekly.zoops %>% filter(!is.na(biomass))

# create columns in hw.exp to hold new values for zooplankton


# add in the chl
chl = read.csv("./formatted data/interpolated_manual_chl_for_slopes.csv")

chl = chl %>% rename(daynum = doyCat, year4 = year, lakeid = lake) %>% 
  select(year4, daynum, lakeid, manual_chl)

cur.casc.weekly.zoops = cur.casc.weekly.zoops %>% left_join(chl, by = c("daynum", "year4", "lakeid"))


hw.exp = hw.exp %>% mutate(daphnia.biomass.before = NA, daphnia.biomass.during = NA, daphnia.biomass.after = NA,
                           holop.biomass.before = NA, holop.biomass.during = NA, holop.biomass.after = NA,
                           total.biomass.before = NA, total.biomass.during = NA, total.biomass.after = NA,
                           manual.chl.before = NA, manual.chl.during = NA, manual.chl.after = NA,
                           zoop.days.before = NA, zoop.days.after.start.during.hw = NA, zoop.days.after.hw = NA,
                           daphnia.length.before = NA, daphnia.length.during = NA, daphnia.length.after = NA, event_index = NA, start.date.group = NA)


cur.casc.weekly.zoops = cur.casc.weekly.zoops %>% distinct()


###### add a grouping number to the heatwave in hw.exp ######

results = hw.exp

results = results %>% mutate(event_group = NA)

# Convert date_start and date_end to Date class
results$date_start <- as.Date(results$date_start)
results$date_end <- as.Date(results$date_end)

results$overlaps = NA
results$event_group = NA


for(i in 1:nrow(results)){
  if(is.na(results$event_group[i])){
    
    cur.start.date = results$date_start[i]
    cur.end.date = results$date_end[i]
    
    
    for(j in 1:nrow(results)){
      results$overlaps[j] = c(cur.start.date, cur.end.date) %overlaps% c(results$date_start[j], results$date_end[j])
      
    }
    
    # assign a group_number
    results = results %>% mutate(event_group = replace(event_group, overlaps == TRUE, k))
    k = k+1
    
  }
}


results = results %>% select(-overlaps)

results = results %>% mutate(event_group = as.factor(event_group))

testing = results %>% group_by(event_group) %>% 
  dplyr::summarize(start_date_avg = mean(date_start))

results = left_join(results, testing, by = "event_group") 

hw.exp = results

i = 1
for(i in 1:nrow(hw.exp)){
  
  # save the start date of current heatwave
  targ.date = hw.exp$date_start[i]
  targ.date.end = hw.exp$date_end[i]
  targ.doy.end = yday(targ.date.end)
  targ.doy = hw.exp$doy[i] # doy at the start of the heatwave
  print(targ.date)
  print(targ.date.end)
  
  # save the lake of the current heatwave
  targ.lake = hw.exp$lake[i]
  targ.year = hw.exp$year[i]

  
  # filter the zooplankton dataframe to match each category
  cur.casc.exp.before = cur.casc.weekly.zoops %>% filter(lakeid == targ.lake, year4 == targ.year, daynum < targ.doy & daynum >= targ.doy - 7)
  cur.casc.exp.during = cur.casc.weekly.zoops %>% filter(lakeid == targ.lake, year4 == targ.year, daynum <= targ.doy.end & daynum >= targ.doy)
  cur.casc.exp.after = cur.casc.weekly.zoops %>% filter(lakeid == targ.lake, year4 == targ.year, daynum > targ.doy.end & daynum <= targ.doy.end + 7)
  
  
  # add in the start date if there is actually data
  if(nrow(cur.casc.exp.before) > 0){  cur.casc.exp.before$start.date.group = hw.exp$start_date_avg[i]}
  if(nrow(cur.casc.exp.during) > 0){  cur.casc.exp.during$start.date.group = hw.exp$start_date_avg[i]}
  if(nrow(cur.casc.exp.after) > 0){  cur.casc.exp.after$start.date.group = hw.exp$start_date_avg[i]}
  
  
  
  # total biomass before, during, and after the heatwave
  cur.casc.exp.before = cur.casc.exp.before %>% filter(daynum == max(daynum))
  if(nrow(cur.casc.exp.before) > 0){ hw.exp$total.biomass.before[i] = sum(cur.casc.exp.before$biomass, na.rm = TRUE)
                                      hw.exp$manual.chl.before[i] = mean(cur.casc.exp.before$manual_chl, na.rm = TRUE)
                                      hw.exp$zoop.days.before[i] = targ.doy - max(cur.casc.exp.before$daynum)}
  
  cur.casc.exp.during = cur.casc.exp.during %>% filter(daynum == min(daynum))
  if(nrow(cur.casc.exp.during) > 0){ hw.exp$total.biomass.during[i] = sum(cur.casc.exp.during$biomass, na.rm = TRUE)
                                      hw.exp$manual.chl.during[i] = mean(cur.casc.exp.during$manual_chl, na.rm = TRUE)
                                      hw.exp$zoop.days.after.start.during.hw[i] = max(cur.casc.exp.before$daynum) - targ.doy}
  
  cur.casc.exp.after = cur.casc.exp.after %>% filter(daynum == min(daynum))
  if(nrow(cur.casc.exp.after) > 0){ hw.exp$total.biomass.after[i] = sum(cur.casc.exp.after$biomass, na.rm = TRUE)
                                     hw.exp$manual.chl.after[i] = mean(cur.casc.exp.after$manual_chl, na.rm = TRUE)
                                     hw.exp$zoop.days.after.hw[i] = min(cur.casc.exp.before$daynum) - targ.doy.end}
  
  # save the whole zooplankton community data
  if(i == 1){zoop.before.all = cur.casc.exp.before
              zoop.before.all$event_index = i}
  if(i > 1 & nrow(cur.casc.exp.before) >= 1){  cur.casc.exp.before$event_index = i
    zoop.before.all = rbind(cur.casc.exp.before, zoop.before.all)}
  
  if(i == 1){ cur.casc.exp.during$event_index = i
    zoop.during.all = cur.casc.exp.during}
  if(i > 1 & nrow(cur.casc.exp.during) >= 1){  cur.casc.exp.during$event_index = i
    zoop.during.all = rbind(cur.casc.exp.during, zoop.during.all)}
  
  if(i == 1){ cur.casc.exp.after$event_index = i
    zoop.after.all = cur.casc.exp.after}
  if(i > 1 & nrow(cur.casc.exp.after) >= 1){ cur.casc.exp.after$event_index = i
    zoop.after.all = rbind(cur.casc.exp.after, zoop.after.all)}
  
  # Daphnia biomass before, during, and after the heatwave
  cur.casc.exp.before = cur.casc.exp.before %>% filter(daynum == max(daynum))
  if(nrow(cur.casc.exp.before) > 0){ 
    
  hw.exp$daphnia.biomass.before[i] = sum(cur.casc.exp.before %>% filter(taxon_name == "Daphnia" | 
                                                                        #taxon_name == "Holopedium gibberum" |
                                                                        taxon_name == "Ceriodaphnia") %>% pull(biomass), na.rm = TRUE)
  
  hw.exp$daphnia.length.before[i] = mean(cur.casc.exp.before %>% filter(taxon_name == "Daphnia") %>% pull(mean_length), na.rm = TRUE)
  
  }
  
  cur.casc.exp.during = cur.casc.exp.during %>% filter(daynum == min(daynum))
  if(nrow(cur.casc.exp.during) > 0){
    
    hw.exp$daphnia.biomass.during[i] = sum(cur.casc.exp.during %>% filter(taxon_name == "Daphnia" |
                                                                          #  taxon_name == "Holopedium gibberum" |
                                                                        taxon_name == "Ceriodaphnia") %>% pull(biomass), na.rm = TRUE)
    
    
    hw.exp$daphnia.length.during[i] = mean(cur.casc.exp.during %>% filter(taxon_name == "Daphnia" 
                                                                               ) %>% pull(mean_length), na.rm = TRUE)
    
    }
  
  cur.casc.exp.after = cur.casc.exp.after %>% filter(daynum == min(daynum))
  if(nrow(cur.casc.exp.after) > 0){ 
    
    hw.exp$daphnia.biomass.after[i] = sum(cur.casc.exp.after %>% filter(taxon_name == "Daphnia" |
                                                                          #taxon_name == "Holopedium gibberum" |
                                                                        taxon_name == "Ceriodaphnia" ) %>% pull(biomass), na.rm = TRUE)
    
    
    hw.exp$daphnia.length.after[i] = mean(cur.casc.exp.after %>% filter(taxon_name == "Daphnia" ) %>% pull(mean_length), na.rm = TRUE)
  
  
  
  }
  
  
}


# combine community data into one and save
zoop.before.all = zoop.before.all %>% mutate(period = "before")
zoop.during.all = zoop.during.all %>% mutate(period = "during")
zoop.after.all = zoop.after.all %>% mutate(period = "after")

zoop.all.hw = rbind(zoop.before.all, zoop.during.all, zoop.after.all)


# add in the event_no


write.csv(zoop.all.hw, "./formatted data/zooplankton/zooplankton community heatwaves.csv", row.names= FALSE)

### compare new changes in zooplankton to changes in chlorophyll
# if we look at the last sampling date during the heatwave, and the closest sampling to before it began, zooplankton decrease



hw.exp = hw.exp %>% mutate(pchange.total.zoop = 100*(total.biomass.during - total.biomass.before)/total.biomass.before,
                           abschange.total.zoop = total.biomass.during- total.biomass.before,
                           pchange.total.zoop.during.to.after = 100*(total.biomass.after - total.biomass.during)/total.biomass.during,
                           pchange.daphnia.length = 100*(daphnia.length.before- daphnia.length.during)/daphnia.length.during)


#hw.exp.zoop = hw.exp %>% select(date_start, date_end, lake, year, percentChange, pchange.daphnia.zoop, pchange.daphnia.length, abschange.daphnia.zoop,
#                                pchange.total.zoop, pchange.total.zoop.during.to.after, zoop.days.before,
#                                daphnia.biomass.before, daphnia.biomass.during, daphnia.biomass.after,
#                                daphnia.length.before, daphnia.length.during, daphnia.length.after,
#                                holop.biomass.before, holop.biomass.during, holop.biomass.after,
#                                total.biomass.before, total.biomass.during, total.biomass.after,
#                                manual.chl.before, manual.chl.during, manual.chl.after,
#                                zoop.days.before, zoop.days.after.start.during.hw, zoop.days.after.hw)



hw.exp = hw.exp %>% mutate(pchange.daphnia.zoop = 100*(daphnia.biomass.during - daphnia.biomass.before)/daphnia.biomass.before,
                           abschange.daphnia.zoop = daphnia.biomass.during- daphnia.biomass.before,
                           abschange.daphnia.zoop.during.to.after = daphnia.biomass.after - daphnia.biomass.during)

hw.exp = hw.exp %>% mutate(pchange.chl.weekly = 100*(manual.chl.after - manual.chl.before)/manual.chl.before,
                           abschange.chl = manual.chl.after- manual.chl.before)


ggplot(hw.exp, aes(x = pchange.total.zoop, y = pchange.chl.weekly, fill = lake))+
  geom_point(size = 4, pch = 21)+
  theme_classic()+
  scale_fill_manual(values = c("R" = "#4AB5C4", "L" = "#ADDAE3", "T" = "#BAAD8D"))
  
ggplot(hw.exp, aes(x = pchange.total.zoop.during.to.after, y = pchange.chl.weekly, fill = lake))+
  geom_point(size = 4, pch = 21)+
  theme_classic()+
  scale_fill_manual(values = c("R" = "#4AB5C4", "L" = "#ADDAE3", "T" = "#BAAD8D"))


ggplot(hw.exp, aes(x = pchange.total.zoop, y = percentChange, fill = lake))+
  geom_point(size = 4, pch = 21)+
  theme_classic()+
  scale_fill_manual(values = c("R" = "#4AB5C4", "L" = "#ADDAE3", "T" = "#BAAD8D"))
  

ggplot(hw.exp, aes(x = pchange.daphnia.zoop, y = percentChange, fill = lake))+
  geom_point(size = 4, pch = 21)+
  theme_classic()+
  scale_fill_manual(values = c("R" = "#4AB5C4", "L" = "#ADDAE3", "T" = "#BAAD8D"))


ggplot(hw.exp, aes(x = abschange.daphnia.zoop, y = percentChange, fill = lake))+
  geom_point(size = 4, pch = 21)+
  theme_classic()+
  scale_fill_manual(values = c("R" = "#4AB5C4", "L" = "#ADDAE3", "T" = "#BAAD8D"))


ggplot(hw.exp, aes(x = lake, y = abschange.daphnia.zoop , fill = lake))+
  geom_boxplot()+
  theme_classic()+
  geom_point()+
  scale_fill_manual(values = c("R" = "#4AB5C4", "L" = "#ADDAE3", "T" = "#BAAD8D"))+
  geom_hline(yintercept = 0, linetype = "dashed")


ggplot(hw.exp, aes(x = lake, y = abschange.daphnia.zoop , fill = lake))+
  geom_boxplot()+
  theme_classic()+
  geom_point(aes(fill = lake), pch = 21, size = 2)+
  scale_fill_manual(values = c("R" = "#4AB5C4", "L" = "#ADDAE3", "T" = "#BAAD8D"))+
  scale_color_manual(values = c("R" = "#4AB5C4", "L" = "#ADDAE3", "T" = "#BAAD8D"))+
  geom_hline(yintercept = 0, linetype = "dashed")


ggplot(hw.exp, aes(x = lake, y = pchange.daphnia.zoop , fill = lake))+
  geom_boxplot()+
  theme_classic()+
  geom_point(aes(fill = lake), pch = 21, size = 2)+
  scale_fill_manual(values = c("R" = "#4AB5C4", "L" = "#ADDAE3", "T" = "#BAAD8D"))+
  scale_color_manual(values = c("R" = "#4AB5C4", "L" = "#ADDAE3", "T" = "#BAAD8D"))+
  geom_hline(yintercept = 0, linetype = "dashed")



ggplot(hw.exp, aes(x = lake, y = pchange.daphnia.length , fill = lake))+
  geom_boxplot()+
  theme_classic()+
  geom_point(aes(fill = lake), pch = 21, size = 2)+
  scale_fill_manual(values = c("R" = "#4AB5C4", "L" = "#ADDAE3", "T" = "#BAAD8D"))+
  scale_color_manual(values = c("R" = "#4AB5C4", "L" = "#ADDAE3", "T" = "#BAAD8D"))+
  geom_hline(yintercept = 0, linetype = "dashed")


ggplot(hw.exp, aes(x = lake, y = abschange.chl , fill = lake))+
  geom_boxplot()+
  theme_classic()+
  geom_point(aes(fill = lake), pch = 21, size = 2)+
  scale_fill_manual(values = c("R" = "#4AB5C4", "L" = "#ADDAE3", "T" = "#BAAD8D"))+
  scale_color_manual(values = c("R" = "#4AB5C4", "L" = "#ADDAE3", "T" = "#BAAD8D"))+
  geom_hline(yintercept = 0, linetype = "dashed")


ggplot(hw.exp, aes(x = lake, y = pchange.total.zoop, fill = lake))+
  geom_boxplot()+
  theme_classic()+
  scale_fill_manual(values = c("R" = "#4AB5C4", "L" = "#ADDAE3", "T" = "#BAAD8D"))+
  geom_hline(yintercept = 0, linetype = "dashed")

ggplot(hw.exp, aes(x = lake, y = pchange.total.zoop.during.to.after, fill = lake))+
  geom_boxplot()+
  theme_classic()+
  scale_fill_manual(values = c("R" = "#4AB5C4", "L" = "#ADDAE3", "T" = "#BAAD8D"))+
  geom_hline(yintercept = 0, linetype = "dashed")


ggplot(hw.exp, aes(x = percentChange, y = pchange.total.zoop.during.to.after, fill = lake))+
  geom_point(pch = 21)+
  theme_classic()+
  scale_fill_manual(values = c("R" = "#4AB5C4", "L" = "#ADDAE3", "T" = "#BAAD8D"))+
  geom_hline(yintercept = 0, linetype = "dashed")


ggplot(hw.exp, aes(x = percentChange, y = pchange.total.zoop.during.to.after, fill = lake))+
  geom_point(pch = 21)+
  theme_classic()+
  scale_fill_manual(values = c("R" = "#4AB5C4", "L" = "#ADDAE3", "T" = "#BAAD8D"))+
  geom_hline(yintercept = 0, linetype = "dashed")



### line plots between before and after points
# hw.zoops.long = hw.exp %>% select(daphnia.biomass.before, daphnia.biomass.during, daphnia.biomass.after, lake, year, X) %>% 
#   pivot_longer(cols = c("daphnia.biomass.before", "daphnia.biomass.during", "daphnia.biomass.after"), names_to ="timing", values_to = "biomass")
# 
# 
# ggplot(hw.zoops.long, aes(x = timing, y = biomass, group = X, color = lake))+
#   geom_point()+
#   geom_line()

# get the average percent increase in zooplankton by lake
mean.perc.increase = hw.exp %>% group_by(lake) %>% summarize(mean.zoop.increase = median(pchange.total.zoop.during.to.after, na.rm = TRUE))

hw.exp = hw.exp %>% mutate(. = replace(., is.nan(.), NA))

summary(lm(hw.exp$percentChange~hw.exp$pchange.daphnia.zoop, na.action = "na.exclude"))

ggplot(hw.exp, aes(x = abschange.total.zoop, y = percentChange))+
  geom_point()+
  theme_classic()


ggplot(hw.exp, aes(y = pchange.total.zoop))+
  geom_boxplot()+
  theme_classic()
  


ggplot(hw.exp)



ggplot(hw.exp, aes(x = lake, y = pchange.total.zoop, fill = lake))+
  geom_boxplot()+
  theme_classic()+
  scale_fill_manual(values = c("R" = "#4AB5C4", "L" = "#ADDAE3", "T" = "#BAAD8D"))+
  geom_hline(yintercept = 0, linetype = "dashed")


### compare the percent change in chl to percent change in zooplankton





  # Daphnia biomass after the heatwave
  # Daphnia biomass during the heatwave
  
  # Holopedium biomass before the heatwave
  # Holopedium biomass after the heatwave
  # Holopedium biomass during the heatwave
  
  

#   
#   # either before or during heatwave
#   j = which(abs(cur.casc.exp.zoop$doy - targ.doy) == min(abs(cur.casc.exp.zoop$doy - targ.doy)))
#   if(nrow(cur.casc.exp.zoop) > 0){ hw.exp$biomass[i] = cur.casc.exp.zoop$biomass[j]}
#   
#   if(nrow(cur.casc.exp.zoop) > 0){
#     cur.casc.exp.zoop = cur.casc.exp %>% filter(date >= targ.date & date <= hw.exp$date_end[i])
#     biomass.during.hw = mean(cur.casc.exp.zoop$biomass, na.rm = TRUE)}
#   
#   if(nrow(cur.casc.exp.zoop) > 0){hw.exp$biomass.during[i] = biomass.during.hw}
#   
#   k = which(abs(cur.casc.exp.color$doy - targ.doy) == min(abs(cur.casc.exp.color$doy - targ.doy)))
#   if(nrow(cur.casc.exp.color) > 0){ hw.exp$PML.g440[i] = cur.casc.exp.color$PML.g440[k]}
#   
#   l = which(abs(cur.casc.exp.nut$doy - targ.doy) == min(abs(cur.casc.exp.nut$doy - targ.doy)))
#   if(nrow(cur.casc.exp.nut) > 0){ hw.exp$cumulative.load[i] = cur.casc.exp.nut$cumulative.load[l]
#   hw.exp$daily.load[i] = cur.casc.exp.nut$daily.load[l]
#   }
#   
# }
# 

















####### Daily Gravimetric zooplankton ########
# read in and format the daily zooplankton data

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


zoop08.11 = dt1

ggplot(data = zoop08.11 %>% filter(year != 2008), aes(x = DOY, y = ZBgrav, color = lake))+
  geom_point()+
  geom_line()+
  facet_wrap(~year)




# save the gravimetric zooplankton data
write.csv(zoop08.11, "./formatted data/zooplankton/gravimetric zooplankton 2024-08-05.csv", row.names = FALSE)




summary(lme(data = hw.exp %>% filter(!is.nan(pchange.daphnia.zoop)), percentChange ~ pchange.daphnia.zoop))

hw.exp = hw.exp %>% mutate(pchange.daphnia.zoop = replace(pchange.daphnia.zoop, is.nan(pchange.daphnia.zoop), NA)) %>% 
  mutate(pchange.daphnia.zoop = replace(pchange.daphnia.zoop, is.infinite(pchange.daphnia.zoop), NA))

test = lmer(percentChange~PML.g440*cumulative.load*rate_onset*doy*pchange.daphnia.zoop + (1|lake), data = hw.exp %>% filter(!is.nan(pchange.daphnia.zoop)))

test = lmer(percentChange~PML.g440*cumulative.load*rate_onset*doy + (1|lake), data = hw.exp)


test = lmer(percentChange~PML.g440*cumulative.load*pchange.daphnia.zoop + (1|lake), data = hw.exp %>% filter(!is.nan(pchange.daphnia.zoop)))

dredge(test, na.action = na.omit)


summary(test)

library(MuMIn)


r.squaredGLMM(test)







##### save the dataframe

write.csv(hw.exp, "./formatted data/master explanatory dataset/heatwaves explained var4.csv", row.names = FALSE)

