### compare lake stability to heatwaves

# read in stability data, which I am using from my profiles project

library(tidyverse)
library(rLakeAnalyzer)

strat = read.csv("./formatted data/stability and stratification/stability results all combinations.csv")

# read in the sonde data to get the lake_years
allData = read.csv("./formatted data/interpolated_manual_chl_for_slopes.csv")

lake_years = unique(allData$lake_year)

# remove 2008, 2009, 2010, and 2011 because we don't have tchain data for those years
lake_years = lake_years[-which(grepl("2009", lake_years))]
lake_years = lake_years[-which(grepl("2010", lake_years))]
lake_years = lake_years[-which(grepl("2011", lake_years))]
lake_years = lake_years[-which(grepl("2008", lake_years))]
lake_years = lake_years[-which(grepl("L_2019", lake_years))]



# load in bathymetric data for the three lakes
bathy = load.bathy("./formatted data/bathymetric data/Tuesday_bathy_formatted_for_rLakeAnalyzer_underc_data.csv")
bathy = bathy %>% filter(!is.na(depths))

bathyR = load.bathy("./formatted data/bathymetric data/Peter_bathy_formatted_for_rLakeAnalyzer_underc_data.csv")
bathyR = bathyR %>% filter(!is.na(depths))

bathyL = load.bathy("./formatted data/bathymetric data/Paul_bathy_formatted_for_rLakeAnalyzer_underc_data.csv")
bathyL = bathyL %>% filter(!is.na(depths))

bathyT = load.bathy("./formatted data/bathymetric data/Tuesday_bathy_formatted_for_rLakeAnalyzer_underc_data.csv")
bathyT = bathyT %>% filter(!is.na(depths))

i = 1
for(i in 1:length(lake_years)){
  
  # get current iteration's lake and year
  cur.lake = strsplit(lake_years[i], "_")[[1]][1]
  cur.year = strsplit(lake_years[i], "_")[[1]][2]
  
  if(cur.lake == "L"){
    lake = "Paul"
    bathy = bathyL
  }
  if(cur.lake == "R"){
    
    lake = "Peter"
    bathy = bathyR
  }
  
  if(cur.lake == "T"){
    lake = "Tuesday"
    bathy = bathyT
    
  }
  
  wtr = load.ts(paste0("./formatted data/LRT temp chains rLakeAnalyzer/", lake, "_temperature_", cur.year, "_rLakeAnalyzer.csv" ))
  
  cur.schmidt =  ts.schmidt.stability(wtr, bathy)
  cur.schmidt$lake = lake
  cur.schmidt$year = cur.year
  
  if(i == 1){all.schmidt = cur.schmidt}
  if(i > 1){all.schmidt = rbind(all.schmidt, cur.schmidt)}
  
}

# add in the rest of the Tuesday 2015 data
# wtr.T = load.ts()


# make a daily stability dataset
schmidt.daily = all.schmidt %>% mutate(doy = yday(datetime)) %>% group_by(lake, year, doy) %>% 
  summarize(schmidt.stability = mean(schmidt.stability, na.rm = TRUE))


ggplot(schmidt.daily, aes(x = doy, y = schmidt.stability, color = lake))+
  facet_wrap(~year)+
  geom_point()+
  geom_line()+
  theme_bw()+
  labs(x = "day of year", y = "Schmidt stability (J/m2)")+
  scale_color_manual(values = c("Peter" = "#4AB5C4", "Paul" = "#ADDAE3", "Tuesday" = "#BAAD8D")) 
  


## compare to heatwave response



## function which combines heatwaves output with the explanatory variables ##
#heatwaves.exp = read.csv("./formatted data/explanatory variables heatwaves/heatwaves with percent.csv")

heatwaves.exp = read.csv("./formatted data/master explanatory dataset/heatwaves explained var2.csv")


heatwaves.exp = heatwaves.exp %>% mutate(doy = yday(date_start))

#heatwaves.exp = heatwaves.exp %>% group_by(lake, year) %>% join_by(casc.color.nut.zoops, date_start > date)


# create new columns of heatwaves.exp for PML.g440, biomass, cumulative.load, daily.load
heatwaves.exp = heatwaves.exp %>% mutate(stability.before = NA, stability.during = NA, stability.after = NA)

schmidt.daily = schmidt.daily %>% 
  mutate(lake = replace(lake, lake == "Peter", "R")) %>% 
  mutate(lake = replace(lake, lake == "Paul", "L")) %>% 
  mutate(lake = replace(lake, lake == "Tuesday", "T"))

i = 7

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
  schmidt.daily.cur = schmidt.daily %>% filter(lake == targ.lake, year == targ.year)
  
  
  schmidt.before = schmidt.daily.cur %>% filter(doy >= start.doy - 6 & doy <= start.doy)
  schmidt.during = schmidt.daily.cur %>% filter(doy >= start.doy & doy <= end.doy)
  schmidt.after = schmidt.daily.cur %>% filter(doy > end.doy & doy <= end.doy + 6)

  
  if(nrow(schmidt.during) > 0){
    schmidt.during.hw = mean(schmidt.during$schmidt.stability, na.rm = TRUE)
    heatwaves.exp$stability.during[i] = schmidt.during.hw
  }
  
  if(nrow(schmidt.after) > 0){
    schmidt.after.hw = mean(schmidt.after$schmidt.stability, na.rm = TRUE)
    heatwaves.exp$stability.after[i] = schmidt.after.hw
    
  }
  
  if(nrow(schmidt.before) > 0){
    schmidt.before.hw = mean(schmidt.before$schmidt.stability, na.rm = TRUE)
    heatwaves.exp$stability.before[i] = schmidt.before.hw
  }
  

  
  
}


stab.before =ggplot(heatwaves.exp %>%  filter(!is.na(stability.before)), aes(x = as.numeric(stability.before), y = percentChange, fill = lake))+
  geom_point(size = 5, color = "black", shape = 21, stroke = 1, alpha = 0.9)+
  labs(x = "Schmidt stability before HW (J/m2)", y = "percent change in chl")+
  theme_classic()+
  scale_fill_manual(values = c("R" = "#4AB5C4", "L" = "#ADDAE3", "T" = "#BAAD8D"))+
  ylim(0, 250)

stab.during = ggplot(heatwaves.exp %>%  filter(!is.na(stability.during)), aes(x = as.numeric(stability.during), y = percentChange, fill = lake))+
  geom_point(size = 5, color = "black", shape = 21, stroke = 1, alpha = 0.9)+
  labs(x = "Schmidt stability during HW (J/m2)", y = "percent change in chl")+
  theme_classic()+
  scale_fill_manual(values = c("R" = "#4AB5C4", "L" = "#ADDAE3", "T" = "#BAAD8D"))+
  ylim(0, 250)

stab.after = ggplot(heatwaves.exp %>%  filter(!is.na(stability.after)), aes(x = as.numeric(stability.after), y = percentChange, fill = lake))+
  geom_point(size = 5, color = "black", shape = 21, stroke = 1, alpha = 0.9)+
  labs(x = "Schmidt stability after HW (J/m2)", y = "percent change in chl")+
  theme_classic()+
  scale_fill_manual(values = c("R" = "#4AB5C4", "L" = "#ADDAE3", "T" = "#BAAD8D"))+
  ylim(0, 250)

ggarrange(stab.before, stab.during, stab.after, nrow = 1, ncol = 3, common.legend = TRUE)


stab.forbox = heatwaves.exp %>% select(date_start, lake, stability.before, stability.after, stability.during)

stab.forbox = stab.forbox %>% pivot_longer(cols = c("stability.before", "stability.after", "stability.during"),
                                          names_to = "time", values_to = "stability")


stab.forbox = stab.forbox %>% filter(!is.na(stability))


ggplot(stab.forbox, aes(x = time, y = as.numeric(stability), fill = lake))+
  geom_boxplot()+
  facet_wrap(~lake)+
  theme_classic()+
  scale_fill_manual(values = c("R" = "#4AB5C4", "L" = "#ADDAE3", "T" = "#BAAD8D"))+
  labs(y = "schmidt stability (J/m2)", x = "timeframe")



### save the dataframe with stability
write.csv(heatwaves.exp, "./formatted data/master explanatory dataset/heatwaves explained var3.csv", row.names = FALSE)

