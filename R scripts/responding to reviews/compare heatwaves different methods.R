### compare heatwaves from different modeling approaches

library(heatwaveR)
library(dplyr)
library(ggpubr)
library(tidyverse)

heatwaves.exp = read.csv("./formatted data/master explanatory dataset/heatwaves explained var6.csv")

# filter so we only have heatwaves where percent change could be calculated
heatwaves.exp = heatwaves.exp %>% filter(!is.na(percentChange))

hw.real = read.csv("./results/heatwave modeled outputs/heatwaves with actual temp data.csv")

hw.real$overlaps = NA


i = 1
j = 1


for(i in 1:nrow(hw.real)){
  
  start.real = as.Date(hw.real$date_start[i])
  end.real = as.Date(hw.real$date_end[i])
  lake.real = hw.real$lake[i]
  
  dates.real = seq(start.real, end.real, by = "day")
  
  cur.hw.exp = heatwaves.exp %>% filter(lake == lake.real)
  
  for(j in 1:nrow(cur.hw.exp)){
    
  start.model = as.Date(cur.hw.exp$date_start[j])
  end.model = as.Date(cur.hw.exp$date_end[j])
  
  dates.model = seq(start.model, end.model, by = "day")
  
  if(any(dates.real %in% dates.model)){
    print("success!")

    hw.real$overlaps[i] = cur.hw.exp$event_no[j]
    
    break
  }
    
  }
  
}




hw.individual.models$overlaps = NA

for(i in 1:nrow(hw.individual.models)){
  
  start.real = as.Date(hw.individual.models$date_start[i])
  end.real = as.Date(hw.individual.models$date_end[i])
  lake.real = hw.individual.models$lake[i]
  
  dates.real = seq(start.real, end.real, by = "day")
  
  cur.hw.exp = heatwaves.exp %>% filter(lake == lake.real)
  
  for(j in 1:nrow(cur.hw.exp)){
    
    start.model = as.Date(cur.hw.exp$date_start[j])
    end.model = as.Date(cur.hw.exp$date_end[j])
    
    dates.model = seq(start.model, end.model, by = "day")
    
    if(any(dates.real %in% dates.model)){
      print("success!")
      
      hw.individual.models$overlaps[i] = cur.hw.exp$event_no[j]
      
      break
    }
    
  }
  
}




test = check_heatwave_matches(hw.real, heatwaves.exp)
test2 = check_heatwave_inclusion(heatwaves.exp, hw.real)
