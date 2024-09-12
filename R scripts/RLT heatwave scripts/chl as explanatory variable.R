#' calculate the average chlorophyll the week before the heatwave
#' to determine if it explains variation in phytoplankton response
#' 
#' 

library(tidyverse)

# read in the chl data
chl.man = read.csv("./formatted data/interpolated_manual_chl_for_slopes.csv")

# read in the heatwaves
paulHW = readRDS(file = "./results/heatwave modeled outputs/paul heatwave outputs modeled categories.rds")
peterHW = readRDS(file = "./results/heatwave modeled outputs/peter heatwave outputs modeled categories.rds")
tuesdayHW = readRDS(file = "./results/heatwave modeled outputs/tuesday heatwave outputs modeled categories.rds")

heatwave.char = read.csv("./formatted data/explanatory variables heatwaves/heatwaves with percent.csv")


heatwave.char$chl_avg_wk = NA
heatwave.char$chl_sd_wk = NA


i = 1
for(i in 1:nrow(heatwave.char)){
  
  cur.chl = chl.man %>% filter(lake_year == heatwave.char$lake_year[i])
  
  cur.hw.date = heatwave.char$date_start[i]
  year.hw = year(cur.hw.date)
  doy = yday(cur.hw.date)
  
  cur.chl = cur.chl %>% filter(year == year.hw & (doyCat <= doy & doyCat >= doy -6))
  print(mean(cur.chl$manual_chl))
  heatwave.char$chl_avg_wk[i] = mean(cur.chl$manual_chl)
  heatwave.char$chl_sd_wk[i] = sd(cur.chl$manual_chl)
  
  
}



ggplot(heatwave.char, aes(x = log10(chl_sd_wk/chl_avg_wk), y = percentChange, color = lake))+
  geom_point(size = 3)+
  labs(x = "Chlorophyll the week before the heatwave (ug/L)", y = "percent change in chl")+
  scale_color_manual(values = c("R" = "#4AB5C4", "L" = "#ADDAE3", "T" = "#BAAD8D"))+
  theme_bw()
