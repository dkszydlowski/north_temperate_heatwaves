# R script for calculating and visualizing heatwaves in the LTER lakes

library(tidyverse)
library(heatwaveR)

# read in the data
SP.surface.HWR = read.csv("./formatted data/LTER daily temperature/SP surface for heatwaveR.csv") %>% mutate(t = as.Date(t))
CB.surface.HWR = read.csv("./formatted data/LTER daily temperature/CB surface for heatwaveR.csv") %>% mutate(t = as.Date(t))
TB.surface.HWR = read.csv("./formatted data/LTER daily temperature/TB surface for heatwaveR.csv") %>% mutate(t = as.Date(t))
TR.surface.HWR = read.csv("./formatted data/LTER daily temperature/TR surface for heatwaveR.csv") %>% mutate(t = as.Date(t))

# calculate climatology and heatwaves for all of the lakes
climOutputSP = ts2clm(SP.surface.HWR, climatologyPeriod = c(min(SP.surface.HWR$t), max(SP.surface.HWR$t)))
SPHW = detect_event(climOutputSP)

climOutputTR = ts2clm(TR.surface.HWR, climatologyPeriod = c(min(TR.surface.HWR$t), max(TR.surface.HWR$t)))
TRHW = detect_event(climOutputTR)

climOutputTB = ts2clm(TB.surface.HWR, climatologyPeriod = c(min(TB.surface.HWR$t), max(TB.surface.HWR$t)))
TBHW = detect_event(climOutputTB)

climOutputCB = ts2clm(CB.surface.HWR, climatologyPeriod = c(min(CB.surface.HWR$t), max(CB.surface.HWR$t)))
CBHW = detect_event(climOutputCB)

# summarize how many heatwaves per year
sparklingHW.perYear = sparklingHW %>% group_by(year) %>% 
  summarize(num.hw = n())

ggplot(sparklingHW.perYear, aes(x = year, y = num.hw))+
  geom_point()

climOutputSP = ts2clm(SP.surface.HWR, climatologyPeriod = c(min(SP.surface.HWR$t), max(SP.surface.HWR$t)))
SPHW = detect_event(climOutputSP)

SPHW = SPHW$event

SPHW = SPHW %>% mutate(year = year(date_start))




#### Combine the heatwaves dataframes #####
TRHW = TRHW %>% mutate(lake = "TR") %>% mutate(year = year(date_start))
SPHW = SPHW %>% mutate(lake = "SP")
TBHW = TBHW %>% mutate(lake = "TB")
CBHW = CBHW %>% mutate(lake = "CB")
HW.all = rbind(TRHW, SPHW, TBHW, CBHW)


# combine the temperature dataframes
SP.surface = SP.surface %>% mutate(lake = "SP")
TR.surface = TR.surface %>% mutate(lake = "TR")
TB.surface = TB.surface %>% mutate(lake = "TB")
CB.surface = CB.surface %>% mutate(lake = "CB")
all.surface = rbind(SP.surface, TR.surface, TB.surface, CB.surface)



years = sort(unique(HW.all$year))
HW.all = HW.all %>% mutate(doy = yday(date_start))

all.surface = all.surface %>% mutate(sampledate = as.Date(sampledate))

for(i in 1:length(years)){
  
  cur.year = years[i]
  
  current = HW.all %>% filter(year == cur.year, doy > 145 & doy < 260)
  temp.current = all.surface %>% filter(year == cur.year)
  
  # restrict the heatwave and temp dates so we are just investigating summer
  temp.current = temp.current %>% filter(doy > 145 & doy < 260)
  
  
  print(ggplot(data = temp.current, aes(x = sampledate, y = wtemp, color = lake)) +
          geom_line(size = 0.9, alpha = 0.7) +
          theme_classic() +
          scale_color_manual(values = c("TR" = "#AA4499", "SP" = "#117733", "TB" = "#88CCEE", CB = "#DDCC77")) +
          labs(title = cur.year) +
          geom_rect(data = current, inherit.aes = FALSE, aes(xmin = date_start, xmax = date_end, ymin = 0, ymax = Inf, fill = lake), alpha = 0.5) +
          scale_fill_manual(values = c("TR" = "#AA4499", "SP" = "#117733", "TB" = "#88CCEE", CB = "#DDCC77")) +
          guides(fill = FALSE))
  
  
}


ggplot(all.surface, aes(x = doy, y = wtemp, color = lake))+
  geom_line(size = 0.8)+
  facet_wrap(~year)+
  theme_bw()





# get a summary of the sampling depths available by year
TB.temp$lake = "TB"
SP.temp$lake = "SP"
TR.temp$lake = "TR"
CB.temp$lake = "CB"

all.temp.all.depths = rbind(TR.temp, SP.temp, TB.temp, CB.temp)

sampling.depths = all.temp.all.depths %>% group_by(year4, lake) %>% 
  reframe(depths = unique(depth))



#### average water temperature by the top 1 m of the water column for each lake ####
all.temp.all.depths = all.temp.all.depths %>% filter(depth <= 1)

avg.temp = all.temp.all.depths %>% group_by(lake, year4, sampledate, daynum) %>% 
  summarize(wtemp = mean(wtemp, na.rm = TRUE)) %>% 
  rename(doy = daynum, year = year4)

ggplot(avg.temp, aes(x = doy, y = wtemp, color = lake))+
  geom_point(size = 0.8)+
  facet_wrap(~year)+
  theme_bw()

avg.temp = avg.temp %>% filter(year >= 2005)

CB.avg.temp = avg.temp %>% filter(lake == "CB") %>% 
  rename(t = sampledate, temp = wtemp) %>% 
  select(t, temp)

SP.avg.temp = avg.temp %>% filter(lake == "SP")  %>% 
  rename(t = sampledate, temp = wtemp) %>% 
  select(t, temp)

TR.avg.temp = avg.temp %>% filter(lake == "TR")  %>% 
  rename(t = sampledate, temp = wtemp) %>% 
  select(t, temp)

TB.avg.temp = avg.temp %>% filter(lake == "TB")  %>% 
  rename(t = sampledate, temp = wtemp) %>% 
  select(t, temp)



climOutputSP = ts2clm(SP.avg.temp, climatologyPeriod = c(min(SP.avg.temp$t), max(SP.avg.temp$t)))
SPHW = detect_event(climOutputSP)$event %>% mutate(lake = "SP")

climOutputTR = ts2clm(TR.avg.temp, climatologyPeriod = c(min(TR.avg.temp$t), max(TR.avg.temp$t)))
TRHW = detect_event(climOutputTR)$event  %>% mutate(lake = "TR")
TRHW2 = detect_event(climOutputTR)
event_line(TRHW2, spread = 100, start_date = "2012-06-01", end_date = "2012-08-30", category = TRUE)

climOutputTB = ts2clm(TB.avg.temp, climatologyPeriod = c(min(TB.avg.temp$t), max(TB.avg.temp$t)))
TBHW = detect_event(climOutputTB)$event  %>% mutate(lake = "TB")
TBHW2 = detect_event(climOutputTB)
event_line(TBHW2, spread = 100, start_date = "2012-06-01", end_date = "2012-08-30", category = TRUE)

climOutputCB = ts2clm(CB.avg.temp, climatologyPeriod = c(min(CB.avg.temp$t), max(CB.avg.temp$t)))
CBHW = detect_event(climOutputCB)$event  %>% mutate(lake = "CB")



all.avg.hw = rbind(SPHW, TRHW, TBHW, CBHW)
all.avg.hw = all.avg.hw %>% mutate(doy = yday(date_start)) 

all.avg.hw = all.avg.hw %>% filter(doy > 145 & doy < 260)



all.avg.hw = all.avg.hw %>% mutate(year = year(date_start))
years = sort(unique(all.avg.hw$year))
all.avg.hw = all.avg.hw %>% mutate(doy = yday(date_start))

all.surface = all.surface %>% mutate(sampledate = as.Date(sampledate))

all.avg.hw = all.avg.hw %>% mutate(bar.height = case_when())

for(i in 1:length(years)){
  
  cur.year = years[i]
  
  current = all.avg.hw %>% filter(year == cur.year, doy > 145 & doy < 260)
  temp.current = avg.temp %>% filter(year == cur.year)
  
  # restrict the heatwave and temp dates so we are just investigating summer
  temp.current = temp.current %>% filter(doy > 145 & doy < 260)
  
  
  print(ggplot(data = temp.current, aes(x = sampledate, y = wtemp, color = lake)) +
          geom_rect(data = current, inherit.aes = FALSE, aes(xmin = date_start, xmax = date_end, ymin = 0, ymax = Inf, fill = lake), alpha = 0.5) +
          scale_fill_manual(values = c("TR" = "#AA4499", "SP" = "#117733", "TB" = "#88CCEE", CB = "#DDCC77")) +
          geom_line(size = 0.9) +
          theme_classic() +
          scale_color_manual(values = c("TR" = "#AA4499", "SP" = "#117733", "TB" = "#88CCEE", CB = "#DDCC77")) +
          labs(title = cur.year) +
          guides(fill = FALSE))
  
  
}





### make geom_flame plots #####


