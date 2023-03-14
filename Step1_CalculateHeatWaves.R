#####Calculate and Plot Heatwaves for Peter, Paul, and Tuesday Lakes#####

library("tidyverse")
library("ggridges")
library("lubridate")
library("zoo")
library(heatwaveR)


allSonde = read.csv("CombinedData.csv")
allSonde$date = as.Date(allSonde$date)

paul = allSonde %>%
  filter(lake == "L") %>% 
  rename(t = date, temp = mean_temp)

peter = allSonde %>%
  filter(lake == "R") %>% 
  rename(t = date, temp = mean_temp)

tuesday = allSonde %>%
  filter(lake == "T") %>% 
  rename(t = date, temp = mean_temp)


paulHWinput = paul %>% select(t, temp)

peterHWinput = peter %>% select(t, temp)

tuesdayHWinput = tuesday %>% select(t, temp)


climOutputL = ts2clm(paulHWinput, climatologyPeriod = c(min(paulHWinput$t), max(paulHWinput$t)))
paulHW = detect_event(climOutputL)

event_line(paulHW, metric = "intensity_max", start_date = "2019-06-01", end_date = "2019-09-15")+
  geom_point()

climOutputR = ts2clm(peterHWinput, climatologyPeriod = c(min(peterHWinput$t), max(peterHWinput$t)))
peterHW = detect_event(climOutputR)

event_line(peterHW, metric = "intensity_max", start_date = "2018-06-01", end_date = "2018-09-15")+
  geom_point()+
  xlim(as.Date("2018-05-01"), as.Date("2018-09-01"))

# the Tuesday data throws an error because technically there aren't three years of data
# can we just add a dummy day to bypass that error? We have three summers of data

tuesdayHWinput[nrow(tuesdayHWinput)+1, ] = NA
tuesdayHWinput[nrow(tuesdayHWinput), 1] = as.Date("2016-06-01")


climOutputT = ts2clm(tuesdayHWinput, climatologyPeriod = c(min(tuesdayHWinput$t), max(tuesdayHWinput$t)))
tuesdayHW = detect_event(climOutputT)

event_line(tuesdayHW, metric = "intensity_max", start_date = "2013-06-01", end_date = "2013-09-15")+
  geom_point()+
  xlim(as.Date("2013-05-01"), as.Date("2013-09-01"))


peterHW$event$lake = "R" 
paulHW$event$lake = "L"
tuesdayHW$event$lake = "T"
heatwaves = rbind(peterHW$event, paulHW$event, tuesdayHW$event)

#####Test Plots#####
ggplot(data = allSonde, aes(x = date, y = mean_temp, color = lake)) + 
  geom_point()+
  geom_line()

year2010 = allSonde %>%
  filter(year == "2010") 
ggplot(data = year2010, aes(x = date, y = mean_temp, color = lake)) + 
  geom_point()+
  geom_line()


allSondeInterp = allSonde %>%
  group_by(lake, year) %>%
  mutate(mean_temp = na.approx(mean_temp, na.rm = FALSE), mean_chl = na.approx(mean_chl, na.rm = FALSE)) %>%
  mutate(normTemp = 100 * mean_temp/mean_temp[1], normChl = 100 * mean_chl/mean_chl[1])

heatwaves = heatwaves %>%
  mutate(year = year(date_peak)) 

heatwaves = heatwaves %>%
  mutate(lake_year = paste(lake, year, sep = "_")) 

#####Plot Heatwaves - Peter 2009#####
Peter2009 = allSondeInterp %>%
  filter(lake == "R", year == "2009")

ggplot(data=Peter2009, aes(x=date, y=normChl)) + 
  geom_point() +
  geom_line(aes(x = date, y = normTemp), size = 1.5)+
  geom_density_line(aes(x = date, y = normTemp), stat = "identity", size = 0.5, fill = "steelblue3", alpha = 0.3)+
  geom_line(size = 1.5) +
  geom_density_line(stat = "identity", size = 1.5, fill = "forestgreen", alpha = 0.3)+
  theme_classic()+
  labs(title = paste(Peter2009$lake[1], "Lake", Peter2009$year[1]), y = "% of initial value")+
  theme(title = element_text(size = 20))+
  annotate("rect", xmin = as.Date("2009-06-20"), xmax = as.Date("2009-06-27"), ymin = 0, ymax = Inf,
           fill = "red", alpha = 0.3)+
  theme(text = element_text(size = 20))

#####Plot Heatwaves - Peter 2010#####
Peter2010 = allSondeInterp %>%
  filter(lake == "R", year == "2010")

ggplot(data=Peter2010, aes(x=date, y=normChl)) + 
  geom_point() +
  geom_line(aes(x = date, y = normTemp), size = 1.5)+
  geom_density_line(aes(x = date, y = normTemp), stat = "identity", size = 0.5, fill = "steelblue3", alpha = 0.3)+
  geom_line(size = 1.5) +
  geom_density_line(stat = "identity", size = 1.5, fill = "forestgreen", alpha = 0.3)+
  theme_classic()+
  labs(title = paste(Peter2010$lake[1], "Lake", Peter2010$year[1]), y = "% of initial value")+
  theme(title = element_text(size = 20))+
  annotate("rect", xmin = as.Date("2010-05-24"), xmax = as.Date("2010-06-03"), ymin = 0, ymax = Inf,
           fill = "red", alpha = 0.3)+
  annotate("rect", xmin = as.Date("2010-08-09"), xmax = as.Date("2010-08-15"), ymin = 0, ymax = Inf,
           fill = "red", alpha = 0.3)+
  theme(text = element_text(size = 20))

#####Plot Heatwaves - Peter 2011#####
Peter2011 = allSondeInterp %>%
  filter(lake == "R", year == "2011")

ggplot(data=Peter2011, aes(x=date, y=normChl)) + 
  geom_point() +
  geom_line(aes(x = date, y = normTemp), size = 1.5)+
  geom_density_line(aes(x = date, y = normTemp), stat = "identity", size = 0.5, fill = "steelblue3", alpha = 0.3)+
  geom_line(size = 1.5) +
  geom_density_line(stat = "identity", size = 1.5, fill = "forestgreen", alpha = 0.3)+
  theme_classic()+
  labs(title = paste(Peter2011$lake[1], "Lake", Peter2011$year[1]), y = "% of initial value")+
  theme(title = element_text(size = 20))+
  annotate("rect", xmin = as.Date("2011-07-18"), xmax = as.Date("2011-07-24"), ymin = 0, ymax = Inf,
           fill = "red", alpha = 0.3)+
  theme(text = element_text(size = 20))
    
#####Plot Heatwaves - Peter 2013#####
Peter2013 = allSondeInterp %>%
  filter(lake == "R", year == "2013")

ggplot(data=Peter2013, aes(x=date, y=normChl)) + 
  geom_point() +
  geom_line(aes(x = date, y = normTemp), size = 1.5)+
  geom_density_line(aes(x = date, y = normTemp), stat = "identity", size = 0.5, fill = "steelblue3", alpha = 0.3)+
  geom_line(size = 1.5) +
  geom_density_line(stat = "identity", size = 1.5, fill = "forestgreen", alpha = 0.3)+
  theme_classic()+
  labs(title = paste(Peter2013$lake[1], "Lake", Peter2013$year[1]), y = "% of initial value")+
  theme(title = element_text(size = 20))+
  annotate("rect", xmin = as.Date("2013-07-15"), xmax = as.Date("2013-07-20"), ymin = 0, ymax = Inf,
           fill = "red", alpha = 0.3)+
  theme(text = element_text(size = 20))

#####Plot Heatwaves - Peter 2014####
Peter2014 = allSondeInterp %>%
  filter(lake == "R", year == "2014")

ggplot(data=Peter2014, aes(x=date, y=normChl)) + 
  geom_point() +
  geom_line(aes(x = date, y = normTemp), size = 1.5)+
  geom_density_line(aes(x = date, y = normTemp), stat = "identity", size = 0.5, fill = "steelblue3", alpha = 0.3)+
  geom_line(size = 1.5) +
  geom_density_line(stat = "identity", size = 1.5, fill = "forestgreen", alpha = 0.3)+
  theme_classic()+
  labs(title = paste(Peter2014$lake[1], "Lake", Peter2014$year[1]), y = "% of initial value")+
  theme(title = element_text(size = 20))+
  annotate("rect", xmin = as.Date("2014-05-29"), xmax = as.Date("2014-06-02"), ymin = 0, ymax = Inf,
           fill = "red", alpha = 0.3)+
  theme(text = element_text(size = 20))

#####Plot Heatwaves - Peter 2018#####
Peter2018 = allSondeInterp %>%
  filter(lake == "R", year == "2018")

ggplot(data=Peter2018, aes(x=date, y=normChl)) + 
  geom_point() +
  geom_line(aes(x = date, y = normTemp), size = 1.5)+
  geom_density_line(aes(x = date, y = normTemp), stat = "identity", size = 0.5, fill = "steelblue3", alpha = 0.3)+
  geom_line(size = 1.5) +
  geom_density_line(stat = "identity", size = 1.5, fill = "forestgreen", alpha = 0.3)+
  theme_classic()+
  labs(title = paste(Peter2018$lake[1], "Lake", Peter2018$year[1]), y = "% of initial value")+
  theme(title = element_text(size = 20))+
  annotate("rect", xmin = as.Date("2018-05-24"), xmax = as.Date("2018-06-01"), ymin = 0, ymax = Inf,
           fill = "red", alpha = 0.3)+
  annotate("rect", xmin = as.Date("2018-06-18"), xmax = as.Date("2018-06-24"), ymin = 0, ymax = Inf,
           fill = "red", alpha = 0.3)+
  annotate("rect", xmin = as.Date("2018-06-29"), xmax = as.Date("2018-07-11"), ymin = 0, ymax = Inf,
           fill = "red", alpha = 0.3)+
  annotate("rect", xmin = as.Date("2018-08-11"), xmax = as.Date("2018-08-20"), ymin = 0, ymax = Inf,
           fill = "red", alpha = 0.3)+
  theme(text = element_text(size = 20))

#####Plot Heatwaves - Peter 2019#####
Peter2019 = allSondeInterp %>%
  filter(lake == "R", year == "2019")

ggplot(data=Peter2019, aes(x=date, y=normChl)) + 
  geom_point() +
  geom_line(aes(x = date, y = normTemp), size = 1.5)+
  geom_density_line(aes(x = date, y = normTemp), stat = "identity", size = 0.5, fill = "steelblue3", alpha = 0.3)+
  geom_line(size = 1.5) +
  geom_density_line(stat = "identity", size = 1.5, fill = "forestgreen", alpha = 0.3)+
  theme_classic()+
  labs(title = paste(Peter2019$lake[1], "Lake", Peter2019$year[1]), y = "% of initial value")+
  theme(title = element_text(size = 20))+
  annotate("rect", xmin = as.Date("2019-08-03"), xmax = as.Date("2019-08-07"), ymin = 0, ymax = Inf,
           fill = "red", alpha = 0.3)+
  theme(text = element_text(size = 20))

#####Plot Heatwaves - Paul 2009#####
Paul2009 = allSondeInterp %>%
  filter(lake == "L", year == "2009")

ggplot(data=Paul2009, aes(x=date, y=normChl)) + 
  geom_point() +
  geom_line(aes(x = date, y = normTemp), size = 1.5)+
  geom_density_line(aes(x = date, y = normTemp), stat = "identity", size = 0.5, fill = "steelblue3", alpha = 0.3)+
  geom_line(size = 1.5) +
  geom_density_line(stat = "identity", size = 1.5, fill = "forestgreen", alpha = 0.3)+
  theme_classic()+
  labs(title = paste(Paul2009$lake[1], "Lake", Paul2009$year[1]), y = "% of initial value")+
  theme(title = element_text(size = 20))+
  annotate("rect", xmin = as.Date("2009-06-20"), xmax = as.Date("2009-06-27"), ymin = 0, ymax = Inf,
           fill = "red", alpha = 0.3)+
  theme(text = element_text(size = 20))

#####Plot Heatwaves - Paul 2010#####
Paul2010 = allSondeInterp %>%
  filter(lake == "L", year == "2010")

ggplot(data=Paul2010, aes(x=date, y=normChl)) + 
  geom_point() +
  geom_line(aes(x = date, y = normTemp), size = 1.5)+
  geom_density_line(aes(x = date, y = normTemp), stat = "identity", size = 0.5, fill = "steelblue3", alpha = 0.3)+
  geom_line(size = 1.5) +
  geom_density_line(stat = "identity", size = 1.5, fill = "forestgreen", alpha = 0.3)+
  theme_classic()+
  labs(title = paste(Paul2010$lake[1], "Lake", Paul2010$year[1]), y = "% of initial value")+
  theme(title = element_text(size = 20))+
  annotate("rect", xmin = as.Date("2010-05-24"), xmax = as.Date("2010-06-03"), ymin = 0, ymax = Inf,
           fill = "red", alpha = 0.3)+
  annotate("rect", xmin = as.Date("2010-08-09"), xmax = as.Date("2010-08-15"), ymin = 0, ymax = Inf,
           fill = "red", alpha = 0.3)+
  theme(text = element_text(size = 20))

#####Plot Heatwaves - Paul 2011#####
Paul2011 = allSondeInterp %>%
  filter(lake == "L", year == "2011")

ggplot(data=Paul2011, aes(x=date, y=normChl)) + 
  geom_point() +
  geom_line(aes(x = date, y = normTemp), size = 1.5)+
  geom_density_line(aes(x = date, y = normTemp), stat = "identity", size = 0.5, fill = "steelblue3", alpha = 0.3)+
  geom_line(size = 1.5) +
  geom_density_line(stat = "identity", size = 1.5, fill = "forestgreen", alpha = 0.3)+
  theme_classic()+
  labs(title = paste(Paul2011$lake[1], "Lake", Paul2011$year[1]), y = "% of initial value")+
  theme(title = element_text(size = 20))+
  annotate("rect", xmin = as.Date("2011-07-18"), xmax = as.Date("2011-07-24"), ymin = 0, ymax = Inf,
           fill = "red", alpha = 0.3)+
  theme(text = element_text(size = 20))

#####Plot Heatwaves - Paul 2013#####
Paul2013 = allSondeInterp %>%
  filter(lake == "L", year == "2013")

ggplot(data=Paul2013, aes(x=date, y=normChl)) + 
  geom_point() +
  geom_line(aes(x = date, y = normTemp), size = 1.5)+
  geom_density_line(aes(x = date, y = normTemp), stat = "identity", size = 0.5, fill = "steelblue3", alpha = 0.3)+
  geom_line(size = 1.5) +
  geom_density_line(stat = "identity", size = 1.5, fill = "forestgreen", alpha = 0.3)+
  theme_classic()+
  labs(title = paste(Paul2013$lake[1], "Lake", Paul2013$year[1]), y = "% of initial value")+
  theme(title = element_text(size = 20))+
  annotate("rect", xmin = as.Date("2013-07-15"), xmax = as.Date("2013-07-20"), ymin = 0, ymax = Inf,
           fill = "red", alpha = 0.3)+
  theme(text = element_text(size = 20))

#####Plot Heatwaves - Paul 2018#####
Paul2018 = allSondeInterp %>%
  filter(lake == "L", year == "2018")

ggplot(data=Paul2018, aes(x=date, y=normChl)) + 
  geom_point() +
  geom_line(aes(x = date, y = normTemp), size = 1.5)+
  geom_density_line(aes(x = date, y = normTemp), stat = "identity", size = 0.5, fill = "steelblue3", alpha = 0.3)+
  geom_line(size = 1.5) +
  geom_density_line(stat = "identity", size = 1.5, fill = "forestgreen", alpha = 0.3)+
  theme_classic()+
  labs(title = paste(Paul2018$lake[1], "Lake", Paul2018$year[1]), y = "% of initial value")+
  theme(title = element_text(size = 20))+
  annotate("rect", xmin = as.Date("2018-05-24"), xmax = as.Date("2018-06-01"), ymin = 0, ymax = Inf,
           fill = "red", alpha = 0.3)+
  annotate("rect", xmin = as.Date("2018-06-18"), xmax = as.Date("2018-06-24"), ymin = 0, ymax = Inf,
           fill = "red", alpha = 0.3)+
  annotate("rect", xmin = as.Date("2018-06-29"), xmax = as.Date("2018-07-07"), ymin = 0, ymax = Inf,
          fill = "red", alpha = 0.3)+
  annotate("rect", xmin = as.Date("2018-08-11"), xmax = as.Date("2018-08-19"), ymin = 0, ymax = Inf,
           fill = "red", alpha = 0.3)+
  theme(text = element_text(size = 20))

#####Plot Heatwaves - Paul 2019#####
Paul2019 = allSondeInterp %>%
  filter(lake == "L", year == "2019")

ggplot(data=Paul2019, aes(x=date, y=normChl)) + 
  geom_point() +
  geom_line(aes(x = date, y = normTemp), size = 1.5)+
  geom_density_line(aes(x = date, y = normTemp), stat = "identity", size = 0.5, fill = "steelblue3", alpha = 0.3)+
  geom_line(size = 1.5) +
  geom_density_line(stat = "identity", size = 1.5, fill = "forestgreen", alpha = 0.3)+
  theme_classic()+
  labs(title = paste(Paul2019$lake[1], "Lake", Paul2019$year[1]), y = "% of initial value")+
  theme(title = element_text(size = 20))+
  annotate("rect", xmin = as.Date("2019-07-15"), xmax = as.Date("2019-07-19"), ymin = 0, ymax = Inf,
           fill = "red", alpha = 0.3)+
  annotate("rect", xmin = as.Date("2019-06-15"), xmax = as.Date("2019-06-23"), ymin = 0, ymax = Inf,
           fill = "blue", alpha = 0.3)+
  theme(text = element_text(size = 20))
 


write.csv(heatwaves, "heatwavesdata.csv", row.names = FALSE)

