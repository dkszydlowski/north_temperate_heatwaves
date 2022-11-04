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

event_line(peterHW, metric = "intensity_max", start_date = "2019-06-01", end_date = "2019-09-15")+
  geom_point()

climOutputT = ts2clm(tuesdayHWinput, climatologyPeriod = c(min(tuesdayHWinput$t), max(tuesdayHWinput$t)))
tuesdayHW = detect_event(climOutputT)

event_line(tuesdayHW, metric = "intensity_max", start_date = "2019-06-01", end_date = "2019-09-15")+
  geom_point()


peterHW$event$lake = "R" 
paulHW$event$lake = "L"
heatwaves = rbind(peterHW$event, paulHW$event)


ggplot(data=sonde08_11_mean_L, aes(x=date, y=normChl)) + 
  geom_point() +
  geom_line(aes(x = date, y = normTemp), size = 1.5)+
  geom_density_line(aes(x = date, y = normTemp), stat = "identity", size = 0.5, fill = "steelblue3", alpha = 0.3)+
  geom_line(size = 1.5) +
  geom_density_line(stat = "identity", size = 1.5, fill = "forestgreen", alpha = 0.3)+
  theme_classic()+
  labs(title = paste(sonde08_11_mean_L$lake[1], "Lake", sonde08_11_mean_L$year[1]), y = "% of initial value")+
  theme(title = element_text(size = 20))+
  annotate("rect", xmin = as.Date("2010-05-24"), xmax = as.Date("2010-06-03"), ymin = 0, ymax = Inf,
           fill = "red", alpha = 0.3)+
  annotate("rect", xmin = as.Date("2010-08-09"), xmax = as.Date("2010-08-15"), ymin = 0, ymax = Inf,
           fill = "red", alpha = 0.3)+
  theme(text = element_text(size = 20))


#####New File - Calculate and Plot Heatwaves for Peter, Paul, and Tuesday Lakes#####

newallSonde = read.csv("NewCombinedData.csv")
newallSonde$Date = as.Date(newallSonde$Date)


paulHWinput = paul %>% select(t, temp)

peterHWinput = peter %>% select(t, temp)

tuesdayHWinput = tuesday %>% select(t, temp)


climOutputL = ts2clm(paulHWinput, climatologyPeriod = c(min(paulHWinput$t), max(paulHWinput$t)))
paulHW = detect_event(climOutputL)

event_line(paulHW, metric = "intensity_max", start_date = "2019-06-01", end_date = "2019-09-15")+
  geom_point()

climOutputR = ts2clm(peterHWinput, climatologyPeriod = c(min(peterHWinput$t), max(peterHWinput$t)))
peterHW = detect_event(climOutputR)

event_line(peterHW, metric = "intensity_max", start_date = "2019-06-01", end_date = "2019-09-15")+
  geom_point()

climOutputT = ts2clm(tuesdayHWinput, climatologyPeriod = c(min(tuesdayHWinput$t), max(tuesdayHWinput$t)))
tuesdayHW = detect_event(climOutputT)

event_line(tuesdayHW, metric = "intensity_max", start_date = "2019-06-01", end_date = "2019-09-15")+
  geom_point()


peterHW$event$lake = "R" 
paulHW$event$lake = "L"
heatwaves = rbind(peterHW$event, paulHW$event)


ggplot(data=sonde08_11_mean_L, aes(x=date, y=normChl)) + 
  geom_point() +
  geom_line(aes(x = date, y = normTemp), size = 1.5)+
  geom_density_line(aes(x = date, y = normTemp), stat = "identity", size = 0.5, fill = "steelblue3", alpha = 0.3)+
  geom_line(size = 1.5) +
  geom_density_line(stat = "identity", size = 1.5, fill = "forestgreen", alpha = 0.3)+
  theme_classic()+
  labs(title = paste(sonde08_11_mean_L$lake[1], "Lake", sonde08_11_mean_L$year[1]), y = "% of initial value")+
  theme(title = element_text(size = 20))+
  annotate("rect", xmin = as.Date("2010-05-24"), xmax = as.Date("2010-06-03"), ymin = 0, ymax = Inf,
           fill = "red", alpha = 0.3)+
  annotate("rect", xmin = as.Date("2010-08-09"), xmax = as.Date("2010-08-15"), ymin = 0, ymax = Inf,
           fill = "red", alpha = 0.3)+
  theme(text = element_text(size = 20))
