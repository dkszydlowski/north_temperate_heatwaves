#####Calculate and Plot Heat Waves for Peter, Paul, and Tuesday Lakes#####

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

heatwaves = heatwaves %>% 
  mutate(year = year(date_peak))


# need to interpolate the NA values for temperature and chlorophyll
allSondeInterp = allSonde %>% 
  mutate(lake_year = paste(lake, year, sep = "_")) %>% 
  group_by(lake_year) %>% 
  mutate(mean_chl = na.approx(mean_chl, na.rm = FALSE), 
         mean_temp = na.approx(mean_temp, na.rm = FALSE))



# make a function which will take the sonde data, heatwaves, and produce the
# plot used in the proposal

plot_heatwaves = function(targLake, targYear, sonde, hw){
  
  temp = sonde %>% 
    filter(lake == targLake, year == targYear)
  
  # calculate normalized chlorophyll and temperature values for the specific
  # lake_year combination we are working with
  
  temp = temp %>% 
    mutate(normChl = mean_chl/mean_chl[1]*100) %>% 
    mutate(normTemp = mean_temp/mean_temp[1]*100)
  
  # also subset the heatwaves dataframe to the lake-year combo we want
  hwTemp = hw %>% 
    filter(lake == targLake, year == targYear)
  
  # create annotation to add however many heatwaves occurred in that year
  
  strings = c(NA)
  for(i in 1:nrow(hwTemp)){
    

    
    if(i == 1){
  strAdd = "annotate('rect', xmin = as.Date(hwTemp$date_start[i]), xmax = as.Date(hwTemp$date_end[i]),
         ymin = 0, ymax = Inf,
         fill = 'red', alpha = 0.3)"
  strings[1] = strAdd
  str = strAdd}
    if(i > 1){
      strAdd = "annotate('rect', xmin = as.Date(hwTemp$date_start[i]), xmax = as.Date(hwTemp$date_end[i]),
         ymin = 0, ymax = Inf,
         fill = 'red', alpha = 0.3)"
      strings[i] = strAdd
      str = paste(str, "+", strAdd, sep = "")
    }
    
}
  
  print(str)
  
 
  p = ggplot(data=temp, aes(x=date, y=normChl)) + 
    geom_point() +
    geom_line(aes(x = date, y = normTemp), size = 1.5)+
    geom_density_line(aes(x = date, y = normTemp), stat = "identity", size = 0.5, fill = "steelblue3", alpha = 0.3)+
    geom_line(size = 1.5) +
    geom_density_line(stat = "identity", size = 1.5, fill = "forestgreen", alpha = 0.3)+
    theme_classic()+
    labs(title = paste(temp$lake[1], "Lake", temp$year[1]), y = "% of initial value")+
    theme(title = element_text(size = 20))+
    theme(text = element_text(size = 20))
 print(p +
  for(k in 1:length(strings)){
  eval(parse(text = strings[k]))
    print(strings[k])
  }
 )
  
}

# run the code for a specific lake-year

plot_heatwaves("R", "2013", allSondeInterp, heatwaves)



hwTemp = heatwaves %>% filter(lake == "L", year == "2018")

for(i in 1:nrow(hwTemp)){
  
  if(i == 1){
    strAdd = "annotate('rect', xmin = as.Date(hwTemp$date_start[i]), xmax = as.Date(hwTemp$date_end[i]),
         ymin = 0, ymax = Inf,
         fill = 'red', alpha = 0.3)"
    str = strAdd}
  if(i > 1){
    strAdd = "annotate('rect', xmin = as.Date(hwTemp$date_start[i]), xmax = as.Date(hwTemp$date_end[i]),
         ymin = 0, ymax = Inf,
         fill = 'red', alpha = 0.3)"
    str = paste(str, strAdd, sep = "+\n")
  }
  
}
