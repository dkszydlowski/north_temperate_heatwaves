# script to plot the chlorophyll, temperature, and heatwave time series

# read in the data
allSonde = read.csv("./formatted data/CombinedData.csv")
allSonde$date = as.Date(allSonde$date)

## update the sonde data so that
# we have normalized temp and chlorophyll to plot on the same plot
allSondeInterp = allSonde %>%
  group_by(lake, year) %>%
  mutate(mean_temp = na.approx(mean_temp, na.rm = FALSE), mean_chl = na.approx(mean_chl, na.rm = FALSE)) %>%
  mutate(normTemp = 100 * mean_temp/mean_temp[1], normChl = 100 * mean_chl/mean_chl[1])



#==============================================================================#
###### SONDE CHLOROPHYLL ######

allSondeInterp$lake_year = paste(allSondeInterp$lake, allSondeInterp$year, sep = "_")

#make raw data plots of sonde chlorophyll for all lake_years
lake_years = unique(allSondeInterp$lake_year)

for(i in 1:length(lake_years)){
  
  temp = allSondeInterp %>% filter(lake_year == lake_years[i]) 
  
  print(
    ggplot(data = temp, aes(x = doyCat, y = mean_chl))+
      geom_point()+
      geom_line()+
      labs(title = lake_years[i])
  )
  
}


####Plot Heatwaves - Peter 2009####
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

#### Plot Heatwaves - Peter 2010 ####
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

####Plot Heatwaves - Peter 2011####
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

####Plot Heatwaves - Peter 2013####
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

####Plot Heatwaves - Peter 2014####
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

####Plot Heatwaves - Peter 2018####
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

####Plot Heatwaves - Peter 2019####
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

####Plot Heatwaves - Paul 2009####
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

####Plot Heatwaves - Paul 2010####
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

####Plot Heatwaves - Paul 2011####
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

####Plot Heatwaves - Paul 2013####
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

####Plot Heatwaves - Paul 2018####
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

####Plot Heatwaves - Paul 2019####
Paul2019 = allSondeInterp %>%
  filter(lake == "L", year == "2019")

text = "annotate('rect', xmin = as.Date('2019-07-15'), xmax = as.Date('2019-07-19'), ymin = 0, ymax = Inf,
         fill = 'red', alpha = 0.3)"

ggplot(data=Paul2019, aes(x=date, y=normChl)) + 
  geom_point() +
  geom_line(aes(x = date, y = normTemp), size = 1.5)+
  geom_density_line(aes(x = date, y = normTemp), stat = "identity", size = 0.5, fill = "steelblue3", alpha = 0.3)+
  geom_line(size = 1.5) +
  geom_density_line(stat = "identity", size = 1.5, fill = "forestgreen", alpha = 0.3)+
  theme_classic()+
  labs(title = paste(Paul2019$lake[1], "Lake", Paul2019$year[1]), y = "% of initial value")+
  theme(title = element_text(size = 20))+
  #eval(parse(text = text))+
  annotate('rect', xmin = as.Date('2019-07-15'), xmax = as.Date('2019-07-19'), ymin = 0, ymax = Inf,
           fill = 'red', alpha = 0.3)+
  theme(text = element_text(size = 20))


####Plot Heatwaves - Tuesday 2013 ####
Tuesday2013 = allSondeInterp %>%
  filter(lake == "T", year == "2013")

ggplot(data=Tuesday2013, aes(x=date, y=normChl)) + 
  geom_point() +
  geom_line(aes(x = date, y = normTemp), size = 1.5)+
  geom_density_line(aes(x = date, y = normTemp), stat = "identity", size = 0.5, fill = "steelblue3", alpha = 0.3)+
  geom_line(size = 1.5) +
  geom_density_line(stat = "identity", size = 1.5, fill = "forestgreen", alpha = 0.3)+
  theme_classic()+
  labs(title = paste(Tuesday2013$lake[1], "Lake", Tuesday2013$year[1]), y = "% of initial value")+
  theme(title = element_text(size = 20))+
  annotate("rect", xmin = as.Date("2013-07-05"), xmax = as.Date("2013-07-09"), ymin = 0, ymax = Inf,
           fill = "red", alpha = 0.3)+
  annotate("rect", xmin = as.Date("2013-07-15"), xmax = as.Date("2013-07-20"), ymin = 0, ymax = Inf,
           fill = "red", alpha = 0.3)+
  theme(text = element_text(size = 20))





####Plot Heatwaves - Tuesday 2014 ####
Tuesday2014 = allSondeInterp %>%
  filter(lake == "T", year == "2014")

ggplot(data=Tuesday2014, aes(x=date, y=normChl)) + 
  geom_point() +
  geom_line(aes(x = date, y = normTemp), size = 1.5)+
  geom_density_line(aes(x = date, y = normTemp), stat = "identity", size = 0.5, fill = "steelblue3", alpha = 0.3)+
  geom_line(size = 1.5) +
  geom_density_line(stat = "identity", size = 1.5, fill = "forestgreen", alpha = 0.3)+
  theme_classic()+
  labs(title = paste(Tuesday2014$lake[1], "Lake", Tuesday2014$year[1]), y = "% of initial value")+
  theme(title = element_text(size = 20))+
  annotate("rect", xmin = as.Date("2014-05-26"), xmax = as.Date("2014-06-03"), ymin = 0, ymax = Inf,
           fill = "red", alpha = 0.3)+
  theme(text = element_text(size = 20))


#### Plot Heatwaves - Tuesday 2015 ####
Tuesday2015 = allSondeInterp %>%
  filter(lake == "T", year == "2015")

ggplot(data=Tuesday2015, aes(x=date, y=normChl)) + 
  geom_point() +
  geom_line(aes(x = date, y = normTemp), size = 1.5)+
  geom_density_line(aes(x = date, y = normTemp), stat = "identity", size = 0.5, fill = "steelblue3", alpha = 0.3)+
  geom_line(size = 1.5) +
  geom_density_line(stat = "identity", size = 1.5, fill = "forestgreen", alpha = 0.3)+
  theme_classic()+
  labs(title = paste(Tuesday2015$lake[1], "Lake", Tuesday2015$year[1]), y = "% of initial value")+
  theme(title = element_text(size = 20))+
  annotate("rect", xmin = as.Date("2015-07-25"), xmax = as.Date("2015-07-29"), ymin = 0, ymax = Inf,
           fill = "red", alpha = 0.3)+
  annotate("rect", xmin = as.Date("2015-08-14"), xmax = as.Date("2015-08-18"), ymin = 0, ymax = Inf,
           fill = "red", alpha = 0.3)+
  theme(text = element_text(size = 20))


# Paul 2008
Paul2008 = allSondeInterp %>%
  filter(lake == "L", year == "2008")

ggplot(data=Paul2008, aes(x=date, y=normChl)) + 
  geom_point() +
  geom_line(aes(x = date, y = normTemp), size = 1.5)+
  geom_density_line(aes(x = date, y = normTemp), stat = "identity", size = 0.5, fill = "steelblue3", alpha = 0.3)+
  geom_line(size = 1.5) +
  geom_density_line(stat = "identity", size = 1.5, fill = "forestgreen", alpha = 0.3)+
  theme_classic()+
  labs(title = paste(Paul2008$lake[1], "Lake", Paul2008$year[1]), y = "% of initial value")+
  theme(title = element_text(size = 20))+
  theme(text = element_text(size = 20))


# Peter 2008
Peter2008 = allSondeInterp %>%
  filter(lake == "R", year == "2008")

ggplot(data=Peter2008, aes(x=date, y=normChl)) + 
  geom_point() +
  geom_line(aes(x = date, y = normTemp), size = 1.5)+
  geom_density_line(aes(x = date, y = normTemp), stat = "identity", size = 0.5, fill = "steelblue3", alpha = 0.3)+
  geom_line(size = 1.5) +
  geom_density_line(stat = "identity", size = 1.5, fill = "forestgreen", alpha = 0.3)+
  theme_classic()+
  labs(title = paste(Peter2008$lake[1], "Lake", Peter2008$year[1]), y = "% of initial value")+
  theme(title = element_text(size = 20))+
  theme(text = element_text(size = 20))



# Peter 2015
Peter2015 = allSondeInterp %>%
  filter(lake == "R", year == "2015")

ggplot(data=Peter2015, aes(x=date, y=normChl)) + 
  geom_point() +
  geom_line(aes(x = date, y = normTemp), size = 1.5)+
  geom_density_line(aes(x = date, y = normTemp), stat = "identity", size = 0.5, fill = "steelblue3", alpha = 0.3)+
  geom_line(size = 1.5) +
  geom_density_line(stat = "identity", size = 1.5, fill = "forestgreen", alpha = 0.3)+
  theme_classic()+
  labs(title = paste(Peter2015$lake[1], "Lake", Peter2015$year[1]), y = "% of initial value")+
  theme(title = element_text(size = 20))+
  theme(text = element_text(size = 20))



# Paul 2014
Paul2014 = allSondeInterp %>%
  filter(lake == "L", year == "2014")

ggplot(data=Paul2014, aes(x=date, y=normChl)) + 
  geom_point() +
  geom_line(aes(x = date, y = normTemp), size = 1.5)+
  geom_density_line(aes(x = date, y = normTemp), stat = "identity", size = 0.5, fill = "steelblue3", alpha = 0.3)+
  geom_line(size = 1.5) +
  geom_density_line(stat = "identity", size = 1.5, fill = "forestgreen", alpha = 0.3)+
  theme_classic()+
  labs(title = paste(Paul2014$lake[1], "Lake", Paul2014$year[1]), y = "% of initial value")+
  theme(title = element_text(size = 20))+
  theme(text = element_text(size = 20))



# Paul 2015
Paul2015 = allSondeInterp %>%
  filter(lake == "L", year == "2015")

ggplot(data=Paul2015, aes(x=date, y=normChl)) + 
  geom_point() +
  geom_line(aes(x = date, y = normTemp), size = 1.5)+
  geom_density_line(aes(x = date, y = normTemp), stat = "identity", size = 0.5, fill = "steelblue3", alpha = 0.3)+
  geom_line(size = 1.5) +
  geom_density_line(stat = "identity", size = 1.5, fill = "forestgreen", alpha = 0.3)+
  theme_classic()+
  labs(title = paste(Paul2015$lake[1], "Lake", Paul2015$year[1]), y = "% of initial value")+
  theme(title = element_text(size = 20))+
  theme(text = element_text(size = 20))



#==============================================================================#
##### MANUAL CHLOROPHYLL #####



