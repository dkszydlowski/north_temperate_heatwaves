##### re-make the plots, but do it in a for loop and show where random days are located
# and where the slopes are calculated following each heatwave


# read in the data
allSonde = read.csv("./formatted data/allSonde_interpolated.csv")
allSlopes = read.csv("./formatted data/results_random_and_heatwaves.csv")

# make normalized temperature and chlorophyll columns in allSonde
allSonde = allSonde %>%  
  mutate(normTemp = 100 * mean_temp/mean_temp[1], normChl = 100 * mean_chl/mean_chl[1])

allSonde$date = as.Date(allSonde$date)

# in a for loop, plot raw data, the heatwave, slope, and random days for
# each lake_year combination

i = 1

lake_years = unique(allSonde$lake_year)

# create a dataframe of lake year slopes
lySlope = allSlopes %>% filter(lake_year == lake_years[i])

# create a dataframe of lake year data
lyAllSonde = allSonde %>% filter(lake_year == lake_years[i])



text = "annotate('rect', xmin = as.Date('2008-07-15'), xmax = as.Date('2008-07-19'), ymin = 0, ymax = Inf,
         fill = 'red', alpha = 0.3)+ annotate('rect', xmin = as.Date('2008-06-15'), xmax = as.Date('2008-07-19'), ymin = 0, ymax = Inf,
         fill = 'red', alpha = 0.3)"

ggplot(data=lyAllSonde, aes(x=date, y=normChl)) + 
  geom_point() +
  geom_line(aes(x = date, y = normTemp), size = 1.5)+
  geom_density_line(aes(x = date, y = normTemp), stat = "identity", size = 0.5, fill = "steelblue3", alpha = 0.3)+
  geom_line(size = 1.5) +
  geom_density_line(stat = "identity", size = 1.5, fill = "forestgreen", alpha = 0.3)+
  theme_classic()+
  labs(title = paste(lyAllSonde$lake[1], "Lake", lyAllSonde$year[1]), y = "% of initial value")+
  theme(title = element_text(size = 20))+
  #eval(parse(text = text))+
 # annotate('rect', xmin = as.Date('2019-06-15'), xmax = as.Date('2019-06-23'), ymin = 0, ymax = Inf,
 #          fill = 'blue', alpha = 0.3)+
  theme(text = element_text(size = 20))

