# step 2, calculate the response of chl to the heatwaves
#install and load slider library for rolling window analysis
if (!require(slider)) install.packages('slider')
library(slider)

if (!require(plyr)) install.packages('plyr')
library(plyr)

if (!require(tidyr)) install.packages('tidyr')
library(tidyr)

if (!require(dplyr)) install.packages('dplyr')
library(dplyr)
library(tidyverse)

if (!require(zoo)) install.packages('zoo')
library(zoo)

if (!require(gganimate)) install.packages('gganimate')
library(gganimate)

if (!require(transformr)) install.packages('transformr')
library(transformr)

if (!require(ggridges)) install.packages('ggridges')
library(ggridges)



# read in the heatwaves data calculated in the previous step
heatwaves = read.csv("Heatwavesdata.csv")

# read in the sonde data from step 1
allSonde = read.csv("CombinedData.csv")
allSonde$date = as.Date(allSonde$date)

# interpolate the data by lake_year
allSonde = allSonde %>% group_by("lake_year") %>% 
  mutate(mean_chl = na.approx(mean_chl),
         mean_doSat = na.approx(mean_doSat),
         mean_pH = na.approx(mean_pH)) %>% 
  ungroup()

######## Testing code ##########
peter15 = allSonde %>% filter(lake == "R" & year == 2015)
# maybe make a function that calculates the slope following a heatwav
#peter15 = as.list(peter15)

# data is the data from the sondes
# variable is the response variable of interest
# start is the start date we want to analyze
# end is how many days to add to that (e.g., 7, 14, etc. for our window)


data_test = slide_dbl(peter15$mean_chl, .f = ., .after = 14, .complete = TRUE)

dataTest = peter15 %>% 
  mutate(roll2_chl = slide_dbl(mean_chl, .f=mean, na.rm = TRUE, .after = 14, .complete = TRUE))



hwResponse = function(data, variable, start, end, lake){
  
  
}


testModel = lm(peter15$mean_chl~peter15$doyCat)$coefficients
testModel$coefficients[2]

#peter15 = as.list(peter15)
#peter15 = data.frame(peter15)

models = as.list(peter15)

# make a list of models
# extract the slope, se, and p-value of each one
models <- slide(
  peter15, 
  ~lm(mean_chl ~ doyCat, data = .x), 
  .before = 7, 
  .complete = TRUE
)

testing <- allSonde %>% 
  group_by(year, lake) %>% 
  slide(
    ~lm(mean_chl ~doyCat, data = .x), 
    .before = 7, 
    .complete = TRUE
  )


peter15$slope = NA
peter15$se = NA
peter15$p_value = NA





####### Start of actual code #########

## extract the slope, se, and p-value
# and add to the dataframe

# make a lake_year column in allSonde
allSonde = allSonde %>% 
  mutate(lake_year = paste(lake, year, sep = "_"))

# make a vector of unique lake years
lake_years = unique(allSonde$lake_year)



# cycle through each lake year, calculate models for each, then store model results
# in the dataframe. Need to initially store the model results in a list

i = 1


for(i in 1:length(lake_years)){
  
  print(lake_years[i])
  temp = allSonde %>% filter(lake_year == lake_years[i]) # temp dataframe for this lake_year
  
  models <- slide(
    temp, 
    ~lm(mean_chl ~ doyCat, data = .x), 
    .before = 7, 
    .complete = TRUE
  )
  
  temp$chl_slope = NA
  temp$se = NA
  temp$p_value = NA
  temp$r_squared = NA
  
  for(j in 1:nrow(temp)){
    
    model = models[[j]]
    coef <- coefficients(model) 
    # extracting the coefficients from the current model
    
    if(!(is.null(coef))){
      Slope <- coef["doyCat"]
      temp$chl_slope[j] = Slope # pull out the flope from the model
      temp$p_value[j] = summary(model)$coefficients[2,4] 
      temp$r_squared[j] = summary(model)$r.squared # pull out r_squared from model
    }
    
    
  }
  
  if(i ==1){ slopes = temp} # if first iteration, creates slopes, the final dataframe
  
  if(i >1){slopes = rbind(slopes, temp)} # else, appends to slopes 
  
  remove(models)
}

# Need to fix the p-value extraction!!!!!!

######### Make a function to extract the slopes following each heatwave event ##########
# heatwave is the date of a heatwave, lake is the lake, data is the data to look for it in

hwSlopes <- function(heatwaveStart, heatwaveEnd,  targLake, data){
  startDate = as.Date(heatwaveEnd)+7
  endDate = as.Date(heatwaveEnd)+13
  
  beforeDate = as.Date(heatwaveStart) -1
  
  startChl = data %>% filter(date == beforeDate) %>% 
    select(mean_chl)
  
  temp = data %>% filter(lake == targLake, date >= startDate, date <= endDate)
  
  temp = temp %>% mutate(chl_before = startChl$mean_chl[1])
  return(temp)
  
}

heatwaves$averageSlope = NA
heatwaves$percentChange = NA



# copy and paste this
test = hwSlopes("2009-06-20", "2009-06-27", "R", slopes)
heatwaves$averageSlope[1] = mean(test$chl_slope, na.rm = TRUE)
heatwaves$percentChange[1] = 100*(mean(test$chl_slope, na.rm = TRUE)*7)/test$chl_before[1]


# 
# hwSlopesBefore <- function(heatwave, targLake, data){
#   startDate = as.Date(heatwave)-14
#   endDate = as.Date(heatwave)+7
#   
#   temp = data %>% filter(lake == targLake, date >= startDate, date <= endDate)
#   return(temp)
#   
# }



#Using a For Loop
lengthHW = nrow(heatwaves)

for(i in 1:lengthHW){
  test = hwSlopes(heatwaves$date_start[i], heatwaves$date_end[i], heatwaves$lake[i], slopes)
  heatwaves$averageSlope[i] = mean(test$chl_slope, na.rm = TRUE)
  heatwaves$percentChange[i] = 100*(mean(test$chl_slope, na.rm = TRUE)*7)/test$chl_before[1]
  
}

write.csv(heatwaves, "./results/heatwaves_with_slopes.csv", row.names = FALSE)


#making plots
results = read.csv("./results/heatwaves_with_slopes.csv")
hist(results)

#break up by lake by filtering data
resultsT = results %>% filter(lake == "T")
resultsL = results %>% filter(lake == "L")
resultsR = results %>% filter(lake == "R")

#make preliminary plots - plot slope 
plot(resultsT$averageSlope ~ resultsT$duration)
plot(resultsR$averageSlope ~ resultsR$duration)

#work on plotting slopes vs different variables, see if any interesting relationships 

#use ggplot to make three plots, one for each lake with date of heatwave on x-axis
#and average slope on the y

#png(filename = "./figures/preliminary figures/R_slopes_2023_02_09", height = 8, width = 11, units = "in", res = 300)

ggplot(data = resultsR, aes(x = date_end, y = averageSlope))+
  geom_bar(stat='identity', fill = "forestgreen")+
  theme_classic()+
  ylab("average chlorophyll slope (ug/L/day)")+
  xlab("end date of heatwave")+
  ggtitle("Peter")
  


rand()


set.seed(21)
sample(seq((as.Date('2008-05-13')), as.Date('2008-08-27'), by = 1), 12)




###### RANDOM DATE COMPARISON ########
# create a new dataframe similar to heatwaves, but with random dates
# heatwave duration is 8 days, on average
# create random start dates and end dates separated by 8 days
# only years and days with dataavailable

lake_years = unique(allSonde$lake_year)

i =1

#randomDays = data.frame(lake_year = , lake, year, start_date, end_date)

for(i in 1: length(lake_years)){
  
  target = allSonde %>% filter(lake_year == lake_years[i])
  minDate = min(target$date)
  maxDate = max(target$date)
  
  set.seed(21)
  startDates = sort(sample(seq((as.Date(minDate)), as.Date(maxDate), by = 1), 5))
  endDates = startDates + 8
  
  current_randomDays = data.frame(lake_year = lake_years[i], lake = unique(target$lake)[1],
                                  year = unique(target$year)[1],
                                  start_date = startDates, end_date = endDates)
  
  if(i == 1){
    randomDays = current_randomDays
  }
  if(i > 1){
    randomDays = rbind(randomDays, current_randomDays)
  }
  
}


# calculate slopes using random days
lengthRandom = nrow(randomDays)
randomDays$percentChange = NA
randomDays$averageSlope = NA

i =1
for(i in 1:lengthRandom){
  test = hwSlopes(randomDays$start_date[i], randomDays$end_date[i], randomDays$lake[i], slopes)
  randomDays$averageSlope[i] = mean(test$chl_slope, na.rm = TRUE)
  randomDays$percentChange[i] = 100*(mean(test$chl_slope, na.rm = TRUE)*7)/test$chl_before[1]
  
}


plot(density(randomDays$percentChange, na.rm = TRUE))

plot(density(heatwaves$percentChange, na.rm = TRUE))



ggplot(randomDays, aes(x=percentChange)) +
  geom_density(alpha=.7, fill = "forestgreen")

ggplot(heatwaves, aes(x=percentChange)) +
  geom_density(alpha=.7, fill = "steelblue2")


randomDays = randomDays %>% rename(date_start = start_date,
                                   date_end = end_date)

heatwaves = heatwaves %>% mutate(date_start = as.Date(date_start),
                                   date_end = as.Date(date_end))

randomDays$variable = "random"
heatwaves$variable = "heatwave"

randomDays = randomDays %>% filter(lake_year %in% c("L_2008", "R_2008", "L_2014", "L_2015"))

allPercent = randomDays %>% 
  full_join(heatwaves, by = c("date_start", "date_end", "year", "variable", "averageSlope", "percentChange", 
                              "lake", "lake_year"))



ggplot(allPercent, aes(x=percentChange, fill = variable)) +
  geom_density(alpha=.5)

nutrients = c("R_2013",
              "R_2014",
              "R_2015",
              "R_2019",
              "T_2013",
              "T_2014",
              "T_2015")



ggplot(allPercent, aes(x=percentChange, fill = variable)) +
  geom_density(alpha=.5)

# test what happens if we remove heatwaves in nutrient addition years

allPercent %>%  filter((lake_year %in% nutrients)) %>% 
ggplot(aes(x=percentChange, fill = variable)) +
  geom_density(alpha=.5)












###### COMPARE DISTRIBUTIONS OVER TIME ##########

## in the slopes dataframe, add a column percent change that is a percent change in chlorophyll from seven days before that slope
# this normalizes across lakes
# then, save as a dataframe

slopes = slopes %>% group_by(lake_year) %>% 
  mutate(percent_change = 100*chl_slope*7/lag(mean_chl, 8, default = NA)) %>% 
  ungroup()

slopes = slopes %>% mutate(period = "all other days")
slopes$shift = NA

# shift is the number of days after the heatwave we want to investigate
shift = 1

# windowSize is the size of the window we want to look at
#minimum is 1
windowSize = 4

for(shift in -7:40){

  # add a column to slopes which indicates whether or not there is a heatwave
for(i in 1:nrow(heatwaves)){
  start = heatwaves$date_start[i]
  end = heatwaves$date_end[i]
  
  dates = seq(start, end, 1)
  datesAnalyzed = seq(end+shift, end + shift+windowSize, 1)
  datesExcluded = seq(end -7, end + 40, 1) # excludes dates that are part of rolling window but not currently considered in after heatwave
  
  slopes = slopes %>% mutate(period = replace(period, date %in% datesExcluded, "exclude after heatwave"))
  slopes = slopes %>% mutate(period = replace(period, date %in% dates, "during heatwave"))
  slopes = slopes %>% mutate(period = replace(period, date %in% datesAnalyzed, "after heatwave"))
  
  
  slopes$shift = shift
}

if(shift == -7){
  allSlopes = slopes
}
if(shift > -7){
  allSlopes = rbind(allSlopes, slopes)
}

}


# save the allPercent data to a dataframe
# save interpolated allSonde data to a dataframe

# write.csv(allSonde, "./formatted data/allSonde_interpolated.csv", row.names = FALSE)
# write.csv(allPercent, "./formatted data/results_random_and_heatwaves.csv")

# calculate mean values by period and shift
mean_df <- allSlopes %>% 
  filter(percent_change < 175, period != "exclude after heatwave", !is.na(percent_change)) %>% 
  group_by(period, shift) %>% 
  dplyr::summarise(mean_percent_change = mean(percent_change)) 

# round mean values to one decimal place
mean_df$mean_percent_change <- round(mean_df$mean_percent_change, 1)

# plot the density ridges with mean values
allSlopes %>% 
  filter(percent_change < 175, period != "exclude after heatwave") %>% 
  ggplot(aes(x = percent_change,
             y = period,
             fill = period)) +
  geom_density_ridges(alpha = .5,
                      quantile_lines = TRUE,
                      quantile_fun = function(x, ...) mean(x), 
                      scale = 3) +
  geom_text(data = mean_df,
            aes(x = mean_percent_change,
                y = period,
                label = as.character(round(mean_percent_change, digits = 0))),
            color = "black",
            size = 4,
            vjust = 2) +
  theme_classic() +
  gganimate::transition_time(shift) +
  labs(title = "Days after heatwave for all lakes: {frame_time}")

  gganimate::animate(plot = last_plot(), fps = 5) # slows down the animation

 #gganimate::anim_save(filename = "./figures/animations/all_lakes_window_7.gif")


  
  


allSlopes %>% filter(percent_change < 500, lake == "T") %>% 
  ggplot( aes(x=percent_change, fill = period)) +
  geom_density(alpha=.5)+
  theme_classic()+
  gganimate::transition_time(shift)+
  labs(title = "Days after heatwave for Tuesday Lake: {frame_time}")


gganimate::anim_save(filename = "./figures/animations/tuesday_window_1.gif")



allSlopes %>% filter(percent_change < 500, lake == "R") %>% 
  ggplot( aes(x=percent_change, fill = period)) +
  geom_density(alpha=.5)+
  theme_classic()+
  gganimate::transition_time(shift)+
  labs(title = "Days after heatwave for Peter Lake: {frame_time}")


gganimate::anim_save(filename = "./figures/animations/peter_window_1.gif")





allSlopes %>% filter(percent_change < 500, lake == "L") %>% 
  ggplot( aes(x=percent_change, fill = period)) +
  geom_density(alpha=.5)+
  theme_classic()+
  gganimate::transition_time(shift)+
  labs(title = "Days after heatwave for Paul Lake: {frame_time}")


gganimate::anim_save(filename = "./figures/animations/paul_window_1.gif")





shift40HW = allSlopes %>% filter(shift == 40, period == "after heatwave")
shift40other = allSlopes %>% filter(shift == 40, period == "all other days")

t.test(shift40HW$percent_change, shift40other$percent_change)




##### Plot the mean percent change in chlorophyll over time   #####

mean_df <- allSlopes %>% 
  filter(percent_change < 175, period != "exclude after heatwave", !is.na(percent_change)) %>% 
  group_by(period, shift) %>% 
  dplyr::summarise(mean_percent_change = mean(percent_change), 
                   sd_percent_change = sd(percent_change), 
                   number_percent_change = n()) 


ggplot(mean_df, aes( x= shift, y = mean_percent_change, color = period))+
  geom_line(size = 1)+
  labs(x = "days after heatwave")+
  theme_classic()







##### Plots of just a couple of interesting days, frozen

allSlopes %>%
  filter(percent_change < 175, period != "exclude after heatwave", shift == 4) %>% 
  ggplot(aes(x = percent_change,
             y = period,
             fill = period)) +
  geom_density_ridges(alpha = .7,
                      quantile_lines = TRUE,
                      quantile_fun = function(x, ...) mean(x), 
                      scale = 3) +
  geom_text(data = mean_df %>% filter(shift == 5),
            aes(x = mean_percent_change,
                y = period,
                label = as.character(round(mean_percent_change, digits = 0))),
            color = "black",
            size = 4,
            vjust = 2) +
  
  geom_text(data = mean_df %>% filter(shift == 5),
            aes(x = -200,
                y = period,
                label = paste("n = ", number_percent_change, sep = "")),
            color = "black",
            size = 4,
            vjust = 2) +
  theme_classic()

gganimate::animate(plot = last_plot(), fps = 5) # slows down the animation



