# step 2, calculate the response of chl to the heatwaves
#install and load slider library for rolling window analysis
if (!require(slider)) install.packages('slider')
library(slider)

if (!require(tidyr)) install.packages('tidyr')
library(tidyr)

if (!require(dplyr)) install.packages('dplyr')
library(dplyr)
library(tidyverse)



# read in the heatwaves data calculated in the previous step
heatwaves = read.csv("Heatwavesdata.csv")

# read in the sonde data from step 1
allSonde = read.csv("CombinedData.csv")
allSonde$date = as.Date(allSonde$date)

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
  startDates = sort(sample(seq((as.Date(minDate)), as.Date(maxDate), by = 1), 4))
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

for(i in 1:lengthRandom){
  test = hwSlopes(randomDays$date_start[i], randomDays$date_end[i], randomDays$lake[i], slopes)
  randomDays$averageSlope[i] = mean(test$chl_slope, na.rm = TRUE)
  randomDays$percentChange[i] = 100*(mean(test$chl_slope, na.rm = TRUE)*7)/test$chl_before[1]
  
}

