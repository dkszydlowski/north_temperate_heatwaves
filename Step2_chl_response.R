# step 2, calculate the response of chl to the heatwaves
#install and load slider library for rolling window analysis
if (!require(slider)) install.packages('slider')
library(slider)

library(tidyr)
library(dplyr)


# read in the heatwaves data calculated in the previous step
heatwaves = read.csv("Heatwavesdata.csv")

# read in the sonde data from step 1
allSonde = read.csv("CombinedData.csv")
allSonde$date = as.Date(allSonde$date)

peter15 = allSonde %>% filter(lake == "R" & year == 2015)
# maybe make a function that calculates the slope following a heatwave

#peter15 = as.list(peter15)
# data is the data from the sondes
# variable is the response variable of interest
# start is the start date we want to analyze
# end is how many days to add to that (e.g., 7, 14, etc. for our window)

data_test = slide_dbl(peter15$mean_chl, .f = ., .after = 14, .complete = TRUE)


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

i =1


## extract the slope, se, and p-value
# and add to the dataframe

# make a lake_year column in allSonde
allSonde = allSonde %>% 
  mutate(lake_year = paste(lake, year, sep = "_"))

# make a vector of unique lake years
lake_years = unique(allSonde$lake_year)

# cycle through each lake year, calculate models for each, then store model results
# in the dataframe. Need to initially store the model results in a list
for(i in 1:length(lake_years)){
  
print(lake_years[i])
temp = allSonde %>% filter(lake_year == lake_years[i]) # temp dataframe for this lake_year

models <- slide(
  temp, 
  ~lm(mean_chl ~ doyCat, data = .x), 
  .before = 7, 
  .complete = TRUE
)

temp$slope = NA
temp$se = NA
temp$p_value = NA
temp$r_squared = NA

for(j in 1:nrow(temp)){
  
  model = models[[j]]
  coef <- coefficients(model) 
  # extracting the coefficients from the current model
  
  if(!(is.null(coef))){
    Slope <- coef["doyCat"]
    temp$slope[j] = Slope
    temp$r_squared[j] = summary(model)$r.squared
  }
  

}

if(i ==1){ slopes = temp} # if first iteration, creates slopes, the final dataframe

if(i >1){slopes = rbind(slopes, temp)} # else, appends to slopes 

remove(models)
}


### Right now, this code does a 


### Make a function which extracts the slopes for a given heatwave


# #Example of rolling windows calculating the mean of thermocline depth in 2, 3, and 4 day windows
# thermo_roll = thermo1 %>%
#   mutate(roll2_tcline = slide_dbl(thermocline, .f=mean, na.rm = TRUE, .before = 1, .complete = TRUE), #2 day rolling window
#          roll3_tcline = slide_dbl(thermocline, .f=mean, na.rm = TRUE, .before = 2, .complete = TRUE), #3 day rolling window
#          roll4_tcline = slide_dbl(thermocline, .f=mean, na.rm = TRUE, .before = 3, .complete = TRUE)) #4 day rolling window



# maybe a function that calculates the slope following a heatwave

testCoef = coefficients(testModel)[2]
