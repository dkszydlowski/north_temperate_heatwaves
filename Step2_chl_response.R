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

# data is the data from the sondes
# variable is the response variable of interest
# start is the start date we want to analyze
# end is how many days to add to that (e.g., 7, 14, etc. for our window)

dataTest = peter15 %>% 
  mutate(roll7_chl = slide_dbl(mean_chl, .f=mean, na.rm = TRUE, .after = 14, .complete = TRUE))




hwResponse = function(data, variable, start, end, lake){
  

}


# #Example of rolling windows calculating the mean of thermocline depth in 2, 3, and 4 day windows
# thermo_roll = thermo1 %>%
#   mutate(roll2_tcline = slide_dbl(thermocline, .f=mean, na.rm = TRUE, .before = 1, .complete = TRUE), #2 day rolling window
#          roll3_tcline = slide_dbl(thermocline, .f=mean, na.rm = TRUE, .before = 2, .complete = TRUE), #3 day rolling window
#          roll4_tcline = slide_dbl(thermocline, .f=mean, na.rm = TRUE, .before = 3, .complete = TRUE)) #4 day rolling window



# maybe a function that calculates the slope following a heatwave

