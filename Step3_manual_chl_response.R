# Re-run step 3 but with the manual chlorophyll data

# manual chlorophyll better represent phytoplankton

# step 3, calculate the response of chl to the heatwaves
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
heatwaves = read.csv("./formatted data/heatwavesdata.csv")

# read in the sonde data from step 1
allData = read.csv("./formatted data/full raw data manual and sonde chlorophyll.csv")
allData$date = as.Date(allData$date)

# in this case, set mean_chl equal to manual_chl
allData$mean_chl = allData$manual_chl

# loop through and remove NA values at the beginning and end of each lake_year combination
# so that I can interpolate and run the code

allData$lake_year = paste(allData$lake, allData$year, sep = "_")
i = 1
for(i in 1:length(lake_years)){
  temp = allData %>% filter(lake_year == lake_years[i])
  temp = temp %>% dplyr::arrange(doyCat)
  
  indexChange = 0 # shifts index if a row is deleted
  
  # remove consecutive values at the beginning of each lake_year
  for(j in 1:nrow(temp)){
    if(is.na(temp$mean_chl[j+indexChange])){
      temp = temp[-1, ] # removes the row
      indexChange = indexChange -1 # fixes index because row was removed
    }else{break}
  }
  
  # remove consecutive values at the end of each lake_year
  for(j in nrow(temp):1){
    print(j)
    if(is.na(temp$mean_chl[j])){
      temp = temp[-j,]# removes the row
      }else{break}
  }
  
  
  
  if(i == 1){cutData = temp}else{cutData = rbind(cutData, temp)}

  }
  

allData = cutData # change allData so it is updated to exclude the leading or ending NA values

# interpolate the data by lake_year
allData = allData %>% group_by("lake_year") %>% 
  mutate(mean_chl = na.approx(mean_chl),
         mean_doSat = na.approx(mean_doSat),
         mean_pH = na.approx(mean_pH)) %>% 
  ungroup()


## extract the slope, se, and p-value
# and add to the dataframe

# make a lake_year column in allData
allData = allData %>% 
  mutate(lake_year = paste(lake, year, sep = "_"))

# make a vector of unique lake years
lake_years = unique(allData$lake_year)

# cycle through each lake year, calculate models for each, then store model results
# in the dataframe. Need to initially store the model results in a list

# Model results are the slopes, originally calculated based on the preceding 7 days
daysBefore = 7

for(i in 1:length(lake_years)){
  
  print(lake_years[i])
  temp = allData %>% filter(lake_year == lake_years[i]) # temp dataframe for this lake_year
  
  models <- slide(
    temp, 
    ~lm(mean_chl ~ doyCat, data = .x), 
    .before = daysBefore, 
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
      temp$chl_slope[j] = Slope # pull out the slope from the model
      temp$p_value[j] = summary(model)$coefficients[2,4] 
      temp$r_squared[j] = summary(model)$r.squared # pull out r_squared from model
    }
    
    
  }
  
  if(i ==1){ slopes = temp} # if first iteration, creates slopes, the final dataframe
  
  if(i >1){slopes = rbind(slopes, temp)} # else, appends to slopes 
  
  remove(models)
}


######### Make a function to extract the target slopes following each heatwave event ##########
# currently, the slopes dataframe has daily slopes for all of the lake_year combinations
# Need to extract the slopes just following a heatwave
# heatwave is the date of a heatwave, lake is the lake, data is the data to look for it in

hwSlopes <- function(heatwaveStart, heatwaveEnd,  targLake, data){
  
  # slopes from 7-14 after a heatwave
  # because our slopes are calculated over a 7-day rolling window, the first slope considered will
  # encompass the first week after the heatwave
  
  startDate = as.Date(heatwaveEnd)+7 
  endDate = as.Date(heatwaveEnd)+13
  
  # this is the date before the heatwave, which we will use to calculate the percent
  # change in chlorophyll
  
  beforeDate = as.Date(heatwaveStart) -1
  
  # get the starting chlorophyll concentration before the heatwave
  
  startChl = data %>% filter(date == beforeDate) %>% 
    select(mean_chl)
  
  # filter out the slopes we are interested in
  
  temp = data %>% filter(lake == targLake, date >= startDate, date <= endDate)
  
  # put the starting chlorophyll value in the dataframe so we can return it with the function
  
  temp = temp %>% mutate(chl_before = startChl$mean_chl[1])
  return(temp)
  
}

heatwaves$averageSlope = NA
heatwaves$percentChange = NA



# copy and paste this
# test = hwSlopes("2009-06-20", "2009-06-27", "R", slopes)
# heatwaves$averageSlope[1] = mean(test$chl_slope, na.rm = TRUE)
# heatwaves$percentChange[1] = 100*(mean(test$chl_slope, na.rm = TRUE)*7)/test$chl_before[1]

# OR

#Using a For Loop
lengthHW = nrow(heatwaves)

# take the average slope 
for(i in 1:lengthHW){
  test = hwSlopes(heatwaves$date_start[i], heatwaves$date_end[i], heatwaves$lake[i], slopes)
  heatwaves$averageSlope[i] = mean(test$chl_slope, na.rm = TRUE)
  heatwaves$percentChange[i] = 100*(mean(test$chl_slope, na.rm = TRUE)*7)/test$chl_before[1]
  
}

#write.csv(heatwaves, "./results/heatwaves_with_average_slopes_MANUAL_CHL.csv", row.names = FALSE)


#making plots
results = read.csv("./results/heatwaves_with_average_slopes_MANUAL_CHL.csv")

#break up by lake by filtering data
resultsT = results %>% filter(lake == "T")
resultsL = results %>% filter(lake == "L")
resultsR = results %>% filter(lake == "R")



### PLOT SLOPES BAR GRAPHS ###
#work on plotting slopes vs different variables, see if any interesting relationships 

#use ggplot to make three plots, one for each lake with date of heatwave on x-axis
#and average slope on the y

#png(filename = "./figures/preliminary figures/R_slopes_2023_02_09", height = 8, width = 11, units = "in", res = 300)

ggplot(data = resultsR, aes(x = date_end, y = percentChange))+
  geom_bar(stat='identity', fill = "forestgreen")+
  theme_classic()+
  ylab("percent change in chl")+
  xlab("end date of heatwave")+
  ggtitle("Peter")

ggplot(data = resultsL, aes(x = date_end, y = percentChange))+
  geom_bar(stat='identity', fill = "forestgreen")+
  theme_classic()+
  ylab("percent change in chl")+
  xlab("end date of heatwave")+
  ggtitle("Paul")

ggplot(data = resultsT, aes(x = date_end, y = percentChange))+
  geom_bar(stat='identity', fill = "forestgreen")+
  theme_classic()+
  ylab("percent change in chl")+
  xlab("end date of heatwave")+
  ggtitle("Tuesday")






###### COMPARE DISTRIBUTIONS OVER TIME ##########

# We might also consider the distributions of chlorophyll slopes following a heatwave,
# rather than just taking the average

## in the slopes dataframe, add a column percent change that is a percent change in chlorophyll from seven days before that slope
# this normalizes across lakes
# then, save as a dataframe

baselineChl = 8 # the number of days before the calculated slope to turn it into a percent

slopes = slopes %>% group_by(lake_year) %>% 
  mutate(percent_change = 100*chl_slope*daysBefore/lag(mean_chl, baselineChl, default = NA)) %>% 
  ungroup()

slopes = slopes %>% mutate(period = "all other days")
slopes$shift = NA

# make sure heatwaves date_start and date_end are formatted as dates
heatwaves$date_end = as.Date(heatwaves$date_end)
heatwaves$date_start = as.Date(heatwaves$date_start)

# write slopes to a file
#write.csv(slopes, "./formatted data/slopes_3day.csv", row.names = FALSE)

# shift is the number of days after the heatwave we want to investigate
# how much is the rolling window shifted
shift = 1

# windowSize is the size of the window we want to look at
#minimum is 1
windowSize = 4

# will investigate slopes from 1-40 days after a heatwave
# and will do this for each heatwave event

### This creates a dataframe, all slopes, that has the slopes categorized
# by their time period (during or after a heatwave, or all other days)
# and includes the number of days after the heatwave event (shift) that is considered
# one column, "exclude after heatwaves," are slopes within 40 days after a heatwave
# that are not in the current rolling window

for(shift in 0:40){
  
  # add a column to slopes which indicates whether or not there is a heatwave
  for(i in 1:nrow(heatwaves)){
    start = heatwaves$date_start[i] # start date of the current heatwave
    end = heatwaves$date_end[i] # end date of the current heatwave
    
    # sequence of dates during the heatwave
    dates = seq(start, end, 1)
    
    # excludes dates that are part of rolling window but not currently considered in after heatwave
    # dates that are within 40 days after a heatwave but not in our window for analysis as
    # after heatwave because of our current shift selection.
    # This gets overwrritten in part by "after heatwave" for those dates that are included
    # in analysis
    datesExcluded = seq(end -7, end + 40, 1) 
    
    # dates to be analyzed after the heatwaves
    # this creates a sequence of numbers, from the end of the heatwave plus whatever
    # shift is set to, to that same value plus the windowSize. So it sets a window shifted
    # X number of days after the heatwave
    
    datesAnalyzed = seq(end+shift, end + shift+windowSize, 1)
    
    # fill the dataframe "period" column with the correct categorization
    slopes = slopes %>% mutate(period = replace(period, date %in% datesExcluded, "exclude after heatwave"))
    slopes = slopes %>% mutate(period = replace(period, date %in% dates, "during heatwave"))
    slopes = slopes %>% mutate(period = replace(period, date %in% datesAnalyzed, "after heatwave"))
    
    
    slopes$shift = shift
  }
  
  # combine to the main dataframe
  if(shift == 0){
    allSlopes = slopes
  }
  if(shift > 0){
    allSlopes = rbind(allSlopes, slopes)
  }
  
}




# save the allPercent data to a dataframe
# save interpolated allData data to a dataframe

# write.csv(allData, "./formatted data/allData_interpolated.csv", row.names = FALSE)
# write.csv(allPercent, "./formatted data/results_random_and_heatwaves.csv")

# calculate mean values by period and shift
# across all lakes, this will give the mean slope following all heatwave events
# by time period

# right now, excluding outliters > 175% change because they are a small portion of
# the dataset
mean_df <- allSlopes %>% 
  filter(period != "exclude after heatwave", !is.na(percent_change)) %>% 
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

#gganimate::animate(plot = last_plot(), fps = 5) # slows down the animation

#gganimate::anim_save(filename = "./figures/animations/all_lakes_window_4.gif")



allSlopes %>% filter(percent_change < 500, lake == "T", SHIFT == 1) %>% 
  ggplot( aes(x=percent_change, fill = period)) +
  geom_density(alpha=.5)+
  theme_classic()+
  #gganimate::transition_time(shift)+
  labs(title = "Days after heatwave for Tuesday Lake: {frame_time}")


#gganimate::anim_save(filename = "./figures/animations/tuesday_window_1.gif")


allSlopes %>% filter(percent_change < 500, lake == "L", shift == 4) %>% 
  ggplot( aes(x=percent_change, fill = period)) +
  geom_density(alpha=.5)+
  theme_classic()+
#  gganimate::transition_time(shift)+
  labs(title = "Days after heatwave for Peter Lake: {frame_time}")


#gganimate::anim_save(filename = "./figures/animations/peter_window_1.gif")



allSlopes %>% filter(percent_change < 500, lake == "L") %>% 
  ggplot( aes(x=percent_change, fill = period)) +
  geom_density(alpha=.5)+
  theme_classic()+
  gganimate::transition_time(shift)+
  labs(title = "Days after heatwave for Paul Lake: {frame_time}")


gganimate::anim_save(filename = "./figures/animations/paul_window_1.gif")




shift4HW = allSlopes %>% filter(shift %in% c(4, 5, 6, 7, 8), period == "after heatwave")
shift4other = allSlopes %>% filter(shift %in% c(4, 5, 6, 7, 8), period == "all other days")
shift4during = allSlopes %>% filter(shift %in% c(4, 5, 6, 7, 8), period == "during heatwave")


t.test(shift4HW$percent_change, shift4other$percent_change)

t.test(shift4during$percent_change, shift4other$percent_change)



##### Plot the mean percent change in chlorophyll over time   #####

mean_df <- allSlopes %>% 
  filter( period != "exclude after heatwave", !is.na(percent_change)) %>% 
  group_by(period, shift) %>% 
  dplyr::summarise(mean_percent_change = mean(percent_change), 
                   sd_percent_change = sd(percent_change), 
                   number_percent_change = n()) 

mean_df %>% filter(shift > 0) %>% 
  ggplot(aes( x= shift, y = mean_percent_change, color = period))+
  geom_line(size = 1)+
  labs(x = "days after heatwave")+
  theme_classic()


mean_dfR <- allSlopes %>% filter(lake == "R") %>% 
  filter(period != "exclude after heatwave", !is.na(percent_change)) %>% 
  group_by(period, shift) %>% 
  dplyr::summarise(mean_percent_change = mean(percent_change), 
                   sd_percent_change = sd(percent_change), 
                   number_percent_change = n()) 



mean_dfL <- allSlopes %>% filter(lake == "L") %>% 
  filter( period != "exclude after heatwave", !is.na(percent_change)) %>% 
  group_by(period, shift) %>% 
  dplyr::summarise(mean_percent_change = mean(percent_change), 
                   sd_percent_change = sd(percent_change), 
                   number_percent_change = n()) 



mean_dfT <- allSlopes %>% filter(lake == "T") %>% 
  filter( period != "exclude after heatwave", !is.na(percent_change)) %>% 
  group_by(period, shift) %>% 
  dplyr::summarise(mean_percent_change = mean(percent_change), 
                   sd_percent_change = sd(percent_change), 
                   number_percent_change = n()) 




##### Plots of just a couple of interesting days, frozen

# all days, with a shift of 4 days after the heatwave
allSlopes %>% 
  filter( period != "exclude after heatwave", shift == 4) %>% 
  ggplot(aes(x = percent_change,
             y = period,
             fill = period)) +
  geom_density_ridges(alpha = .7,
                      quantile_lines = TRUE,
                      quantile_fun = function(x, ...) mean(x), 
                      scale = 3) +
  geom_text(data = mean_df %>% filter(shift == 4),
            aes(x = mean_percent_change,
                y = period,
                label = as.character(round(mean_percent_change, digits = 0))),
            color = "black",
            size = 4,
            vjust = 2) +
  
  geom_text(data = mean_df %>% filter(shift == 4),
            aes(x = -200,
                y = period,
                label = paste("n = ", number_percent_change, sep = "")),
            color = "black",
            size = 4,
            vjust = 2) +
  ylab("")+
  labs(title = "All lakes")+
  theme_classic()







# Peter
allSlopes %>% filter(lake == "R") %>% 
  filter( period != "exclude after heatwave", shift == 4) %>% 
  ggplot(aes(x = percent_change,
             y = period,
             fill = period)) +
  geom_density_ridges(alpha = .7,
                      quantile_lines = TRUE,
                      quantile_fun = function(x, ...) mean(x), 
                      scale = 3) +
  geom_text(data = mean_dfR %>% filter(shift == 1),
            aes(x = mean_percent_change,
                y = period,
                label = as.character(round(mean_percent_change, digits = 0))),
            color = "black",
            size = 4,
            vjust = 2) +
  
  geom_text(data = mean_dfR %>% filter(shift == 4),
            aes(x = -200,
                y = period,
                label = paste("n = ", number_percent_change, sep = "")),
            color = "black",
            size = 4,
            vjust = 2) +
  ylab("")+
  labs(title = "Peter")+
  theme_classic()




#Paul

allSlopes %>% filter(lake == "L") %>% 
  filter( period != "exclude after heatwave", shift == 4) %>% 
  ggplot(aes(x = percent_change,
             y = period,
             fill = period)) +
  geom_density_ridges(alpha = .7,
                      quantile_lines = TRUE,
                      quantile_fun = function(x, ...) mean(x), 
                      scale = 3) +
  geom_text(data = mean_dfL %>% filter(shift == 4),
            aes(x = mean_percent_change,
                y = period,
                label = as.character(round(mean_percent_change, digits = 0))),
            color = "black",
            size = 4,
            vjust = 2) +
  geom_text(data = mean_dfL %>% filter(shift == 1),
            aes(x = -200,
                y = period,
                label = paste("n = ", number_percent_change, sep = "")),
            color = "black",
            size = 4,
            vjust = 2) +
  ylab("")+
  labs(title = "Paul")+
  theme_classic()




#Tuesday
allSlopes %>% filter(lake == "T") %>% 
  filter(period != "exclude after heatwave", shift == 4) %>% 
  ggplot(aes(x = percent_change,
             y = period,
             fill = period)) +
  geom_density_ridges(alpha = .7,
                      quantile_lines = TRUE,
                      quantile_fun = function(x, ...) mean(x), 
                      scale = 3) +
  geom_text(data = mean_dfT %>% filter(shift == 4),
            aes(x = mean_percent_change,
                y = period,
                label = as.character(round(mean_percent_change, digits = 0))),
            color = "black",
            size = 4,
            vjust = 2) +
  
  geom_text(data = mean_dfT %>% filter(shift == 1),
            aes(x = -200,
                y = period,
                label = paste("n = ", number_percent_change, sep = "")),
            color = "black",
            size = 4,
            vjust = 2) +
  ylab("")+
  labs(title = "Tuesday")+
  scale_fill_manual(values = c("all other days" = "#88CCEE", "after heatwave" = "#117733", "during heatwave" = "#CC6677")) +  # Specify fill colors for groups
  theme_classic()







# Create a factor variable with the desired order for 'period'
desired_order <- c("after heatwave", "during heatwave", "all other days")

allSlopes %>%
  filter(lake == "T") %>%
  filter(period != "exclude after heatwave", shift == 4) %>%
  ggplot(aes(x = percent_change,
             y = factor(period, levels = desired_order),  # Use factor with desired order
             fill = period)) +
  geom_density_ridges(alpha = 0.7,
                      quantile_lines = TRUE,
                      quantile_fun = function(x, ...) mean(x), 
                      scale = 3, size = 0.7) +
  geom_text(data = mean_dfT %>% filter(shift == 4),
            aes(x = mean_percent_change,
                y = factor(period, levels = desired_order),  # Use factor with desired order
                label = as.character(round(mean_percent_change, digits = 0))),
            color = "black",
            size = 4,
            vjust = 2) +
  geom_text(data = mean_dfT %>% filter(shift == 1),
            aes(x = -200,
                y = factor(period, levels = desired_order),  # Use factor with desired order
                label = paste("n = ", number_percent_change, sep = "")),
            color = "black",
            size = 4,
            vjust = 2) +
  ylab("") +
  xlab("% change in surface chlorophyll-a")+
  labs(title = "Tuesday") +
  scale_fill_manual(values = c("during heatwave" = "#ff0000", "after heatwave" = "#ffc100", "all other days" = "#88CCEE")) +  # Specify fill colors for groups
  theme_classic()




history = read.csv("./formatted data/manipulation_history.csv")






#### Science in the northwoods talk ####


# Create a factor variable with the desired order for 'period'
desired_order <- c("after heatwave", "during heatwave", "all other days")

png("./figures/science in the northwoods figures/Tuesday Lake heatwave results 4 days after.png", height = 7, width = 13, units = "in", res = 600)

allSlopes %>%
  filter(lake == "T") %>%
  filter(period != "exclude after heatwave", shift == 4) %>%
  ggplot(aes(x = percent_change,
             y = factor(period, levels = desired_order),  # Use factor with desired order
             fill = period)) +
  geom_density_ridges(alpha = 0.7,
                      quantile_lines = TRUE,
                      quantile_fun = function(x, ...) mean(x), 
                      scale = 2, size = 0.7) +
  # geom_text(data = mean_dfT %>% filter(shift == 4),
  #           aes(x = mean_percent_change,
  #               y = factor(period, levels = desired_order),  # Use factor with desired order
  #               label = as.character(round(mean_percent_change, digits = 0))),
  #           color = "black",
  #           size = 4,
  #           vjust = 2) +
  geom_text(data = mean_dfT %>% filter(shift == 4),
            aes(x = -200,
                y = factor(period, levels = desired_order),  # Use factor with desired order
                label = paste("n = ", number_percent_change, sep = "")),
            color = "black",
            size = 6,
            vjust = 2) +
  ylab("") +
  xlab("% change in surface chlorophyll-a")+
  labs(title = "Tuesday") +
  scale_fill_manual(values = c("during heatwave" = "#ff0000", "after heatwave" = "#ffc100", "all other days" = "#88CCEE")) +  # Specify fill colors for groups
  theme_classic()+
  theme(axis.text=element_text(size=14),
         axis.title=element_text(size=18,face="bold"))

dev.off()

desired_order <- c("after heatwave", "during heatwave", "all other days")



png("./figures/science in the northwoods figures/Peter Lake heatwave results 4 days after.png", height = 7, width = 13, units = "in", res = 600)


allSlopes %>%
  filter(lake == "R") %>%
  filter(period != "exclude after heatwave", shift == 4) %>%
  ggplot(aes(x = percent_change,
             y = factor(period, levels = desired_order),  # Use factor with desired order
             fill = period)) +
  geom_density_ridges(alpha = 0.7,
                      quantile_lines = TRUE,
                      quantile_fun = function(x, ...) mean(x), 
                      scale = 2, size = 0.7) +
  # geom_text(data = mean_dfR %>% filter(shift == 4),
  #           aes(x = mean_percent_change,
  #               y = factor(period, levels = desired_order),  # Use factor with desired order
  #               label = as.character(round(mean_percent_change, digits = 0))),
  #           color = "black",
  #           size = 4,
  #           vjust = 2) +
  geom_text(data = mean_dfR %>% filter(shift == 4),
            aes(x = -200,
                y = factor(period, levels = desired_order),  # Use factor with desired order
                label = paste("n = ", number_percent_change, sep = "")),
            color = "black",
            size = 4,
            vjust = 2) +
  ylab("") +
  xlab("% change in surface chlorophyll-a")+
  labs(title = "Peter") +
  scale_fill_manual(values = c("during heatwave" = "#ff0000", "after heatwave" = "#ffc100", "all other days" = "#88CCEE")) +  # Specify fill colors for groups
  theme_classic()+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=18,face="bold"))

dev.off()

png("./figures/science in the northwoods figures/Paul Lake heatwave results 4 days after.png", height = 7, width = 13, units = "in", res = 600)


allSlopes %>%
  filter(lake == "L") %>%
  filter(period != "exclude after heatwave", shift == 4) %>%
  ggplot(aes(x = percent_change,
             y = factor(period, levels = desired_order),  # Use factor with desired order
             fill = period)) +
  geom_density_ridges(alpha = 0.7,
                      quantile_lines = TRUE,
                      quantile_fun = function(x, ...) mean(x), 
                      scale = 1.5, size = 0.7) +
  # geom_text(data = mean_dfL %>% filter(shift == 4),
  #           aes(x = mean_percent_change,
  #               y = factor(period, levels = desired_order),  # Use factor with desired order
  #               label = as.character(round(mean_percent_change, digits = 0))),
  #           color = "black",
  #           size = 4,
  #           vjust = 2) +
  geom_text(data = mean_dfL %>% filter(shift == 4),
            aes(x = -200,
                y = factor(period, levels = desired_order),  # Use factor with desired order
                label = paste("n = ", number_percent_change, sep = "")),
            color = "black",
            size = 4,
            vjust = 2) + 
  ylab("") +
  xlab("% change in surface chlorophyll-a")+
  labs(title = "Paul") +
  scale_fill_manual(values = c("during heatwave" = "#ff0000", "after heatwave" = "#ffc100", "all other days" = "#88CCEE")) +  # Specify fill colors for groups
  theme_classic()+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=18,face="bold"))

dev.off()
