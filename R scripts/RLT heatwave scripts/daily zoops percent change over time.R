### calculate the daily percent change in zooplankton across lakes using gravimetric data
### only have this data between 2008 and 2011


#### install packages ####
#install and load slider library for rolling window analysis
if (!require(slider)) install.packages('slider')
library(slider) 

if (!require(dplyr)) install.packages('plyr')
library(dplyr)

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

if (!require(readxl)) install.packages('readxl')
library(readxl)

if (!require(ggpubr)) install.packages('ggpubr')
library(ggpubr)

library(DescTools)

#==============================================================================#
#### levers we can pull ####

# slope and percent calculations
slopeLength = 7 # length of the rolling window slope to be calculated

# build in options here for during the heatwave, or at the beginning of the calculated slope

# slope aggregation choices

daysAfter = 0 # time lag of how many days after the heatwave we want to look
numSlopes = 5 # the number of slopes we want to include in analysis

exclude.after.heatwaves = FALSE # if TRUE, excludes slopes for days within 20 days 
# of the heatwave that don't fall within our aggregating window

# timing choices
# change analysis so that we can look during the heatwave, or after the heatwave? 




#==============================================================================#
#### loop for the whole code to investigate across combinations ####

# # the dataframe looped results will contain the mean values for the specified combinations
# looped.results = data.frame(matrix(nrow = 3024, ncol = 15))
# names(looped.results) = c("slopeLength", "daysAfter", "numSlopes", 
#                           "all.all.other.days", "all.during.heatwave", "all.after.heatwave",
#                           "R.all.other.days", "R.during.heatwave", "R.after.heatwave",
#                           "L.all.other.days", "L.during.heatwave", "L.after.heatwave",
#                           "T.all.other.days", "T.during.heatwave", "T.after.heatwave")
# L = 1
# for(slopeLength in 3:14){
#   for(daysAfter in 0:20){
#     for(numSlopes in 3:14){
#       
#       looped.results$slopeLength[L] = slopeLength
#       looped.results$daysAfter[L] = daysAfter
#       looped.results$numSlopes[L] = numSlopes




#==============================================================================#
#### read in the data ####
# read in the heatwaves data calculated in the previous step
# heatwaves = read.csv("./formatted data/heatwavesdata.csv")
heatwaves = read.csv("./results/heatwave modeled outputs/heatwave events LRT.csv")

# read in the zooplankton data
zoopData = read.csv("./formatted data/zooplankton/gravimetric zooplankton 2024-08-05.csv")

# rename and reformat columns in zoopdata so they match the chlorophyll data
# this will let me run the script on daily zooplankton instead of daily chlorophyll

zoopData = zoopData %>%
  rename(doyCat = DOY) %>% 
  mutate(lake = replace(lake, lake == "Paul", "L")) %>% 
  mutate(lake = replace(lake, lake == "Peter", "R")) %>% 
  mutate(lake_year = paste(lake, year, sep= "_")) %>% 
  mutate(date = as.Date(paste0(year, "-", doyCat), format="%Y-%j"))


# read in the gravimetric zooplankton data
allData = read.csv("./formatted data/interpolated_manual_chl_for_slopes.csv")

# make a vector of unique lake years
lake_years = unique(zoopData$lake_year)

#==============================================================================#
#### CALCULATE SLOPES FOR CHLOROPHYLL ####

# cycle through each lake year, fit linear models for each rolling window, then store model slopes
# in the 'slopes' dataframe. Need to initially store the model results in a list

# then, calculate the percent change in chlorophyll relative to the baseline specified above

# Model results are the slopes, originally calculated based on the preceding 7 days

i = 1
for(i in 1:length(lake_years)){
  
  temp = zoopData %>% filter(lake_year == lake_years[i]) # temp dataframe for this lake_year
  
  models <- slide(
    temp, 
    ~lm(ZBgrav ~ doyCat, data = .x), 
    .before = slopeLength -1, # add the minus one so the slopeLength matches the user input
    .complete = TRUE
  )
  
  temp$zoop_slope = NA
  temp$se = NA
  temp$p_value = NA
  temp$r_squared = NA
  
  for(j in 1:nrow(temp)){
    
    model = models[[j]]
    coef <- coefficients(model) 
    # extracting the coefficients from the current model
    
    if(!(is.null(coef))){
      Slope <- coef["doyCat"] # coefficient explained by day of year
      temp$zoop_slope[j] = Slope # pull out the slope from the model
      temp$p_value[j] = summary(model)$coefficients[2,4] 
      temp$se[j] = summary(model)$coefficients["doyCat", "Std. Error"] 
      temp$r_squared[j] = summary(model)$r.squared # pull out r_squared from model
    }
    
    temp$percent_change = 100*temp$zoop_slope*slopeLength/lag(temp$ZBgrav, slopeLength-1, default = NA)
    temp$chl_baseline = lag(temp$ZBgrav, slopeLength-1, default = NA)
  }
  
  if(i ==1){ slopes = temp} # if first iteration, creates slopes, the final dataframe
  
  if(i >1){slopes = rbind(slopes, temp)} # else, appends to slopes 
  
}


# test case to make sure the slide function works as we think it does
# test = slopes[1:7, ]
# summary(lm(test$ZBgrav~test$doyCat))
# 



#==============================================================================#
#### SELECT SLOPES IN SPECIFIED WINDOW ####
# currently, the slopes dataframe has daily rolling window slopes for all of the lake_year combinations
# Need to select the slopes just following a heatwave
# heatwave is the date of a heatwave, targLake is the lake, data is our dataset of all the calculated slopes

hwSlopes <- function(heatwaveStart, heatwaveEnd,  targLake, data){
  
  startDate = as.Date(heatwaveEnd)+daysAfter 
  endDate = as.Date(heatwaveEnd)+daysAfter+numSlopes-1
  
  # filter out the slopes we are interested in
  selected.slopes = data %>% filter(lake == targLake, date >= startDate, date <= endDate)
  
}


#==============================================================================#
#### CALCULATE AVG CHL SLOPES FOLLOWING HEATWAVES ####
heatwaves$averageSlope = NA
heatwaves$percentChange = NA
heatwaves$sdSlope = NA
heatwaves$sdSlopePercent = NA
heatwaves$medianSlopePercent

#Using a For Loop
lengthHW = nrow(heatwaves)

# take the average slope and calculate the percent change in chlorophyll based on the slope

for(i in 1:lengthHW){
  slopes.subset = hwSlopes(heatwaves$date_start[i], heatwaves$date_end[i], heatwaves$lake[i], slopes)
  
  # save the mean of the slopes to the heatwaves dataframe
  heatwaves$averageSlope[i] = mean(slopes.subset$zoop_slope, na.rm = TRUE)
  
  # save the standard deviation of the slopes to the heatwaves dataframe
  heatwaves$sdSlope[i] = sd(slopes.subset$zoop_slope, na.rm = TRUE)
  
  # save the mean of the percent change to the heatwaves dataframe
  heatwaves$percentChange[i] = mean(slopes.subset$percent_change, na.rm = TRUE)
  
  # save the standard deviation of the percent changes to the heatwaves dataframe
  heatwaves$sdSlopePercent[i] = sd(slopes.subset$percent_change, na.rm = TRUE)
  
  # save the median of the percent changes to the heatwaves dataframe
  heatwaves$medianSlopePercent[i] = median(slopes.subset$percent_change, na.rm = TRUE)
  
}

# save the static results
# write.csv(heatwaves, "./results/heatwaves_with_average_slopes_MANUAL_CHL.csv", row.names = FALSE)




#==============================================================================#
#### PLOT AVG SLOPES ####
#results = read.csv("./results/heatwaves_with_average_slopes_MANUAL_CHL.csv")


results = heatwaves

#break up by lake by filtering data
resultsT = results %>% filter(lake == "T")
resultsL = results %>% filter(lake == "L")
resultsR = results %>% filter(lake == "R")

# make sure heatwaves date is a date
heatwaves = heatwaves %>% mutate(date_start = as.Date(date_start))


#png(filename = "./figures/preliminary figures/R_slopes_2023_02_09", height = 8, width = 11, units = "in", res = 300)

indHWResp <- ggplot(data = results, aes(x = as.character(date_start), y = percentChange, fill = lake))+
  geom_bar(stat = "identity", position = "identity", alpha = 0.5, color = "black")+
  theme_classic()+  
  ylab("percent change in chl")+
  xlab("start date of heatwave")+
  ggtitle("Average chl percent change by event")+
  scale_fill_manual(values = c("R"=  "#4AB5C4", "L" = "#ADDAE3", "T"=  "#BAAD8D"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  annotate("text",  x=Inf, y = Inf, label = paste("Days after heatwave: ", daysAfter, sep = ""), vjust=1, hjust=1)+
  annotate("text",  x=Inf, y = Inf, label = paste("Number of slopes averaged per event: ", numSlopes, sep = ""), vjust=2.5, hjust=1)


#write.csv(heatwaves, "./formatted data/explanatory variables heatwaves/heatwaves with percent.csv")


#==============================================================================#
#### COMPARE DISTRIBUTIONS OVER TIME ####

# We might also consider the distributions of chlorophyll slopes following a heatwave,
# rather than just taking the average for a certain window

# make sure heatwaves date_start and date_end are formatted as dates
heatwaves$date_end = as.Date(heatwaves$date_end)
heatwaves$date_start = as.Date(heatwaves$date_start)
slopes$date = as.Date(slopes$date)
slopes$period = "all other days"

# write slopes to a file
#write.csv(slopes, "./formatted data/slopes_3day.csv", row.names = FALSE)

### This creates a dataframe, all slopes, that has the slopes categorized
# by their time period (during or after a heatwave, or all other days)
# and includes the number of days after the heatwave event (daysAfterLoop) that is considered
# one column, "exclude after heatwaves," are slopes within 40 days after a heatwave
# that are not in the current rolling window

for(daysAfterLoop in -14:40){
  
  # add a column to slopes which indicates whether or not there is a heatwave
  for(i in 1:nrow(heatwaves)){
    start = heatwaves$date_start[i] # start date of the current heatwave
    end = heatwaves$date_end[i] # end date of the current heatwave
    
    # sequence of dates during the heatwave
    dates = seq(start, end, 1)
    
    # excludes dates that are part of rolling window but not currently considered in after heatwave
    # dates that are within 40 days after a heatwave but not in our window for analysis as
    # after heatwave because of our current daysAfterLoop selection.
    # This gets overwrritten in part by "after heatwave" for those dates that are included
    # in analysis
    datesExcluded = seq(end -7, end + 40, 1) 
    
    # dates to be analyzed after the heatwaves
    # this creates a sequence of numbers, from the end of the heatwave plus whatever
    # daysAfterLoop is set to, to that same value plus the numSlopes. So it sets a window daysAfterLooped
    # X number of days after the heatwave
    
    datesAnalyzed = seq(end+daysAfterLoop, end + daysAfterLoop+numSlopes-1, 1)
    
    # fill the dataframe "period" column with the correct categorization
    
    # fill the dataframe "period" column with the correct categorization
    slopes = slopes %>% mutate(period = replace(period, date %in% datesExcluded, "exclude after heatwave"))
    slopes = slopes %>% mutate(period = replace(period, date %in% dates, "during heatwave"))
    slopes = slopes %>% mutate(period = replace(period, date %in% datesAnalyzed, "after heatwave"))
    
    slopes$daysAfter = daysAfterLoop
  }
  
  # combine to the main dataframe
  if(daysAfterLoop == -14){
    allSlopes = slopes
  }
  if(daysAfterLoop > -14){
    allSlopes = rbind(allSlopes, slopes)
  }
  
}

# update so that we are not excluding other dates after the heatwave
allSlopes = allSlopes %>% mutate(period = replace(period, period == "exclude after heatwave", "all other days")) %>%
  mutate(period = replace(period, period == "during heatwave", "all other days"))


# save the allPercent data to a dataframe
# save interpolated allData data to a dataframe

# write.csv(allData, "./formatted data/allData_interpolated.csv", row.names = FALSE)
# write.csv(allPercent, "./formatted data/results_random_and_heatwaves.csv")

# calculate mean values by period and daysAfter
# across all lakes, this will give the mean slope following all heatwave events
# by time period

mean_df <- allSlopes %>% 
  filter(period != "exclude after heatwave", !is.na(percent_change)) %>% 
  group_by(period, daysAfter) %>% 
  dplyr::summarise(mean_percent_change = mean(percent_change)) 

# round mean values to one decimal place
mean_df$mean_percent_change <- round(mean_df$mean_percent_change, 1)


allSlopes = allSlopes %>% distinct()

#==============================================================================#
#### RESPONSE TIMING LINE PLOTS ####

# calculate mean values of percent change for each lake by # of days after heatwave
mean_df <- allSlopes %>%  filter(year %in% c(2008, 2009, 2010, 2011)) %>% 
  filter( period != "exclude after heatwave", !is.na(percent_change)) %>% 
  group_by(period, daysAfter) %>% 
  dplyr::summarise(mean_percent_change = mean(percent_change), 
                   sd_percent_change = sd(percent_change), 
                   number_percent_change = n(),
                   median_percent_change = median(percent_change)) 

# Peter
mean_dfR <- allSlopes %>% filter(lake == "R") %>%  filter(year %in% c(2008, 2009, 2010, 2011)) %>% 
  filter(period != "exclude after heatwave", !is.na(percent_change)) %>% 
  group_by(period, daysAfter) %>% 
  dplyr::summarise(mean_percent_change = mean(percent_change), 
                   sd_percent_change = sd(percent_change), 
                   number_percent_change = n()) 

# Paul
mean_dfL <- allSlopes %>% filter(lake == "L") %>%  filter(year %in% c(2008, 2009, 2010, 2011)) %>% 
  filter( period != "exclude after heatwave", !is.na(percent_change)) %>% 
  group_by(period, daysAfter) %>% 
  dplyr::summarise(mean_percent_change = mean(percent_change), 
                   sd_percent_change = sd(percent_change), 
                   number_percent_change = n()) 

# Tuesday
mean_dfT <- allSlopes %>% filter(lake == "T") %>% filter(year %in% c(2008, 2009, 2010, 2011)) %>% 
  filter( period != "exclude after heatwave", !is.na(percent_change)) %>% 
  group_by(period, daysAfter) %>% 
  dplyr::summarise(mean_percent_change = mean(percent_change), 
                   sd_percent_change = sd(percent_change), 
                   number_percent_change = n()) 

#==============================================================================#
#==============================================================================#
#### END OF FOR LOOPS ####
# 
# # save results to the looped dataframe
# # being careful tp specify daysAfter as the global environment variable
# # and not the column
# 
# looped.results$all.all.other.days[L] = mean_df %>%
#   filter(daysAfter == get("daysAfter", envir=globalenv()) & period == "all other days") %>% 
#   pull(mean_percent_change)
# 
# looped.results$all.during.heatwave[L] = mean_df %>%
#           filter(daysAfter == get("daysAfter", envir=globalenv()) & period == "during heatwave") %>% 
#           pull(mean_percent_change)
# 
# looped.results$all.after.heatwave[L] = mean_df %>%
#   filter(daysAfter == get("daysAfter", envir=globalenv()) & period == "after heatwave") %>% 
#   pull(mean_percent_change)
# 
# # results for R
# looped.results$R.all.other.days[L] = mean_dfR %>%
#   filter(daysAfter == get("daysAfter", envir=globalenv()) & period == "all other days") %>% 
#   pull(mean_percent_change)
# 
# looped.results$R.during.heatwave[L] = mean_dfR %>%
#   filter(daysAfter == get("daysAfter", envir=globalenv()) & period == "during heatwave") %>% 
#   pull(mean_percent_change)
# 
# looped.results$R.after.heatwave[L] = mean_dfR %>%
#   filter(daysAfter == get("daysAfter", envir=globalenv()) & period == "after heatwave") %>% 
#   pull(mean_percent_change)
# 
# # results for L
# looped.results$L.all.other.days[L] = mean_dfL %>%
#   filter(daysAfter == get("daysAfter", envir=globalenv()) & period == "all other days") %>% 
#   pull(mean_percent_change)
# 
# looped.results$L.during.heatwave[L] = mean_dfL %>%
#   filter(daysAfter == get("daysAfter", envir=globalenv()) & period == "during heatwave") %>% 
#   pull(mean_percent_change)
# 
# looped.results$L.after.heatwave[L] = mean_dfL %>%
#   filter(daysAfter == get("daysAfter", envir=globalenv()) & period == "after heatwave") %>% 
#   pull(mean_percent_change)
# 
# # results for T
# looped.results$T.all.other.days[L] = mean_dfT %>%
#   filter(daysAfter == get("daysAfter", envir=globalenv()) & period == "all other days") %>% 
#   pull(mean_percent_change)
# 
# looped.results$T.during.heatwave[L] = mean_dfT %>%
#   filter(daysAfter == get("daysAfter", envir=globalenv()) & period == "during heatwave") %>% 
#   pull(mean_percent_change)
# 
# looped.results$T.after.heatwave[L] = mean_dfT %>%
#   filter(daysAfter == get("daysAfter", envir=globalenv()) & period == "after heatwave") %>% 
#   pull(mean_percent_change)
# 
# print(L)
# L = L+1
# 
#     }
#   }
# }
#==============================================================================#
#==============================================================================#

# combine individual estimates of the mean so we can standardize axes
mean.all.by.lake = rbind(mean_dfR, mean_dfL)
mean.all.by.lake = rbind(mean.all.by.lake, mean_dfT)

# mean.all.other.days = mean_df %>% filter(period == )

# Plot the response over time
All.over.time <- mean_df %>% filter(period == "after heatwave") %>% 
  ggplot(aes( x= daysAfter, y = mean_percent_change, color = period))+
  # scale_color_manual(values = c("during heatwave" = "#ff0000", "after heatwave" = "forestgreen", "all other days" = "#88CCEE"))+
  geom_rect(aes(xmin = -7, xmax = 0, ymin = -Inf, ymax = Inf), 
            fill = "red", alpha = 0.3) +
  #scale_fill_manual(values = c("during heatwave" = "#ff0000", "after heatwave" = "#ffc100", "all other days" = "#88CCEE"))+
  geom_line(size = 1)+
  geom_point()+
  labs(title = "All lakes HW response over time")+
  ylim(min(mean.all.by.lake$mean_percent_change), max(mean.all.by.lake$mean_percent_change)-30)+
  #geom_ribbon(aes(ymin = mean_percent_change - sd_percent_change, ymax = mean_percent_change + sd_percent_change, fill = period), alpha = 0.1)+
  labs(x = "days after heatwave")+
  theme_classic()+
  geom_hline(yintercept = 0, linetype = "dashed")

R.over.time <- mean_dfR %>% 
  ggplot(aes( x= daysAfter, y = mean_percent_change, color = period))+
  annotate("rect", xmin=-7, xmax=0, ymin=-Inf, ymax=Inf, alpha=0.2, fill="red") +
  geom_line(size = 1)+
  geom_point()+
  labs(x = "days after heatwave")+
  labs(title = "Peter Lake HW response over time")+
  scale_color_manual(values = c("during heatwave" = "#ff0000", "after heatwave" = "#ffc100", "all other days" = "#88CCEE"))+
  ylim(min(mean.all.by.lake$mean_percent_change)-10, max(mean.all.by.lake$mean_percent_change)+10)+
  theme_classic()

L.over.time <- mean_dfL  %>% 
  ggplot(aes( x= daysAfter, y = mean_percent_change, color = period))+
  annotate("rect", xmin=-7, xmax=0, ymin=-Inf, ymax=Inf, alpha=0.2, fill="red") +
  geom_line(size = 1)+
  geom_point()+
  labs(x = "days after heatwave")+
  labs(title = "Paul Lake HW response over time")+
  ylim(min(mean.all.by.lake$mean_percent_change)-10, max(mean.all.by.lake$mean_percent_change)-30)+
  scale_color_manual(values = c("during heatwave" = "#ff0000", "after heatwave" = "#ffc100", "all other days" = "#88CCEE"))+
  theme_classic()

T.over.time <- mean_dfT  %>% 
  ggplot(aes( x= daysAfter - 7, y = mean_percent_change, color = period))+
  scale_color_manual(values = c("during heatwave" = "#ff0000", "after heatwave" = "#ffc100", "all other days" = "#88CCEE"))+
  #scale_fill_manual(values = c("during heatwave" = "#ff0000", "after heatwave" = "#ffc100", "all other days" = "#88CCEE"))+
  geom_line(size = 1)+
  geom_point()+
  labs(title = "Tuesday Lake HW response over time")+
  ylim(min(mean.all.by.lake$mean_percent_change)-10, max(mean.all.by.lake$mean_percent_change)+10)+
  #geom_ribbon(aes(ymin = mean_percent_change - sd_percent_change, ymax = mean_percent_change + sd_percent_change, fill = period), alpha = 0.1)+
  labs(x = "days after heatwave")+
  theme_classic()



#=============================================================================#
#### combined plot over time ####
mean_dfT = mean_dfT %>% mutate(lake = "T")
mean_dfR = mean_dfR %>% mutate(lake = "R")
mean_dfL = mean_dfL %>% mutate(lake = "L")
mean_df = mean_df %>% mutate(lake = "all")

mean.all = rbind(mean_dfL, mean_dfR, mean_df) %>% filter(period !=  "all other days")

mean.all  %>% filter(lake != "all") %>% 
  ggplot(aes( x= daysAfter, y = mean_percent_change, color = lake))+
  #scale_color_manual(values = c("during heatwave" = "#ff0000", "after heatwave" = "#ffc100", "all other days" = "#88CCEE"))+
  #scale_fill_manual(values = c("during heatwave" = "#ff0000", "after heatwave" = "#ffc100", "all other days" = "#88CCEE"))+
  annotate("rect", xmin=-5, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  annotate("rect", xmin=-5, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  # annotate("rect", xmin=-5, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  # annotate("rect", xmin=-5, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  # annotate("rect", xmin=-5, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  # annotate("rect", xmin=-5, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  # annotate("rect", xmin=-5, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  # annotate("rect", xmin=-5, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  # annotate("rect", xmin=-5, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  # annotate("rect", xmin=-5, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  # annotate("rect", xmin=-5, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  # annotate("rect", xmin=-6, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  # annotate("rect", xmin=-6, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
# annotate("rect", xmin=-7, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
# annotate("rect", xmin=-7, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
# annotate("rect", xmin=-7, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
annotate("rect", xmin=-7, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  annotate("rect", xmin=-7, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  annotate("rect", xmin=-7, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  annotate("rect", xmin=-7, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  # annotate("rect", xmin=-8, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  # annotate("rect", xmin=-8, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  # annotate("rect", xmin=-8, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  # annotate("rect", xmin=-8, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  # annotate("rect", xmin=-9, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  # annotate("rect", xmin=-9, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  annotate("rect", xmin=-10, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  annotate("rect", xmin=-10, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  annotate("rect", xmin=-10, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  annotate("rect", xmin=-10, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  annotate("rect", xmin=-13, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  annotate("rect", xmin=-13, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  annotate("rect", xmin=-15, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  annotate("rect", xmin=-16, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  geom_line(size = 1.5)+
  geom_point(size = 2)+
  labs(title = "response of zooplankton to HW over time")+
  ylim(min(mean.all.by.lake$mean_percent_change)-10, max(mean.all.by.lake$mean_percent_change)+10)+
  #geom_ribbon(aes(ymin = mean_percent_change - sd_percent_change, ymax = mean_percent_change + sd_percent_change, fill = period), alpha = 0.1)+
  labs(x = "days after heatwave")+
  theme_classic()+
  scale_color_manual(values = c("R"=  "#4AB5C4", "L" = "#ADDAE3", "T"=  "#BAAD8D", "all" = "grey"))+
  geom_hline(yintercept = 0, linetype = "dashed")



### save the mean dataset
write.csv(mean.all, "./results/response over time/grav zooplankton response over time.csv", row.names = FALSE)


