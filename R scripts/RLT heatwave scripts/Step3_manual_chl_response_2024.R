# Re-run step 3 but with the manual chlorophyll data
# updated in January 2024 to be cleaned up and allow for more flexibility

# manual chlorophyll better represents phytoplankton

# this code is also updated to fix a bug in the slope length,
# where slope length is inclusive of the current day in the rolling window

# step 3, calculate the response of chl to the heatwaves

#### install packages ####
#install and load slider library for rolling window analysis
if (!require(slider)) install.packages('slider')
library(slider) 

library(scales)

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

if (!require(ggborderline)) install.packages('ggborderline')
library(ggborderline)

if (!require(ggallin)) install.packages('ggallin')
library(ggallin)

library(DescTools)

#==============================================================================#
#### levers we can pull ####

# slope and percent calculations
slopeLength = 8 # length of the rolling window slope to be calculated

# build in options here for during the heatwave, or at the beginning of the calculated slope

# slope aggregation choices

numSlopes = 1 # the number of slopes we want to include in analysis

daysAfter = 3 # time lag of how many days after the heatwave we want to look

# create a separate daysAfter variable for Tuesday because the analysis peaks one day
# earlier than Peter and Paul
daysAfterT = 2

exclude.after.heatwaves = FALSE # if TRUE, excludes slopes for days within 20 days 
# of the heatwave that don't fall within our aggregating window

relative.to.hw = "end"


# timing choices
# change analysis so that we can look during the heatwave, or after the heatwave? 


#==============================================================================#
#### output of settings ####

cur_date_time = format(Sys.time(), "%Y-%m-%d %H:%M:%S")

plot_metadata <- data.frame(slopeLength = paste("Length of each slope: ", slopeLength, sep = ""),
                             time = paste("Plots generated ", cur_date_time, sep = ""), 
                            daysAfter = paste("Number of days after heatwave: ", daysAfter, sep = ""),
                            numSlopes = paste("Number of slopes aggregated per event: ", numSlopes, sep = ""),
                            exclude.after.heatwaves = paste("Exclude other days after heatwaves? ", exclude.after.heatwaves, sep = ""))



# Create a ggplot with the metadata for the data analysis and plot production, 
# including all of the choices made above
metadata_plot <- ggplot() +
  theme_void() +  # Removes axes and other elements
  geom_text(data = plot_metadata, aes(x = 5, y = 5, label = slopeLength),
            size = 5, hjust = 0.5, vjust = -14)+
  geom_text(data = plot_metadata, aes(x = 5, y = 5, label = daysAfter),
            size = 5, hjust = 0.5, vjust = -8)+
  geom_text(data = plot_metadata, aes(x = 5, y = 5, label = numSlopes),
            size = 5, hjust = 0.5, vjust = -6)+
  geom_text(data = plot_metadata, aes(x = 5, y = 5, label = exclude.after.heatwaves),
            size = 5, hjust = 0.5, vjust = -4)+
  geom_text(data = plot_metadata, aes(x = 5, y = 5, label = time),
            size = 5, hjust = 0.5, vjust = 14)
                  

#==============================================================================#
#### loop for the whole code to investigate across combinations ####

# 
# # # the dataframe looped results will contain the mean values for the specified combinations
# looped.results = data.frame(matrix(nrow = 4320, ncol = 16))
# names(looped.results) = c("slopeLength", "daysAfter", "numSlopes", "orientation",
#                           "all.all.other.days", "all.during.heatwave", "all.after.heatwave",
#                           "R.all.other.days", "R.during.heatwave", "R.after.heatwave",
#                           "L.all.other.days", "L.during.heatwave", "L.after.heatwave",
#                           "T.all.other.days", "T.during.heatwave", "T.after.heatwave")
# 
# rel.hw = c("start", "end")

#==============================================================================#
#### read in the data ####
# read in the heatwaves data calculated in the previous step
# heatwaves = read.csv("./formatted data/heatwavesdata.csv")
heatwaves = read.csv("./results/heatwave modeled outputs/heatwave events LRT.csv")

#permanent.heatwaves = heatwaves
# read in the manual chl data
allData = read.csv("./formatted data/interpolated_manual_chl_for_slopes.csv")

# make a vector of unique lake years
lake_years = unique(allData$lake_year)

# 
# L = 1
# for(slopeLength in 3:8){
#   for(daysAfter in -15:20){
#     for(numSlopes in 1:10){
#       for(relative.to.hw in rel.hw){
#       
# 
#       looped.results$slopeLength[L] = slopeLength
#       looped.results$daysAfter[L] = daysAfter
#       looped.results$numSlopes[L] = numSlopes
#       looped.results$orientation[L] = relative.to.hw
# 

#heatwaves = permanent.heatwaves


#==============================================================================#
#### CALCULATE SLOPES FOR CHLOROPHYLL ####

# cycle through each lake year, fit linear models for each rolling window, then store model slopes
# in the 'slopes' dataframe. Need to initially store the model results in a list

# then, calculate the percent change in chlorophyll relative to the baseline specified above

# Model results are the slopes, originally calculated based on the preceding 8 days

for(i in 1:length(lake_years)){
  
  temp = allData %>% filter(lake_year == lake_years[i]) # temp dataframe for this lake_year
  
  models <- slide(
    temp, 
    ~lm(mean_chl ~ doyCat, data = .x), 
    .before = slopeLength -1, # add the minus one so the slopeLength matches the user input
    .complete = TRUE
  )
  
  temp$chl_slope = NA
  temp$chl_intercept = NA
  temp$se = NA
  temp$p_value = NA
  temp$r_squared = NA
  
  for(j in 1:nrow(temp)){
    
    model = models[[j]]
    coef <- coefficients(model) 
    # extracting the coefficients from the current model
    
    if(!(is.null(coef))){
      Slope <- coef["doyCat"]
      Intercept <- coef["(Intercept)"]
      temp$chl_intercept[j] = Intercept
      temp$chl_slope[j] = Slope # pull out the slope from the model
      temp$p_value[j] = summary(model)$coefficients[2,4] 
      temp$se[j] = summary(model)$coefficients["doyCat", "Std. Error"] 
      temp$r_squared[j] = summary(model)$r.squared # pull out r_squared from model
    }
    
    temp$absolute_change = temp$chl_slope*slopeLength
    temp$percent_change = 100*temp$chl_slope*slopeLength/lag(temp$mean_chl, slopeLength-1, default = NA)
    temp$chl_baseline = lag(temp$mean_chl, slopeLength-1, default = NA)
    temp$se.percent = 100*temp$se*slopeLength/lag(temp$mean_chl, slopeLength-1, default = NA)
  }
  
  if(i ==1){ slopes = temp} # if first iteration, creates slopes, the final dataframe
  
  if(i >1){slopes = rbind(slopes, temp)} # else, appends to slopes 
  
}


# test case to make sure the slide function works as we think it does
# test = slopes[1:7, ]
# summary(lm(test$mean_chl~test$doyCat))
# 

#==============================================================================#
# add heatwave information to the slopes dataframe
# the goal is to have the event_no of each heatwave listed just where the slopes
# include data from during the heatwave
# this will help us exclude data from analysis of one heatwave that is from a subsequent event

# the default event code will be -100

slopes = slopes %>% mutate(event_code = -100)
heatwaves = heatwaves %>% mutate(date_start = as.Date(date_start), date_end = as.Date(date_end))

for(i in 1:nrow(heatwaves)){
  
  start = heatwaves$date_start[i] # start date of the current heatwave
  end = heatwaves$date_end[i] # end date of the current heatwave
  event = heatwaves$event_no[i]
  
  # current lake of the heatwave
  hw.lake = heatwaves$lake[i]
  
  # sequence of dates during the heatwave
  dates = seq(start, end+slopeLength-1, 1)
  
  #print(dates)
  
  slopes = slopes %>% mutate(event_code = replace(event_code, date %in% dates, event))
  
  
}

#==============================================================================#
#### SELECT SLOPES IN SPECIFIED WINDOW ####
# currently, the slopes dataframe has daily rolling window slopes for all of the lake_year combinations
# Need to select the slopes just following a heatwave
# heatwave is the date of a heatwave, targLake is the lake, data is our dataset of all the calculated slopes

hwSlopes <- function(heatwaveStart, heatwaveEnd,  targLake, data){
  
  if(targLake == "T"){
  if(relative.to.hw == "end"){
  startDate = as.Date(heatwaveEnd)+daysAfterT 
  endDate = as.Date(heatwaveEnd)+daysAfterT+numSlopes-1
  }
  
  if(relative.to.hw == "start"){
    startDate = as.Date(heatwaveStart)+daysAfterT 
    endDate = as.Date(heatwaveStart)+daysAfterT+numSlopes-1
  }
  
  }
  if(targLake != "T"){
    
    if(relative.to.hw == "end"){
      startDate = as.Date(heatwaveEnd)+daysAfter 
      endDate = as.Date(heatwaveEnd)+daysAfter+numSlopes-1
    }
    
    if(relative.to.hw == "start"){
      startDate = as.Date(heatwaveStart)+daysAfter 
      endDate = as.Date(heatwaveStart)+daysAfter+numSlopes-1
    }
  }
    
  # filter out the slopes we are interested in
  selected.slopes = data %>% filter(lake == targLake, date >= startDate, date <= endDate)
  
}


# ==============================================================================#
 #### CALCULATE AVG CHL SLOPES FOLLOWING HEATWAVES ####
 heatwaves$averageSlope = NA
 heatwaves$intercept = NA
 heatwaves$SE = NA
 heatwaves$SE.percent = NA
 heatwaves$absoluteChange = NA
 heatwaves$percentChange = NA
 heatwaves$sdSlope = NA
 heatwaves$sdSlopePercent = NA
 heatwaves$medianSlopePercent = NA
 heatwaves$num.slopes.included = NA


 #Using a For Loop
 lengthHW = nrow(heatwaves)

 # take the average slope and calculate the percent change in chlorophyll based on the slope

 for(i in 1:lengthHW){
   slopes.subset = hwSlopes(heatwaves$date_start[i], heatwaves$date_end[i], heatwaves$lake[i], slopes)

   # save the mean of the slopes to the heatwaves dataframe
   heatwaves$averageSlope[i] = mean(slopes.subset$chl_slope, na.rm = TRUE)

   heatwaves$absoluteChange[i] = mean(slopes.subset$absolute_change, na.rm = TRUE)
   
   heatwaves$intercept[i] = mean(slopes.subset$chl_intercept, na.rm = TRUE)
   
   heatwaves$SE[i] = mean(slopes.subset$se, na.rm = TRUE)
   
   heatwaves$SE.percent[i] = mean(slopes.subset$se.percent, na.rm = TRUE)
   
   # save the standard deviation of the slopes to the heatwaves dataframe
   heatwaves$sdSlope[i] = sd(slopes.subset$chl_slope, na.rm = TRUE)

   # save the mean of the percent change to the heatwaves dataframe
   heatwaves$percentChange[i] = mean(slopes.subset$percent_change, na.rm = TRUE)

   # save the standard deviation of the percent changes to the heatwaves dataframe
   heatwaves$sdSlopePercent[i] = sd(slopes.subset$percent_change, na.rm = TRUE)

   # save the median of the percent changes to the heatwaves dataframe
   heatwaves$medianSlopePercent[i] = median(slopes.subset$percent_change, na.rm = TRUE)
   
   # save the number of slopes averaged to the heatwaves dataframe
   heatwaves$num.slopes.included[i] = nrow(slopes.subset)
   
   
   # save the slopes dataframe
   if(i == 1 & nrow(slopes.subset)> 0){
     slopes.of.interest = slopes.subset
   }
   if(i > 1 & nrow(slopes.subset)> 0){
     slopes.of.interest = rbind(slopes.of.interest, slopes.subset)
   }

 }

 # save the static results
 #write.csv(heatwaves, "./results/heatwaves_with_average_slopes_MANUAL_CHL.csv", row.names = FALSE)
 
###### CATEGORIZE SLOPES BY NUTRIENT ADDITION STATUS ########

 # create a nutrient addition column
slopes.of.interest = slopes.of.interest %>% mutate(nutrient.addition = "no nutrients added")
 
slopes.of.interest = slopes.of.interest %>% 
  mutate(nutrient.addition = 
  replace(nutrient.addition, year == 2013 & (lake == "R" | lake == "T") & doyCat >= 154, "nutrients added")) %>% 
  mutate(nutrient.addition = 
           replace(nutrient.addition, year == 2014 & (lake == "R" | lake == "T") & doyCat >= 153, "nutrients added")) %>% 
  mutate(nutrient.addition = 
           replace(nutrient.addition, year == 2015 & (lake == "R" | lake == "T") & doyCat >= 152, "nutrients added")) %>% 
  mutate(nutrient.addition = 
           replace(nutrient.addition, year == 2013 & (lake == "R") & doyCat >= 161, "nutrients added"))

  
ggplot(slopes.of.interest, aes(x = nutrient.addition, y = percent_change, fill = nutrient.addition))+
  geom_boxplot()+
  theme_classic()+
  labs(x = "", y = "percent change in chlorophyll")+
  stat_compare_means(method = "t.test")+
  theme(legend.position = "none")+
  scale_fill_manual(values = c("no nutrients added" = "blue", "nutrients added" = "green"))
  


  
 #   # nut.load <- nut.load %>%
 # mutate(daily.load = case_when(
 #   year == 2013 & (lake == "R" | lake == "T") & doy >= 154 ~ 0.5 + 0.3125 * ((doy - 154) %/% 7),
 #   year == 2013 & (lake == "R" | lake == "T") & doy < 154 ~ 0,
 #   year == 2013 & (lake == "R" | lake == "T") & doy > 203 ~ 0.5 + 0.625 * ((doy - 203) %/% 7),
 #   (lake == "R" | lake == "T") & year == 2014 & doy >= 153 & doy <= 241 ~ 3,
 #   (lake == "R") & year == 2015 & doy >= 152 & doy <= 180 ~ 3,
 #   (lake == "T") & year == 2015 & doy >= 152 & doy <= 240 ~ 3,
 #   (lake == "R") & year == 2019 & doy >= 161 & doy <= 237 ~ calculate_daily_load_2019(doy),
 #   TRUE ~ daily.load
 # ))

#slopes.of.interest %>% group_by(lake) %>%  summarize(median(percent_change, na.rm = TRUE))

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
   scale_fill_manual(values = c("R"=  "#60BFCC", "L" = "#D9EEF3", "T"=  "#544C34"))+
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


# need to add dates.to.exclude to slopes

# update to better categorize slopes
# and exclucde from analysis non-target slopes

slopes$period = "all other days"

for(i in 1:nrow(heatwaves)){
 

  start = heatwaves$date_start[i] # start date of the current heatwave
  end = heatwaves$date_end[i] # end date of the current heatwave

  # current lake of the heatwave
  hw.lake = heatwaves$lake[i]
  hw.year = heatwaves$year[i]
  
  # create 
  dates.to.exclude = seq(start, end+8-1, 1)
  
  slopes = slopes %>% mutate(period = replace(period, date %in% dates.to.exclude & lake == hw.lake, "exclude after heatwave"))
  
  
}

slopes.periods = slopes$period

# test case for -15
daysAfterLoop = -15

for(daysAfterLoop in -16:20){
  
  slopes$period = slopes.periods
  slopes$numSlopes = numSlopes
  
  # add a column to slopes which indicates whether or not there is a heatwave
  for(i in 1:nrow(heatwaves)){
    
    start = heatwaves$date_start[i] # start date of the current heatwave
    end = heatwaves$date_end[i] # end date of the current heatwave
    event = heatwaves$event_no[i]
    
    # current lake of the heatwave
    hw.lake = heatwaves$lake[i]
    hw.year = heatwaves$year[i]
    
    # sequence of dates during the heatwave
    dates = seq(start, end, 1)
    
    dates.to.exclude = seq(start, end+8, 1)
    
    
    # numSlopes = length(dates)-3
    
    # excludes dates that are part of rolling window but not currently considered in after heatwave
    # after heatwave because of our current daysAfterLoop selection.
    # This gets overwritten in part by "after heatwave" for those dates that are included
    # in analysis
    #datesExcluded = seq(end -14, end + 40, 1) 
    
    # dates to be analyzed after the heatwaves
    # this creates a sequence of numbers, from the end of the heatwave plus whatever
    # daysAfterLoop is set to, to that same value plus the numSlopes. So it sets a window daysAfterLooped
    # X number of days after the heatwave
    
    if(relative.to.hw == "end"){
      
      datesAnalyzed = seq(end+daysAfterLoop,  end + daysAfterLoop+numSlopes-1, 1)
      
    }
    
    if(relative.to.hw == "start"){
      
      datesAnalyzed = seq(start+daysAfterLoop,  start + daysAfterLoop+numSlopes-1, 1)
      
    }
    
    # fill the dataframe "period" column with the correct categorization
    
    # fill the dataframe "period" column with the correct categorization
    slopes = slopes %>% mutate(period = replace(period, date %in% datesAnalyzed & lake == hw.lake, "after heatwave"))
    # slopes = slopes %>% mutate(period = replace(period, date %in% dates & lake == hw.lake, "during heatwave"))
    # slopes = slopes %>% mutate(period = replace(period, event_code != -100 & event_code != event & lake == hw.lake, "exclude after heatwave"))
    
    slopes$daysAfter = daysAfterLoop
    
    # combine to the main dataframe
    if(daysAfterLoop == -16 & i == 1){
      slopes$event_no = heatwaves$event_no[i]
      allSlopes = slopes %>% filter(lake == hw.lake & year == hw.year)
    }
    if(daysAfterLoop != -16 | i != 1){
      slopes$event_no = heatwaves$event_no[i]
      allSlopes = rbind(allSlopes, slopes %>% filter(lake == hw.lake & year == hw.year))
    }
    
    print(daysAfterLoop)
    print(i)
    
  }
  
  
  
}






# write.csv(allSlopes, "./R scripts/RLT heatwave scripts/bar chart shiny app/slopes.csv")

#allSlopes.after = allSlopes %>% filter(period == "after heatwave")


# update so that we are not excluding other dates after the heatwave
# allSlopes = allSlopes %>% mutate(period = replace(period, period == "exclude after heatwave", "all other days"))
  #mutate(period = replace(period, period == "during heatwave", "all other days"))


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
  dplyr::summarise(mean_percent_change = mean(percent_change), n.included = n()) 

# round mean values to one decimal place
mean_df$mean_percent_change <- round(mean_df$mean_percent_change, 1)

allSlopes = allSlopes %>% select(-event_no)

allSlopes = allSlopes %>% distinct()

# # clean to get rid of pseudoreplication
# collapsed_all_other_days <- allSlopes %>%
#   filter(period == "all other days") %>%
#   group_by(across(-c(daysAfter))) %>%
#   summarise(dayAfter = first(daysAfter), event_no = first(event_no), .groups = "drop")
# 
# # Keep rows where period is not "all other days"
# other_periods <- allSlopes %>%
#   filter(period != "all other days")
# 
# # Combine the two parts back together
# allSlopes_cleaned <- bind_rows(other_periods, collapsed_all_other_days)
# 
# # View the cleaned dataframe
# head(allSlopes_cleaned)
# 
# allSlopes = allSlopes_cleaned

#==============================================================================#
#### RESPONSE TIMING LINE PLOTS ####

# track down which of the heatwaves is not included when aggregated
test = allSlopes %>% filter(daysAfter == 3 & period == "after heatwave")
test.0 = allSlopes %>% filter(daysAfter == 0 & period == "after heatwave")

test = test %>% mutate(unique.id = paste(lake, year, as.Date(doyCat - 1, origin = paste0(year, "-01-01"))))
test.0 = test.0 %>%  mutate(unique.id = paste(lake, year, as.Date(doyCat +2, origin = paste0(year, "-01-01"))))

test.id = unique(test$unique.id)
test0.id = unique(test.0$unique.id)

which(!(test0.id %in% test.id))

test0.id[which(!(test0.id %in% test.id))]

slopes13 = slopes %>% filter(year == 2013)
# the last day of year in 2013 is 244. So why does the individual response work?

# calculate mean values of percent change for each lake by # of days after heatwave
mean_df <- allSlopes %>%  #filter(year %in% c(2008, 2009, 2010, 2011)) %>% 
  filter( period != "exclude after heatwave", !is.na(percent_change)) %>% 
  group_by(period, daysAfter) %>% 
  dplyr::summarise(mean_percent_change = mean(percent_change), 
                   sd_percent_change = sd(percent_change), 
                   number_percent_change = n(),
                   median_percent_change = median(percent_change, na.rm = TRUE)) 

# Peter
mean_dfR = allSlopes %>% filter(lake == "R") %>%  
  filter(period != "exclude after heatwave", !is.na(percent_change)) %>% 
  group_by(period, daysAfter) %>% 
  dplyr::summarise(mean_percent_change = mean(percent_change), 
                   sd_percent_change = sd(percent_change), 
                   number_percent_change = n(),
                   median_percent_change = median(percent_change, na.rm = TRUE) )

# Paul
mean_dfL <- allSlopes %>% filter(lake == "L") %>%  #filter(year %in% c(2008, 2009, 2010, 2011)) %>% 
  filter( period != "exclude after heatwave", !is.na(percent_change)) %>% 
  group_by(period, daysAfter) %>% 
  dplyr::summarise(mean_percent_change = mean(percent_change), 
                   sd_percent_change = sd(percent_change), 
                   number_percent_change = n(),
                   median_percent_change = median(percent_change, na.rm = TRUE)) 

# Tuesday
mean_dfT <- allSlopes %>% filter(lake == "T") %>% # filter(year %in% c(2008, 2009, 2010, 2011)) %>% 
  filter( period != "exclude after heatwave", !is.na(percent_change)) %>% 
  group_by(period, daysAfter) %>% 
  dplyr::summarise(mean_percent_change = mean(percent_change), 
                   sd_percent_change = sd(percent_change), 
                   number_percent_change = n(),
                   median_percent_change = median(percent_change, na.rm = TRUE)) 




### version with distinct rows only used to calculate means ###
# mean_dfL <- allSlopes %>%
#   filter(lake == "L") %>%
#   filter(period != "exclude after heatwave", !is.na(percent_change)) %>%
#   group_by(period, daysAfter) %>%
#   summarise(
#     mean_percent_change = mean(distinct(across(-c(daysAfter, event_no)))$percent_change, na.rm = TRUE),
#     sd_percent_change = sd(distinct(across(-c(daysAfter, event_no)))$percent_change, na.rm = TRUE),
#     number_percent_change = n_distinct(percent_change),
#     median_percent_change = median(distinct(across(-c(daysAfter, event_no)))$percent_change, na.rm = TRUE)
#   )
# 
# 
# 
# mean_dfR <- allSlopes %>%
#   filter(lake == "R") %>%
#   filter(period != "exclude after heatwave", !is.na(percent_change)) %>%
#   group_by(period, daysAfter) %>%
#   summarise(
#     mean_percent_change = mean(distinct(across(-c(daysAfter, event_no)))$percent_change, na.rm = TRUE),
#     sd_percent_change = sd(distinct(across(-c(daysAfter, event_no)))$percent_change, na.rm = TRUE),
#     number_percent_change = n_distinct(percent_change),
#     median_percent_change = median(distinct(across(-c(daysAfter, event_no)))$percent_change, na.rm = TRUE)
#   )
# 
# mean_dfT <- allSlopes %>%
#   filter(lake == "T") %>%
#   filter(period != "exclude after heatwave", !is.na(percent_change)) %>%
#   group_by(period, daysAfter) %>%
#   summarise(
#     mean_percent_change = mean(distinct(across(-c(daysAfter, event_no)))$percent_change, na.rm = TRUE),
#     sd_percent_change = sd(distinct(across(-c(daysAfter, event_no)))$percent_change, na.rm = TRUE),
#     number_percent_change = n_distinct(percent_change),
#     median_percent_change = median(distinct(across(-c(daysAfter, event_no)))$percent_change, na.rm = TRUE)
#   )
# 
# 
# 
# mean_dfT <- allSlopes %>%
#   filter(lake == "T") %>%
#   filter(period != "exclude after heatwave", !is.na(percent_change)) %>%
#   group_by(period, daysAfter) %>%
#   summarise(
#     distinct_rows = distinct(across(-c(daysAfter, event_no))), # Get distinct rows for each group
#     mean_percent_change = mean(distinct_rows$percent_change, na.rm = TRUE),
#     sd_percent_change = sd(distinct_rows$percent_change, na.rm = TRUE),
#     number_percent_change = n_distinct(distinct_rows$percent_change),
#     median_percent_change = median(distinct_rows$percent_change, na.rm = TRUE)
#   ) %>%
#   ungroup()
# 
# 
# 
# mean_dfR <- allSlopes %>%
#   filter(lake == "R") %>%
#   filter(period != "exclude after heatwave", !is.na(percent_change)) %>%
#   group_by(period, daysAfter) %>%
#   summarise(
#     distinct_rows = distinct(across(-c(daysAfter, event_no))), # Get distinct rows for each group
#     mean_percent_change = mean(distinct_rows$percent_change, na.rm = TRUE),
#     sd_percent_change = sd(distinct_rows$percent_change, na.rm = TRUE),
#     number_percent_change = n_distinct(distinct_rows$percent_change),
#     median_percent_change = median(distinct_rows$percent_change, na.rm = TRUE)
#   ) %>%
#   ungroup()



#==============================================================================#
#==============================================================================#
# #### END OF FOR LOOPS ####
# 
# # save results to the looped dataframe
# # being careful tp specify daysAfter as the global environment variable
# # and not the column
# 
# looped.results$all.all.other.days[L] = mean_df %>%
#   filter(daysAfter == get("daysAfter", envir=globalenv()) & period == "all other days") %>%
#   pull(mean_percent_change)
# 
# # looped.results$all.during.heatwave[L] = mean_df %>%
# #           filter(daysAfter == get("daysAfter", envir=globalenv()) & period == "after heatwave") %>%
# #           pull(mean_percent_change)
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
# # looped.results$R.during.heatwave[L] = mean_dfR %>%
# #   filter(daysAfter == get("daysAfter", envir=globalenv()) & period == "after heatwave") %>%
# #   pull(mean_percent_change)
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
# # looped.results$L.during.heatwave[L] = mean_dfL %>%
# #   filter(daysAfter == get("daysAfter", envir=globalenv()) & period == "after heatwave") %>%
# #   pull(mean_percent_change)
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
# # looped.results$T.during.heatwave[L] = mean_dfT %>%
# #   filter(daysAfter == get("daysAfter", envir=globalenv()) & period == "after heatwave") %>%
# #   pull(mean_percent_change)
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
  geom_rect(aes(xmin = -16, xmax = 0, ymin = -Inf, ymax = Inf), 
            fill = "red", alpha = 0.3) +
  #scale_fill_manual(values = c("during heatwave" = "#ff0000", "after heatwave" = "#ffc100", "all other days" = "#88CCEE"))+
  geom_line(size = 1)+
  geom_point()+
  labs(title = "All lakes HW response over time")+
 # ylim(min(mean.all.by.lake$mean_percent_change), max(mean.all.by.lake$mean_percent_change)-30)+
 #geom_ribbon(aes(ymin = mean_percent_change - sd_percent_change, ymax = mean_percent_change + sd_percent_change, fill = period), alpha = 0.1)+
  labs(x = "days after heatwave")+
theme_classic()+
  geom_hline(yintercept = 0, linetype = "dashed")

R.over.time <- mean_dfR %>% 
  ggplot(aes( x= daysAfter, y = mean_percent_change, color = period))+
  annotate("rect", xmin=-16, xmax=0, ymin=-Inf, ymax=Inf, alpha=0.2, fill="red") +
  geom_line(size = 1)+
  geom_point()+
  labs(x = "days after heatwave")+
  labs(title = "Peter Lake HW response over time")+
  scale_color_manual(values = c("during heatwave" = "#ff0000", "after heatwave" = "#ffc100", "all other days" = "#88CCEE"))+
  ylim(min(mean.all.by.lake$mean_percent_change)-10, max(mean.all.by.lake$mean_percent_change)+10)+
  theme_classic()

L.over.time <- mean_dfL  %>% 
  ggplot(aes( x= daysAfter, y = mean_percent_change, color = period))+
  annotate("rect", xmin=-16, xmax=0, ymin=-Inf, ymax=Inf, alpha=0.2, fill="red") +
  geom_line(size = 1)+
  geom_point()+
  labs(x = "days after heatwave")+
  labs(title = "Paul Lake HW response over time")+
  #ylim(min(mean.all.by.lake$mean_percent_change)-10, max(mean.all.by.lake$mean_percent_change)-30)+
  scale_color_manual(values = c("during heatwave" = "#ff0000", "after heatwave" = "#ffc100", "all other days" = "#88CCEE"))+
  theme_classic()

T.over.time <- mean_dfT  %>% 
  ggplot(aes( x= daysAfter - 16, y = mean_percent_change, color = period))+
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

mean.all = rbind(mean_dfL, mean_dfR, mean_dfT, mean_df) %>% filter(period == "after heatwave")

mean.all  %>% filter(lake != "all") %>% 
  ggplot(aes( x= daysAfter, y = mean_percent_change, color = lake))+
  #scale_color_manual(values = c("during heatwave" = "#ff0000", "after heatwave" = "#ffc100", "all other days" = "#88CCEE"))+
  #scale_fill_manual(values = c("during heatwave" = "#ff0000", "after heatwave" = "#ffc100", "all other days" = "#88CCEE"))+
  annotate("rect", xmin=-5, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="#920000") +
  annotate("rect", xmin=-5, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="#920000") +
  # annotate("rect", xmin=-5, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="#920000") +
  # annotate("rect", xmin=-5, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="#920000") +
  # annotate("rect", xmin=-5, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="#920000") +
  # annotate("rect", xmin=-5, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="#920000") +
  # annotate("rect", xmin=-5, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="#920000") +
  # annotate("rect", xmin=-5, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="#920000") +
  # annotate("rect", xmin=-5, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="#920000") +
  # annotate("rect", xmin=-5, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="#920000") +
  # annotate("rect", xmin=-5, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="#920000") +
  # annotate("rect", xmin=-6, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="#920000") +
  # annotate("rect", xmin=-6, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="#920000") +
  # annotate("rect", xmin=-7, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="#920000") +
  # annotate("rect", xmin=-7, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="#920000") +
  # annotate("rect", xmin=-7, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="#920000") +
  annotate("rect", xmin=-7, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="#920000") +
  annotate("rect", xmin=-7, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="#920000") +
  annotate("rect", xmin=-7, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="#920000") +
  annotate("rect", xmin=-7, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="#920000") +
  # annotate("rect", xmin=-8, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="#920000") +
  # annotate("rect", xmin=-8, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="#920000") +
  # annotate("rect", xmin=-8, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="#920000") +
  # annotate("rect", xmin=-8, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="#920000") +
  # annotate("rect", xmin=-9, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="#920000") +
  # annotate("rect", xmin=-9, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="#920000") +
  annotate("rect", xmin=-10, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="#920000") +
  annotate("rect", xmin=-10, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="#920000") +
  annotate("rect", xmin=-10, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="#920000") +
  annotate("rect", xmin=-10, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="#920000") +
  annotate("rect", xmin=-13, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="#920000") +
  annotate("rect", xmin=-13, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="#920000") +
  annotate("rect", xmin=-15, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="#920000") +
  annotate("rect", xmin=-16, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="#920000") +
  geom_line(size = 1.5)+
  geom_point(size = 2)+
  labs(title = "HW response over time")+
  ylim(min(mean.all.by.lake$mean_percent_change)-10, max(mean.all.by.lake$mean_percent_change)+10)+
  #geom_ribbon(aes(ymin = mean_percent_change - sd_percent_change, ymax = mean_percent_change + sd_percent_change, fill = period), alpha = 0.1)+
  labs(x = "days after heatwave")+
  theme_classic()+
  scale_color_manual(values = c("R"=  "#60BFCC", "L" = "#D9EEF3", "T"=  "#544C34", "all" = "grey"))+
  xlim(-16, 20)+
  geom_hline(yintercept = 0, linetype = "dashed")
  


#scale_fill_manual(values = c("R"=  "#60BFCC", "L" = "#D9EEF3", "T"=  "#544C34"))+
  

#write.csv(mean.all, "./results/response over time/chl response over time 2008-2011.csv", row.names = FALSE)

#==============================================================================#
#### PLOTTING DISTRIBUTIONS ####

# Create a factor variable with the desired order for 'period'
desired_order <- c("after heatwave", "during heatwave", "all other days")

#png("./figures/science in the northwoods figures/Tuesday Lake heatwave results 4 days after.png", height = 7, width = 13, units = "in", res = 600)


Alldist <- allSlopes %>%
  filter(period != "exclude after heatwave", daysAfter == get("daysAfter", envir=globalenv())) %>%
  ggplot(aes(x = percent_change,
             y = factor(period, levels = desired_order),  # Use factor with desired order
             fill = period)) +
  geom_density_ridges(alpha = 0.7,
                      quantile_lines = TRUE,
                      quantile_fun = function(x, ...) median(x), 
                      scale = 2, size = 0.7) +
  geom_density_ridges(alpha = 0.7,
                      quantile_lines = TRUE,
                      quantile_fun = function(x, ...) mean(x), 
                      scale = 2, size = 0.7) +
  geom_text(data = mean_df %>% filter(daysAfter == get("daysAfter", envir=globalenv())),
            aes(x = mean_percent_change,
                y = factor(period, levels = desired_order),  # Use factor with desired order
                label = as.character(round(mean_percent_change, digits = 0))),
            color = "black",
            size = 4,
            vjust = 2) +
  geom_text(data = mean_df %>% filter(daysAfter == get("daysAfter", envir=globalenv())),
            aes(x = -200,
                y = factor(period, levels = desired_order),  # Use factor with desired order
                label = paste("n = ", number_percent_change, sep = "")),
            color = "black",
            size = 4,
            vjust = 2) +
  ylab("") +
  xlab("% change in surface chlorophyll-a")+
  labs(title = "Peter, Paul, Tuesday combined") +
  scale_fill_manual(values = c("during heatwave" = "#ff0000", "after heatwave" = "#ffc100", "all other days" = "#88CCEE")) +  # Specify fill colors for groups
  theme_classic()+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=18,face="bold"))+
  annotate("text",  x=Inf, y = Inf, label = paste("Days after heatwave: ", daysAfter, sep = ""), vjust=1, hjust=1)

  

# Tuesday
Tdist <- allSlopes %>%
  filter(lake == "T") %>%
  filter(period != "exclude after heatwave", daysAfter == get("daysAfter", envir=globalenv())) %>%
  ggplot(aes(x = percent_change,
             y = factor(period, levels = desired_order),  # Use factor with desired order
             fill = period)) +
  geom_density_ridges(alpha = 0.7,
                      quantile_lines = TRUE,
                      quantile_fun = function(x, ...) median(x), 
                      scale = 2, size = 0.7) +
  geom_density_ridges(alpha = 0.7,
                      quantile_lines = TRUE,
                      quantile_fun = function(x, ...) mean(x), 
                      scale = 2, size = 0.7) +
  geom_text(data = mean_dfT %>% filter(daysAfter == get("daysAfter", envir=globalenv())),
            aes(x = mean_percent_change,
                y = factor(period, levels = desired_order),  # Use factor with desired order
                label = as.character(round(mean_percent_change, digits = 0))),
            color = "black",
            size = 4,
            vjust = 2) +
  geom_text(data = mean_dfT %>% filter(daysAfter == get("daysAfter", envir=globalenv())),
            aes(x = -200,
                y = factor(period, levels = desired_order),  # Use factor with desired order
                label = paste("n = ", number_percent_change, sep = "")),
            color = "black",
            size = 4,
            vjust = 2) +
  ylab("") +
  xlab("% change in chlorophyll-a")+
  labs(title = "Tuesday") +
  scale_fill_manual(values = c("during heatwave" = "#ff0000", "after heatwave" = "grey", "all other days" = "#544C34")) +  # Specify fill colors for groups
  theme_classic()+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=18,face="bold"))+
  annotate("text",  x=Inf, y = Inf, label = paste("Days after heatwave: ", daysAfter, sep = ""), vjust=1, hjust=1)


#dev.off()

desired_order <- c("after heatwave", "during heatwave", "all other days")



#png("./figures/science in the northwoods figures/Peter Lake heatwave results 4 days after.png", height = 7, width = 13, units = "in", res = 600)

Rdist <- allSlopes %>%
  filter(lake == "R") %>%
  filter(period != "exclude after heatwave", daysAfter == get("daysAfter", envir=globalenv())) %>%
  ggplot(aes(x = percent_change,
             y = factor(period, levels = desired_order),  # Use factor with desired order
             fill = period)) +
  geom_density_ridges(alpha = 0.7,
                      quantile_lines = TRUE,
                      quantile_fun = function(x, ...) median(x), 
                      scale = 2, size = 0.7) +
  geom_density_ridges(alpha = 0.7,
                      quantile_lines = TRUE,
                      quantile_fun = function(x, ...) mean(x), 
                      scale = 2, size = 0.7) +
  geom_text(data = mean_dfR %>% filter(daysAfter == get("daysAfter", envir=globalenv())),
            aes(x = mean_percent_change,
                y = factor(period, levels = desired_order),  # Use factor with desired order
                label = as.character(round(mean_percent_change, digits = 0))),
            color = "black",
            size = 4,
            vjust = 2) +
  geom_text(data = mean_dfR %>% filter(daysAfter == get("daysAfter", envir=globalenv())),
            aes(x = -200,
                y = factor(period, levels = desired_order),  # Use factor with desired order
                label = paste("n = ", number_percent_change, sep = "")),
            color = "black",
            size = 4,
            vjust = 2) +
  ylab("") +
  xlab("% change in chlorophyll-a")+
  labs(title = "Peter") +
  scale_fill_manual(values = c("during heatwave" = "#ff0000", "after heatwave" = "grey", "all other days" = "#60BFCC")) +  # Specify fill colors for groups
  theme_classic()+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=18,face="bold"))+
  annotate("text",  x=Inf, y = Inf, label = paste("Days after heatwave: ", daysAfter, sep = ""), vjust=1, hjust=1)


#dev.off()

#png("./figures/science in the northwoods figures/Paul Lake heatwave results 4 days after.png", height = 7, width = 13, units = "in", res = 600)


Ldist <- allSlopes %>%
  filter(lake == "L") %>%
  filter(period != "exclude after heatwave", daysAfter == get("daysAfter", envir=globalenv())) %>%
  ggplot(aes(x = percent_change,
             y = factor(period, levels = desired_order),  # Use factor with desired order
             fill = period)) +
  geom_density_ridges(alpha = 0.7,
                      quantile_lines = TRUE,
                      quantile_fun = function(x, ...) median(x), 
                      scale = 2, size = 0.7) +
  geom_density_ridges(alpha = 0.7,
                      quantile_lines = TRUE,
                      quantile_fun = function(x, ...) mean(x), 
                      scale = 2, size = 0.7) +
  geom_text(data = mean_dfL %>% filter(daysAfter == get("daysAfter", envir=globalenv())),
            aes(x = mean_percent_change,
                y = factor(period, levels = desired_order),  # Use factor with desired order
                label = as.character(round(mean_percent_change, digits = 0))),
            color = "black",
            size = 4,
            vjust = 2) +
  geom_text(data = mean_dfL %>% filter(daysAfter == get("daysAfter", envir=globalenv())),
            aes(x = -200,
                y = factor(period, levels = desired_order),  # Use factor with desired order
                label = paste("n = ", number_percent_change, sep = "")),
            color = "black",
            size = 4,
            vjust = 2) + 
 # ylab("") +
  xlab("% change in chlorophyll-a")+
  labs(title = "Paul") +
  scale_fill_manual(values = c("during heatwave" = "#ff0000", "after heatwave" = "grey", "all other days" = "#D9EEF3")) +  # Specify fill colors for groups
  theme_classic()+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=18,face="bold"))+
  annotate("text",  x=Inf, y = Inf, label = paste("Days after heatwave: ", daysAfter, sep = ""), vjust=1, hjust=1)


#dev.off()



#-------------------------------------------------------------------------------#
###### COMBINED TIMESCALES AND DIST FIGURE ######
# over.time = 
#   mean.all  %>% filter(lake != "all") %>% 
#   ggplot(aes( x= daysAfter, y = mean_percent_change, color = lake))+
#   annotate("rect", xmin=0, xmax=5, ymin=-Inf, ymax=20, alpha = 0.07, fill="#D9EEF3", color = "grey") +
#   annotate("rect", xmin=0, xmax=5, ymin=-Inf, ymax=20, alpha = 0.07, fill="#D9EEF3", color = "grey") +
#   annotate("rect", xmin=0, xmax=5, ymin=-Inf, ymax=20, alpha = 0.07, fill="#D9EEF3", color = "grey") +
#   annotate("rect", xmin=0, xmax=5, ymin=70, ymax=Inf, alpha = 0.07, fill="#544C34", color = "grey") +
#   annotate("rect", xmin=0, xmax=5, ymin=70, ymax=Inf, alpha = 0.07, fill="#544C34", color = "grey") +
#   annotate("rect", xmin=0, xmax=5, ymin=70, ymax=Inf, alpha = 0.07, fill="#544C34", color = "grey") +
#   annotate("rect", xmin=0, xmax=5, ymin=20, ymax=70, alpha = 0.07, fill="#60BFCC", color = "grey") +
#   annotate("rect", xmin=0, xmax=5, ymin=20, ymax=70, alpha = 0.07, fill="#60BFCC", color = "grey") +
#   annotate("rect", xmin=0, xmax=5, ymin=20, ymax=70, alpha = 0.07, fill="#60BFCC", color = "grey") +
#   annotate("rect", xmin=0, xmax=5, ymin=20, ymax=70, alpha = 0.07, fill="#60BFCC", color = "grey") +
#   annotate("rect", xmin=0, xmax=5, ymin=20, ymax=70, alpha = 0.07, fill="#60BFCC", color = "grey") +
#   annotate("rect", xmin=0, xmax=6, ymin=-75, ymax=Inf, alpha = 0.07, fill="#D9EEF3", color = "grey") +
#   annotate("rect", xmin=0, xmax=6, ymin=20, ymax=70, alpha = 0.07, fill="#60BFCC", color = "grey") +
#   annotate("rect", xmin=0, xmax=7, ymin=-Inf, ymax=20, alpha = 0.07, fill="#D9EEF3", color = "grey") +
#   annotate("rect", xmin=0, xmax=7, ymin=-Inf, ymax=20, alpha = 0.07, fill="#D9EEF3", color = "grey") +
#   annotate("rect", xmin=0, xmax=7, ymin=-Inf, ymax=20, alpha = 0.07, fill="#D9EEF3", color = "grey") +
#   annotate("rect", xmin=0, xmax=7, ymin=-Inf, ymax=20, alpha = 0.07, fill="#D9EEF3", color = "grey") +
#   annotate("rect", xmin=0, xmax=7, ymin=20, ymax=70, alpha = 0.07, fill="#60BFCC", color = "grey") +
#   annotate("rect", xmin=0, xmax=7, ymin=20, ymax=70, alpha = 0.07, fill="#60BFCC", color = "grey") +
#   annotate("rect", xmin=0, xmax=7, ymin=20, ymax=70, alpha = 0.07, fill="#60BFCC", color = "grey") +
#   annotate("rect", xmin=0, xmax=8, ymin=-Inf, ymax=20, alpha = 0.07, fill="#D9EEF3", color = "grey") +
#   annotate("rect", xmin=0, xmax=8, ymin=70, ymax=Inf, alpha = 0.07, fill="#544C34", color = "grey") +
#   annotate("rect", xmin=0, xmax=8, ymin=20, ymax=70, alpha = 0.07, fill="#60BFCC", color = "grey") +
#   annotate("rect", xmin=0, xmax=8, ymin=20, ymax=70, alpha = 0.07, fill="#60BFCC", color = "grey") +
#   annotate("rect", xmin=0, xmax=9, ymin=70, ymax=Inf, alpha = 0.07, fill="#544C34", color = "grey") +
#   annotate("rect", xmin=0, xmax=9, ymin=70, ymax=Inf, alpha = 0.07, fill="#544C34", color = "grey") +
#   annotate("rect", xmin=0, xmax=10, ymin=-Inf, ymax=20, alpha = 0.07, fill="#D9EEF3", color = "grey") +
#   annotate("rect", xmin=0, xmax=10, ymin=-Inf, ymax=20, alpha = 0.07, fill="#D9EEF3", color = "grey") +
#   annotate("rect", xmin=0, xmax=10, ymin=20, ymax=70, alpha = 0.07, fill="#60BFCC", color = "grey") +
#   annotate("rect", xmin=0, xmax=10, ymin=20, ymax=70, alpha = 0.07, fill="#60BFCC", color = "grey") +
#   annotate("rect", xmin=0, xmax=13, ymin=-Inf, ymax=20, alpha = 0.07, fill="#D9EEF3", color = "grey") +
#   annotate("rect", xmin=0, xmax=13, ymin=20, ymax=70, alpha = 0.07, fill="#60BFCC", color = "grey") +
#   annotate("rect", xmin=0, xmax=15, ymin=20, ymax=70, alpha = 0.07, fill="#60BFCC", color = "grey") +
#   annotate("rect", xmin=0, xmax=16, ymin=-Inf, ymax=20, alpha = 0.07, fill="#D9EEF3", color = "grey") +
#   geom_borderline(size = 1, bordercolour = "black")+
#   scale_color_manual(values = c("R"=  "#60BFCC", "L" = "#D9EEF3", "T"=  "#544C34", "all" = "grey"), labels = c("L" = "Paul", "R" = "Peter", "T" = "Tuesday"))+
#   geom_point(size = 2.5, pch = 21, aes(fill = lake),  color = "black", stroke = 0.7)+
#   labs(title = "Response of chlorophyll to heatwaves over time")+
#   ylim(min(mean.all.by.lake$mean_percent_change)-10, max(mean.all.by.lake$mean_percent_change)+1)+
#   labs(x = "days relative to start of heatwave", y = "mean % change in chlorophyll \nacross heatwaves")+
#   theme_classic()+
#   scale_fill_manual(values = c("R"=  "#60BFCC", "L" = "#D9EEF3", "T"=  "#544C34", "all" = "grey"), labels = c("L" = "Paul", "R" = "Peter", "T" = "Tuesday"))+
#   xlim(-23, 40)+
#   geom_hline(yintercept = 0, linetype = "dashed")+
#   theme(plot.title = element_text(hjust = 0.5))+
#   theme(axis.text=element_text(size=12),
#         axis.title=element_text(size=12))+
#   geom_vline(xintercept = 0, linetype = "dashed")+
#   geom_text(aes(x = 20,
#                 y = 120,  
#                 label = "end of heatwaves",
#                 color = "black",
#                 vjust = 1))
# 


# create a dataframe that will work with the rug plots....
test = mean.all %>% filter(daysAfter >= -17 & daysAfter <= 16 & period == "after heatwave" & lake != "all")


over.time = 
  mean.all %>% filter(lake != "all") %>% 
  ggplot(aes(x = daysAfter, y = mean_percent_change, color = lake)) +
  #Adjusted the x-axis limits for the rectangles
  # annotate("rect", xmin = -5, xmax = 0, ymin = -Inf, ymax = 20, alpha = 0.07, fill = "#D9EEF3", color = NA) +
  # annotate("rect", xmin = -5, xmax = 0, ymin = -Inf, ymax = 20, alpha = 0.07, fill = "#D9EEF3", color = NA) +
  # annotate("rect", xmin = -5, xmax = 0, ymin = -Inf, ymax = 20, alpha = 0.07, fill = "#D9EEF3", color = NA) +
  # annotate("rect", xmin = -5, xmax = 0, ymin = 70, ymax = Inf, alpha = 0.07, fill = "#544C34", color = NA) +
  # annotate("rect", xmin = -5, xmax = 0, ymin = 70, ymax = Inf, alpha = 0.07, fill = "#544C34", color = NA) +
  # annotate("rect", xmin = -5, xmax = 0, ymin = 70, ymax = Inf, alpha = 0.07, fill = "#544C34", color = NA) +
  # annotate("rect", xmin = -5, xmax = 0, ymin = 20, ymax = 70, alpha = 0.07, fill = "#60BFCC", color = NA) +
  # annotate("rect", xmin = -5, xmax = 0, ymin = 20, ymax = 70, alpha = 0.07, fill = "#60BFCC", color = NA) +
  # annotate("rect", xmin = -5, xmax = 0, ymin = 20, ymax = 70, alpha = 0.07, fill = "#60BFCC", color = NA) +
  # annotate("rect", xmin = -5, xmax = 0, ymin = 20, ymax = 70, alpha = 0.07, fill = "#60BFCC", color = NA) +
  # annotate("rect", xmin = -5, xmax = 0, ymin = 20, ymax = 70, alpha = 0.07, fill = "#60BFCC", color = NA) +
  # annotate("rect", xmin = -6, xmax = 0, ymin = -75, ymax = Inf, alpha = 0.07, fill = "#D9EEF3", color = NA) +
  # annotate("rect", xmin = -6, xmax = 0, ymin = 20, ymax = 70, alpha = 0.07, fill = "#60BFCC", color = NA) +
  # annotate("rect", xmin = -7, xmax = 0, ymin = -Inf, ymax = 20, alpha = 0.07, fill = "#D9EEF3", color = NA) +
  # annotate("rect", xmin = -7, xmax = 0, ymin = -Inf, ymax = 20, alpha = 0.07, fill = "#D9EEF3", color = NA) +
  # annotate("rect", xmin = -7, xmax = 0, ymin = -Inf, ymax = 20, alpha = 0.07, fill = "#D9EEF3", color = NA) +
  # annotate("rect", xmin = -7, xmax = 0, ymin = -Inf, ymax = 20, alpha = 0.07, fill = "#D9EEF3", color = NA) +
  # annotate("rect", xmin = -7, xmax = 0, ymin = 20, ymax = 70, alpha = 0.07, fill = "#60BFCC", color = NA) +
  # annotate("rect", xmin = -7, xmax = 0, ymin = 20, ymax = 70, alpha = 0.07, fill = "#60BFCC", color = NA) +
  # annotate("rect", xmin = -7, xmax = 0, ymin = 20, ymax = 70, alpha = 0.07, fill = "#60BFCC", color = NA) +
  # annotate("rect", xmin = -8, xmax = 0, ymin = -Inf, ymax = 20, alpha = 0.07, fill = "#D9EEF3", color = NA) +
  # annotate("rect", xmin = -8, xmax = 0, ymin = 70, ymax = Inf, alpha = 0.07, fill = "#544C34", color = NA) +
  # annotate("rect", xmin = -8, xmax = 0, ymin = 20, ymax = 70, alpha = 0.07, fill = "#60BFCC", color = NA) +
  # annotate("rect", xmin = -8, xmax = 0, ymin = 20, ymax = 70, alpha = 0.07, fill = "#60BFCC", color = NA) +
  # annotate("rect", xmin = -9, xmax = 0, ymin = 70, ymax = Inf, alpha = 0.07, fill = "#544C34", color = NA) +
  # annotate("rect", xmin = -9, xmax = 0, ymin = 70, ymax = Inf, alpha = 0.07, fill = "#544C34", color = "grey") +
  # annotate("rect", xmin = -10, xmax = 0, ymin = -Inf, ymax = 20, alpha = 0.07, fill = "#D9EEF3", color = NA) +
  # annotate("rect", xmin = -10, xmax = 0, ymin = -Inf, ymax = 20, alpha = 0.07, fill = "#D9EEF3", color = NA) +
  # annotate("rect", xmin = -10, xmax = 0, ymin = 20, ymax = 70, alpha = 0.07, fill = "#60BFCC", color = NA) +
  # annotate("rect", xmin = -10, xmax = 0, ymin = 20, ymax = 70, alpha = 0.07, fill = "#60BFCC", color = NA) +
  # annotate("rect", xmin = -13, xmax = 0, ymin = -Inf, ymax = 20, alpha = 0.07, fill = "#D9EEF3", color = NA) +
  # annotate("rect", xmin = -13, xmax = 0, ymin = 20, ymax = 70, alpha = 0.07, fill = "#60BFCC", color = NA) +
  # annotate("rect", xmin = -15, xmax = 0, ymin = 20, ymax = 70, alpha = 0.07, fill = "#60BFCC", color = "grey") +
  # annotate("rect", xmin = -16, xmax = 0, ymin = -Inf, ymax = 20, alpha = 0.07, fill = "#D9EEF3", color = "grey") +
  geom_borderline(size = 1, bordercolour = "black") +
  scale_color_manual(values = c("R" = "#60BFCC", "L" = "#D9EEF3", "T" = "#544C34", "all" = "grey"), 
                     labels = c("L" = "Paul", "R" = "Peter", "T" = "Tuesday")) +
  geom_point(size = 3.5, pch = 21, aes(fill = lake), color = "black", stroke = 0.7) +
  #labs(title = "Response of chlorophyll to heatwaves over time") +
  ylim(min(mean.all.by.lake$mean_percent_change) - 50, max(mean.all.by.lake$mean_percent_change) + 1) +
  labs(x = "days relative to end of heatwave", y = "mean % change\nin chlorophyll\nacross heatwaves") +
  theme_classic() +
  scale_fill_manual(values = c("R" = "#60BFCC", "L" = "#D9EEF3", "T" = "#544C34", "all" = "grey"), 
                    labels = c("L" = "Paul", "R" = "Peter", "T" = "Tuesday")) +
  xlim(-16, 14) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        axis.title.y = element_text(size = 16, angle = 0, vjust = 0.5)) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  theme(legend.position = "bottom")+
  theme(legend.text = element_text(size = 16)) + 
  guides(color = guide_legend(title = NULL), fill = guide_legend(title = NULL))



# Tuesday
Tdist <- allSlopes %>%
  filter(lake == "T") %>%
  filter(period != "exclude after heatwave" & period != "during heatwave", daysAfter == 2) %>%
  ggplot(aes(x = percent_change,
             y = factor(period, levels = desired_order),  # Use factor with desired order
             fill = period)) +
 # xlim(-200, 600)+
  # geom_density_ridges(alpha = 0.7,
  #                     quantile_lines = TRUE,
  #                     quantile_fun = function(x, ...) median(x), 
  #                     scale = 2, size = 0.7) +
  geom_density_ridges(alpha = 0.9,
                      quantile_lines = TRUE,
                      quantile_fun = function(x, ...) mean(x), 
                      scale = 2, size = 0.7) +
  # geom_text(data = mean_dfT %>% filter(daysAfter == 2),
  #           aes(x = mean_percent_change,
  #               y = factor(period, levels = desired_order),  # Use factor with desired order
  #               label = paste0(round(mean_percent_change, digits = 0), "%")),
  #           color = "black",
  #           size = 4,
  #           vjust = 1) +
  # geom_text(data = mean_dfT %>% filter(daysAfter == 2),
  #           aes(x = 400,
  #               y = factor(period, levels = desired_order),  # Use factor with desired order
  #               label = paste("n = ", number_percent_change, sep = "")),
  #           color = "black",
  #           size = 4,
  #           vjust = 2) +
  ylab("") +
  xlab("\n\n")+
  labs(title = "Tuesday") +
  scale_fill_manual(values = c("during heatwave" = "red3", "all other days" = "darkgrey", "after heatwave" = "#544C34")) +  # Specify fill colors for groups
  theme_classic()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16))+
  theme(axis.text.y = element_blank())+
  theme(legend.position="none")


#dev.off()


# read in the fish graphic 
# library(png)
# library(grid)
# img <- readPNG("./figures/annotated icons/bass outline.png")
# g <- rasterGrob(img, interpolate=TRUE)
# 
# qplot(1:10, 1:10, geom="blank")

#png("./figures/science in the northwoods figures/Peter Lake heatwave results 4 days after.png", height = 7, width = 13, units = "in", res = 600)

Rdist <- allSlopes %>%
  filter(lake == "R") %>%
  filter(period != "exclude after heatwave" & period != "during heatwave", daysAfter == 3) %>%
  ggplot(aes(x = percent_change,
             y = factor(period, levels = desired_order),  # Use factor with desired order
             fill = period)) +
  #xlim(-200, 800)+
  # geom_density_ridges(alpha = 0.7,
  #                     quantile_lines = TRUE,
  #                     quantile_fun = function(x, ...) median(x), 
  #                     scale = 2, size = 0.7) +
  geom_density_ridges(alpha = 0.9,
                      quantile_lines = TRUE,
                      quantile_fun = function(x, ...) mean(x), 
                      scale = 2, size = 0.7) +
  # geom_text(data = mean_dfR %>% filter(daysAfter == get("daysAfter", envir=globalenv())),
  #           aes(x = mean_percent_change,
  #               y = factor(period, levels = desired_order),  # Use factor with desired order
  #               label = paste0(round(mean_percent_change, digits = 0), "%")),            color = "black",
  #           size = 4,
  #           vjust = 2) +
  # geom_text(data = mean_dfR %>% filter(daysAfter == get("daysAfter", envir=globalenv())),
  #           aes(x = 400,
  #               y = factor(period, levels = desired_order),  # Use factor with desired order
  #               label = paste("n = ", number_percent_change, sep = "")),
  #           color = "black",
  #           size = 4,
  #           vjust = 2) +
  ylab("") +
  xlab("\n\n% change in chlorophyll a")+
  labs(title = "Peter") +
  scale_fill_manual(values = c("after heatwave" = "#60BFCC", "all other days" = "darkgrey")) +  # Specify fill colors for groups
  theme_classic()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16))+
  theme(axis.text.y = element_blank())+
  theme(legend.position="none")


#dev.off()

#png("./figures/science in the northwoods figures/Paul Lake heatwave results 4 days after.png", height = 7, width = 13, units = "in", res = 600)



Ldist <- allSlopes %>%
  filter(lake == "L") %>%
  filter(period != "exclude after heatwave" & period != "during heatwave", daysAfter == 3) %>%
  ggplot(aes(x = percent_change,
             y = factor(period, levels = desired_order),  # Use factor with desired order
             fill = period)) +
  #xlim(-200, 500)+
  # geom_density_ridges(alpha = 0.7,
  #                     quantile_lines = TRUE,
  #                     quantile_fun = function(x, ...) median(x), 
  #                     scale = 2, size = 0.7) +
  geom_density_ridges(alpha = 0.9,
                      quantile_lines = TRUE,
                      quantile_fun = function(x, ...) mean(x), 
                      scale = 2, size = 0.7) +
  # geom_text(data = mean_dfL %>% filter(daysAfter == get("daysAfter", envir=globalenv())),
  #           aes(x = mean_percent_change,
  #               y = factor(period, levels = desired_order),  # Use factor with desired order
  #               label = paste0(round(mean_percent_change, digits = 0), "%")),
  #           color = "black",
  #           size = 4,
  #           vjust = 2) +
  # geom_text(data = mean_dfL %>% filter(daysAfter == get("daysAfter", envir=globalenv())),
  #           aes(x = 200,
  #               y = factor(period, levels = desired_order),  # Use factor with desired order
  #               label = paste("n = ", number_percent_change, sep = "")),
  #           color = "black",
  #           size = 4,
  #           vjust = 2) +
  #ylab("") +
  xlab("\n\n")+
  labs(title = "Paul", y = "") +
  scale_fill_manual(values = c("after heatwave" = "#D9EEF3", "all other days" = "darkgrey")) +  # Specify fill colors for groups
  theme_classic()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16))+
  #theme(axis.text.y = element_blank())+
  theme(legend.position="none")



png("./figures/manuscript draft 2024-11-11/heatwave results 2_or_3 days after.png", height = 7, width = 10, units = "in", res = 600)
ggarrange(over.time, ggarrange(Ldist, Rdist, Tdist, ncol = 3),
          nrow = 2, legend = "top")
dev.off()









3# Ensure panel.spacing is consistent
# Ldist_adjusted <- Ldist + theme(panel.spacing = unit(2, "lines"), 
#                                 plot.margin = margin(1, 20, 5.5, 5.5))  # Adjust margins
# 
# Rdist_adjusted <- Rdist + theme(panel.spacing = unit(1, "lines"),
#                                 plot.margin = margin(1, 5.5, 5.5, 5.5))  # Adjust margins
# 
# Tdist_adjusted <- Tdist + theme(panel.spacing = unit(1, "lines"), 
#                                 plot.margin = margin(1, 5.5, 5.5, 5.5))  # Adjust margins

# Arrange the plots
# ggarrange(over.time, ggarrange(Ldist, Rdist_adjusted, Tdist_adjusted, ncol = 3, widths = c(2, 1, 1)), 
#           nrow = 2, legend = "top", align = "v")


#==============================================================================#
#### Explanatory plots with average slopes ####

# add the new calculated average slopes to the old exp dataframe
exp = read_xlsx("./formatted data/explanatory_variables_heatwaves.xlsx")

# length of each heatwave
exp$length = as.numeric(exp$end_date - exp$start_date)

# make a new dataframe that only has the columns of results we are interested in for now
priority.results = results %>% select(lake, year, date_start, date_end, absoluteChange, percentChange) %>% 
  dplyr::rename(start_date = date_start, end_date = date_end) %>% 
  mutate(start_date = as.Date(start_date), end_date = as.Date(end_date))

exp = exp %>% select(-comments, -percent_change)

exp = exp %>% full_join(priority.results, by = c("lake", "year", "start_date", "end_date")) %>% 
  dplyr::rename(percent_change = percentChange)

exp$percent_change = as.numeric(exp$percent_change)


hw.pload <- ggplot(data = exp, aes(x = p_loading_mg_m2, y = abs(absoluteChange)))+
  geom_point(size = 3, fill = "orangered3", pch = 21)+
  theme_classic()+
  labs(y = "abs. % change in surface chl",  x = "P loading (mg/m2)")


hw.color <- ggplot(data = exp, aes(x = color_m_1, y = abs(absoluteChange)))+
  geom_point(size = 3, fill = "saddlebrown", pch = 21)+
  theme_classic()+
  labs(y = "abs. % change in surface chl",  x = "water color g440 (m-1)")


# plot of the percent change in chlorophyll seasonally

exp = exp %>% mutate(doy.start = yday(start_date)) %>% 
  mutate(month = month(start_date))

hw.doy.start <- ggplot(data = exp, aes(x = doy.start, y = abs(absoluteChange), fill = lake))+
  geom_point(size = 3, color = "black", shape = 21, stroke = 1, alpha = 0.7)+
  theme_classic()+
  labs(y = "abs. % change in surface chl", x = "heatwave day of year")+
  scale_fill_manual(values = c("L" = "steelblue2", "R" = "black", "T" = "white"))

hw.length <- ggplot(data = exp, aes(x = length, y = abs(absoluteChange), fill = lake))+
  geom_point(size = 3, color = "black", shape = 21, stroke = 1, alpha = 0.7)+
  theme_classic()+
  labs(y = "abs. % change in surface chl", x = "heatwave length (days)")+
  scale_fill_manual(values = c("L" = "steelblue2", "R" = "black", "T" = "white"))



#==============================================================================#
#### CREATE PDF OF OUTPUTS ####

# save numbers of old runs
oldRuns = list.files("./figures/sensitivity tests/")
oldRuns <- data.frame(oldRuns)

# Use the tidyverse to extract the first chunk of characters before the first '_'
# Then number the new run based on what runs are already in the folder

oldRuns <- oldRuns %>%
  mutate(numbersUsed = str_extract(oldRuns, "^[^_]+"))

runNumber = as.numeric(max(oldRuns$numbersUsed))+1

# name the pdf with the current date and the variables
curDate = cur_date_time = format(Sys.Date(), "%Y_%m_%d")
pdfName = paste(runNumber, "_date_", curDate, "_slopeLength_", slopeLength, "_daysAfter_", daysAfter, "_numSlopes_", numSlopes, ".pdf", sep = "")


# pdf(paste("./figures/sensitivity tests/" , pdfName, sep = ""), height = 6, width = 10)

print(metadata_plot)
print(Alldist)
print(Rdist)
print(Ldist)
print(Tdist)
print(indHWResp)
#print(All.over.time)
#print(R.over.time)
#print(L.over.time)
#print(T.over.time)
print(ggarrange(hw.pload, hw.color, hw.doy.start, hw.length, nrow = 2, ncol = 2))

# dev.off()

# make a PDF of all the raw plots with highlighted areas showing where slopes are calculated
makePDFrawPlots(allSlopes, daysAfter, numSlopes, metadata_plot, runNumber, slopeLength)




# secondary output that has the raw data plots, all of them with the dates that were analyzed
# call the function which is in a second R script
# pass it allSlopes, daysAfter, numSlopes, slopeLength, runNumber, along with the output information



##### Arrange four distribution plots ########
# Figure 1 of the paper

library(patchwork)

allSlopes = allSlopes %>% mutate(period = replace(period, period == "after heatwave", "during heatwave"))
desired_order <- c("during heatwave", "all other days")


mean_df = mean_df %>% mutate(period = replace(period, period == "after heatwave", "during heatwave"))
mean_dfL = mean_dfL %>% mutate(period = replace(period, period == "after heatwave", "during heatwave"))
mean_dfR = mean_dfR %>% mutate(period = replace(period, period == "after heatwave", "during heatwave"))
mean_dfT = mean_dfT %>% mutate(period = replace(period, period == "after heatwave", "during heatwave"))


allSlopes = allSlopes %>% mutate(period = as.factor(period))

allSlopes = allSlopes %>% mutate(period = relevel(period, ref = "all other days"))

# color for the paper was mediumseagreen, color for slides is red4

Alldist <- allSlopes %>%
  filter(period != "exclude after heatwave", daysAfter == get("daysAfter", envir=globalenv())) %>%
  ggplot(aes(x = percent_change,
             y = factor(period, levels = desired_order),  # Use factor with desired order
             fill = period)) +
  xlim(-200, 600)+
  # geom_density_ridges(alpha = 0.7,
  #                     quantile_lines = TRUE,
  #                     quantile_fun = function(x, ...) median(x), 
  #                     scale = 2, size = 0.7) +
  geom_density_ridges(alpha = 0.7,
                      quantile_lines = TRUE,
                      quantile_fun = function(x, ...) mean(x), 
                      scale = 2, size = 0.7) +
  # geom_text(data = mean_df %>% filter(daysAfter == get("daysAfter", envir=globalenv())),
  #           aes(x = mean_percent_change,
  #               y = factor(period, levels = desired_order),  # Use factor with desired order
  #               label = as.character(round(mean_percent_change, digits = 0))),
  #           color = "black",
  #           size = 4,
  #           vjust = 2) +
  # geom_text(data = mean_df %>% filter(daysAfter == get("daysAfter", envir=globalenv())),
  #           aes(x = 400,
  #               y = factor(period, levels = desired_order),  # Use factor with desired order
  #               label = paste("n = ", number_percent_change, sep = "")),
  #           color = "black",
  #           size = 4,
  #           vjust = 2) +
  ylab("") +
  xlab("")+
  #labs(title = "All lakes") +
  scale_fill_manual(values = c("during heatwave" = "red4", "all other days" = "#88CCEE")) +  # Specify fill colors for groups
  theme_classic()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12))+
  theme(axis.text.y = element_blank())+
  theme(legend.position="top")





# Tuesday
Tdist <- allSlopes %>%
  filter(lake == "T") %>%
  filter(period != "exclude after heatwave", daysAfter == get("daysAfterT", envir=globalenv())) %>%
  ggplot(aes(x = percent_change,
             y = factor(period, levels = desired_order),  # Use factor with desired order
             fill = period)) +
  #xlim(-200, 600)+
  # geom_density_ridges(alpha = 0.7,
  #                     quantile_lines = TRUE,
  #                     quantile_fun = function(x, ...) median(x), 
  #                     scale = 2, size = 0.7) +
  geom_density_ridges(alpha = 0.7,
                      quantile_lines = TRUE,
                      quantile_fun = function(x, ...) mean(x), 
                      scale = 2, size = 0.7) +
  # geom_text(data = mean_dfT %>% filter(daysAfter == get("daysAfterT", envir=globalenv())),
  #           aes(x = mean_percent_change,
  #               y = factor(period, levels = desired_order),  # Use factor with desired order
  #               label = as.character(round(mean_percent_change, digits = 0))),
  #           color = "black",
  #           size = 4,
  #           vjust = 1) +
  # geom_text(data = mean_dfT %>% filter(daysAfter == get("daysAfterT", envir=globalenv())),
  #           aes(x = 400,
  #               y = factor(period, levels = desired_order),  # Use factor with desired order
  #               label = paste("n = ", number_percent_change, sep = "")),
  #           color = "black",
  #           size = 4,
  #           vjust = 2) +
  ylab("") +
  xlab("% change in surface \n chlorophyll-a")+
  labs(title = "Tuesday") +
  scale_fill_manual(values = c("after heatwave" = "red3", "all other days" = "#88CCEE")) +  # Specify fill colors for groups
  theme_classic()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12))+
  theme(axis.text.y = element_blank())+
  theme(legend.position="none")


#dev.off()




#png("./figures/science in the northwoods figures/Peter Lake heatwave results 4 days after.png", height = 7, width = 13, units = "in", res = 600)

Rdist <- allSlopes %>%
  filter(lake == "R") %>%
  filter(period != "exclude after heatwave", daysAfter == get("daysAfter", envir=globalenv())) %>%
  ggplot(aes(x = percent_change,
             y = factor(period, levels = desired_order),  # Use factor with desired order
             fill = period)) +
 # xlim(-200, 600)+
  # geom_density_ridges(alpha = 0.7,
  #                     quantile_lines = TRUE,
  #                     quantile_fun = function(x, ...) median(x), 
  #                     scale = 2, size = 0.7) +
  geom_density_ridges(alpha = 0.7,
                      quantile_lines = TRUE,
                      quantile_fun = function(x, ...) mean(x), 
                      scale = 2, size = 0.7) +
  # geom_text(data = mean_dfR %>% filter(daysAfter == get("daysAfter", envir=globalenv())),
  #           aes(x = mean_percent_change,
  #               y = factor(period, levels = desired_order),  # Use factor with desired order
  #               label = as.character(round(mean_percent_change, digits = 0))),
  #           color = "black",
  #           size = 4,
  #           vjust = 2) +
  # geom_text(data = mean_dfR %>% filter(daysAfter == get("daysAfter", envir=globalenv())),
  #           aes(x = 400,
  #               y = factor(period, levels = desired_order),  # Use factor with desired order
  #               label = paste("n = ", number_percent_change, sep = "")),
  #           color = "black",
  #           size = 4,
  #           vjust = 2) +
  ylab("") +
  xlab("% change in surface \n chlorophyll-a")+
  labs(title = "Peter") +
  scale_fill_manual(values = c("after heatwave" = "red3", "all other days" = "#88CCEE")) +  # Specify fill colors for groups
  theme_classic()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12))+
  theme(axis.text.y = element_blank())+
  theme(legend.position="none")


#dev.off()

#png("./figures/science in the northwoods figures/Paul Lake heatwave results 4 days after.png", height = 7, width = 13, units = "in", res = 600)



Ldist <- allSlopes %>%
  filter(lake == "L") %>%
  filter(period != "exclude after heatwave", daysAfter == get("daysAfter", envir=globalenv())) %>%
  ggplot(aes(x = percent_change,
             y = factor(period, levels = desired_order),  # Use factor with desired order
             fill = period)) +
  #xlim(-200, 600)+
  # geom_density_ridges(alpha = 0.7,
  #                     quantile_lines = TRUE,
  #                     quantile_fun = function(x, ...) median(x), 
  #                     scale = 2, size = 0.7) +
  geom_density_ridges(alpha = 0.7,
                      quantile_lines = TRUE,
                      quantile_fun = function(x, ...) mean(x), 
                      scale = 2, size = 0.7) +
  geom_text(data = mean_dfL %>% filter(daysAfter == get("daysAfter", envir=globalenv())),
            aes(x = mean_percent_change,
                y = factor(period, levels = desired_order),  # Use factor with desired order
                label = as.character(round(mean_percent_change, digits = 0))),
            color = "black",
            size = 4,
            vjust = 2) +
  geom_text(data = mean_dfL %>% filter(daysAfter == get("daysAfter", envir=globalenv())),
            aes(x = 400,
                y = factor(period, levels = desired_order),  # Use factor with desired order
                label = paste("n = ", number_percent_change, sep = "")),
            color = "black",
            size = 4,
            vjust = 2) +
  ylab("") +
  xlab("% change in surface \n chlorophyll-a")+
  labs(title = "Paul") +
  scale_fill_manual(values = c("after heatwave" = "red3", "all other days" = "#88CCEE")) +  # Specify fill colors for groups
  theme_classic()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12))+
  theme(axis.text.y = element_blank())+
  theme(legend.position="none")


#big_plot <- Alldist + plot_layout(guides = 'keep')
#small_plots <- Ldist | Rdist | Tdist


# save to a png

#png("./figures/manuscript 03_18_2024/distributions.png", height = 8, width = 9, res = 300, units = "in")

#big_plot/small_plots

#dev.off()

png("./figures/ASLO figures/distributions red.png", height = 3.87, width = 8.25, units = "in", res = 300 )
ggarrange(Ldist, Rdist, Tdist, nrow = 1, ncol = 3)
dev.off()


#===============================================================================#
###### save individual heatwave response

results = results %>% mutate(event_group = NA)

# Convert date_start and date_end to Date class
results$date_start <- as.Date(results$date_start)
results$date_end <- as.Date(results$date_end)

results$overlaps = NA
results$event_group = NA


k = 1

for(i in 1:nrow(results)){
  if(is.na(results$event_group[i])){
    
cur.start.date = results$date_start[i]
cur.end.date = results$date_end[i]


for(j in 1:nrow(results)){
  results$overlaps[j] = c(cur.start.date, cur.end.date) %overlaps% c(results$date_start[j], results$date_end[j])
  
}

# assign a group_number
results = results %>% mutate(event_group = replace(event_group, overlaps == TRUE, k))
k = k+1
    
  }
}


results = results %>% select(-overlaps)

results = results %>% mutate(event_group = as.factor(event_group))

testing = results %>% group_by(event_group) %>% 
  dplyr::summarize(start_date_avg = mean(date_start))

results = left_join(results, testing, by = "event_group") 

# remove row with NA
results = results %>% filter(percentChange != "NaN")

#write.csv(results, "./results/heatwaves with grouping.csv", row.names = FALSE)

  ggplot(data = results, aes(x = as.factor(start_date_avg), y = percentChange, fill = lake))+
  geom_bar(stat = "identity", position = "identity", alpha = 0.5, color = "black")+
  theme_classic()+  
  ylab("% change in surface chlorophyll-a")+
  xlab("start date of heatwave")+
  ggtitle("Average chl percent change by event")+
  scale_fill_manual(values = c("R"=  "#60BFCC", "L" = "#D9EEF3", "T"=  "#544C34"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#  facet_wrap(~event_group)
  
#png("./figures/manuscript 03_18_2024/individual events.png", height = 4, width = 6, res = 300, units = "in")
  

results %>%  filter(!(event_group %in% c(5, 6, 16, 17))) %>% 
  ggplot(aes(x = lake, y = percentChange, fill = lake)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), alpha = 0.9, color = "black", width = 0.9) +
  theme_classic() +  
  ylab("% change in surface chlorophyll-a") +
  xlab("") +
  scale_fill_manual(values = c("R" = "#60BFCC", "L" = "#D9EEF3", "T" = "#544C34")) +
  theme(axis.text.x = element_blank())+
  facet_wrap(~start_date_avg)+
  geom_hline(yintercept = 0)+
  scale_y_continuous(trans = pseudolog10_trans, labels = scales::comma)

scale_x_continuous(trans = pseudolog10_trans)


#dev.off()

#png("./figures/manuscript 03_18_2024/individual events standard error bars.png", height = 4, width = 6, res = 300, units = "in")

# calculate SE of percent change
#results = results %>% mutate(SE = sdSlopePercent/sqrt(5))
#results = results %>% mutate(error.top = percentChange + SE, error.bottom = percentChange - SE)

  ggplot(results, aes(x = lake, y = percentChange, fill = lake)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9), alpha = 0.5, color = "black", width = 0.9) +
    #geom_errorbar( aes(x=lake, ymin= error.bottom, ymax=error.top), position = position_dodge(width = 0.1), alpha = 0.5, color = "black", width = 0) +
    theme_classic() +  
    ylab("% change in surface chlorophyll-a") +
    xlab("") +
    scale_fill_manual(values = c("R" = "#60BFCC", "L" = "#D9EEF3", "T" = "#544C34")) +
    theme(axis.text.x = element_blank())+
    facet_wrap(~start_date_avg)+
    geom_hline(yintercept = 0)
  
  
#dev.off()

# standard deviation
  
#   pseudolog10_test <- trans_new(
#     name = "pseudolog10",
#     transform = function(x) sign(x) * log10(1 + abs(x)),
#     inverse = function(x) sign(x) * (10^abs(x) - 1),
#     breaks = log_breaks(10),
#     domain = c(-Inf, Inf)
#   )
#   
#   
# # log scale and asinh scale
# results %>% # filter(!(event_group %in% c(5, 6, 16, 17))) %>% 
# ggplot(aes(x = lake, y = percentChange, fill = lake)) +
#   geom_bar(stat = "identity", position = position_dodge(width = 0.9), alpha = 0.5, color = "black", width = 0.9) +
#   geom_errorbar( aes(x=lake, ymin= percentChange -SE.percent, ymax= percentChange +SE.percent), position = position_dodge(width = 0.1), alpha = 0.5, color = "black", width = 0) +
#   theme_classic() +  
#   ylab("% change in surface chlorophyll-a") +
#   xlab("") +
#   scale_fill_manual(values = c("R" = "#60BFCC", "L" = "#D9EEF3", "T" = "#544C34")) +
#   theme(axis.text.x = element_blank())+
#   facet_wrap(~start_date_avg)+
#   geom_hline(yintercept = 0)+
#   scale_y_continuous(trans = pseudolog10_test, labels = scales::comma)

# 
# pseudolog10_test <- trans_new(
#   name = "pseudolog10",
#   transform = function(x) sign(x) * log10(1 + abs(x)),
#   inverse = function(x) sign(x) * (10^abs(x) - 1),
#   breaks = log_breaks(10),
#   domain = c(-Inf, Inf)
# )




pseudolog10_test <- trans_new(
  name = "pseudolog10",
  transform = function(x) sign(x) * log10(1 + abs(x)),
  inverse = function(x) sign(x) * (10^abs(x) - 1),
  breaks = function(x) pretty_breaks()(x),
  domain = c(-Inf, Inf)
)


png("./figures/manuscript draft 2024-11-11/individual response.png", res = 300, height= 5, width = 6.5, units = "in")

results %>%  filter(!(event_group %in% c(5, 6, 16, 17))) %>% 
  ggplot(aes(x = lake, y = percentChange, fill = lake)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black", width = 0.9) +
  geom_errorbar(aes(x = lake, ymin = percentChange - SE.percent, ymax = percentChange + SE.percent), 
                position = position_dodge(width = 0.1), alpha = 0.5, color = "black", width = 0) +
  theme_classic() +  
  ylab("% change in surface chlorophyll-a") +
  xlab("") +
  scale_fill_manual(values = c("R" = "#60BFCC", "L" = "#D9EEF3", "T" = "#544C34"),
                    labels = c("L" = "Paul", "R" = "Peter", "T" = "Tuesday")) +
  facet_wrap(~start_date_avg) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(trans = pseudolog10_test, labels = scales::comma, breaks = c( -100, -10, 0, 10, 100, 1000))+
  theme(axis.text.x = element_blank(),
        legend.position = "top",
        legend.title = element_blank(),  
        legend.text = element_text(size = 12),  
        axis.title.y = element_text(size = 14))
  
dev.off()

  # ggplot(data = results, aes(x = as.factor(start_date_avg), y = percentChange, fill = lake)) +
  #   geom_bar(stat = "identity", position = position_dodge(width = 1), alpha = 0.5, color = "black", width = 1 / max_group_width) +
  #   theme_classic() +  
  #   ylab("% change in surface chlorophyll-a") +
  #   xlab("start date of heatwave") +
  #   ggtitle("Average chl percent change by event") +
  #   scale_fill_manual(values = c("R" = "#60BFCC", "L" = "#D9EEF3", "T" = "#544C34")) +
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1))


  # ggplot(data = results, aes(x = as.character(date_start), y = percentChange, fill = lake))+
  #   geom_bar(stat = "identity", position = "dodge", alpha = 0.5, color = "black")+
  #   theme_classic()+  
  #   ylab("% change in surface chlorophyll-a")+
  #   xlab("start date of heatwave")+
  #   ggtitle("Average chl percent change by event")+
  #   scale_fill_manual(values = c("R"=  "#60BFCC", "L" = "#D9EEF3", "T"=  "#544C34"))+
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1))
  

# c("2009-06-02", "2009-06-07") %overlaps% c("2009-06-07", "2009-06-09")


# testing color


#"R" = "#60BFCC", "L" = "#D9EEF3", "T"=  "#544C34"

# 
# Rdist<- allSlopes %>%
#   filter(period != "exclude after heatwave", lake == "R", daysAfter == get("daysAfter", envir=globalenv())) %>%
#   ggplot(aes(x = percent_change, y = factor(period, levels = desired_order), fill = stat(x))) +
#   geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, quantile_lines = TRUE,
#                                quantile_fun = function(x, ...) mean(x), 
#                                size = 0.7, alpha = 0.7) +
#   scale_fill_gradientn(colours = c("#60BFCC", "forestgreen"),
#                        values = scales::rescale(c(-100, 200, 600, 600))) +
#   labs(title = 'Peter', y = "") +
#   theme_classic() +
#   geom_text(data = mean_dfR %>% filter(daysAfter == get("daysAfter", envir=globalenv())),
#             aes(x = mean_percent_change,
#                 y = factor(period, levels = desired_order),
#                 label = as.character(round(mean_percent_change, digits = 0))),
#             color = "black",
#             size = 4,
#             vjust = 2)
# 
# 
# 
# Ldist <- allSlopes %>%
#   filter(period != "exclude after heatwave", lake == "L", daysAfter == get("daysAfter", envir=globalenv())) %>%
#   ggplot(aes(x = percent_change, y = factor(period, levels = desired_order), fill = stat(x))) +
#   geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, quantile_lines = TRUE,
#                                quantile_fun = function(x, ...) mean(x), 
#                                size = 0.7, alpha = 0.7) +
#   scale_fill_gradientn(colours = c("#D9EEF3", "forestgreen"),
#                        values = scales::rescale(c(-100, 300, 600, 600))) +
#   labs(title = 'Paul', y = "") +
#   theme_classic() +
#   geom_text(data = mean_dfL %>% filter(daysAfter == get("daysAfter", envir=globalenv())),
#             aes(x = mean_percent_change,
#                 y = factor(period, levels = desired_order),
#                 label = as.character(round(mean_percent_change, digits = 0))),
#             color = "black",
#             size = 4,
#             vjust = 2)
# 
# 
# 
# Tdist <- 
#   allSlopes %>%
#   filter(period != "exclude after heatwave", lake == "T", daysAfter == get("daysAfter", envir=globalenv())) %>%
#   ggplot(aes(x = percent_change, y = factor(period, levels = desired_order), fill = stat(x))) +
#   geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, quantile_lines = TRUE,
#                                quantile_fun = function(x, ...) mean(x), 
#                                size = 0.7, alpha = 0.7) +
#   scale_fill_gradientn(colours = c("#544C34", "forestgreen"),
#                        values = scales::rescale(c(-50, 200, 600))) +
#   labs(title = 'Tuesday', y = "") +
#   theme_classic() +
#   geom_text(data = mean_dfT %>% filter(daysAfter == get("daysAfter", envir=globalenv())),
#             aes(x = mean_percent_change,
#                 y = factor(period, levels = desired_order),
#                 label = as.character(round(mean_percent_change, digits = 0))),
#             color = "black",
#             size = 4,
#             vjust = 2)

#"R" = "#60BFCC", "L" = "#D9EEF3", "T"=  "#544C34"



#==============================================================================#
#### TEST STANDARD ERROR OF SLOPES ####

# cycle through each lake year, fit linear models for each rolling window, then store model slopes
# in the 'slopes' dataframe. Need to initially store the model results in a list
# 
# ses = data.frame(matrix(nrow = 30, ncol = 2))
# names(ses) = c("slopeLength", "mean.se")
# 
# # Model results are the slopes, originally calculated based on the preceding 7 days
# for(slopeLength in 3:30){
#   for(i in 1:length(lake_years)){
#     
#     #print(lake_years[i])
#     temp = allData %>% filter(lake_year == lake_years[i]) # temp dataframe for this lake_year
#     
#     models <- slide(
#       temp, 
#       ~lm(mean_chl ~ doyCat, data = .x), 
#       .before = slopeLength, 
#       .complete = TRUE
#     )
#     
#     temp$chl_slope = NA
#     temp$se = NA
#     temp$p_value = NA
#     temp$r_squared = NA
#     
#     for(j in 1:nrow(temp)){
#       
#       model = models[[j]]
#       coef <- coefficients(model) 
#       # extracting the coefficients from the current model
#       
#       if(!(is.null(coef))){
#         Slope <- coef["doyCat"]
#         temp$chl_slope[j] = Slope # pull out the slope from the model
#         temp$p_value[j] = summary(model)$coefficients[2,4] 
#         temp$se[j] =  summary(model)$coefficients["doyCat", "Std. Error"] 
#         temp$r_squared[j] = summary(model)$r.squared # pull out r_squared from model
#       }
#       
#       
#     }
#     
#     if(i ==1){ slopes = temp} # if first iteration, creates slopes, the final dataframe
#     
#     if(i >1){slopes = rbind(slopes, temp)} # else, appends to slopes 
#     
#     
#   }
#   print(slopeLength)
#   print(mean(slopes$se, na.rm = TRUE))
#   
#   ses$slopeLength[slopeLength] = slopeLength
#   ses$mean.se[slopeLength] = mean(slopes$se, na.rm = TRUE)
#   
# }
# 
# ses$sample_size_component = sqrt(1/(ses$slopeLength - 2))
# ses$data_component = ses$mean.se/ses$sample_size_component
# ses$mean_actual_se = ses$mean.se
# 
# ses = pivot_longer(ses, cols = c("sample_size_component", "data_component", "mean_actual_se"), names_to = "se.component", values_to = "se")
# 
# ggplot(data = ses, aes(x = slopeLength, y = se, color = se.component))+
#   geom_point()+
#   geom_line()+
#   theme_classic()+
#   xlim(min = 3, max = 15)+
#   ylim(min =0 , max = 1)


#==============================================================================#
#### PLOT LOOPED RESULTS ####

# Peter
R.after.days.num <- ggplot(data = looped.results, aes(x = daysAfter, y = numSlopes, fill = R.after.heatwave))+
  geom_tile(color = "black")+
  scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse")

R.after.days.slope <- ggplot(data = looped.results, aes(x = daysAfter, y = slopeLength, fill = R.after.heatwave))+
  geom_tile(color = "black")+
  scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse")

R.after.num.slope <- ggplot(data = looped.results %>% filter(daysAfter == 7), aes(x = slopeLength, y = numSlopes, fill = R.after.heatwave))+
  geom_tile(color = "black")+
  scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse")

# Paul
L.after.days.num <- ggplot(data = looped.results, aes(x = daysAfter, y = numSlopes, fill = L.after.heatwave))+
  geom_tile(color = "black")+
  scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse")

L.after.days.slope <- ggplot(data = looped.results, aes(x = daysAfter, y = slopeLength, fill = L.after.heatwave))+
  geom_tile(color = "black")+
  scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse")

L.after.num.slope <- ggplot(data = looped.results %>% filter(daysAfter == 7), aes(x = slopeLength, y = numSlopes, fill = L.after.heatwave))+
  geom_tile(color = "black")+
  scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse")

# Tuesday
T.after.days.num <- ggplot(data = looped.results, aes(x = daysAfter, y = numSlopes, fill = T.after.heatwave))+
  geom_tile(color = "black")+
  scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse")

T.after.days.slope <- ggplot(data = looped.results, aes(x = daysAfter, y = slopeLength, fill = T.after.heatwave))+
  geom_tile(color = "black")+
  scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse")

T.after.num.slope <- ggplot(data = looped.results %>% filter(daysAfter == 7), aes(x = slopeLength, y = numSlopes, fill = T.after.heatwave))+
  geom_tile(color = "black")+
  scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse")

ggarrange(R.after.days.num, R.after.days.slope, R.after.num.slope, 
          L.after.days.num, L.after.days.slope, L.after.num.slope,
          T.after.days.num, T.after.days.slope, T.after.num.slope, nrow = 3, ncol = 3)





# Peter
R.during.days.num <- ggplot(data = looped.results, aes(x = daysAfter, y = numSlopes, fill = R.during.heatwave))+
  geom_tile(color = "black")+
  scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse")

R.during.days.slope <- ggplot(data = looped.results, aes(x = daysAfter, y = slopeLength, fill = R.during.heatwave))+
  geom_tile(color = "black")+
  scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse")

R.during.num.slope <- ggplot(data = looped.results, aes(x = slopeLength, y = numSlopes, fill = R.during.heatwave))+
  geom_tile(color = "black")+
  scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse")

# Paul
L.during.days.num <- ggplot(data = looped.results, aes(x = daysAfter, y = numSlopes, fill = L.during.heatwave))+
  geom_tile(color = "black")+
  scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse")

L.during.days.slope <- ggplot(data = looped.results, aes(x = daysAfter, y = slopeLength, fill = L.during.heatwave))+
  geom_tile(color = "black")+
  scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse")

L.during.num.slope <- ggplot(data = looped.results, aes(x = slopeLength, y = numSlopes, fill = L.during.heatwave))+
  geom_tile(color = "black")+
  scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse")

# Tuesday
T.during.days.num <- ggplot(data = looped.results, aes(x = daysAfter, y = numSlopes, fill = T.during.heatwave))+
  geom_tile(color = "black")+
  scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse")

T.during.days.slope <- ggplot(data = looped.results, aes(x = daysAfter, y = slopeLength, fill = T.during.heatwave))+
  geom_tile(color = "black")+
  scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse")

T.during.num.slope <- ggplot(data = looped.results %>% filter(daysAfter == 7), aes(x = slopeLength, y = numSlopes, fill = T.during.heatwave))+
  geom_tile(color = "black")+
  scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse")


ggarrange(R.during.days.num, R.during.days.slope, R.during.num.slope, 
          L.during.days.num, L.during.days.slope, L.during.num.slope,
          T.during.days.num, T.during.days.slope, T.during.num.slope, nrow = 3, ncol = 3)




# Peter
R.other.days.num <- ggplot(data = looped.results, aes(x = daysAfter, y = numSlopes, fill = R.all.other.days))+
  geom_tile(color = "black")+
  scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse")

R.other.days.slope <- ggplot(data = looped.results, aes(x = daysAfter, y = slopeLength, fill = R.all.other.days))+
  geom_tile(color = "black")+
  scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse")

R.other.num.slope <- ggplot(data = looped.results, aes(x = slopeLength, y = numSlopes, fill = R.all.other.days))+
  geom_tile(color = "black")+
  scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse")

# Paul
L.other.days.num <- ggplot(data = looped.results, aes(x = daysAfter, y = numSlopes, fill = L.all.other.days))+
  geom_tile(color = "black")+
  scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse")

L.other.days.slope <- ggplot(data = looped.results, aes(x = daysAfter, y = slopeLength, fill = L.all.other.days))+
  geom_tile(color = "black")+
  scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse")

L.other.num.slope <- ggplot(data = looped.results, aes(x = slopeLength, y = numSlopes, fill = L.all.other.days))+
  geom_tile(color = "black")+
  scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse")

# Tuesday
T.other.days.num <- ggplot(data = looped.results, aes(x = daysAfter, y = numSlopes, fill = T.all.other.days))+
  geom_tile(color = "black")+
  scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse")

T.other.days.slope <- ggplot(data = looped.results, aes(x = daysAfter, y = slopeLength, fill = T.all.other.days))+
  geom_tile(color = "black")+
  scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse")

T.other.num.slope <- ggplot(data = looped.results %>% filter(daysAfter == 7), aes(x = slopeLength, y = numSlopes, fill = T.all.other.days))+
  geom_tile(color = "black")+
  scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse")


ggarrange(R.other.days.num, R.other.days.slope, R.other.num.slope, 
          L.other.days.num, L.other.days.slope, L.other.num.slope,
          T.other.days.num, T.other.days.slope, T.other.num.slope, nrow = 3, ncol = 3)



# compare heatwave responses between lakes
ggplot(data = looped.results, aes(x = T.all.other.days, y = R.after.heatwave))+
  geom_point()+
  geom_smooth(stat = "smooth")

ggplot(data = looped.results, aes(x = T.during.heatwave, y = R.during.heatwave))+
  geom_point()+
  geom_smooth(stat = "smooth")

ggplot(data = looped.results, aes(x = L.all.other.days, y = R.all.other.days))+
  geom_point()+
  geom_smooth(stat = "smooth")

ggplot(data = looped.results, aes(x = L.all.other.days, y = R.all.other.days))+
  geom_point()+
  geom_smooth(stat = "smooth")


ggplot(data = looped.results, aes(x = R.after.heatwave, y = L.after.heatwave))+
  geom_point()+
  geom_smooth(stat = "smooth")

looped.results.actual = looped.results %>% filter(daysAfter == 0, numSlopes == 5 & slopeLength == 7)

ggplot(data = looped.results.actual, aes(x = R.after.heatwave, y = L.after.heatwave))+
  geom_point()+
  geom_smooth(stat = "smooth")

summary(lm(looped.results.actual$R.after.heatwave~looped.results.actual$L.after.heatwave))

#write.csv(looped.results, "./results/sensitivity results/looped results.csv", row.names = FALSE)



# are the slopes of chlorophyll between lakes correlated at all?

ggplot(allSlopes, aes(x = doyCat, y = percent_change))+
  geom_point()+
  geom_line()+
  facet_grid(lake~year)


all.Slopes.wide = allSlopes %>% 
  filter(daysAfterStart == 0) %>% 
  filter(!is.na(percent_change)) %>% 
  select(doyCat, lake_year, percent_change)

all.Slopes.wide = all.Slopes.wide %>% 
  group_by(doyCat) %>% 
  pivot_wider(names_from = lake_year, values_from = percent_change)









# 
# 
# ###### MOVING BOX OVER TIME #####
# 
# # Install and load necessary packages
# # install.packages("ggplot2")
# # install.packages("gganimate")
# #library(ggplot2)
# library(gganimate)
# 
# # Create a sample dataset
# data <- data.frame(x = 0:40)
# 
# # Define the width of the box
# box_width <- 5
# 
# # Create the plot
# p <- ggplot(data, aes(x = x)) +
#   geom_rect(aes(xmin = x - box_width / 2, xmax = x + box_width / 2, ymin = -Inf, ymax = Inf),
#             fill = "blue", alpha = 0.3) +
#   scale_x_continuous(limits = c(0, 40)) +
#   theme_minimal()
# 
# # Animate the plot
# anim <- p +
#   transition_states(x, transition_length = 2, state_length = 1) +
#   ease_aes('linear')
# 
# # Save the animation (optional)
# anim_save("moving_box.gif", animation = anim)
# 
# 
# 
# 
# 
# 
# library(ggplot2)
# library(gganimate)
# 
# # Create a sample dataset
# data <- data.frame(x = 0:40, y = 0:40)
# 
# # Define the width of the box
# box_width <- 5
# 
# # Create the plot with clamped xmin and xmax
# p <- ggplot(data, aes(x = x, y = y)) +
#   geom_point()+
#   geom_rect(aes(xmin = pmax(x - box_width / 2, 0), 
#                 xmax = pmin(x + box_width / 2, 40), ymin = -Inf, ymax = Inf),
#             fill = "blue", alpha = 0.3) +
#  # scale_x_continuous(limits = c(0, 40)) +
#   theme_minimal()
# 
# # Animate the plot
# anim <- p +
#   transition_states(x, transition_length = 1, state_length = 0.5) +
#   ease_aes('linear')
# 
# # Save the animation (optional)
# anim_save("moving_box.gif", animation = anim)
# 
# 
# 
# 
# 
# 
# library(ggplot2)
# library(gganimate)
# 
# # Create a sample dataset
# data <- data.frame(x = -5:45, y = 0:40)
# 
# # Define the width of the box
# box_width <- 5
# 
# data = data %>% mutate(rect.min = x)
# data = data %>% mutate(rect.max = x+box_width)
# 
# 
# 
# 
# # Create the plot with clamped xmin and xmax
# p <- ggplot(data, aes(x = x, y = y)) +
#   geom_point() +
#   geom_rect(aes(xmin = rect.min, 
#                 xmax = x + rect.max / 2, ymin = -Inf, ymax = Inf),
#             fill = "blue", alpha = 0.3) +
#   scale_x_continuous(limits = c(0, 50)) +
#   theme_bw()
# 
# # Animate the plot using transition_reveal for smoother movement
# anim <- p +
#   transition_states(x, transition_length = 1, state_length = 0.5) +
#   ease_aes('linear')
# 
# anim
# 
# # Save the animation (optional)
# anim_save("moving_box_smooth.gif", animation = anim)
# 
# 
# 
# anim <- p +
#   transition_reveal(along = x) +
#   ease_aes('linear')
# 
# anim
# 
# 
# 
# 
# #### more points
# 
# 
# 
# # Create a finer sample dataset for smoother movement
# data <- data.frame(x = seq(-5, 45, by = 0.1), y = seq(0, 40, length.out = 501))
# 
# # Define the width of the box
# box_width <- 5
# 
# data <- data %>% 
#   mutate(rect.min = x,
#          rect.max = x + box_width)
# 
# # Create the plot with clamped xmin and xmax
# # Create the plot with clamped xmin and xmax
# p <- ggplot(data, aes(x = x, y = y)) +
#   geom_point() +
#   geom_rect(aes(xmin = rect.min, 
#                 xmax = rect.max, ymin = -Inf, ymax = Inf, frames = x),
#             fill = "blue", alpha = 0.3) +
#   scale_x_continuous(limits = c(0, 50), 
#                      breaks = seq(0, 50, by = 5), 
#                      labels = seq(0, 50, by = 5)) +
#   theme_bw() +
#   theme(axis.ticks = element_line(color = "black"),
#         axis.text = element_text(color = "black"),
#         plot.margin = margin(1, 1, 1, 1, "cm"))
# 
# # Animate the plot with transition_manual for frame-by-frame movement
# anim <- p +
#   transition_manual(frames = x) +
#   ease_aes('linear')+
#   geom_point()
# 
# # Display the animation
# anim
# 
# 
# allSlopes.after = allSlopes %>% filter(period == "after heatwave")
# 
# allSlopes.after %>% filter(lake == "R") %>% 
#   ggplot(aes(x = daysAfter, y = percent_change, color = event_no))+
#   geom_point()
# 
# 
# allSlopes.after %>% group_by(daysAfter) %>% summarize(nrow(.))






##### save looped results #####
# write.csv(looped.results, "./results/sensitivity results/October 2024 looped results/start hw 2024_10_04.csv", row.names = FALSE)

# new colors to test:


# check which h