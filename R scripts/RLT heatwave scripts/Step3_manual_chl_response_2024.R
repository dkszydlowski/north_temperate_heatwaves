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

if (!require(readxl)) install.packages('readxl')
library(readxl)

if (!require(ggpubr)) install.packages('ggpubr')
library(ggpubr)

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

# read in the manual chl data
allData = read.csv("./formatted data/interpolated_manual_chl_for_slopes.csv")

# make a vector of unique lake years
lake_years = unique(allData$lake_year)

#==============================================================================#
#### CALCULATE SLOPES FOR CHLOROPHYLL ####

# cycle through each lake year, fit linear models for each rolling window, then store model slopes
# in the 'slopes' dataframe. Need to initially store the model results in a list

# then, calculate the percent change in chlorophyll relative to the baseline specified above

# Model results are the slopes, originally calculated based on the preceding 7 days

for(i in 1:length(lake_years)){
  
  temp = allData %>% filter(lake_year == lake_years[i]) # temp dataframe for this lake_year
  
  models <- slide(
    temp, 
    ~lm(mean_chl ~ doyCat, data = .x), 
    .before = slopeLength -1, # add the minus one so the slopeLength matches the user input
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
      temp$se[j] = summary(model)$coefficients["doyCat", "Std. Error"] 
      temp$r_squared[j] = summary(model)$r.squared # pull out r_squared from model
    }
    
    temp$percent_change = 100*temp$chl_slope*slopeLength/lag(temp$mean_chl, slopeLength-1, default = NA)
    temp$chl_baseline = lag(temp$mean_chl, slopeLength-1, default = NA)
  }
  
  if(i ==1){ slopes = temp} # if first iteration, creates slopes, the final dataframe
  
  if(i >1){slopes = rbind(slopes, temp)} # else, appends to slopes 
  
}


# test case to make sure the slide function works as we think it does
# test = slopes[1:7, ]
# summary(lm(test$mean_chl~test$doyCat))
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

 #Using a For Loop
 lengthHW = nrow(heatwaves)

 # take the average slope and calculate the percent change in chlorophyll based on the slope

 for(i in 1:lengthHW){
   slopes.subset = hwSlopes(heatwaves$date_start[i], heatwaves$date_end[i], heatwaves$lake[i], slopes)

   # save the mean of the slopes to the heatwaves dataframe
   heatwaves$averageSlope[i] = mean(slopes.subset$chl_slope, na.rm = TRUE)

   # save the standard deviation of the slopes to the heatwaves dataframe
   heatwaves$sdSlope[i] = sd(slopes.subset$chl_slope, na.rm = TRUE)

   # save the mean of the percent change to the heatwaves dataframe
   heatwaves$percentChange[i] = mean(slopes.subset$percent_change, na.rm = TRUE)
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


write.csv(heatwaves, "./formatted data/explanatory variables heatwaves/heatwaves with percent.csv")


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

for(daysAfterLoop in 0:20){
  
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
  if(daysAfterLoop == 0){
    allSlopes = slopes
  }
  if(daysAfterLoop > 0){
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




#==============================================================================#
#### RESPONSE TIMING LINE PLOTS ####

# calculate mean values of percent change for each lake by # of days after heatwave
mean_df <- allSlopes %>% 
  filter( period != "exclude after heatwave", !is.na(percent_change)) %>% 
  group_by(period, daysAfter) %>% 
  dplyr::summarise(mean_percent_change = mean(percent_change), 
                   sd_percent_change = sd(percent_change), 
                   number_percent_change = n(),
                   median_percent_change = median(percent_change)) 

# Peter
mean_dfR <- allSlopes %>% filter(lake == "R") %>% 
  filter(period != "exclude after heatwave", !is.na(percent_change)) %>% 
  group_by(period, daysAfter) %>% 
  dplyr::summarise(mean_percent_change = mean(percent_change), 
                   sd_percent_change = sd(percent_change), 
                   number_percent_change = n()) 

# Paul
mean_dfL <- allSlopes %>% filter(lake == "L") %>% 
  filter( period != "exclude after heatwave", !is.na(percent_change)) %>% 
  group_by(period, daysAfter) %>% 
  dplyr::summarise(mean_percent_change = mean(percent_change), 
                   sd_percent_change = sd(percent_change), 
                   number_percent_change = n()) 

# Tuesday
mean_dfT <- allSlopes %>% filter(lake == "T") %>% 
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

# Plot the response over time
All.over.time <- mean_df %>% filter(daysAfter > 0) %>% 
  ggplot(aes( x= daysAfter, y = mean_percent_change, color = period))+
  scale_color_manual(values = c("during heatwave" = "#ff0000", "after heatwave" = "#ffc100", "all other days" = "#88CCEE"))+
  scale_fill_manual(values = c("during heatwave" = "#ff0000", "after heatwave" = "#ffc100", "all other days" = "#88CCEE"))+
  geom_line(size = 1)+
  geom_point()+
  labs(title = "All lakes HW response over time")+
  ylim(min(mean.all.by.lake$mean_percent_change)-10, max(mean.all.by.lake$mean_percent_change)+10)+
 # geom_ribbon(aes(ymin = mean_percent_change - sd_percent_change, ymax = mean_percent_change + sd_percent_change, fill = period), alpha = 0.1)+
  labs(x = "days after heatwave")+
theme_classic()

R.over.time <- mean_dfR %>% filter(daysAfter > 0) %>% 
  ggplot(aes( x= daysAfter, y = mean_percent_change, color = period))+
  geom_line(size = 1)+
  geom_point()+
  labs(x = "days after heatwave")+
  labs(title = "Peter Lake HW response over time")+
  scale_color_manual(values = c("during heatwave" = "#ff0000", "after heatwave" = "#ffc100", "all other days" = "#88CCEE"))+
  ylim(min(mean.all.by.lake$mean_percent_change)-10, max(mean.all.by.lake$mean_percent_change)+10)+
  theme_classic()

L.over.time <- mean_dfL %>% filter(daysAfter > 0) %>% 
  ggplot(aes( x= daysAfter, y = mean_percent_change, color = period))+
  geom_line(size = 1)+
  geom_point()+
  labs(x = "days after heatwave")+
  labs(title = "Paul Lake HW response over time")+
  ylim(min(mean.all.by.lake$mean_percent_change)-10, max(mean.all.by.lake$mean_percent_change)+10)+
  scale_color_manual(values = c("during heatwave" = "#ff0000", "after heatwave" = "#ffc100", "all other days" = "#88CCEE"))+
  theme_classic()

T.over.time <- mean_dfT %>% filter(daysAfter > 0) %>% 
  ggplot(aes( x= daysAfter, y = mean_percent_change, color = period))+
  scale_color_manual(values = c("during heatwave" = "#ff0000", "after heatwave" = "#ffc100", "all other days" = "#88CCEE"))+
  #scale_fill_manual(values = c("during heatwave" = "#ff0000", "after heatwave" = "#ffc100", "all other days" = "#88CCEE"))+
  geom_line(size = 1)+
  geom_point()+
  labs(title = "Tuesday Lake HW response over time")+
  ylim(min(mean.all.by.lake$mean_percent_change)-10, max(mean.all.by.lake$mean_percent_change)+10)+
  #geom_ribbon(aes(ymin = mean_percent_change - sd_percent_change, ymax = mean_percent_change + sd_percent_change, fill = period), alpha = 0.1)+
  labs(x = "days after heatwave")+
  theme_classic()


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



 
# testing color




allSlopes %>%
  filter(period != "exclude after heatwave", lake == "L", daysAfter == get("daysAfter", envir=globalenv())) %>%
  ggplot(aes(x = percent_change, y = factor(period, levels = desired_order), fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_gradientn(colours = c("#4AB5C4", "forestgreen"),
                       values = scales::rescale(c(-200, 300, 300, 600))) +
  labs(title = 'All Lakes') +
  theme_classic() +
  geom_text(data = mean_dfL %>% filter(daysAfter == get("daysAfter", envir=globalenv())),
            aes(x = mean_percent_change,
                y = factor(period, levels = desired_order),
                label = as.character(round(mean_percent_change, digits = 0))),
            color = "black",
            size = 4,
            vjust = 2)



#"R" = "#4AB5C4", "L" = "#ADDAE3", "T"=  "#BAAD8D"
  
  
  

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
  xlab("% change in surface chlorophyll-a")+
  labs(title = "Tuesday") +
  scale_fill_manual(values = c("during heatwave" = "#ff0000", "after heatwave" = "#ffc100", "all other days" = "#88CCEE")) +  # Specify fill colors for groups
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
  xlab("% change in surface chlorophyll-a")+
  labs(title = "Peter") +
  scale_fill_manual(values = c("during heatwave" = "#ff0000", "after heatwave" = "#ffc100", "all other days" = "#88CCEE")) +  # Specify fill colors for groups
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
  ylab("") +
  xlab("% change in surface chlorophyll-a")+
  labs(title = "Paul") +
  scale_fill_manual(values = c("during heatwave" = "#ff0000", "after heatwave" = "#ffc100", "all other days" = "#88CCEE")) +  # Specify fill colors for groups
  theme_classic()+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=18,face="bold"))+
  annotate("text",  x=Inf, y = Inf, label = paste("Days after heatwave: ", daysAfter, sep = ""), vjust=1, hjust=1)


#dev.off()


#==============================================================================#
#### Explanatory plots with average slopes ####

# add the new calculated average slopes to the old exp dataframe
exp = read_xlsx("./formatted data/explanatory_variables_heatwaves.xlsx")

# length of each heatwave
exp$length = as.numeric(exp$end_date - exp$start_date)

# make a new dataframe that only has the columns of results we are interested in for now
priority.results = results %>% select(lake, year, date_start, date_end, percentChange) %>% 
  dplyr::rename(start_date = date_start, end_date = date_end) %>% 
  mutate(start_date = as.Date(start_date), end_date = as.Date(end_date))

exp = exp %>% select(-comments, -percent_change)

exp = exp %>% full_join(priority.results, by = c("lake", "year", "start_date", "end_date")) %>% 
  dplyr::rename(percent_change = percentChange)

exp$percent_change = as.numeric(exp$percent_change)


hw.pload <- ggplot(data = exp, aes(x = p_loading_mg_m2, y = abs(percent_change)))+
  geom_point(size = 3, fill = "orangered3", pch = 21)+
  theme_classic()+
  labs(y = "abs. % change in surface chl",  x = "P loading (mg/m2)")


hw.color <- ggplot(data = exp, aes(x = color_m_1, y = abs(percent_change)))+
  geom_point(size = 3, fill = "saddlebrown", pch = 21)+
  theme_classic()+
  labs(y = "abs. % change in surface chl",  x = "water color g440 (m-1)")


# plot of the percent change in chlorophyll seasonally

exp = exp %>% mutate(doy.start = yday(start_date)) %>% 
  mutate(month = month(start_date))

hw.doy.start <- ggplot(data = exp, aes(x = doy.start, y = abs(percent_change), fill = lake))+
  geom_point(size = 3, color = "black", shape = 21, stroke = 1, alpha = 0.7)+
  theme_classic()+
  labs(y = "abs. % change in surface chl", x = "heatwave day of year")+
  scale_fill_manual(values = c("L" = "steelblue2", "R" = "black", "T" = "white"))

hw.length <- ggplot(data = exp, aes(x = length, y = abs(percent_change), fill = lake))+
  geom_point(size = 3, color = "black", shape = 21, stroke = 1, alpha = 0.7)+
  theme_classic()+
  labs(y = "abs. % change in surface chl", x = "heatwave length (days)")+
  scale_fill_manual(values = c("L" = "steelblue2", "R" = "black", "T" = "white"))

#==============================================================================#
#### Example plot of what the heatwave window is looking at ####




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


pdf(paste("./figures/sensitivity tests/" , pdfName, sep = ""), height = 6, width = 10)

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

dev.off()

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
  geom_text(data = mean_df %>% filter(daysAfter == get("daysAfter", envir=globalenv())),
            aes(x = mean_percent_change,
                y = factor(period, levels = desired_order),  # Use factor with desired order
                label = as.character(round(mean_percent_change, digits = 0))),
            color = "black",
            size = 4,
            vjust = 2) +
  geom_text(data = mean_df %>% filter(daysAfter == get("daysAfter", envir=globalenv())),
            aes(x = 400,
                y = factor(period, levels = desired_order),  # Use factor with desired order
                label = paste("n = ", number_percent_change, sep = "")),
            color = "black",
            size = 4,
            vjust = 2) +
  ylab("") +
  xlab("")+
  #labs(title = "All lakes") +
  scale_fill_manual(values = c("during heatwave" = "mediumseagreen", "all other days" = "#88CCEE")) +  # Specify fill colors for groups
  theme_classic()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12))+
  theme(axis.text.y = element_blank())+
  theme(legend.position="top")





# Tuesday
Tdist <- allSlopes %>%
  filter(lake == "T") %>%
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
  geom_text(data = mean_dfT %>% filter(daysAfter == get("daysAfter", envir=globalenv())),
            aes(x = mean_percent_change,
                y = factor(period, levels = desired_order),  # Use factor with desired order
                label = as.character(round(mean_percent_change, digits = 0))),
            color = "black",
            size = 4,
            vjust = 1) +
  geom_text(data = mean_dfT %>% filter(daysAfter == get("daysAfter", envir=globalenv())),
            aes(x = 400,
                y = factor(period, levels = desired_order),  # Use factor with desired order
                label = paste("n = ", number_percent_change, sep = "")),
            color = "black",
            size = 4,
            vjust = 2) +
  ylab("") +
  xlab("% change in surface \n chlorophyll-a")+
  labs(title = "Tuesday") +
  scale_fill_manual(values = c("during heatwave" = "mediumseagreen", "all other days" = "#88CCEE")) +  # Specify fill colors for groups
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
  xlim(-200, 600)+
  # geom_density_ridges(alpha = 0.7,
  #                     quantile_lines = TRUE,
  #                     quantile_fun = function(x, ...) median(x), 
  #                     scale = 2, size = 0.7) +
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
            aes(x = 400,
                y = factor(period, levels = desired_order),  # Use factor with desired order
                label = paste("n = ", number_percent_change, sep = "")),
            color = "black",
            size = 4,
            vjust = 2) +
  ylab("") +
  xlab("% change in surface \n chlorophyll-a")+
  labs(title = "Peter") +
  scale_fill_manual(values = c("during heatwave" = "mediumseagreen", "all other days" = "#88CCEE")) +  # Specify fill colors for groups
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
  xlim(-200, 600)+
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
  scale_fill_manual(values = c("during heatwave" = "mediumseagreen", "all other days" = "#88CCEE")) +  # Specify fill colors for groups
  theme_classic()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12))+
  theme(axis.text.y = element_blank())+
  theme(legend.position="none")


big_plot <- Alldist + plot_layout(guides = 'keep')
small_plots <- Ldist | Rdist | Tdist


# save to a png

png("./figures/manuscript 03_18_2024/distributions.png", height = 8, width = 9, res = 300, units = "in")

big_plot/small_plots

dev.off()




# save individual heatwave response

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

results <- results %>%
  group_by(event_group) %>%
  mutate(event_group_start = mean(date_start))

results <- results %>%
  ungroup() %>% 
  group_by(event_group) %>%
  mutate(event_group_start = mean(as.numeric(date_start)))

grouped_start_dates <- results %>%
  dplyr::group_by(event_group) %>%
  summarize(event_group_start = mean(as.numeric(date_start)))


  ggplot(data = results, aes(x = as.factor(event_group_start), y = percentChange, fill = lake))+
  geom_bar(stat = "identity", position = "dodge", alpha = 0.5, color = "black")+
  theme_classic()+  
  ylab("% change in surface chlorophyll-a")+
  xlab("start date of heatwave")+
  ggtitle("Average chl percent change by event")+
  scale_fill_manual(values = c("R"=  "#4AB5C4", "L" = "#ADDAE3", "T"=  "#BAAD8D"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#  facet_wrap(~event_group)


  ggplot(data = results, aes(x = as.character(date_start), y = percentChange, fill = lake))+
    geom_bar(stat = "identity", position = "dodge", alpha = 0.5, color = "black")+
    theme_classic()+  
    ylab("% change in surface chlorophyll-a")+
    xlab("start date of heatwave")+
    ggtitle("Average chl percent change by event")+
    scale_fill_manual(values = c("R"=  "#4AB5C4", "L" = "#ADDAE3", "T"=  "#BAAD8D"))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  

c("2009-06-02", "2009-06-07") %overlaps% c("2009-06-07", "2009-06-09")


# testing color


#"R" = "#4AB5C4", "L" = "#ADDAE3", "T"=  "#BAAD8D"

# 
# Rdist<- allSlopes %>%
#   filter(period != "exclude after heatwave", lake == "R", daysAfter == get("daysAfter", envir=globalenv())) %>%
#   ggplot(aes(x = percent_change, y = factor(period, levels = desired_order), fill = stat(x))) +
#   geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, quantile_lines = TRUE,
#                                quantile_fun = function(x, ...) mean(x), 
#                                size = 0.7, alpha = 0.7) +
#   scale_fill_gradientn(colours = c("#4AB5C4", "forestgreen"),
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
#   scale_fill_gradientn(colours = c("#ADDAE3", "forestgreen"),
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
#   scale_fill_gradientn(colours = c("#BAAD8D", "forestgreen"),
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

#"R" = "#4AB5C4", "L" = "#ADDAE3", "T"=  "#BAAD8D"



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

#write.csv(looped.results, "./results/sensitivity results/looped results.csv", row.names = FALSE)

