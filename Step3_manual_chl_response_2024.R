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

#==============================================================================#
#### levers we can pull ####

# slope and percent calculations
slopeLength = 8 # length of the rolling window slope to be calculated
baselineDate = 1 # how many days before the start of the heatwave to use as baseline chl conditions, currently an integer

# build in options here for during the heatwave, or at the beginning of the calculated slope

# slope aggregation choices

daysAfter = 3 # time lag of how many days after the heatwave we want to look
numSlopes = 5 # the number of slopes we want to include in analysis

exclude.after.heatwaves = FALSE # if TRUE, excludes slopes for days within 20 days 
# of the heatwave that don't fall within our aggregating window

# timing choices
# change analysis so that we can look during the heatwave, or after the heatwave? 


#==============================================================================#
#### output of settings ####

cur_date_time = format(Sys.time(), "%Y-%m-%d %H:%M:%S")

plot_metadata <- data.frame(slopeLength = paste("Length of each slope: ", slopeLength, sep = ""),
                             baselineDate = paste("Baseline chl for calculating percent change: ", baselineDate, sep = ""),
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
  geom_text(data = plot_metadata, aes(x = 5, y = 5, label = baselineDate),
            size = 5, hjust = 0.5, vjust = -12)+
  geom_text(data = plot_metadata, aes(x = 5, y = 5, label = daysAfter),
            size = 5, hjust = 0.5, vjust = -8)+
  geom_text(data = plot_metadata, aes(x = 5, y = 5, label = numSlopes),
            size = 5, hjust = 0.5, vjust = -6)+
  geom_text(data = plot_metadata, aes(x = 5, y = 5, label = exclude.after.heatwaves),
            size = 5, hjust = 0.5, vjust = -4)+
  geom_text(data = plot_metadata, aes(x = 5, y = 5, label = time),
            size = 5, hjust = 0.5, vjust = 14)
                  



#==============================================================================#
#### read in the data ####
# read in the heatwaves data calculated in the previous step
heatwaves = read.csv("./formatted data/heatwavesdata.csv")

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

  # because our slopes are calculated over a 7-day rolling window, the first slope considered will
  # encompass the first week after the heatwave
  
  startDate = as.Date(heatwaveEnd)+daysAfter 
  endDate = as.Date(heatwaveEnd)+daysAfter+numSlopes
  
  # this is the date before the heatwave, which we will use to calculate the percent
  # change in chlorophyll
  
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

#break up by lake by filtering data
resultsT = results %>% filter(lake == "T")
resultsL = results %>% filter(lake == "L")
resultsR = results %>% filter(lake == "R")

#use ggplot to make three plots, one for each lake with date of heatwave on x-axis
#and average slope on the y

#png(filename = "./figures/preliminary figures/R_slopes_2023_02_09", height = 8, width = 11, units = "in", res = 300)

indHWResp <- ggplot(data = results, aes(x = date_start, y = percentChange, fill = lake))+
  geom_bar(stat = "identity", position = "identity", alpha = 0.5, color = "black")+
  theme_classic()+
  ylab("percent change in chl")+
  xlab("start date of heatwave")+
  ggtitle("Average chl percent change by event")+
  scale_fill_manual(values = c("R"=  "#4AB5C4", "L" = "#ADDAE3", "T"=  "#BAAD8D"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(data = resultsR, aes(x = date_end, y = percentChange))+
  geom_bar(stat='identity', fill = "#4AB5C4", color = "black")+
  theme_classic()+
  ylab("percent change in chl")+
  xlab("end date of heatwave")+
  ggtitle("Peter")

ggplot(data = resultsL, aes(x = date_end, y = percentChange))+
  geom_bar(stat='identity', fill = "#ADDAE3", color = "black")+
  theme_classic()+
  ylab("percent change in chl")+
  xlab("end date of heatwave")+
  ggtitle("Paul")

ggplot(data = resultsT, aes(x = date_end, y = percentChange))+
  geom_bar(stat='identity', fill = "#BAAD8D", color = "black")+
  theme_classic()+
  ylab("percent change in chl")+
  xlab("end date of heatwave")+
  ggtitle("Tuesday")



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
    
    datesAnalyzed = seq(end+daysAfterLoop, end + daysAfterLoop+numSlopes, 1)
    
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


# Plot the response over time
all.over.time <- mean_df %>% filter(daysAfter > 0) %>% 
  ggplot(aes( x= daysAfter, y = mean_percent_change, color = period))+
  scale_color_manual(values = c("during heatwave" = "#ff0000", "after heatwave" = "#ffc100", "all other days" = "#88CCEE"))+
  scale_fill_manual(values = c("during heatwave" = "#ff0000", "after heatwave" = "#ffc100", "all other days" = "#88CCEE"))+
  geom_line(size = 1)+
  geom_point()+
 # geom_ribbon(aes(ymin = mean_percent_change - sd_percent_change, ymax = mean_percent_change + sd_percent_change, fill = period), alpha = 0.1)+
  labs(x = "days after heatwave")+
theme_classic()



R.over.time <- mean_dfR %>% filter(daysAfter > 0) %>% 
  ggplot(aes( x= daysAfter, y = mean_percent_change, color = period))+
  geom_line(size = 1)+
  geom_point()+
  labs(x = "days after heatwave")+
  scale_color_manual(values = c("during heatwave" = "#ff0000", "after heatwave" = "#ffc100", "all other days" = "#88CCEE"))+
  theme_classic()

L.over.time <- mean_dfL %>% filter(daysAfter > 0) %>% 
  ggplot(aes( x= daysAfter, y = mean_percent_change, color = period))+
  geom_line(size = 1)+
  geom_point()+
  labs(x = "days after heatwave")+
  scale_color_manual(values = c("during heatwave" = "#ff0000", "after heatwave" = "#ffc100", "all other days" = "#88CCEE"))+
  theme_classic()


T.over.time <- mean_dfT %>% filter(daysAfter > 0) %>% 
  ggplot(aes( x= daysAfter, y = mean_percent_change, color = period))+
  scale_color_manual(values = c("during heatwave" = "#ff0000", "after heatwave" = "#ffc100", "all other days" = "#88CCEE"))+
  #scale_fill_manual(values = c("during heatwave" = "#ff0000", "after heatwave" = "#ffc100", "all other days" = "#88CCEE"))+
  geom_line(size = 1)+
  geom_point()+
  #geom_ribbon(aes(ymin = mean_percent_change - sd_percent_change, ymax = mean_percent_change + sd_percent_change, fill = period), alpha = 0.1)+
  labs(x = "days after heatwave")+
  theme_classic()


#==============================================================================#
#### PLOTTING DISTRIBUTIONS ####


# Create a factor variable with the desired order for 'period'
desired_order <- c("after heatwave", "during heatwave", "all other days")

#png("./figures/science in the northwoods figures/Tuesday Lake heatwave results 4 days after.png", height = 7, width = 13, units = "in", res = 600)

allSlopes = allSlopes %>% mutate(period = replace(period, period == "exclude after heatwave", "all other days"))

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
        axis.title=element_text(size=18,face="bold"))



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
        axis.title=element_text(size=18,face="bold"))

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
        axis.title=element_text(size=18,face="bold"))

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
        axis.title=element_text(size=18,face="bold"))

#dev.off()





#==============================================================================#
#### CREATE PDF OF OUTPUTS ####

pdfName = paste("daysAfter", daysAfter, "slopeLength", slopeLength, sep = "_")

pdf("./figures/sensitivity tests/testing.pdf", height = 6, width = 10)

print(metadata_plot)
print(Alldist)
print(Rdist)
print(Ldist)
print(Tdist)
print(indHWResp)


dev.off()











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
