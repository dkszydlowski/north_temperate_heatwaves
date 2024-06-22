# step 2- format the data so it is useable in the slopes calculations
# interpolates any missing values in the chlorophyll

# produces an interpolated dataset
# write.csv(allData, "./formatted data/interpolated_manual_chl_for_slopes.csv", row.names = FALSE)


#### read in packages ####
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse) 


#### Read in and interpolate data ####
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


# make a lake_year column in allData
allData = allData %>% 
  mutate(lake_year = paste(lake, year, sep = "_"))


# save allData so that it can be loaded in the next step for calculating slopes

write.csv(allData, "./formatted data/interpolated_manual_chl_for_slopes.csv", row.names = FALSE)
