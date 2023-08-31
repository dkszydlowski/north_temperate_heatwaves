# old code

# code that has been cut but that might be useful later
# currently has random date comparison



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




##### RANDOM DAYS ######




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




