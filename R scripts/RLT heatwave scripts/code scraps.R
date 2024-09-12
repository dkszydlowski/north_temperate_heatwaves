
# original sorting code

# variable which lets us toggle whether results are relative to start or end of heatwave
relative.to.hw = "start"

for(daysAfterLoop in -20:40){
  
  slopes$period = "all other days"
  
  # add a column to slopes which indicates whether or not there is a heatwave
  for(i in 1:nrow(heatwaves)){
    
    start = heatwaves$date_start[i] # start date of the current heatwave
    end = heatwaves$date_end[i] # end date of the current heatwave
    
    # current lake of the heatwave
    hw.lake = heatwaves$lake[i]
    hw.year = heatwaves$year[i]
    
    # sequence of dates during the heatwave
    dates = seq(start, end, 1)
    
    numSlopes = length(dates)
    
    # excludes dates that are part of rolling window but not currently considered in after heatwave
    # dates that are within 40 days after a heatwave but not in our window for analysis as
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
    #slopes = slopes %>% mutate(period = replace(period, date %in% datesExcluded & lake == hw.lake, "exclude after heatwave"))
    slopes = slopes %>% mutate(period = replace(period, date %in% datesAnalyzed & lake == hw.lake, "after heatwave"))
    slopes = slopes %>% mutate(period = replace(period, date %in% dates & lake == hw.lake, "during heatwave"))
    
    slopes$daysAfter = daysAfterLoop
    
    # combine to the main dataframe
    if(daysAfterLoop == -20 & i == 1){
      slopes$event_no = heatwaves$event_no[i]
      allSlopes = slopes %>% filter(lake == hw.lake & year == hw.year)
    }
    if(daysAfterLoop > -20 & i > 1){
      slopes$event_no = heatwaves$event_no[i]
      allSlopes = rbind(allSlopes, slopes) %>% filter(lake == hw.lake & year == hw.year)
    }
    
  }
  
  
  
}





# old code

# code that has been cut but that might be useful later
# currently has random date comparison


# testing color gradients in distribution plots

# 
# allSlopes %>%
#   filter(period != "exclude after heatwave", lake == "L", daysAfter == get("daysAfter", envir=globalenv())) %>%
#   ggplot(aes(x = percent_change, y = factor(period, levels = desired_order), fill = stat(x))) +
#   geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
#   scale_fill_gradientn(colours = c("#4AB5C4", "forestgreen"),
#                        values = scales::rescale(c(-200, 300, 300, 600))) +
#   labs(title = 'All Lakes') +
#   theme_classic() +
#   geom_text(data = mean_dfL %>% filter(daysAfter == get("daysAfter", envir=globalenv())),
#             aes(x = mean_percent_change,
#                 y = factor(period, levels = desired_order),
#                 label = as.character(round(mean_percent_change, digits = 0))),
#             color = "black",
#             size = 4,
#             vjust = 2)

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




##### VISUALIZE REGIONS CALCULATED #######

##### re-make the plots, but do it in a for loop and show where random days are located
# and where the slopes are calculated following each heatwave

library(tidyverse)

# read in the data
allSonde = read.csv("./formatted data/allSonde_interpolated.csv")
allSlopes = read.csv("./formatted data/results_random_and_heatwaves.csv")

# make normalized temperature and chlorophyll columns in allSonde
allSonde = allSonde %>%  
  mutate(normTemp = 100 * mean_temp/mean_temp[1], normChl = 100 * mean_chl/mean_chl[1])

allSonde$date = as.Date(allSonde$date)

# in a for loop, plot raw data, the heatwave, slope, and random days for
# each lake_year combination

i = 1

lake_years = unique(allSonde$lake_year)

# create a dataframe of lake year slopes
lySlope = allSlopes %>% filter(lake_year == lake_years[i])

# create a dataframe of lake year data
lyAllSonde = allSonde %>% filter(lake_year == lake_years[i])



text = "annotate('rect', xmin = as.Date('2008-07-15'), xmax = as.Date('2008-07-19'), ymin = 0, ymax = Inf,
         fill = 'red', alpha = 0.3)+ annotate('rect', xmin = as.Date('2008-06-15'), xmax = as.Date('2008-07-19'), ymin = 0, ymax = Inf,
         fill = 'red', alpha = 0.3)"

ggplot(data=lyAllSonde, aes(x=date, y=normChl)) + 
  geom_point() +
  geom_line(aes(x = date, y = normTemp), size = 1.5)+
  geom_density_line(aes(x = date, y = normTemp), stat = "identity", size = 0.5, fill = "steelblue3", alpha = 0.3)+
  geom_line(size = 1.5) +
  geom_density_line(stat = "identity", size = 1.5, fill = "forestgreen", alpha = 0.3)+
  theme_classic()+
  labs(title = paste(lyAllSonde$lake[1], "Lake", lyAllSonde$year[1]), y = "% of initial value")+
  theme(title = element_text(size = 20))+
  #eval(parse(text = text))+
  # annotate('rect', xmin = as.Date('2019-06-15'), xmax = as.Date('2019-06-23'), ymin = 0, ymax = Inf,
  #          fill = 'blue', alpha = 0.3)+
  theme(text = element_text(size = 20))











####### OLD SHINY APP #############


# Sensitivity test shiny app
# allows us to manually look at different outcomes if we select different variables

if (!require(shiny)) install.packages('shiny')
library(shiny)

if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

if (!require(ggpubr)) install.packages('ggpubr')
library(ggpubr)


#### Shiny app ####


# Define UI
ui <- fluidPage(
  titlePanel("Vary slope length"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("slope_length_slider", "slopeLength:", min = 3, max = 14, value = 7, step = 1)
    ),
    mainPanel(
      plotOutput("plot", height = "500px", width = "800px")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Load the data
  looped.results <- reactive({
    read.csv("looped results.csv")
    
    
    
  })
  
  # Filter data based on slider input
  filtered_data <- reactive({
    req(looped.results())  # Ensure the data is loaded
    looped.results() %>% filter(slopeLength == input$slope_length_slider)
  })
  
  # Create ggplot
  output$plot <- renderPlot({
    req(filtered_data())  # Ensure the filtered data is available
    
    a <- ggplot(data = filtered_data(), aes(x = daysAfter, y = numSlopes, fill = R.after.heatwave)) +
      geom_tile(color = "black") +
      scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse") +
      labs(title = "Peter during and after heatwave") +
      theme_classic()
    
    b <- ggplot(data = filtered_data(), aes(x = daysAfter, y = numSlopes, fill = L.after.heatwave)) +
      geom_tile(color = "black") +
      scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse") +
      labs(title = "Paul during and after heatwave") +
      theme_classic()
    
    c <- ggplot(data = filtered_data(), aes(x = daysAfter, y = numSlopes, fill = T.after.heatwave)) +
      geom_tile(color = "black") +
      scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse") +
      labs(title = "Tuesday during and after heatwave") +
      theme_classic()
    
    d <- ggplot(data = filtered_data(), aes(x = daysAfter, y = numSlopes, fill = R.all.other.days)) +
      geom_tile(color = "black") +
      scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse") +
      labs(title = "Peter all other days") +
      theme_classic()
    
    e <- ggplot(data = filtered_data(), aes(x = daysAfter, y = numSlopes, fill = L.all.other.days)) +
      geom_tile(color = "black") +
      scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse") +
      labs(title = "Paul all other days") +
      theme_classic()
    
    f <- ggplot(data = filtered_data(), aes(x = daysAfter, y = numSlopes, fill = T.all.other.days)) +
      geom_tile(color = "black") +
      scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse") +
      labs(title = "Tuesday all other days") +
      theme_classic()
    
    ggarrange(a, b, c, d, e, f, nrow = 3, ncol = 2)
  })
}

# Run the application
shinyApp(ui, server)






####### old version of the slopes code ########
Slopes = 1
daysAfterLoop = -14
i = 1
slopes$event_no = NA

for(daysAfterLoop in -14:40){
  
  # add a column to slopes which indicates whether or not there is a heatwave
  for(i in 1:nrow(heatwaves)){
    
    print(i)
    start = heatwaves$date_start[i] # start date of the current heatwave
    end = heatwaves$date_end[i] # end date of the current heatwave
    
    hw.lake = heatwaves$lake[i]
    
    # sequence of dates during the heatwave
    dates = seq(start, end, 1)
    
    # excludes dates that are part of rolling window but not currently considered in after heatwave
    # dates that are within 40 days after a heatwave but not in our window for analysis as
    # after heatwave because of our current daysAfterLoop selection.
    # This gets overwrritten in part by "after heatwave" for those dates that are included
    # in analysis
    datesExcluded = seq(end -14, end + 40, 1) 
    
    # dates to be analyzed after the heatwaves
    # this creates a sequence of numbers, from the end of the heatwave plus whatever
    # daysAfterLoop is set to, to that same value plus the numSlopes. So it sets a window daysAfterLooped
    # X number of days after the heatwave
    
    datesAnalyzed = seq(end+daysAfterLoop, end + daysAfterLoop+numSlopes-1, 1)
    
    # fill the dataframe "period" column with the correct categorization
    
    # fill the dataframe "period" column with the correct categorization
    slopes = slopes %>% mutate(period = replace(period, date %in% datesExcluded & lake == hw.lake & period != "after heatwave", "exclude after heatwave"))
    slopes = slopes %>% mutate(period = replace(period, date %in% dates & lake == hw.lake, "during heatwave"))
    slopes = slopes %>% mutate(period = replace(period, date %in% datesAnalyzed & lake == hw.lake, "after heatwave"))
    
    slopes$daysAfter = daysAfterLoop
  }
  
  # combine to the main dataframe
  if(daysAfterLoop == -14){
    slopes$event_no = heatwaves$event_no[i]
    allSlopes = slopes
  }
  if(daysAfterLoop > -14){
    slopes$event_no = heatwaves$event_no[i]
    allSlopes = rbind(allSlopes, slopes)
  }
  
}

allSlopes.after = allSlopes %>% filter(period == "after heatwave")


# update so that we are not excluding other dates after the heatwave
allSlopes = allSlopes %>% mutate(period = replace(period, period == "exclude after heatwave", "all other days")) %>%
  mutate(period = replace(period, period == "during heatwave", "all other days"))



