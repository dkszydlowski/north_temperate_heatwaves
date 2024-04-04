# plots of temperature 

library(tidyverse)
library(heatwaveR)
library(ggplot2)
library(ggjoy)
library(ggbraid)

allSonde = read.csv("./formatted data/CombinedData.csv")


# read in the heatwave outputs
peterHW = readRDS(file = "./results/heatwave modeled outputs/peter heatwave outputs modeled.rds")
paulHW = readRDS(file = "./results/heatwave modeled outputs/paul heatwave outputs modeled.rds")
tuesdayHW = readRDS(file = "./results/heatwave modeled outputs/tuesday heatwave outputs modeled.rds")

climatology.R = peterHW$climatology %>% mutate(lake = "peter")
climatology.L = paulHW$climatology %>% mutate(lake = "paul")
climatology.T = tuesdayHW$climatology %>% mutate(lake = "tuesday")

climatology.R$year = as.factor(year(climatology.R$t))
climatology.T$year = as.factor(year(climatology.T$t))
climatology.L$year = as.factor(year(climatology.L$t))

ggplot(climatology.R, aes(x = doy, y = temp))+
  ylim(10, 30)+
  geom_line(color = "black", size = 1)+
  geom_area(fill = "lightblue")+
  geom_line(aes(x = doy, y = thresh), color = "forestgreen", size = 0.7)+
  geom_line(aes(x = doy, y = seas), size = 0.7)+
  geom_flame(aes(y2 = thresh), fill = "red")+
  facet_wrap(~as.factor(year))+
  theme_classic()



ggplot(climatology.R, aes(x = doy, y = temp, color = as.factor(year)))+
  geom_line(alpha = 0.5)


ggplot(climatology.R, aes(x = doy, y = temp, fill = as.factor(year))) +
  geom_joy(scale = 0.5) +
  theme_minimal()

library(ggridges)




ggplot(climatology.R, aes(x = doy, y = temp, group = year)) +
  geom_line(aes(color = as.factor(year)), alpha = 0.5) +
  facet_wrap(~year, ncol = 1) +
  theme_minimal()


ggplot(climatology.R, aes(x = doy, y = temp, group = year)) +
  geom_line(aes(color = as.factor(year)), alpha = 0.5) +
  facet_wrap(~year, ncol = 1) +
  theme_minimal()+
  geom_area(fill = as.factor(year), alpha = 0.8)


# Convert year to a factor for discrete y-axis
climatology.R$year <- as.factor(climatology.R$year)

climatology.R = climatology.R %>% mutate(heatwave = temp > thresh)


##### Plotting final plots ######
ggplot(climatology.R, aes(x = doy, y = year, height = temp/10-1.2)) +
  geom_ridgelines2(fill = "#4AB5C4") +
  #geom_line(aes(x = doy, y = thresh, group = year), color = "red") +  # Add individual lines
  theme_minimal()+
  geom_ridgeline(aes(x = doy, y = year, height = thresh/10 - 1.2), fill = NA, color = "forestgreen")
  #geom_vline(xintercept = 200, aes(group = year))

climatology.R = climatology.R %>% mutate(event_no = replace(event_no, is.na(event_no), 100))

ggplot(climatology.R, aes(x = doy, y = year, height = temp/10- 1.2)) +
  geom_ridgeline(aes(fill = as.factor(event_no))) +
  #geom_line(aes(x = doy, y = thresh, group = year), color = "orange") +  # Add individual lines
  theme_minimal()+
  scale_fill_manual(values = c("1" = "orange", "2" = "orange", "3" = "orange", "4" = "orange",
                               "5" = "orange", "6" = "orange", "7" = "orange", "8" = "orange", "9" = "orange",
                                "10" = "orange", "11" = "orange", "12" = "orange", 
                                "13" = "orange", "14" = "orange", "15" = "orange" , "100" = "#4AB5C4")) +  # Specify fill colors manually
  geom_ridgeline(aes(x = doy, y = year, height = thresh/10 - 1.2), fill = NA, color = "forestgreen")+
  geom_ridgeline(aes(x = doy, y = year, height = seas/10 - 1.2), fill = NA, color = "black")

#geom_vline(xintercept = 200, aes(group = year))



ggplot(climatology.R, aes(x = doy, y = year, height = temp/10 - 1.2, fill = fill_color)) +
  geom_ridgeline() +
  scale_fill_manual(values = c("red", "orange", "orange", "orange", "orange","orange", "orange", "orange", "orange", "orange",
                               "orange", "orange", "orange", "orange", "blue")) +  # Specify fill colors manually
  theme_minimal() +
  geom_ridgeline(aes(x = doy, y = year, height = thresh/10 - 1.2), fill = NA, color = "forestgreen") +
  geom_rect(data = subset(climatology.R, heatwave == TRUE),
            aes(xmin = doy - 0.5, xmax = doy + 0.5, ymin = year - 0.5, ymax = year + 0.5),
            fill = "red", alpha = 0.3) +
  labs(fill = "Event Number")  # Optional: Add fill legend

ggplot(climatology.R, aes(x = doy, y = year, height = temp/20)) +
  geom_ridgeline(aes(fill = ifelse(heatwave, "red", "#4AB5C4"))) +
  theme_minimal() +
  geom_ridgeline(aes(x = doy, y = year, height = thresh/10 - 1.2), fill = NA, color = "forestgreen")


# Plot the ridgeline for each section separately
ggplot(climatology.R, aes(x = doy, y = year, height = temp/20)) +
  geom_ridgeline(data = subset(climatology.R, heatwave == TRUE),
                 fill = "red", color = NA) +
  geom_ridgeline(data = subset(climatology.R, heatwave == FALSE),
                  color = NA, fill = "#4AB5C4") +
  theme_minimal()


ggplot(climatology.R, aes(x = doy, y = year, height = temp/20)) +
  geom_ridgeline(data = subset(climatology.R, !heatwave), aes(fill = "#4AB5C4")) +
  geom_ridgeline(data = subset(climatology.R, heatwave), aes(fill = "red")) +
  theme_minimal() +
  geom_ridgeline(aes(x = doy, y = year, height = thresh/10 - 1.2), fill = NA, color = "forestgreen")


ggplot(climatology.R, aes(x = doy, y = year, height = temp/20)) +
  geom_ridgeline(fill = heatwave) +
  #geom_line(aes(x = doy, y = thresh, group = year), color = "red") +  # Add individual lines
  theme_minimal()+
  geom_ridgeline(aes(x = doy, y = year, height = thresh/10 - 1.2), fill = NA, color = "forestgreen")
#geom_vline(xintercept = 200, aes(group = year))



#### Final plots ####
ggplot(climatology.L, aes(x = doy, y = year, height = temp/10 -1.2)) +
  geom_ridgeline(fill = "#ADDAE3") +
  geom_ridgeline(aes(x = doy, y = year, height = thresh/10 - 1.2), fill = NA, color = "forestgreen")+
  #geom_ridgeline(aes(x = doy, y = year, height = seas/10 - 1), fill = NA, color = "black")+
  theme_minimal()


ggplot(climatology.R, aes(x = doy, y = year, height = temp/10 - 1.2, fill = heatwave)) +
  geom_ridgeline_gradient(
    gradient_lwd = 2,
    alpha = 0.6) +
  scale_fill_gradient(values = c("red", "blue"))+
  theme_minimal()


ggplot(climatology.R, aes(doy, year, height = temp/10-1.1, group = year, fill = factor(event_no))) +
  geom_ridgeline_gradient() +
  scale_fill_viridis_d(direction = -1) +
  theme(legend.position = 'none')

my_colors.R = c("#FFC866", "#FFC866", "#FFC866", "#FFC866", "#FFC866","#FFC866", "#FFC866", "#FFC866", "#FFC866", "#FFC866",
                "#FFC866", "#FFC866", "#FFC866", "#FFC866", "#FFC866", "#4AB5C4")


my_colors.L = c("#FFC866", "#FFC866", "#FFC866","#FFC866", "#FFC866", "#FFC866", "#FFC866", "#FFC866",
                "#FFC866", "#FFC866", "#FFC866", "#FFC866", "#FFC866", "#ADDAE3")


my_colors.T = c( "#FFC866", "#FFC866",
                "#FFC866", "#FFC866", "#FFC866", "#FFC866", "#BAAD8D")

climatology.L = climatology.L %>% mutate(event_no = replace(event_no, is.na(event_no), 100))
climatology.T = climatology.T %>% mutate(event_no = replace(event_no, is.na(event_no), 100))


#### FINALL
ggplot(climatology.R, aes(doy, year, height = temp/10-1.1, group = year, fill = factor(event_no))) +
  geom_ridgeline_gradient(size =0.7) +
  theme_minimal()+
  labs(x = "day of year", y = "")+
  scale_fill_manual(values = my_colors.R) +  # Use custom color scale
  theme(legend.position = 'none')+
  theme(panel.grid = element_blank())+
  geom_ridgeline(aes(x = doy, y = year, height = thresh/10 - 1.1), fill = NA, color = "black", linetype = "dashed", size =0.7)
  
  
ggplot(climatology.L, aes(doy, year, height = temp/10-1.1, group = year, fill = factor(event_no))) +
  geom_ridgeline_gradient(size =0.7) +
  theme_minimal()+
  theme(panel.grid = element_blank())+
  labs(x = "day of year", y = "")+
  scale_fill_manual(values = my_colors.L) +  # Use custom color scale
  theme(legend.position = 'none')+
  geom_ridgeline(aes(x = doy, y = year, height = thresh/10 - 1.1), fill = NA, color = "black", linetype = "dashed", size =0.7)


ggplot(climatology.T, aes(doy, year, height = temp/10-1.1, group = year, fill = factor(event_no))) +
  geom_ridgeline_gradient(size =0.7) +
  theme_minimal()+
  theme(panel.grid = element_blank())+
  labs(x = "day of year", y = "")+
  scale_fill_manual(values = my_colors.T) +  # Use custom color scale
  theme(legend.position = 'none')+
  geom_ridgeline(aes(x = doy, y = year, height = thresh/10 - 1.1), fill = NA, color = "black", linetype = "dashed", size =0.7)




ggplot(climatology.R, aes(x = doy, y = year, height = temp/10 -1.2)) +
  geom_ridgeline(fill = "#4AB5C4") +
  geom_ridgeline(aes(x = doy, y = year, height = thresh/10 - 1.2), fill = NA, color = "forestgreen")+
  #geom_ridgeline(aes(x = doy, y = year, height = seas/10 - 1), fill = NA, color = "black")+
  theme_minimal()

ggplot(climatology.T, aes(x = doy, y = year, height = temp/10 -1.2)) +
  geom_ridgeline(fill = "#4AB5C4") +
  geom_ridgeline(aes(x = doy, y = year, height = thresh/10 - 1.2), fill = NA, color = "forestgreen")+
  #geom_ridgeline(aes(x = doy, y = year, height = seas/10 - 1), fill = NA, color = "black")+
  theme_minimal()



ggplot(climatology.L, aes(x = doy, y = year)) +
  geom_ridgeline(aes(height = temp/10 - 1.2, 
                     fill = ifelse(temp > thresh, pmin(temp, thresh), NA))) +
  geom_ridgeline(aes(height = thresh/10 - 1.2), fill = NA, color = "forestgreen") +
  scale_fill_manual(values = c("red", "#ADDAE3"), guide = FALSE) + # Set fill colors
  theme_minimal()


# Fill the areas of temp values higher than thresh with red up to the threshold value
ggplot(climatology.L, aes(x = doy, y = year)) +
  geom_ridgeline(aes(height = temp/10 - 1.2, 
                     fill = factor(ifelse(temp > thresh, pmin(temp, thresh), NA)))) +
  geom_ridgeline(aes(height = thresh/10 - 1.2), fill = NA, color = "forestgreen") +
  scale_fill_manual(values = c("red", "#ADDAE3"), guide = FALSE) + # Set fill colors
  theme_minimal()

#climatology.T$year = factor(year(climatology.T))


# create dummy data so that T plots the same years as L and R
dummy_data <- data.frame(
  doy = 150:220,  # Range from 150 to 220
  t = as.Date(paste0(2008:2011, "-01-01")) + (150:220 - 1),
  temp = NA,
  seas = NA,
  thresh = NA,
  threshCriterion = FALSE,
  durationCriterion = FALSE,
  event = FALSE,
  event_no = NA,
  lake = "",
  year = factor(rep(2008:2011, each = 71))  # Adjust the number of days per year accordingly
)


dummy_data18_19 <- data.frame(
  doy = 150:220,  # Range from 150 to 220
  t = as.Date(paste0(2018:2019, "-01-01")) + (150:220 - 1),
  temp = NA,
  seas = NA,
  thresh = NA,
  threshCriterion = FALSE,
  durationCriterion = FALSE,
  event = FALSE,
  event_no = NA,
  lake = "",
  year = factor(rep(2018:2019, each = 71))  # Adjust the number of days per year accordingly
)

# Combine dummy_data with climatology.T
climatology.T <- rbind(dummy_data, climatology.T)
climatology.T <- rbind(climatology.T, dummy_data18_19)

ggplot(climatology.T, aes(x = doy, y = year, height = temp/20)) +
  geom_ridgeline(fill = "#BAAD8D") +
  geom_ridgeline(aes(x = doy, y = year, height = thresh/20), fill = NA)+
#  geom_line(aes(x = doy, y = thresh, color = year)) +  # Add individual lines
  theme_classic()

# R = #4AB5C4


ggplot(climatology.T, aes(x = doy, y = year, height = temp/10 -1.2)) +
  geom_ridgeline(fill = "#BAAD8D") +
  geom_ridgeline(aes(x = doy, y = year, height = thresh/10 - 1.2), fill = NA, color = "forestgreen")+
  #geom_ridgeline(aes(x = doy, y = year, height = seas/10 - 1), fill = NA, color = "black")+
  theme_minimal()





# Function to find consecutive days where temp > thresh
find_consecutive_days <- function(temp, thresh, min_consecutive = 5) {
  exceed_thresh <- temp > thresh
  consecutive_exceed <- rle(exceed_thresh)
  start_indices <- which(consecutive_exceed$values & consecutive_exceed$lengths >= min_consecutive)
  if (length(start_indices) == 0) {
    return(NULL)
  }
  end_indices <- start_indices + consecutive_exceed$lengths[start_indices] - 1
  return(data.frame(start = start_indices, end = end_indices))
}

# Find consecutive days where temp > thresh
consecutive_days <- find_consecutive_days(climatology.T$temp, climatology.T$thresh, min_consecutive = 5)


# Convert start and end columns to numeric
consecutive_days$start <- as.numeric(consecutive_days$start)
consecutive_days$end <- as.numeric(consecutive_days$end)

# Plot
ggplot(climatology.T, aes(x = doy, y = year, height = temp/20)) +
  geom_ridgeline(fill = "#BAAD8D") +
  geom_ridgeline(aes(x = doy, y = year, height = thresh/20), fill = NA) +
  theme_classic() +
  geom_ribbon(data = consecutive_days, aes(x = start, xmax = end, ymin = year - 0.5, ymax = year + 0.5), fill = "red", alpha = 0.5)



# Plot
ggplot(climatology.T, aes(x = doy, y = year, height = temp/20)) +
  geom_ridgeline(fill = "#BAAD8D") +
  geom_ridgeline(aes(x = doy, y = year, height = thresh/20), fill = NA) +
  theme_classic() +
  geom_ribbon(data = consecutive_days, aes(x = start, xmax = end, ymin = year - 0.5, ymax = year + 0.5), fill = "red", alpha = 0.5)

climatology.T$year <- as.numeric(as.character(climatology.T$year))



ggplot(climatology.T, aes(x = doy, y = year, height = temp/20)) +
  geom_ridgeline(fill = "#BAAD8D") +
  geom_ridgeline(aes(x = doy, y = year, height = thresh/20), fill = NA) +
  theme_classic() +
  geom_ribbon(data = consecutive_days, aes(x = start, xmax = end, ymin = year - 0.5, ymax = year + 0.5), fill = "red", alpha = 0.5)










library(ggplot2)

# Determine the maximum number of observations per year
max_obs <- max(table(climatology.R$year))

# Calculate the height ratio for each year's facet panel
facet_heights <- 1 + (max_obs - table(climatology.R$year)) / max_obs

ggplot(climatology.R, aes(x = doy, y = temp, group = year)) +
  geom_line(aes(color = as.factor(year)), alpha = 0.5) +
  facet_wrap(~year, ncol = 1, strip.position = "bottom", scales = "free_y", as.table = facet_heights) +
  theme_minimal() +
  theme(strip.placement = "outside") +
  guides(color = FALSE) +
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  theme(panel.spacing = unit(0, "lines"),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  coord_cartesian(clip = "off")













theme_henrik <- function(grid=TRUE, legend.position=NA, base_family='Lato Light', highlight_family='Lato') {
  #th <- ggplot2::theme_minimal(base_family = 'LM Roman Dunhill 10', base_size = 13)
  #th <- ggplot2::theme_minimal(base_family = 'Playfair Display', base_size = 13)
  #th <- ggplot2::theme_minimal(base_family = 'Lato Light', base_size = 13)
  th <- ggplot2::theme_minimal(base_family = base_family, base_size = 12)
  
  th <- th + theme(text = element_text(color='#333333'))
  
  th <- th + theme(legend.background = element_blank())
  th <- th + theme(legend.key = element_blank())
  
  # Straight out of hrbrthemes
  if (inherits(grid, "character") | grid == TRUE) {
    th <- th + theme(panel.grid=element_line(color="#cccccc", size=0.3))
    th <- th + theme(panel.grid.major=element_line(color="#cccccc", size=0.3))
    th <- th + theme(panel.grid.minor=element_line(color="#cccccc", size=0.15))
    
    if (inherits(grid, "character")) {
      if (regexpr("X", grid)[1] < 0) th <- th + theme(panel.grid.major.x=element_blank())
      if (regexpr("Y", grid)[1] < 0) th <- th + theme(panel.grid.major.y=element_blank())
      if (regexpr("x", grid)[1] < 0) th <- th + theme(panel.grid.minor.x=element_blank())
      if (regexpr("y", grid)[1] < 0) th <- th + theme(panel.grid.minor.y=element_blank())
    }
    
  } else {
    th <- th + theme(panel.grid=element_blank())
  }
  
  th <- th + theme(axis.text = element_text(family=highlight_family))
  th <- th + theme(axis.ticks = element_blank())
  
  th <- th + theme(axis.text.x=element_text(margin=margin(t=0.5)))
  th <- th + theme(axis.text.y=element_text(margin=margin(r=0.5)))
  
  th <- th + theme(plot.title = element_text(family="Playfair Display"),
                   plot.subtitle = element_text(margin=margin(b=15), family="Playfair Display"),
                   plot.caption = element_text(face='italic', size=10))
  
  if (!is.na(legend.position)) th <- th + theme(legend.position = legend.position)
  
  return (th)
}

climatology.R$year = as.factor(climatology.R$year)
years = levels(climatology.R)

ggplot(climatology.R, aes(doy, y = temp, group= year, fill=year))+
  geom_ribbon(aes(ymin = temp, ymax = temp), color='black', size=0.4) +
  #scale_x_continuous(breaks=seq(from = 140, to = 200, by = 3)) +
  scale_y_continuous(breaks = 1:9, labels = function(y) {year[y]})+ 
  theme_henrik(grid='', legend.position='none')+
  
  # # Zebra color for readability; will change colors of labels in Inkscape later
  scale_fill_manual(values = c('2008' = '#2A7FFF', '2009' = '#5599FF')) 
  # labs(x="", y="", caption='@hnrklndbrg | Source: American Time Use Survey') +
 
  # theme(axis.ticks.x = element_line(size=0.3))











climatology.R = climatology.R %>% mutate(year = as.factor(year))


  
  # Run it it's own block as we're doing some trickery with the activity levels
  {
    activities <- levels(.$year)
    
    # Plot each activity on base Y as the integer of each factor, up to 2 "levels" high.
    # The two liner tricks mentioned a few lines above make this work; without the ribbons
    # renders on top of each others in a way that makes it look like nothing
    ggplot(., aes(doy, group=year.f, fill=year)) +
      geom_ribbon(aes(ymin = as.integer(year), ymax = as.integer(year)), color='white', size=0.4) +
      scale_x_continuous(breaks=seq(from = 140, to = 250, by = 50)) +
      # "Re-add" activities by names as labels in the Y scale
      scale_y_continuous(breaks = 1:length(activities), labels = function(y) {activities[y]}) +
      
      # Zebra color for readability; will change colors of labels in Inkscape later
      scale_fill_manual(values = c('0' = '#2A7FFF', '1' = '#5599FF')) +
     # labs(x="", y="", caption='@hnrklndbrg | Source: American Time Use Survey') +
      #theme_henrik(grid='', legend.position='none') +
      theme(axis.ticks.x = element_line(size=0.3))
  }







activities <- levels(climatology.R$year)

arrange(climatology.R) %>%
  mutate(year.f = reorder(as.character(year), desc(year))) %>%
ggplot(aes(doy, group=year.f, fill=year)) +
  geom_ribbon(aes(ymin = as.integer(year), ymax = as.integer(year)+2 *temp/30, group = year.f), color='white', size=0.7, fill = "#4AB5C4", method = "line") +
  scale_x_continuous(breaks=seq(from = 140, to = 250, by = 50)) +
  # "Re-add" activities by names as labels in the Y scale
  scale_y_continuous(breaks = 1:length(activities), labels = function(y) {activities[y]}) +
  
  # Zebra color for readability; will change colors of labels in Inkscape later
  scale_fill_manual(values = c('0' = '#2A7FFF', '1' = '#5599FF')) +
  # labs(x="", y="", caption='@hnrklndbrg | Source: American Time Use Survey') +
  theme(axis.ticks.x = element_line(size=0.3))+
  theme_classic()+
  geom_line(aes(x = doy, y = thresh/30 +as.integer(year)+0.5, group = year.f), fill = NA, color = "forestgreen")
  
