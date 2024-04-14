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



# Convert year to a factor for discrete y-axis
climatology.R$year <- as.factor(climatology.R$year)

climatology.R = climatology.R %>% mutate(heatwave = temp > thresh)


##### Plotting final plots ######

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





ggplot(climatology.T, aes(doy, year, height = temp/10-1.1, group = year, fill = factor(event_no))) +
  geom_ridgeline_gradient(size =0.7) +
  theme_minimal()+
  theme(panel.grid = element_blank())+
  labs(x = "day of year", y = "")+
  scale_fill_manual(values = my_colors.T) +  # Use custom color scale
  theme(legend.position = 'none')+
  geom_ridgeline(aes(x = doy, y = year, height = thresh/10 - 1.1), fill = NA, color = "black", linetype = "dashed", size =0.7)












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




