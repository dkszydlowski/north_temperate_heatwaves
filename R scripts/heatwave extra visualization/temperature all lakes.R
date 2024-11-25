# plots of temperature 

library(tidyverse)
library(heatwaveR)
library(ggplot2)
library(ggjoy)
library(ggbraid)
library(ggridges)


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







ggplot(climatology.R, aes(x = doy, y = temp, group = year)) +
  geom_line(aes(color = as.factor(year)), alpha = 0.5) +
  facet_wrap(~year, ncol = 1) +
  theme_minimal()



# Convert year to a factor for discrete y-axis
climatology.R$year <- as.factor(climatology.R$year)

climatology.R = climatology.R %>% mutate(heatwave = temp > thresh)


##### attempt #1 of visualizing ######

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

# old color is #FFC86
# old color is #D86262

my_colors.R = c("#D86262", "#D86262", "#D86262", "#D86262", "#D86262","#D86262", "#D86262", "#D86262", "#D86262", "#D86262",
                "#D86262", "#D86262", "#D86262", "#D86262", "#D86262", "#4AB5C4")


my_colors.L = c("#D86262", "#D86262", "#D86262","#D86262", "#D86262", "#D86262", "#D86262", "#D86262",
                "#D86262", "#D86262", "#D86262", "#D86262", "#D86262", "#ADDAE3")


my_colors.T = c( "#D86262", "#D86262",
                "#D86262", "#D86262", "#D86262", "#D86262", "#BAAD8D")

climatology.L = climatology.L %>% mutate(event_no = replace(event_no, is.na(event_no), 100))
climatology.T = climatology.T %>% mutate(event_no = replace(event_no, is.na(event_no), 100))


#### FINALL
R.ridge = ggplot(climatology.R, aes(doy, year, height = temp/10-1.1, group = year, fill = factor(event_no))) +
  geom_ridgeline_gradient(size =0.7) +
  theme_minimal()+
  labs(x = "day of year", y = "")+
  scale_fill_manual(values = my_colors.R) +  # Use custom color scale
  theme(legend.position = 'none')+
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14))+
  geom_ridgeline(aes(x = doy, y = year, height = thresh/10 - 1.1), fill = NA, color = "black", linetype = "dashed", size =0.7)+
  scale_y_discrete(labels = NULL)  # Remove y-axis labels

  
  
L.ridge = ggplot(climatology.L, aes(doy, year, height = temp/10-1.1, group = year, fill = factor(event_no))) +
  geom_ridgeline_gradient(size =0.7) +
  theme_minimal()+
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14))+
  labs(x = "day of year", y = "")+
  scale_fill_manual(values = my_colors.L) +  # Use custom color scale
  theme(legend.position = 'none')+
  geom_ridgeline(aes(x = doy, y = year, height = thresh/10 - 1.1), fill = NA, color = "black", linetype = "dashed", size =0.7)+
  labs(y = "")


T.ridge = ggplot(climatology.T %>% filter(year == 2013 | year == 2014 | year == 2015 | year == 2008), aes(doy, year, height = temp/10-1.1, group = year, fill = factor(event_no))) +
  geom_ridgeline_gradient(size =0.7) +
  theme_minimal()+
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14))+
  labs(x = "day of year", y = "")+
  scale_fill_manual(values = my_colors.T) +  # Use custom color scale
  theme(legend.position = 'none')+
  geom_ridgeline(aes(x = doy, y = year, height = thresh/10 - 1.1), fill = NA, color = "black", linetype = "dashed", size =0.7)

png("./figures/ASLO figures/LR all temp w hw.png", height = 6, width = 9, res = 300, units = "in")
ggarrange(L.ridge, R.ridge, ncol = 2, nrow = 1)
dev.off()


### version with just the threshold ###
R.ridge.blank = ggplot(climatology.R, aes(doy, year, height = temp/10-1.1, group = year)) +
  geom_ridgeline_gradient(size =0.7, fill = "#4AB5C4") +
  theme_minimal()+
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14))+
  labs(x = "day of year", y = "")+
  #scale_fill_manual(values = "#ADDAE3") +  # Use custom color scale
  theme(legend.position = 'none')+
  geom_ridgeline(aes(x = doy, y = year, height = thresh/10 - 1.1), fill = NA, color = "black", linetype = "dashed", size =0.7)+
  labs(y = "")+
  scale_y_discrete(labels = NULL)  # Remove y-axis labels


L.ridge.blank = ggplot(climatology.L, aes(doy, year, height = temp/10-1.1, group = year)) +
  geom_ridgeline_gradient(size =0.7, fill = "#ADDAE3") +
  theme_minimal()+
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14))+
  labs(x = "day of year", y = "")+
  #scale_fill_manual(values = "#ADDAE3") +  # Use custom color scale
  theme(legend.position = 'none')+
  geom_ridgeline(aes(x = doy, y = year, height = thresh/10 - 1.1), fill = NA, color = "black", linetype = "dashed", size =0.7)+
  labs(y = "")


png("./figures/ASLO figures/LR all temp blank.png", height = 6, width = 9, res = 300, units = "in")
ggarrange(L.ridge.blank, R.ridge.blank, ncol = 2, nrow = 1)
dev.off()



### version without hw or threshold ###
R.ridge.blank = ggplot(climatology.R, aes(doy, year, height = temp/10-1.1, group = year)) +
  geom_ridgeline_gradient(size =0.7, fill = "#4AB5C4") +
  theme_minimal()+
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14))+
  labs(x = "day of year", y = "")+
  #scale_fill_manual(values = "#ADDAE3") +  # Use custom color scale
  theme(legend.position = 'none')+
  # geom_ridgeline(aes(x = doy, y = year, height = thresh/10 - 1.1), fill = NA, color = "black", linetype = "dashed", size =0.7)+
  labs(y = "")+
  scale_y_discrete(labels = NULL)  # Remove y-axis labels


L.ridge.blank = ggplot(climatology.L, aes(doy, year, height = temp/10-1.1, group = year)) +
  geom_ridgeline_gradient(size =0.7, fill = "#ADDAE3") +
  theme_minimal()+
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14))+
  labs(x = "day of year", y = "")+
  #scale_fill_manual(values = "#ADDAE3") +  # Use custom color scale
  theme(legend.position = 'none')+
 # geom_ridgeline(aes(x = doy, y = year, height = thresh/10 - 1.1), fill = NA, color = "black", linetype = "dashed", size =0.7)+
  labs(y = "")


png("./figures/ASLO figures/LR all temp blank.png", height = 6, width = 9, res = 300, units = "in")
ggarrange(L.ridge.blank, R.ridge.blank, ncol = 2, nrow = 1)
dev.off()




# Tuesday plot
png("./figures/ASLO figures/Tuesday all temp hw.png", height = 2.98, width = 4.29, res = 300, units = "in")

ggplot(climatology.T %>% filter(year == 2013 | year == 2014 | year == 2015 | year == 2008), aes(doy, year, height = temp/10-1.1, group = year, fill = factor(event_no))) +
  geom_ridgeline_gradient(size =0.7) +
  theme_minimal()+
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14))+
  labs(x = "day of year", y = "")+
  scale_fill_manual(values = my_colors.T) +  # Use custom color scale
  theme(legend.position = 'none')+
  geom_ridgeline(aes(x = doy, y = year, height = thresh/10 - 1.1), fill = NA, color = "black", linetype = "dashed", size =0.7)+
  scale_y_discrete(labels = NULL)

dev.off()


# Tuesday plot blank
png("./figures/ASLO figures/Tuesday all temp blank.png", height = 2.98, width = 4.29, res = 300, units = "in")

ggplot(climatology.T %>% filter(year == 2013 | year == 2014 | year == 2015 | year == 2008), aes(doy, year, height = temp/10-1.1, group = year)) +
  geom_ridgeline_gradient(size =0.7, fill = "#BAAD8D") +
  theme_minimal()+
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14))+
  labs(x = "day of year", y = "")+
  scale_fill_manual(values = my_colors.T) +  # Use custom color scale
  theme(legend.position = 'none')+
 # geom_ridgeline(aes(x = doy, y = year, height = thresh/10 - 1.1), fill = NA, color = "black", linetype = "dashed", size =0.7)+
  scale_y_discrete(labels = NULL)

dev.off()


# Tuesday plot blank with threshold
png("./figures/ASLO figures/Tuesday all temp thresh.png", height = 2.98, width = 4.29, res = 300, units = "in")

ggplot(climatology.T %>% filter(year == 2013 | year == 2014 | year == 2015 | year == 2008), aes(doy, year, height = temp/10-1.1, group = year)) +
  geom_ridgeline_gradient(size =0.7, fill = "#BAAD8D") +
  theme_minimal()+
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14))+
  labs(x = "day of year", y = "")+
  scale_fill_manual(values = my_colors.T) +  # Use custom color scale
  theme(legend.position = 'none')+
  geom_ridgeline(aes(x = doy, y = year, height = thresh/10 - 1.1), fill = NA, color = "black", linetype = "dashed", size =0.7)+
  scale_y_discrete(labels = NULL)

dev.off()


# create dummy data filled with NA so that T plots the same years as L and R

dummy_data <- data.frame(
  doy = 150:220,  # Range from 150 to 220
  t = as.Date(rep(as.Date(paste0(2008:2011, "-01-01")), each = 71)) + (150:220 - 1),
  temp = NA,
  seas = 0.1,
  thresh = 0.1,
  threshCriterion = FALSE,
  durationCriterion = FALSE,
  event = FALSE,
  event_no = 100,
  lake = "tuesday",
  year = factor(rep(2008:2011, each = 71))  # Adjust the number of days per year accordingly
)

dummy_data18_19 <- data.frame(
  doy = 150:220,  # Range from 150 to 220
  t = as.Date(rep(as.Date(paste0(2018:2019, "-01-01")), each = 71)) + (150:220 - 1),
  temp = NA,
  seas = 0.1,
  thresh = 0.1,
  threshCriterion = FALSE,
  durationCriterion = FALSE,
  event = FALSE,
  event_no = 100,
  lake = "tuesday",
  year = factor(rep(2018:2019, each = 71))  # Adjust the number of days per year accordingly
)

# Combine dummy_data with climatology.T
climatology.T <- rbind(dummy_data, climatology.T)
climatology.T <- rbind(climatology.T, dummy_data18_19)

climatology.T = climatology.T %>% mutate(event_no = replace(event_no, is.na(event_no), 100))

ggplot(climatology.T, aes(x = doy, y = year, group = year, height = temp/20, fill = factor(event_no))) +
  geom_ridgeline_gradient() +
 # geom_ridgeline(aes(x = doy, y = year, height = thresh/20), fill = NA)+
#  geom_line(aes(x = doy, y = thresh, color = year)) +  # Add individual lines
  theme_classic()+
  scale_fill_manual(values = my_colors.T)   # Use custom color scale
  

# R = #4AB5C4





ggplot(climatology.T, aes(doy, year, height = temp/10-1.1, group = year, fill = factor(event_no))) +

#  geom_ridgeline_gradient(size =0.7) +
  theme_minimal()+
  theme(panel.grid = element_blank())+
  labs(x = "day of year", y = "")+
  #scale_fill_manual(values = my_colors.T) +  # Use custom color scale
  theme(legend.position = 'none')+
  # geom_ridgeline(aes(x = doy, y = year, height = thresh/10 - 1.1), fill = NA, color = "black", linetype = "dashed", size =0.7)


climatology.T = climatology.T %>% mutate(temp = replace(temp, is.na(temp), 0))
climatology.T = climatology.T %>% mutate(event_no = replace(event_no, is.na(event_no), 100))
climatology.T = climatology.T %>% mutate(thresh = replace(thresh, is.na(thresh), 0))
climatology.T = climatology.T %>% mutate(seas = replace(seas, is.na(seas), 0))

climatology.T = climatology.T %>% mutate(event_no = as.factor(event_no))

ggplot(climatology.T, aes(doy, year, height = temp/10-1.1, group = year, fill = event_no)) +
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




