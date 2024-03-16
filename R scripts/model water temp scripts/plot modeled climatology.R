# script for plotting the different modeled climatology for each lake

library(tidyverse)
library(heatwaveR)

# Sparkling Lake modeled climatology
peterHW = readRDS(file = "./results/heatwave modeled outputs/peter heatwave outputs modeled.rds")
paulHW = readRDS(file = "./results/heatwave modeled outputs/paul heatwave outputs modeled.rds")
tuesdayHW = readRDS(file = "./results/heatwave modeled outputs/tuesday heatwave outputs modeled.rds")

climatology.R = peterHW$climatology %>% mutate(lake = "peter")
climatology.L = paulHW$climatology %>% mutate(lake = "paul")
climatology.T = tuesdayHW$climatology %>% mutate(lake = "tuesday")

climatology.all = rbind(climatology.R, climatology.L, climatology.T) %>% mutate(model = "Sparkling Lake temp 1m model")


# climatology from actual temp data
peterHW = readRDS(file = "./results/heatwave modeled outputs/peter heatwave outputs actual data.rds")
paulHW = readRDS(file = "./results/heatwave modeled outputs/paul heatwave outputs actual data.rds")
tuesdayHW = readRDS(file = "./results/heatwave modeled outputs/tuesday heatwave outputs actual data.rds")

climatology.R = peterHW$climatology %>% mutate(lake = "peter")
climatology.L = paulHW$climatology %>% mutate(lake = "paul")
climatology.T = tuesdayHW$climatology %>% mutate(lake = "tuesday")

climatology.all.real = rbind(climatology.R, climatology.L, climatology.T) %>% mutate(model = "Actual temp data")


climatology.all = rbind(climatology.all, climatology.all.real)






# climatology from Crystal Bog data 

# Sparkling Lake modeled climatology
peterHW = readRDS(file = "./results/heatwave modeled outputs/peter heatwave outputs modeled CB 1m.rds")
paulHW = readRDS(file = "./results/heatwave modeled outputs/paul heatwave outputs modeled CB 1m.rds")
tuesdayHW = readRDS(file = "./results/heatwave modeled outputs/tuesday heatwave outputs modeled CB 1m.rds")

climatology.R = peterHW$climatology %>% mutate(lake = "peter")
climatology.L = paulHW$climatology %>% mutate(lake = "paul")
climatology.T = tuesdayHW$climatology %>% mutate(lake = "tuesday")

climatology.all.CB = rbind(climatology.R, climatology.L, climatology.T) %>% mutate(model = "Crystal Bog temp 1m model")

climatology.all = rbind(climatology.all, climatology.all.CB)

png("./figures/modeled temperature/climatologies/all climatologies.png", height = 4, width = 11, units = "in", res = 300)

## Make a plot ##
ggplot(climatology.all, aes(x = doy, y = seas, linetype = model, color = model))+
  geom_line(size = 1, alpha = 0.7)+
  facet_wrap(~lake)+
  labs(y = "Baseline climatology (°C)", x = "day of year")+
  ylim(12, 25)+
  xlim(100, 280)+
  theme_classic()+
  scale_color_manual(values = c("gray5", "coral2", "cornflowerblue"))

dev.off()

