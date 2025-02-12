# explaining the variation in heatwaves
# compare to color and TP

library(readxl)
library(lme4)
library(ggpubr)
library(lme4)
library(lmerTest)
library(MuMIn)
library(tidyverse)
library(gridExtra)
library(grid)




##### Make a new version of the dataset for the modeled heatwaves #####

peterHW = readRDS(file = "./results/heatwave modeled outputs/peter heatwave outputs modeled.rds")
paulHW = readRDS(file = "./results/heatwave modeled outputs/paul heatwave outputs modeled.rds")
tuesdayHW = readRDS(file = "./results/heatwave modeled outputs/tuesday heatwave outputs modeled.rds")

paulHW = readRDS(file = "./results/heatwave modeled outputs/paul heatwave outputs modeled categories.rds")
peterHW = readRDS(file = "./results/heatwave modeled outputs/peter heatwave outputs modeled categories.rds")
tuesdayHW = readRDS(file = "./results/heatwave modeled outputs/tuesday heatwave outputs modeled categories.rds")

peterHW = peterHW %>% mutate(lake = "R")
paulHW = paulHW %>% mutate(lake = "L")
tuesdayHW = tuesdayHW %>% mutate(lake = "T")

hw.all = rbind(peterHW, paulHW, tuesdayHW) %>% select(-season)

hw.all = hw.all %>% mutate(year = year(date_start), lake_year = paste(lake, year, sep = "_"))

#write.csv(hw.all, "./results/heatwave modeled outputs/heatwave events LRT.csv", row.names = FALSE)







# check if heatwave characteristics are at all related to heatwave response
# heatwave.char = read.csv("./formatted data/explanatory variables heatwaves/heatwaves with percent.csv")
# 
# 
# global.model = lm(data = heatwave.char, percentChange ~ intensity_max+     +   intensity_mean    +                        +   intensity_max    +                         +   intensity_var    +                    
#       +   intensity_cumulative    +                  +   intensity_mean_relThresh    +              +   intensity_max_relThresh    +          
#        +   intensity_var_relThresh    +               +   intensity_cumulative_relThresh    +        +   intensity_mean_abs    +               
#         +   intensity_max_abs    +                     +   intensity_var_abs    +                     +   intensity_cumulative_abs    +         
#        +   rate_onset    +                            +   rate_decline,    na.action = na.pass)
# 
# summary(lm(data = heatwave.char, ))
# 
# 
# summary(global.model)
# dredge(global.model)






##### Explanatory variables analysis updated 2024_03_17

# heatwaves.exp = read.csv("./formatted data/explanatory variables heatwaves/heatwaves with percent zoop color nutrients.csv")
heatwaves.exp = read.csv("./formatted data/master explanatory dataset/heatwaves explained var6.csv")

a = ggplot(heatwaves.exp, aes(x = log10(biomass), y = percentChange, fill = lake))+
  geom_point(size = 5, color = "black", shape = 21, stroke = 1, alpha = 0.95)+
  labs(x = expression("log10(Daphnia dry biomass g/m"^2*")"), y = "")+
  theme_classic()+
  scale_fill_manual(values = c("R" = "#60BFCC", "L" = "#D9EEF3", "T" = "#544C34"),
                    labels = c("R" = "Peter", "L" = "Paul", "T" = "Tuesday"))+
  theme(legend.text = element_text(size = 16),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) + 
  guides(fill = guide_legend(title = NULL), fill = guide_legend(title = NULL))+
  guides(fill = guide_legend(override.aes = list(shape = 22), title = NULL)) 


heatwaves.exp %>% group_by(lake) %>% summarize(mean(percentChange, na.rm = TRUE)) %>% ungroup()

b = ggplot(heatwaves.exp, aes(x = (PML.g440), y = (percentChange), fill = lake))+
  geom_point(size = 5, color = "black", shape = 21, stroke = 1, alpha = 0.95)+
  labs(x = expression("surface water color - g440 (m"^-1*")"), y = "% change in chlorophyll-a")+
  theme_classic()+
  scale_fill_manual(values = c("R" = "#60BFCC", "L" = "#D9EEF3", "T" = "#544C34"),
                    labels = c("R" = "Peter", "L" = "Paul", "T" = "Tuesday"))+
  theme(legend.text = element_text(size = 16),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) + 
  guides(fill = guide_legend(title = NULL), fill = guide_legend(title = NULL))+
  guides(fill = guide_legend(override.aes = list(shape = 22), title = NULL)) 

c = ggplot(heatwaves.exp, aes(x = (cumulative.load), y = (percentChange), fill = lake))+
  geom_point(size = 5, color = "black", shape = 21, stroke = 1, alpha = 0.95)+
  labs(x = expression("cumulative P added (mg/m"^2*")"), y = "% change in chlorophyll-a")+
  theme_classic()+
  scale_fill_manual(values = c("R" = "#60BFCC", "L" = "#D9EEF3", "T" = "#544C34"),
                    labels = c("R" = "Peter", "L" = "Paul", "T" = "Tuesday"))+
  theme(legend.text = element_text(size = 16),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) + 
  guides(fill = guide_legend(title = NULL), fill = guide_legend(title = NULL))+
  guides(fill = guide_legend(override.aes = list(shape = 22), title = NULL)) 


d = ggplot(heatwaves.exp, aes(x = (daily.load), y = (percentChange), fill = lake))+
  geom_point(size = 5, color = "black", shape = 21, stroke = 1, alpha = 0.95)+
  labs(x = expression("daily P load (mg/m"^2*")"), y = "")+
  theme_classic()+
  scale_fill_manual(values = c("R" = "#60BFCC", "L" = "#D9EEF3", "T" = "#544C34"),
                    labels = c("R" = "Peter", "L" = "Paul", "T" = "Tuesday"))+
  theme(legend.text = element_text(size = 16),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) + 
  guides(fill = guide_legend(title = NULL), fill = guide_legend(title = NULL))+
  guides(fill = guide_legend(override.aes = list(shape = 22), title = NULL)) 

#png("./figures/manuscript draft 2024-11-11/explanatory plots.png", height = 6, width = 8, units = "in", res = 300)
ggarrange(b, a, c, d, common.legend = TRUE)
#dev.off()




###### MORE MECHANISTIC response to reviewers figure ######

# heatwaves.exp = read.csv("./formatted data/explanatory variables heatwaves/heatwaves with percent zoop color nutrients.csv")
heatwaves.exp = read.csv("./formatted data/master explanatory dataset/heatwaves explained var6.csv")

daph = ggplot(heatwaves.exp, aes(x = log10(biomass), y = percentChange, fill = lake))+
  geom_point(size = 5, color = "black", shape = 21, stroke = 1, alpha = 0.95)+
  labs(x = expression("log10(Daphnia dry biomass g/m"^2*")"), y = "% change in chlorophyll-a")+
  theme_classic()+
  scale_fill_manual(values = c("R" = "#60BFCC", "L" = "#D9EEF3", "T" = "#544C34"),
                    labels = c("R" = "Peter", "L" = "Paul", "T" = "Tuesday"))+
  theme(legend.text = element_text(size = 16),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) + 
  guides(fill = guide_legend(title = NULL), fill = guide_legend(title = NULL))+
  guides(fill = guide_legend(override.aes = list(shape = 22), title = NULL)) 


temp = ggplot(heatwaves.exp, aes(x = intensity_mean, y = percentChange, fill = lake))+
  geom_point(size = 5, color = "black", shape = 21, stroke = 1, alpha = 0.95)+
  labs(x =("mean heatwave intensity (°C)"), y = "% change in chlorophyll-a")+
  theme_classic()+
  scale_fill_manual(values = c("R" = "#60BFCC", "L" = "#D9EEF3", "T" = "#544C34"),
                    labels = c("R" = "Peter", "L" = "Paul", "T" = "Tuesday"))+
  theme(legend.text = element_text(size = 16),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "none") + 
  guides(fill = guide_legend(title = NULL), fill = guide_legend(title = NULL))+
  guides(fill = guide_legend(override.aes = list(shape = 22), title = NULL)) 

colo = ggplot(heatwaves.exp, aes(x = (PML.g440), y = (percentChange), fill = lake))+
  geom_point(size = 5, color = "black", shape = 21, stroke = 1, alpha = 0.95)+
  labs(x = expression("surface water color - g440 (m"^-1*")"), y = "")+
  theme_classic()+
  scale_fill_manual(values = c("R" = "#60BFCC", "L" = "#D9EEF3", "T" = "#544C34"),
                    labels = c("R" = "Peter", "L" = "Paul", "T" = "Tuesday"))+
  theme(legend.text = element_text(size = 16),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) + 
  guides(fill = guide_legend(title = NULL), fill = guide_legend(title = NULL))+
  guides(fill = guide_legend(override.aes = list(shape = 22), title = NULL)) 

light = ggplot(heatwaves.exp, aes(x = (mean.par.during), y = (percentChange), fill = lake))+
  geom_point(size = 5, color = "black", shape = 21, stroke = 1, alpha = 0.95)+
  labs(x = expression("mean PAR (mg/m"^2*")"), y = "")+
  theme_classic()+
  scale_fill_manual(values = c("R" = "#60BFCC", "L" = "#D9EEF3", "T" = "#544C34"),
                    labels = c("R" = "Peter", "L" = "Paul", "T" = "Tuesday"))+
  theme(legend.text = element_text(size = 16),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "none") + 
  guides(fill = guide_legend(title = NULL), fill = guide_legend(title = NULL))+
  guides(fill = guide_legend(override.aes = list(shape = 22), title = NULL)) 


tp = ggplot(heatwaves.exp, aes(x = (tp_ugL.during), y = (percentChange), fill = lake))+
  geom_point(size = 5, color = "black", shape = 21, stroke = 1, alpha = 0.95)+
  labs(x = expression("total P (mg/m"^2*")"), y = "")+
  theme_classic()+
  scale_fill_manual(values = c("R" = "#60BFCC", "L" = "#D9EEF3", "T" = "#544C34"),
                    labels = c("R" = "Peter", "L" = "Paul", "T" = "Tuesday"))+
  theme(legend.text = element_text(size = 16),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "none") + 
  guides(fill = guide_legend(title = NULL), fill = guide_legend(title = NULL))+
  guides(fill = guide_legend(override.aes = list(shape = 22), title = NULL)) 


strat = ggplot(heatwaves.exp, aes(x = (stability.during), y = (percentChange), fill = lake))+
  geom_point(size = 5, color = "black", shape = 21, stroke = 1, alpha = 0.95)+
  labs(x = expression("Schmidt stability (J/m"^2*")"), y = "", title = "sinking")+
  theme_classic()+
  scale_fill_manual(values = c("R" = "#60BFCC", "L" = "#D9EEF3", "T" = "#544C34"),
                    labels = c("R" = "Peter", "L" = "Paul", "T" = "Tuesday"))+
  theme(legend.text = element_text(size = 16),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) + 
  guides(fill = guide_legend(title = NULL), fill = guide_legend(title = NULL))+
  guides(fill = guide_legend(override.aes = list(shape = 22), title = NULL)) 


get_legend <- function(plot) {
  g <- ggplotGrob(plot)
  legend <- g$grobs[[which(sapply(g$grobs, function(x) x$name) == "guide-box")]]
  return(legend)
}

legend = get_legend(strat)

top = ggarrange(temp, light, tp, nrow = 1, legend = NULL, align = "h")
bottom = ggarrange(daph, strat, legend, nrow = 1, ncol = 3, common.legend = TRUE, legend = "none", align = "h")

ggarrange(top, bottom, nrow = 2, common.legend = TRUE)
#png("./figures/manuscript draft 2024-11-11/explanatory plots.png", height = 6, width = 8, units = "in", res = 300)
#dev.off()

### make each of the panels for the figure individually

png("./figures/revisions draft 2025-01-27/figure 4 panels/stratification.png", height = 2.48, width = 2.376, units = "in", res = 300)
ggplot(heatwaves.exp, aes(x = (stability.during), y = (percentChange), fill = lake))+
  geom_point(size = 4, color = "black", shape = 21, stroke = 1, alpha = 0.95)+
  labs(x = expression("Schmidt stability (J/m"^2*")"), y = "")+
  theme_classic()+
  scale_fill_manual(values = c("R" = "#60BFCC", "L" = "#D9EEF3", "T" = "#544C34"),
                    labels = c("R" = "Peter", "L" = "Paul", "T" = "Tuesday"))+
  theme(
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 8)) + 
  guides(fill = guide_legend(title = NULL), fill = guide_legend(title = NULL))+
  guides(fill = guide_legend(override.aes = list(shape = 22), title = NULL)) +
  theme(legend.position = "none")
dev.off()



png("./figures/revisions draft 2025-01-27/figure 4 panels/phosphorus.png", height = 2.48, width = 2.376, units = "in", res = 300)
ggplot(heatwaves.exp, aes(x = (tp_ugL.during), y = (percentChange), fill = lake))+
  geom_point(size = 4, color = "black", shape = 21, stroke = 1, alpha = 0.95)+
  labs(x = expression("total P (μg/L)"), y = "")+
  theme_classic()+
  scale_fill_manual(values = c("R" = "#60BFCC", "L" = "#D9EEF3", "T" = "#544C34"),
                    labels = c("R" = "Peter", "L" = "Paul", "T" = "Tuesday"))+
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 8)) + 
  guides(fill = guide_legend(title = NULL), fill = guide_legend(title = NULL))+
  guides(fill = guide_legend(override.aes = list(shape = 22), title = NULL)) +
  theme(legend.position = "none")
dev.off()

png("./figures/revisions draft 2025-01-27/figure 4 panels/daphnia.png", height = 2.48, width = 2.376, units = "in", res = 300)
ggplot(heatwaves.exp, aes(x = log10(biomass), y = (percentChange), fill = lake))+
  geom_point(size = 4, color = "black", shape = 21, stroke = 1, alpha = 0.95)+
  labs(x = expression("log10(Daphnia biomass g/m"^2*")"), y = "% change in chl-a")+
  theme_classic()+
  scale_fill_manual(values = c("R" = "#60BFCC", "L" = "#D9EEF3", "T" = "#544C34"),
                    labels = c("R" = "Peter", "L" = "Paul", "T" = "Tuesday"))+
  theme(
    axis.title.y = element_text(size = 12),
    axis.title.x = element_text(size = 10),
    axis.text = element_text(size = 8)) + 
  guides(fill = guide_legend(title = NULL), fill = guide_legend(title = NULL))+
  guides(fill = guide_legend(override.aes = list(shape = 22), title = NULL)) +
  theme(legend.position = "none")
dev.off()



png("./figures/revisions draft 2025-01-27/figure 4 panels/water color.png", height = 2.48, width = 2.376, units = "in", res = 300)
ggplot(heatwaves.exp, aes(x = PML.g440, y = (percentChange), fill = lake))+
  geom_point(size = 4, color = "black", shape = 21, stroke = 1, alpha = 0.95)+
  labs(x = expression("water color - g440 (m"^-1*")"), y = "")+
  theme_classic()+
  scale_fill_manual(values = c("R" = "#60BFCC", "L" = "#D9EEF3", "T" = "#544C34"),
                    labels = c("R" = "Peter", "L" = "Paul", "T" = "Tuesday"))+
  theme(
    axis.title.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.text = element_text(size = 8)) + 
  guides(fill = guide_legend(title = NULL), fill = guide_legend(title = NULL))+
  guides(fill = guide_legend(override.aes = list(shape = 22), title = NULL)) +
  theme(legend.position = "none")
dev.off()


png("./figures/revisions draft 2025-01-27/figure 4 panels/heatwave intensity.png", height = 2.48, width = 2.376, units = "in", res = 300)
ggplot(heatwaves.exp, aes(x = intensity_mean_relThresh, y = (percentChange), fill = lake))+
  geom_point(size = 4, color = "black", shape = 21, stroke = 1, alpha = 0.95)+
  labs(x =("heatwave intensity (°C)"), y = "% change in chl-a")+
  theme_classic()+
  scale_fill_manual(values = c("R" = "#60BFCC", "L" = "#D9EEF3", "T" = "#544C34"),
                    labels = c("R" = "Peter", "L" = "Paul", "T" = "Tuesday"))+
  theme(
    axis.title.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.text = element_text(size = 8)) + 
  guides(fill = guide_legend(title = NULL), fill = guide_legend(title = NULL))+
  guides(fill = guide_legend(override.aes = list(shape = 22), title = NULL)) +
  theme(legend.position = "none")
dev.off()




#### top row ####


### make new total P column so that it includes week before heatwave ###

temp = ggplot(heatwaves.exp, aes(x = intensity_mean_relThresh, y = (percentChange), fill = lake))+
  geom_point(size = 4, color = "black", shape = 21, stroke = 1, alpha = 0.95)+
  labs(x =("heatwave intensity (°C)"), y = "% change in chl-a")+
  theme_classic()+
  scale_fill_manual(values = c("R" = "#60BFCC", "L" = "#D9EEF3", "T" = "#544C34"),
                    labels = c("R" = "Peter", "L" = "Paul", "T" = "Tuesday"))+
  theme(
    axis.title.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.text = element_text(size = 8)) + 
  guides(fill = guide_legend(title = NULL), fill = guide_legend(title = NULL))+
  guides(fill = guide_legend(override.aes = list(shape = 22), title = NULL)) +
  theme(legend.position = "none")


heatwaves.exp = heatwaves.exp %>% 
  mutate(total.P.during.or.before = coalesce(tp_ugL.during, tp_ugL.before))

heatwaves.exp = heatwaves.exp %>% 
  mutate(daphnia.during.or.before = coalesce(daphnia.biomass.during, daphnia.biomass.before))

heatwaves.exp = heatwaves.exp %>% 
  mutate(schmidt.during.or.before = coalesce(stability.during, stability.before))


tp = ggplot(heatwaves.exp, aes(x = (total.P.during.or.before), y = (percentChange), fill = lake))+
  geom_point(size = 4, color = "black", shape = 21, stroke = 1, alpha = 0.95)+
  labs(x = expression("total P (μg/L)"), y = "")+
  theme_classic()+
  scale_fill_manual(values = c("R" = "#60BFCC", "L" = "#D9EEF3", "T" = "#544C34"),
                    labels = c("R" = "Peter", "L" = "Paul", "T" = "Tuesday"))+
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 8)) + 
  guides(fill = guide_legend(title = NULL), fill = guide_legend(title = NULL))+
  guides(fill = guide_legend(override.aes = list(shape = 22), title = NULL)) +
  theme(legend.position = "none")

zoop = ggplot(heatwaves.exp, aes(x = log10(daphnia.during.or.before+0.001), y = (percentChange), fill = lake))+
  geom_point(size = 4, color = "black", shape = 21, stroke = 1, alpha = 0.95)+
  labs(x = expression("Daphnia dry biomass (g/m"^2*")"), y = "% change in chl-a")+
  theme_classic()+
  scale_fill_manual(values = c("R" = "#60BFCC", "L" = "#D9EEF3", "T" = "#544C34"),
                    labels = c("R" = "Peter", "L" = "Paul", "T" = "Tuesday"))+
  theme(
    axis.title.y = element_text(size = 12),
    axis.title.x = element_text(size = 10),
    axis.text = element_text(size = 8)) + 
  guides(fill = guide_legend(title = NULL), fill = guide_legend(title = NULL))+
  guides(fill = guide_legend(override.aes = list(shape = 22), title = NULL)) +
  theme(legend.position = "none")+
  scale_x_continuous(
    breaks = c(-3, -2, -1, 0),
    labels = c("0", "-0.01", "0.1", "1")
  )


color = ggplot(heatwaves.exp, aes(x = PML.g440, y = (percentChange), fill = lake))+
  geom_point(size = 4, color = "black", shape = 21, stroke = 1, alpha = 0.95)+
  labs(x = expression("water color - g440 (m"^-1*")"), y = "")+
  theme_classic()+
  scale_fill_manual(values = c("R" = "#60BFCC", "L" = "#D9EEF3", "T" = "#544C34"),
                    labels = c("R" = "Peter", "L" = "Paul", "T" = "Tuesday"))+
  theme(
    axis.title.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.text = element_text(size = 8)) + 
  guides(fill = guide_legend(title = NULL), fill = guide_legend(title = NULL))+
  guides(fill = guide_legend(override.aes = list(shape = 22), title = NULL)) +
  theme(legend.position = "none")


strat = ggplot(heatwaves.exp, aes(x = (stability.during), y = (percentChange), fill = lake))+
  geom_point(size = 4, color = "black", shape = 21, stroke = 1, alpha = 0.95)+
  labs(x = expression("Schmidt stability (J/m"^2*")"), y = "")+
  theme_classic()+
  scale_fill_manual(values = c("R" = "#60BFCC", "L" = "#D9EEF3", "T" = "#544C34"),
                    labels = c("R" = "Peter", "L" = "Paul", "T" = "Tuesday"))+
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 8)) + 
  guides(fill = guide_legend(title = NULL), fill = guide_legend(title = NULL))+
  guides(fill = guide_legend(override.aes = list(shape = 22), title = NULL)) +
  theme(legend.position = "none")

top = ggarrange(temp, color, tp, align = "hv", nrow = 1, ncol = 3)

bottom = ggarrange(zoop, strat, align = "hv", nrow = 1, ncol = 2)


png("./figures/revisions draft 2025-01-27/figure 4 panels/top panels.png", height = 2.48, width = 7.13, units = "in", res = 300)
top
dev.off()


png("./figures/revisions draft 2025-01-27/figure 4 panels/bottom panels.png", height = 2.48, width = 4.78, units = "in", res = 300)
bottom
dev.off()







##### explanatory plots for ASLO ######

a = ggplot(heatwaves.exp, aes(x = log10(biomass), y = percentChange, fill = lake))+
  geom_point(size = 5, color = "black", shape = 21, stroke = 1, alpha = 0.7)+
  labs(x = "log10(Daphnia biomass)", y = "% change in surface chlorophyll a")+
  theme_classic()+
  scale_fill_manual(values = c("L" = "steelblue2", "R" = "black", "T" = "white"))



b = ggplot(heatwaves.exp, aes(x = (PML.g440), y = (percentChange), fill = lake))+
  geom_point(size = 5, color = "black", shape = 21, stroke = 1, alpha = 0.7)+
  labs(x = "surface water color - g440 (m-1)", y = "% change in surface chlorophyll-a")+
  theme_classic()+
  scale_fill_manual(values = c("L" = "steelblue2", "R" = "black", "T" = "white"))



c = ggplot(heatwaves.exp, aes(x = (cumulative.load), y = (percentChange), fill = lake))+
  geom_point(size = 5, color = "black", shape = 21, stroke = 1, alpha = 0.7)+
  labs(x = "cumulative P (mg/m^2)", y = "% change in surface chlorophyll-a")+
  theme_classic()+
  scale_fill_manual(values = c("L" = "steelblue2", "R" = "black", "T" = "white"))



d = ggplot(heatwaves.exp, aes(x = (daily.load), y = (percentChange), fill = lake))+
  geom_point(size = 5, color = "black", shape = 21, stroke = 1, alpha = 0.7)+
  labs(x = "daily P load (mg/m^2)", y = "% change in surface chlorophyll-a")+
  theme_classic()+
  scale_fill_manual(values = c("L" = "steelblue2", "R" = "black", "T" = "white"))

png("./figures/manuscript 03_18_2024/explanatory plots.png", height = 8, width = 8, units = "in", res = 300)
ggarrange(b, a, c, d, common.legend = TRUE)
dev.off()

#============================================================================================#

ggplot(heatwaves.exp, aes(x = PML.g440, y = percentChange, color = lake))+
  geom_point()+
  theme_classic()


ggplot(heatwaves.exp  %>% filter(lake == "L"), aes(x = PML.g440, y = percentChange, color = lake))+
  geom_point()+
  theme_classic()



ggplot(heatwaves.exp  %>% filter(lake == "L"), aes(x = PML.g440, y = biomass, color = lake))+
  geom_point()+
  theme_classic()

ggplot(heatwaves.exp, aes(x = cumulative.load, y = percentChange, color = lake))+
  geom_point()+
  theme_classic()


ggplot(heatwaves.exp, aes(x = daily.load, y = percentChange, color = lake))+
  geom_point()+
  theme_classic()


ggplot(heatwaves.exp, aes(x = PML.g440, y = averageSlope, color = lake))+
  geom_point()+
  theme_classic()



test = lmer(percentChange~PML.g440*cumulative.load*rate_onset+doy + (1|lake), data = heatwaves.exp)

summary(test)
r.squaredGLMM(test)



heatwaves.exp.synchronous = heatwaves.exp %>%
  mutate(event = seq(1, nrow(heatwaves.exp))) %>% 
  filter((!event %in% c(5, 6, 29, 33)))


heatwaves.exp.synchronous = heatwaves.exp %>% 
  mutate(event.grouped = NA) %>% 
  mutate(event.grouped = case_when(event == 1 | event == 16 ~ 1
    
  ))


ggplot(heatwaves.exp.synchronous, aes(x = biomass, y = percentChange, color = lake))+
  geom_point()+
  theme_classic()


ggplot(heatwaves.exp.synchronous, aes(x = PML.g440, y = percentChange, color = lake))+
  geom_point()+
  theme_classic()


ggplot(heatwaves.exp.synchronous, aes(x = cumulative.load, y = percentChange, color = lake))+
  geom_point()+
  theme_classic()


ggplot(heatwaves.exp.synchronous, aes(x = daily.load, y = percentChange, color = lake))+
  geom_point()+
  theme_classic()







### Is the difference between L and R explained by color? Grazing? Nutrients?

# bring in the dataframe that has the grouping

grouping = read.csv("./results/heatwaves with grouping.csv")

grouping = grouping %>% select(lake, year, date_start, date_end, event_group, start_date_avg)


heatwaves.exp = heatwaves.exp %>% left_join(grouping, by = c("lake", "year", "date_start", "date_end"))

heatwaves.exp.l.r = heatwaves.exp %>% filter(!is.na(event_group)) %>% 
  filter(lake == "L" | lake == "R")


avg.l.r. = heatwaves.exp.l.r %>% group_by(event_group) %>% dplyr::summarize(mean.percent = mean(percentChange, na.rm = TRUE), mean.color = mean(PML.g440, na.rm = TRUE))








# 
# ### Old code with old values 
# exp = read_xlsx("./formatted data/explanatory_variables_heatwaves.xlsx")
# 
# exp$percent_change = as.numeric(exp$percent_change)
# 
# 
# ggplot(data = exp, aes(x = p_loading_mg_m2, y = abs(percent_change)))+
#   geom_point(size = 3, color = "steelblue2")+
#   theme_classic()
# 
# 
# ggplot(data = exp, aes(x = color_m_1, y = abs(percent_change)))+
#   geom_point(size = 3, color = "steelblue2")+
#   theme_classic()
# 
# 
# # plot of the percent change in chlorophyll seasonally
# 
# exp = exp %>% mutate(doy.start = yday(start_date))
# 
# png("./figures/seasonality/seasonality_of_percent_change.png", width = 4, height = 4, units = "in", res = 600)
# 
# ggplot(data = exp, aes(x = doy.start, y = abs(percent_change), fill = lake))+
#   geom_point(size = 3, color = "black", shape = 21, stroke = 1, alpha = 0.7)+
#   theme_classic()+
#   labs(y = "percent change in surface chlorophyll", x = "day of year of heatwave")+
#   scale_fill_manual(values = c("L" = "steelblue2", "R" = "black", "T" = "white"))
# 
# dev.off()
# 
