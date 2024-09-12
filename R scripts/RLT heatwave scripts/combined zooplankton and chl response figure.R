### combined chl and zooplankton response figures

library(tidyverse)

# read in the zooplankton response data

zoop.resp = read.csv("./results/response over time/grav zooplankton response over time.csv")

chl.resp = read.csv("./results/response over time/chl response over time 2008-2011.csv")

zoop.resp$category = "zooplankton"
chl.resp$category = "chl"

all.resp = rbind(zoop.resp, chl.resp)







all.resp  %>% filter(lake == "L") %>% 
  ggplot(aes( x= daysAfter, y = mean_percent_change, color = category))+
  #scale_color_manual(values = c("during heatwave" = "#ff0000", "after heatwave" = "#ffc100", "all other days" = "#88CCEE"))+
  #scale_fill_manual(values = c("during heatwave" = "#ff0000", "after heatwave" = "#ffc100", "all other days" = "#88CCEE"))+
  annotate("rect", xmin=-5, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  annotate("rect", xmin=-5, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  # annotate("rect", xmin=-5, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  # annotate("rect", xmin=-5, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  # annotate("rect", xmin=-5, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  # annotate("rect", xmin=-5, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  # annotate("rect", xmin=-5, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  # annotate("rect", xmin=-5, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  # annotate("rect", xmin=-5, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  # annotate("rect", xmin=-5, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  # annotate("rect", xmin=-5, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  # annotate("rect", xmin=-6, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  # annotate("rect", xmin=-6, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
# annotate("rect", xmin=-7, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
# annotate("rect", xmin=-7, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
# annotate("rect", xmin=-7, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
annotate("rect", xmin=-7, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  annotate("rect", xmin=-7, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  annotate("rect", xmin=-7, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  annotate("rect", xmin=-7, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  # annotate("rect", xmin=-8, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  # annotate("rect", xmin=-8, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  # annotate("rect", xmin=-8, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  # annotate("rect", xmin=-8, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  # annotate("rect", xmin=-9, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  # annotate("rect", xmin=-9, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  annotate("rect", xmin=-10, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  annotate("rect", xmin=-10, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  annotate("rect", xmin=-10, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  annotate("rect", xmin=-10, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  annotate("rect", xmin=-13, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  annotate("rect", xmin=-13, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  annotate("rect", xmin=-15, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  annotate("rect", xmin=-16, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  geom_line(size = 1.5)+
  geom_point(size = 2)+
  labs(title = "response of zooplankton to HW over time - Paul Lake")+
  ylim(min(mean.all.by.lake$mean_percent_change)-10, max(mean.all.by.lake$mean_percent_change)+20)+
  #geom_ribbon(aes(ymin = mean_percent_change - sd_percent_change, ymax = mean_percent_change + sd_percent_change, fill = period), alpha = 0.1)+
  labs(x = "days after heatwave")+
  theme_classic()+
  scale_color_manual(values = c("zooplankton"=  "black", "chl" = "forestgreen"))+
  geom_hline(yintercept = 0, linetype = "dashed")









all.resp  %>% filter(lake == "R") %>% 
  ggplot(aes( x= daysAfter, y = mean_percent_change, color = category))+
  #scale_color_manual(values = c("during heatwave" = "#ff0000", "after heatwave" = "#ffc100", "all other days" = "#88CCEE"))+
  #scale_fill_manual(values = c("during heatwave" = "#ff0000", "after heatwave" = "#ffc100", "all other days" = "#88CCEE"))+
  annotate("rect", xmin=-5, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  annotate("rect", xmin=-5, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  # annotate("rect", xmin=-5, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  # annotate("rect", xmin=-5, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  # annotate("rect", xmin=-5, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  # annotate("rect", xmin=-5, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  # annotate("rect", xmin=-5, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  # annotate("rect", xmin=-5, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  # annotate("rect", xmin=-5, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  # annotate("rect", xmin=-5, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  # annotate("rect", xmin=-5, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  # annotate("rect", xmin=-6, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  # annotate("rect", xmin=-6, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
# annotate("rect", xmin=-7, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
# annotate("rect", xmin=-7, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
# annotate("rect", xmin=-7, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
annotate("rect", xmin=-7, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  annotate("rect", xmin=-7, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  annotate("rect", xmin=-7, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  annotate("rect", xmin=-7, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  # annotate("rect", xmin=-8, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  # annotate("rect", xmin=-8, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  # annotate("rect", xmin=-8, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  # annotate("rect", xmin=-8, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  # annotate("rect", xmin=-9, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  # annotate("rect", xmin=-9, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  annotate("rect", xmin=-10, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  annotate("rect", xmin=-10, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  annotate("rect", xmin=-10, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  annotate("rect", xmin=-10, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  annotate("rect", xmin=-13, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  annotate("rect", xmin=-13, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  annotate("rect", xmin=-15, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  annotate("rect", xmin=-16, xmax=0, ymin=-Inf, ymax=Inf, alpha = 0.05, fill="red") +
  geom_line(size = 1.5)+
  geom_point(size = 2)+
  labs(title = "response of zooplankton to HW over time - Peter Lake")+
  ylim(min(mean.all.by.lake$mean_percent_change)-10, max(mean.all.by.lake$mean_percent_change)+25)+
  #geom_ribbon(aes(ymin = mean_percent_change - sd_percent_change, ymax = mean_percent_change + sd_percent_change, fill = period), alpha = 0.1)+
  labs(x = "days after heatwave")+
  theme_classic()+
  scale_color_manual(values = c("zooplankton"=  "black", "chl" = "forestgreen"))+
  geom_hline(yintercept = 0, linetype = "dashed")
