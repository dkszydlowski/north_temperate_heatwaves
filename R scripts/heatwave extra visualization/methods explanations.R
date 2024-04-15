##### figures for explaining the methods #######

library(heatwaveR)
library(tidyverse)
library(ggpubr)


# need to run the code to analyze the data first

slopes = allSlopes %>% filter(daysAfter == 0)
slopes = slopes %>% dplyr::rename(doy = doyCat)


# Explain the methods using Paul 2013
# choosing this heatwave because it is leading to a 50% increase in chlorophyll
# in our reference lake


#### heatwave plot #####
hw.l13.plot = event_line_DKS("paul", 2013)+
  labs(title = "Paul 2013")+
  #theme(legend.position="none")+
  xlim(180, 215)+
  ylim(18, 28.5) +
  #geom_point()+
  labs(x = "")+
  theme(legend.position = "none")



l13 = slopes %>%  filter(year == 2013, lake == "L")


  chl_slope = ggplot(data = l13, aes( x = doy, y = chl_slope))+
  geom_line(color = "black", size = 1)+
  theme_classic()+
  ylim(-0.4, 0.3)+
  labs(y = "Chlorophyll slope (μg/L/day)", x = "")+
  xlim(180, 215)+
    # geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
    #                                               ymax= Inf), color="transparent", 
    #           fill="red3", alpha=0.3)+
    theme(legend.position="none")+
    geom_vline(data=hw.l13, inherit.aes=FALSE, aes(xintercept = yday(date_start)), linetype = "dashed")+
    geom_vline(data=hw.l13, inherit.aes=FALSE, aes(xintercept = yday(date_end)), linetype = "dashed")

  
  scaleFUN <- function(x) sprintf("%.2f", x)
  
  
  
chl_plot = ggplot(data = l13, aes(x = doy, y = manual_chl))+
  geom_line(color = "forestgreen", size = 1)+
  theme_classic()+
  geom_point()+
 # geom_line(aes(x = doy, y = chl_slope*2 + 2), size = 2)+
  geom_area(fill = "forestgreen", alpha = 0.5)+
  ylim(0, 5)+
  labs(y = "Chlorophyll (μg/L)", x = "")+
  xlim(180, 215)+
  # geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
  #                                               ymax= Inf), color="transparent", 
  #           fill="red3", alpha=0.4)+
  theme(legend.position="none")+
  geom_vline(data=hw.l13, inherit.aes=FALSE, aes(xintercept = yday(date_start)), linetype = "dashed")+
  geom_vline(data=hw.l13, inherit.aes=FALSE, aes(xintercept = yday(date_end)), linetype = "dashed")+
  # geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=(start_analysis), xmax=(end_analysis), ymin= - Inf,
  #                                               ymax= Inf), color="transparent", 
  #           fill="grey", alpha=0.4)+
  theme(legend.position="none")+
  scale_y_continuous(labels=scaleFUN, limits = c(0, 4.5))


chl_percent_change = ggplot(data = l13, aes( x = doy, y = percent_change))+
  geom_line(size = 1, color = "black")+
  theme_classic()+
  geom_point()+
  xlim(180, 215)+
  labs(y = "% change in chlorophyll", x = "day of year")+
  # geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
  #                                               ymax= Inf), color="transparent", 
  #           fill="red3", alpha=0.3)+
  theme(legend.position="none")+
  geom_vline(data=hw.l13, inherit.aes=FALSE, aes(xintercept = yday(date_start)), linetype = "dashed")+
  geom_vline(data=hw.l13, inherit.aes=FALSE, aes(xintercept = yday(date_end)), linetype = "dashed")+
  # geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=(start_analysis), xmax=(end_analysis), ymin= - Inf,
  #                                               ymax= Inf), color="transparent", 
  #           fill="grey", alpha=0.3)+
  theme(legend.position="none")+
  # geom_vline(data=hw.l13, inherit.aes=FALSE, aes(xintercept = (start_analysis)), linetype = "dashed")+
  # geom_vline(data=hw.l13, inherit.aes=FALSE, aes(xintercept = (end_analysis)), linetype = "dashed")+
  ylim(-100, 200)

ggarrange(hw.l13.plot, chl_plot, chl_slope, chl_percent_change, nrow = 4, ncol = 1)


png("./figures/ASLO figures/slopes plot multi panel.png", height = 5, width = 8, res = 300, units = "in")
ggarrange(hw.l13.plot, chl_plot, chl_slope, chl_percent_change, nrow = 4, ncol = 1)
dev.off()




















#### Multiple slopes overlaid

png("./figures/methods figures/slopes included/Paul 2013 just heatwave.png", height = 3, width = 5, units = "in", res = 300)

ggplot(data = l13, aes( x = doy, y = percent_change))+
  geom_line(size = 1, color = "black")+
  theme_classic()+
  geom_point()+
  xlim(180, 215)+
  labs(y = "% change in chlorophyll", x = "day of year")+
  # geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
  #                                               ymax= Inf), color="transparent", 
  #           fill="orange", alpha=0.3)+
  theme(legend.position="none")+
  geom_vline(data=hw.l13, inherit.aes=FALSE, aes(xintercept = yday(date_start)), linetype = "dashed")+
  geom_vline(data=hw.l13, inherit.aes=FALSE, aes(xintercept = yday(date_end)), linetype = "dashed")+
  # geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=(end_analysis-7), xmax=(end_analysis), ymin= - Inf,
  #                                               ymax= Inf), color="transparent", 
  #           fill="grey", alpha=0.3)+
  # geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=(end_analysis-7-1), xmax=(end_analysis-1), ymin= - Inf,
  #                                               ymax= Inf), color="transparent", 
  #           fill="grey", alpha=0.3)+
  # geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=(end_analysis-7-2), xmax=(end_analysis-2), ymin= - Inf,
  #                                               ymax= Inf), color="transparent", 
  #           fill="grey", alpha=0.3)+
  # geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=(end_analysis-7-3), xmax=(end_analysis-3), ymin= - Inf,
  #                                               ymax= Inf), color="transparent", 
  #           fill="grey", alpha=0.3)+
  # geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=(end_analysis-7-4), xmax=(end_analysis-4), ymin= - Inf,
  #                                               ymax= Inf), color="transparent", 
  #           fill="grey", alpha=0.3)+
  theme(legend.position="none")+
  #geom_vline(data=hw.l13, inherit.aes=FALSE, aes(xintercept = (start_analysis)), linetype = "dashed")+
  #geom_vline(data=hw.l13, inherit.aes=FALSE, aes(xintercept = (end_analysis)), linetype = "dashed")+
  ylim(-100, 200)

dev.off()


png("./figures/methods figures/slopes included/Paul 2013 one slope.png", height = 3, width = 5, units = "in", res = 300)

ggplot(data = l13, aes( x = doy, y = percent_change))+
  geom_line(size = 1, color = "black")+
  theme_classic()+
  geom_point()+
  xlim(180, 215)+
  labs(y = "% change in chlorophyll", x = "day of year")+
  # geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
  #                                               ymax= Inf), color="transparent", 
  #           fill="orange", alpha=0.3)+
  theme(legend.position="none")+
  geom_vline(data=hw.l13, inherit.aes=FALSE, aes(xintercept = yday(date_start)), linetype = "dashed")+
  geom_vline(data=hw.l13, inherit.aes=FALSE, aes(xintercept = yday(date_end)), linetype = "dashed")+
  # geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=(end_analysis-7), xmax=(end_analysis), ymin= - Inf,
  #                                               ymax= Inf), color="transparent", 
  #           fill="grey", alpha=0.3)+
  # geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=(end_analysis-7-1), xmax=(end_analysis-1), ymin= - Inf,
  #                                               ymax= Inf), color="transparent", 
  #           fill="grey", alpha=0.3)+
  # geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=(end_analysis-7-2), xmax=(end_analysis-2), ymin= - Inf,
  #                                               ymax= Inf), color="transparent", 
  #           fill="grey", alpha=0.3)+
  # geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=(end_analysis-7-3), xmax=(end_analysis-3), ymin= - Inf,
  #                                               ymax= Inf), color="transparent", 
#           fill="grey", alpha=0.3)+
geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=(end_analysis-7-4), xmax=(end_analysis-4), ymin= - Inf,
                                              ymax= Inf), color="transparent",
          fill="red4", alpha=0.4)+
theme(legend.position="none")+
  # geom_vline(data=hw.l13, inherit.aes=FALSE, aes(xintercept = (start_analysis)), linetype = "dashed")+
  # geom_vline(data=hw.l13, inherit.aes=FALSE, aes(xintercept = (end_analysis)), linetype = "dashed")+
  ylim(-100, 200)

dev.off()





png("./figures/methods figures/slopes included/Paul 2013 two slopes.png", height = 3, width = 5, units = "in", res = 300)

ggplot(data = l13, aes( x = doy, y = percent_change))+
  geom_line(size = 1, color = "black")+
  theme_classic()+
  geom_point()+
  xlim(180, 215)+
  labs(y = "% change in chlorophyll", x = "day of year")+
  # geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
  #                                               ymax= Inf), color="transparent", 
  #           fill="orange", alpha=0.3)+
  theme(legend.position="none")+
  geom_vline(data=hw.l13, inherit.aes=FALSE, aes(xintercept = yday(date_start)), linetype = "dashed")+
  geom_vline(data=hw.l13, inherit.aes=FALSE, aes(xintercept = yday(date_end)), linetype = "dashed")+
  # geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=(end_analysis-7), xmax=(end_analysis), ymin= - Inf,
  #                                               ymax= Inf), color="transparent", 
  #           fill="grey", alpha=0.3)+
  # geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=(end_analysis-7-1), xmax=(end_analysis-1), ymin= - Inf,
  #                                               ymax= Inf), color="transparent", 
  #           fill="grey", alpha=0.3)+
  # geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=(end_analysis-7-2), xmax=(end_analysis-2), ymin= - Inf,
  #                                               ymax= Inf), color="transparent", 
  #           fill="grey", alpha=0.3)+
geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=(end_analysis-7-3), xmax=(end_analysis-3), ymin= - Inf,
                                              ymax= Inf), color="transparent",
          fill="red4", alpha=0.3)+
geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=(end_analysis-7-4), xmax=(end_analysis-4), ymin= - Inf,
                                              ymax= Inf), color="transparent",
          fill="red4", alpha=0.4)+
  theme(legend.position="none")+
  # geom_vline(data=hw.l13, inherit.aes=FALSE, aes(xintercept = (start_analysis)), linetype = "dashed")+
  # geom_vline(data=hw.l13, inherit.aes=FALSE, aes(xintercept = (end_analysis)), linetype = "dashed")+
  ylim(-100, 200)

dev.off()







png("./figures/methods figures/slopes included/Paul 2013 three slopes.png", height = 3, width = 5, units = "in", res = 300)

ggplot(data = l13, aes( x = doy, y = percent_change))+
  geom_line(size = 1, color = "black")+
  theme_classic()+
  geom_point()+
  xlim(180, 215)+
  labs(y = "% change in chlorophyll", x = "day of year")+
  # geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
  #                                               ymax= Inf), color="transparent", 
  #           fill="orange", alpha=0.3)+
  theme(legend.position="none")+
  geom_vline(data=hw.l13, inherit.aes=FALSE, aes(xintercept = yday(date_start)), linetype = "dashed")+
  geom_vline(data=hw.l13, inherit.aes=FALSE, aes(xintercept = yday(date_end)), linetype = "dashed")+
  # geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=(end_analysis-7), xmax=(end_analysis), ymin= - Inf,
  #                                               ymax= Inf), color="transparent", 
  #           fill="grey", alpha=0.3)+
  # geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=(end_analysis-7-1), xmax=(end_analysis-1), ymin= - Inf,
  #                                               ymax= Inf), color="transparent", 
  #           fill="grey", alpha=0.3)+
  geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=(end_analysis-7-2), xmax=(end_analysis-2), ymin= - Inf,
                                                ymax= Inf), color="transparent",
            fill="red4", alpha=0.3)+
  geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=(end_analysis-7-3), xmax=(end_analysis-3), ymin= - Inf,
                                                ymax= Inf), color="transparent",
            fill="red4", alpha=0.3)+
  geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=(end_analysis-7-4), xmax=(end_analysis-4), ymin= - Inf,
                                                ymax= Inf), color="transparent",
            fill="red4", alpha=0.4)+
  theme(legend.position="none")+
  # geom_vline(data=hw.l13, inherit.aes=FALSE, aes(xintercept = (start_analysis)), linetype = "dashed")+
  # geom_vline(data=hw.l13, inherit.aes=FALSE, aes(xintercept = (end_analysis)), linetype = "dashed")+
  ylim(-100, 200)

dev.off()




png("./figures/methods figures/slopes included/Paul 2013 four slopes.png", height = 3, width = 5, units = "in", res = 300)

ggplot(data = l13, aes( x = doy, y = percent_change))+
  geom_line(size = 1, color = "black")+
  theme_classic()+
  geom_point()+
  xlim(180, 215)+
  labs(y = "% change in chlorophyll", x = "day of year")+
  # geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
  #                                               ymax= Inf), color="transparent", 
  #           fill="orange", alpha=0.3)+
  theme(legend.position="none")+
  geom_vline(data=hw.l13, inherit.aes=FALSE, aes(xintercept = yday(date_start)), linetype = "dashed")+
  geom_vline(data=hw.l13, inherit.aes=FALSE, aes(xintercept = yday(date_end)), linetype = "dashed")+
  # geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=(end_analysis-7), xmax=(end_analysis), ymin= - Inf,
  #                                               ymax= Inf), color="transparent", 
  #           fill="grey", alpha=0.3)+
  geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=(end_analysis-7-1), xmax=(end_analysis-1), ymin= - Inf,
                                                ymax= Inf), color="transparent",
            fill="red4", alpha=0.3)+
  geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=(end_analysis-7-2), xmax=(end_analysis-2), ymin= - Inf,
                                                ymax= Inf), color="transparent",
            fill="red4", alpha=0.3)+
  geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=(end_analysis-7-3), xmax=(end_analysis-3), ymin= - Inf,
                                                ymax= Inf), color="transparent",
            fill="red4", alpha=0.3)+
  geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=(end_analysis-7-4), xmax=(end_analysis-4), ymin= - Inf,
                                                ymax= Inf), color="transparent",
            fill="red4", alpha=0.4)+
  theme(legend.position="none")+
  # geom_vline(data=hw.l13, inherit.aes=FALSE, aes(xintercept = (start_analysis)), linetype = "dashed")+
  # geom_vline(data=hw.l13, inherit.aes=FALSE, aes(xintercept = (end_analysis)), linetype = "dashed")+
  ylim(-100, 200)

dev.off()




png("./figures/methods figures/slopes included/Paul 2013 five slopes.png", height = 3, width = 5, units = "in", res = 300)

ggplot(data = l13, aes( x = doy, y = percent_change))+
  geom_line(size = 1, color = "black")+
  theme_classic()+
  geom_point()+
  xlim(180, 215)+
  labs(y = "% change in chlorophyll", x = "day of year")+
  # geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
  #                                               ymax= Inf), color="transparent", 
  #           fill="orange", alpha=0.3)+
  theme(legend.position="none")+
  geom_vline(data=hw.l13, inherit.aes=FALSE, aes(xintercept = yday(date_start)), linetype = "dashed")+
  geom_vline(data=hw.l13, inherit.aes=FALSE, aes(xintercept = yday(date_end)), linetype = "dashed")+
  geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=(end_analysis-7), xmax=(end_analysis), ymin= - Inf,
                                                ymax= Inf), color="transparent",
            fill="red4", alpha=0.3)+
  geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=(end_analysis-7-1), xmax=(end_analysis-1), ymin= - Inf,
                                                ymax= Inf), color="transparent",
            fill="red4", alpha=0.3)+
  geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=(end_analysis-7-2), xmax=(end_analysis-2), ymin= - Inf,
                                                ymax= Inf), color="transparent",
            fill="red4", alpha=0.3)+
  geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=(end_analysis-7-3), xmax=(end_analysis-3), ymin= - Inf,
                                                ymax= Inf), color="transparent",
            fill="red4", alpha=0.3)+
  geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=(end_analysis-7-4), xmax=(end_analysis-4), ymin= - Inf,
                                                ymax= Inf), color="transparent",
            fill="red4", alpha=0.4)+
  theme(legend.position="none")+
  # geom_vline(data=hw.l13, inherit.aes=FALSE, aes(xintercept = (start_analysis)), linetype = "dashed")+
  # geom_vline(data=hw.l13, inherit.aes=FALSE, aes(xintercept = (end_analysis)), linetype = "dashed")+
  ylim(-100, 200)

dev.off()

