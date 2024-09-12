# make a plot of the slopes over time and the raw chl data
# updated for event_line plots

library(heatwaveR)
library(dplyr)
library(ggpubr)
library(tidyverse)

#slopes = read.csv("./formatted data/slopes.csv")
#slopes = read.csv("./formatted data/slopes_3day.csv")

#slopes = slopes %>% dplyr::rename(doy = doyCat)

# pdf("./figures/Heatwave raw data plots/temp_chl_heatwaves_slopes_7day.pdf", onefile = TRUE)

# set the number of days after heatwave and number of days to include
#daysAfter = 1 # 3 days after heatwave
#numSlopes = 5 # 5-day numSlopes

makePDFrawPlots <- function(allSlopes, daysAfter, numSlopes, metadata_plot, runNumber, slopeLength){
  
  # make a new dataframe of rectangles
 heatwaves = read.csv("./results/heatwave modeled outputs/heatwave events LRT.csv")

 # select just the columns we want
 heatwaves = heatwaves %>% select(lake, year, date_start, date_end, event_no)

 # calculate where the analysis is going to actually happen, including slopes picked and 
 # the number of slopes included
 
 heatwaves = heatwaves %>% mutate(start_analysis = yday(date_end)+ daysAfter - slopeLength)
 heatwaves = heatwaves %>% mutate(end_analysis = yday(date_end) + daysAfter + numSlopes -1)
 
 hw.l08 = heatwaves %>% filter(lake == "L" & year == 2008)
 hw.r08 = heatwaves %>% filter(lake == "R" & year == 2008)
 hw.l09 = heatwaves %>% filter(lake == "L" & year == 2009)
 hw.r09 = heatwaves %>% filter(lake == "R" & year == 2009)
 hw.l10 = heatwaves %>% filter(lake == "L" & year == 2010)
 hw.r10 = heatwaves %>% filter(lake == "R" & year == 2010)
 hw.l11 = heatwaves %>% filter(lake == "L" & year == 2011)
 hw.r11 = heatwaves %>% filter(lake == "R" & year == 2011)
 
 hw.l13 = heatwaves %>% filter(lake == "L" & year == 2013)
 hw.r13 = heatwaves %>% filter(lake == "R" & year == 2013)
 hw.t13 = heatwaves %>% filter(lake == "T" & year == 2013)
 
 hw.l14 = heatwaves %>% filter(lake == "L" & year == 2014)
 hw.r14 = heatwaves %>% filter(lake == "R" & year == 2014)
 hw.t14 = heatwaves %>% filter(lake == "T" & year == 2014)
 
 hw.l15 = heatwaves %>% filter(lake == "L" & year == 2015)
 hw.r15 = heatwaves %>% filter(lake == "R" & year == 2015)
 hw.t15 = heatwaves %>% filter(lake == "T" & year == 2015)
 
 hw.l18 = heatwaves %>% filter(lake == "L" & year == 2018)
 hw.r18 = heatwaves %>% filter(lake == "R" & year == 2018)
 
 hw.l19 = heatwaves %>% filter(lake == "L" & year == 2019)
 hw.r19 = heatwaves %>% filter(lake == "R" & year == 2019)

  curDate = cur_date_time = format(Sys.Date(), "%Y_%m_%d")
  pdfName = paste(runNumber, "_TIME_SERIES_date_", curDate, "_slopeLength_", slopeLength, "_daysAfter_", daysAfter, "_numSlopes_", numSlopes, ".pdf", sep = "")
  
  # make all of the event_line plots so they are not plotted to the pdf
  l08temp = event_line_DKS("paul", 2008)+
    labs(title = "Paul 2008")+
    theme(legend.position="none")+
    xlim(140, 250)
  
  r08temp = event_line_DKS("peter", 2008)+
    labs(title = "Peter 2008")+
    theme(legend.position="none")+
    xlim(140, 250)
  
  l09temp = event_line_DKS("paul", 2009)+
    labs(title = "Paul 2009")+
    theme(legend.position="none")+
    xlim(140, 250)
  
  r09temp = event_line_DKS("peter", 2009)+
    labs(title = "Peter 2009")+
    theme(legend.position="none")+
    xlim(140, 250)
  
  l10temp = event_line_DKS("paul", 2010)+
    labs(title = "Paul 2010")+
    theme(legend.position="none")+
    xlim(140, 250)
  
  r10temp = event_line_DKS("peter", 2010)+
    labs(title = "Peter 2010")+
    theme(legend.position="none")+
    xlim(140, 250)
  
  l11temp = event_line_DKS("paul", 2011)+
    labs(title = "Paul 2011")+
    theme(legend.position="none")+
    xlim(140, 250)
  
  r11temp = event_line_DKS("peter", 2011)+
    labs(title = "Peter 2011")+
    theme(legend.position="none")+
    xlim(140, 250)
  
  l10temp = event_line_DKS("paul", 2010)+
    labs(title = "Paul 2010")+
    theme(legend.position="none")+
    xlim(140, 250)
  
  r10temp = event_line_DKS("peter", 2010)+
    labs(title = "Peter 2010")+
    theme(legend.position="none")+
    xlim(140, 250)
  
  l13temp = event_line_DKS("paul", 2013)+
    labs(title = "Paul 2013")+
    theme(legend.position="none")+
    xlim(140, 250)
  
  r13temp = event_line_DKS("peter", 2013)+
    labs(title = "Peter 2013")+
    theme(legend.position="none")+
    xlim(140, 250)
  
  
  l10temp = event_line_DKS("paul", 2010)+
    labs(title = "Paul 2010")+
    theme(legend.position="none")+
    xlim(140, 250)
  
  r10temp = event_line_DKS("peter", 2010)+
    labs(title = "Peter 2010")+
    theme(legend.position="none")+
    xlim(140, 250)
  
  l14temp = event_line_DKS("paul", 2014)+
    labs(title = "Paul 2014")+
    theme(legend.position="none")+
    xlim(140, 250)
  
  r14temp = event_line_DKS("peter", 2014)+
    labs(title = "Peter 2014")+
    theme(legend.position="none")+
    xlim(140, 250)
  
  l10temp = event_line_DKS("paul", 2010)+
    labs(title = "Paul 2010")+
    theme(legend.position="none")+
    xlim(140, 250)
  
  r10temp = event_line_DKS("peter", 2010)+
    labs(title = "Peter 2010")+
    theme(legend.position="none")+
    xlim(140, 250)
  
  l15temp = event_line_DKS("paul", 2015)+
    labs(title = "Paul 2015")+
    theme(legend.position="none")+
    xlim(140, 250)
  
  r15temp = event_line_DKS("peter", 2015)+
    labs(title = "Peter 2015")+
    theme(legend.position="none")+
    xlim(140, 250)
  
  
  l10temp = event_line_DKS("paul", 2010)+
    labs(title = "Paul 2010")+
    theme(legend.position="none")+
    xlim(140, 250)
  
  r10temp = event_line_DKS("peter", 2010)+
    labs(title = "Peter 2010")+
    theme(legend.position="none")+
    xlim(140, 250)
  
  l18temp = event_line_DKS("paul", 2018)+
    labs(title = "Paul 2018")+
    theme(legend.position="none")+
    xlim(140, 250)
  
  r18temp = event_line_DKS("peter", 2018)+
    labs(title = "Peter 2018")+
    theme(legend.position="none")+
    xlim(140, 250)
  
  l19temp = event_line_DKS("paul", 2019)+
    labs(title = "Paul 2019")+
    theme(legend.position="none")+
    xlim(140, 250)
  
  r19temp = event_line_DKS("peter", 2019)+
    labs(title = "Peter 2019")+
    theme(legend.position="none")+
    xlim(140, 250)
  
  t13temp = event_line_DKS("tuesday", 2013)+
    labs(title = "Tuesday 2013")+
    theme(legend.position="none")+
    xlim(140, 250)
  
  
  t14temp = event_line_DKS("tuesday", 2014)+
    labs(title = "Tuesday 2014")+
    theme(legend.position="none")+
    xlim(140, 250)
  
  t15temp = event_line_DKS("tuesday", 2015)+
    labs(title = "Tuesday 2015")+
    theme(legend.position="none")+
    xlim(140, 250)
  
  
  # png("./figures/final time series plots/time series plots PNG 2024_03_16/time series plots 2024_03_16.png", height = 8, width = 9, res = 300, units = "in")
  pdf(paste("./figures/sensitivity tests/" , pdfName, sep = ""), height = 8, width = 9)
  
  print(metadata_plot)
  
  slopes = allSlopes %>% filter(daysAfter == 0)
  slopes = slopes %>% dplyr::rename(doy = doyCat)
  
  # 
  # slopes.other = slopes %>% filter(period == "all other days")
  # slopes.hw = slopes %>% filter(period == "after heatwave")
  # 
  # t.test(slopes.other$percent_change, slopes.hw$percent_change)
  
  # save to a pdf with a similar name to the data outputs
  
  ##### 2008 ####
  t08 = slopes %>%  filter(year == 2008, lake == "T")
  l08 = slopes %>%  filter(year == 2008, lake == "L")
  r08 = slopes %>%  filter(year == 2008, lake == "R")
  
  # Paul
  l08slope = ggplot(data = l08, aes( x = doy, y = chl_slope))+
    geom_line(color = "black", size = 1)+
    theme_classic()+
    ylim(-4.1, 4.5)+
    labs(y = "Chlorophyll slope (μg/L/day)")+
    xlim(140, 250)
  
  l08percent_change = ggplot(data = l08, aes( x = doy, y = percent_change))+
    geom_line(color = "black", size = 1)+
    theme_classic()+
    xlim(138, 250)+
    labs(y = "% change in chlorophyll", x = "day of year")+
    ylim(-200, 600)
  
  l08percent_change = ggplot(data = l08, aes( x = doy, y = percent_change))+
    geom_line(size = 1, color = "black")+
    theme_classic()+
    xlim(138, 250)+
    labs(y = "% change in chlorophyll")+
    geom_rect(data=hw.l08, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
                                                  ymax= Inf), color="transparent", 
              fill="orange", alpha=0.3)+
    theme(legend.position="none")+
    geom_vline(data=hw.l08, inherit.aes=FALSE, aes(xintercept = yday(date_start)), linetype = "dashed")+
    geom_vline(data=hw.l08, inherit.aes=FALSE, aes(xintercept = yday(date_end)), linetype = "dashed")+
    geom_rect(data=hw.l08, inherit.aes=FALSE, aes(xmin=(start_analysis), xmax=(end_analysis), ymin= - Inf,
                                                  ymax= Inf), color="transparent", 
              fill="magenta", alpha=0.3)+
    theme(legend.position="none")+
    geom_vline(data=hw.l08, inherit.aes=FALSE, aes(xintercept = (start_analysis)), linetype = "dashed")+
    geom_vline(data=hw.l08, inherit.aes=FALSE, aes(xintercept = (end_analysis)), linetype = "dashed")+
    ylim(-200, 600)
  
  l08chl = ggplot(data = l08, aes(x = doy, y = manual_chl))+
    geom_line(color = "forestgreen", size = 1)+
    theme_classic()+
    labs(y = "Chlorophyll (μg/L)")+
    xlim(140, 250)
  
  # l13.after.hw = l13 %>% filter(period == "after heatwave")
  
  # ggplot(l13, aes(x = doy, y = mean_temp)) +
  #   geom_line() +
  #   geom_rect(data = subset(l13, period == "after heatwave"), aes(xmin = doy, xmax = lead(doy), ymin = -Inf, ymax = Inf), fill = "red", alpha = 0.2) +
  #   labs(x = "Day of Year", y = "Percent Change") +
  #   theme_classic()+
  #   xlim(140, 250)

  
  # Peter
  r08slope = ggplot(data = r08, aes( x = doy, y = chl_slope))+
    geom_line(color = "black", size = 1)+
    theme_classic()+
    ylim(-4.1, 4.5)+
    labs(y = "Chlorophyll slope (μg/L/day)")+
    xlim(140, 250)
  
  r08chl = ggplot(data = r08, aes(x = doy, y = manual_chl))+
    geom_line(color = "forestgreen", size = 1)+
    theme_classic()+
    labs(y = "Chlorophyll (μg/L)")+
    xlim(140, 250)
  
  r08percent_change = ggplot(data = r08, aes( x = doy, y = percent_change))+
    geom_line(color = "black", size = 1)+
    theme_classic()+
    xlim(138, 250)+
    labs(y = "% change in chlorophyll", x = "day of year")+
    ylim(-200, 600)
  
  r08percent_change = ggplot(data = r08, aes( x = doy, y = percent_change))+
    geom_line(size = 1, color = "black")+
    theme_classic()+
    xlim(138, 250)+
    labs(y = "% change in chlorophyll")+
    geom_rect(data=hw.r08, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
                                                  ymax= Inf), color="transparent", 
              fill="orange", alpha=0.3)+
    theme(legend.position="none")+
    geom_vline(data=hw.r08, inherit.aes=FALSE, aes(xintercept = yday(date_start)), linetype = "dashed")+
    geom_vline(data=hw.r08, inherit.aes=FALSE, aes(xintercept = yday(date_end)), linetype = "dashed")+
    geom_rect(data=hw.r08, inherit.aes=FALSE, aes(xmin=(start_analysis), xmax=(end_analysis), ymin= - Inf,
                                                  ymax= Inf), color="transparent", 
              fill="magenta", alpha=0.3)+
    theme(legend.position="none")+
    geom_vline(data=hw.r08, inherit.aes=FALSE, aes(xintercept = (start_analysis)), linetype = "dashed")+
    geom_vline(data=hw.r08, inherit.aes=FALSE, aes(xintercept = (end_analysis)), linetype = "dashed")+
    ylim(-200, 600)
  
  print(ggarrange(nrow = 4, ncol = 2, r08temp, l08temp, r08chl, l08chl, r08slope, l08slope, r08percent_change, l08percent_change))
  
  
  
  ##### 2009 ####
  l09 = slopes %>%  filter(year == 2009, lake == "L")
  r09 = slopes %>%  filter(year == 2009, lake == "R")
  
  # Paul
  l09slope = ggplot(data = l09, aes( x = doy, y = chl_slope))+
    geom_line(color = "black", size = 1)+
    theme_classic()+
    ylim(-4.7, 5.5)+
    labs(y = "Chlorophyll slope (μg/L/day)")+
    xlim(140, 250)
  
  l09chl = ggplot(data = l09, aes(x = doy, y = manual_chl))+
    geom_line(color = "forestgreen", size = 1)+
    theme_classic()+
    labs(y = "Chlorophyll (μg/L)")+
    xlim(140, 250)
  
 l09percent_change = ggplot(data = l09, aes( x = doy, y = percent_change))+
   geom_line(size = 1, color = "black")+
    theme_classic()+
    xlim(138, 250)+
    labs(y = "% change in chlorophyll")+
   geom_rect(data=hw.l09, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
                                                ymax= Inf), color="transparent", 
             fill="orange", alpha=0.3)+
   theme(legend.position="none")+
   geom_vline(data=hw.l09, inherit.aes=FALSE, aes(xintercept = yday(date_start)), linetype = "dashed")+
   geom_vline(data=hw.l09, inherit.aes=FALSE, aes(xintercept = yday(date_end)), linetype = "dashed")+
   geom_rect(data=hw.l09, inherit.aes=FALSE, aes(xmin=(start_analysis), xmax=(end_analysis), ymin= - Inf,
                                                 ymax= Inf), color="transparent", 
             fill="magenta", alpha=0.3)+
   theme(legend.position="none")+
   geom_vline(data=hw.l09, inherit.aes=FALSE, aes(xintercept = (start_analysis)), linetype = "dashed")+
   geom_vline(data=hw.l09, inherit.aes=FALSE, aes(xintercept = (end_analysis)), linetype = "dashed")+
   ylim(-200, 600)

  
  # Peter
  r09slope = ggplot(data = r09, aes( x = doy, y = chl_slope))+
    geom_line(color = "black", size = 1)+
    theme_classic()+
    ylim(-4.7, 5.5)+
    labs(y = "Chlorophyll slope (μg/L/day)")+
    xlim(140, 250)
  
  
  r09slope = ggplot(data = r09, aes( x = doy, y = chl_slope))+
    geom_line(color = "black", size = 1)+
    theme_classic()+
    ylim(-4.7, 5.5)+
    labs(y = "Chlorophyll slope (μg/L/day)")+
    xlim(140, 250)
  
  r09chl = ggplot(data = r09, aes(x = doy, y = manual_chl))+
    geom_line(color = "forestgreen", size = 1)+
    theme_classic()+
    labs(y = "Chlorophyll (μg/L)")+
    xlim(140, 250)
  
  r09percent_change = ggplot(data = r09, aes( x = doy, y = percent_change, color = period))+
    geom_line(color = "black", size = 1)+
    theme_classic()+
    xlim(138, 250)+
    labs(y = "% change in chlorophyll", x = "day of year")+
    theme(legend.position="none")+
    ylim(-200, 600)
  
  r09percent_change = ggplot(data = r09, aes( x = doy, y = percent_change))+
    geom_line(size = 1, color = "black")+
    theme_classic()+
    xlim(138, 250)+
    labs(y = "% change in chlorophyll")+
    geom_rect(data=hw.r09, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
                                                  ymax= Inf), color="transparent", 
              fill="orange", alpha=0.3)+
    theme(legend.position="none")+
    geom_vline(data=hw.r09, inherit.aes=FALSE, aes(xintercept = yday(date_start)), linetype = "dashed")+
    geom_vline(data=hw.r09, inherit.aes=FALSE, aes(xintercept = yday(date_end)), linetype = "dashed")+
    geom_rect(data=hw.r09, inherit.aes=FALSE, aes(xmin=(start_analysis), xmax=(end_analysis), ymin= - Inf,
                                                  ymax= Inf), color="transparent", 
              fill="magenta", alpha=0.3)+
    theme(legend.position="none")+
    geom_vline(data=hw.r09, inherit.aes=FALSE, aes(xintercept = (start_analysis)), linetype = "dashed")+
    geom_vline(data=hw.r09, inherit.aes=FALSE, aes(xintercept = (end_analysis)), linetype = "dashed")+
    ylim(-200, 600)
  
  print(ggarrange(nrow = 4, ncol = 2, r09temp, l09temp, r09chl, l09chl, r09slope, l09slope, r09percent_change, l09percent_change))
  
  
  ##### 2010 ####
  t10 = slopes %>%  filter(year == 2010, lake == "T")
  l10 = slopes %>%  filter(year == 2010, lake == "L")
  r10 = slopes %>%  filter(year == 2010, lake == "R")
  
  # Paul
  l10slope = ggplot(data = l10, aes( x = doy, y = chl_slope))+
    geom_line(color = "black", size = 1)+
    theme_classic()+
    ylim(-4.7, 5.5)+
    labs(y = "Chlorophyll slope (μg/L/day)")+
    xlim(140, 250)
  
  l10chl = ggplot(data = l10, aes(x = doy, y = manual_chl))+
    geom_line(color = "forestgreen", size = 1)+
    theme_classic()+
    labs(y = "Chlorophyll (μg/L)")+
    xlim(140, 250)
  
  #png("./figures/methods figures/paul_2010_percent_change.png", height = 5, width = 8, units = "in", res = 300)
  
  #ggsave("filename.eps", device=cairo_ps)
  
  
  l10percent_change = ggplot(data = l10, aes( x = doy, y = percent_change))+
    geom_line(size = 1, color = "black")+
    geom_point()+
    theme_classic()+
    xlim(138, 250)+
    labs(y = "% change in chlorophyll")+
    geom_rect(data=hw.l10, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
                                                  ymax= Inf), color="transparent", 
              fill="orange", alpha=0.3)+
    theme(legend.position="none")+
    geom_vline(data=hw.l10, inherit.aes=FALSE, aes(xintercept = yday(date_start)), linetype = "dashed")+
    geom_vline(data=hw.l10, inherit.aes=FALSE, aes(xintercept = yday(date_end)), linetype = "dashed")+
    geom_rect(data=hw.l10, inherit.aes=FALSE, aes(xmin=(start_analysis), xmax=(end_analysis), ymin= - Inf,
                                                  ymax= Inf), color="transparent", 
              fill="grey", alpha=0.3)+
    theme(legend.position="none")+
    geom_vline(data=hw.l10, inherit.aes=FALSE, aes(xintercept = (start_analysis)), linetype = "dashed")+
    geom_vline(data=hw.l10, inherit.aes=FALSE, aes(xintercept = (end_analysis)), linetype = "dashed")+
    ylim(-200, 600)
  # l10temp = ggplot(data = l10, aes(x = doy, y = mean_temp))+
  #   geom_line(color = "steelblue", size = 1)+
  #   theme_classic()+
  #   labs(title = "Paul 2010")+
  #   annotate("rect", xmin = as.Date(144), xmax = as.Date(154), ymin = -Inf, ymax = Inf,
  #            fill = "red", alpha = 0.5)+
  #   annotate("rect", xmin = as.Date(221), xmax = as.Date(227), ymin = -Inf, ymax = Inf,
  #            fill = "red", alpha = 0.5)
  
  
  # Peter
  r10slope = ggplot(data = r10, aes( x = doy, y = chl_slope))+
    geom_line(color = "black", size = 1)+
    theme_classic()+
    ylim(-4.7, 5.5)+
    labs(y = "Chlorophyll slope (μg/L/day)")+
    xlim(140, 250)
  
  
  r10chl = ggplot(data = r10, aes(x = doy, y = manual_chl))+
    geom_line(color = "forestgreen", size = 1)+
    theme_classic()+
    labs(y = "Chlorophyll (μg/L)")+
    xlim(140, 250)
  
  r10percent_change = ggplot(data = r10, aes( x = doy, y = percent_change))+
    geom_line(size = 1, color = "black")+
    theme_classic()+
    xlim(138, 250)+
    labs(y = "% change in chlorophyll")+
    geom_rect(data=hw.r10, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
                                                  ymax= Inf), color="transparent", 
              fill="orange", alpha=0.3)+
    theme(legend.position="none")+
    geom_vline(data=hw.r10, inherit.aes=FALSE, aes(xintercept = yday(date_start)), linetype = "dashed")+
    geom_vline(data=hw.r10, inherit.aes=FALSE, aes(xintercept = yday(date_end)), linetype = "dashed")+
    geom_rect(data=hw.r10, inherit.aes=FALSE, aes(xmin=(start_analysis), xmax=(end_analysis), ymin= - Inf,
                                                  ymax= Inf), color="transparent", 
              fill="magenta", alpha=0.3)+
    theme(legend.position="none")+
    geom_vline(data=hw.r10, inherit.aes=FALSE, aes(xintercept = (start_analysis)), linetype = "dashed")+
    geom_vline(data=hw.r10, inherit.aes=FALSE, aes(xintercept = (end_analysis)), linetype = "dashed")+
    ylim(-200, 600)
  
  # r10temp = ggplot(data = r10, aes(x = doy, y = mean_temp))+
  #   geom_line(color = "steelblue", size = 1)+
  #   theme_classic()+
  #   labs(title = "Peter 2010")+
  #   annotate("rect", xmin = as.Date(144), xmax = as.Date(154), ymin = -Inf, ymax = Inf,
  #            fill = "red", alpha = 0.5)+
  #   annotate("rect", xmin = as.Date(221), xmax = as.Date(227), ymin = -Inf, ymax = Inf,
  #            fill = "red", alpha = 0.5)
  
  
  print(ggarrange(nrow = 4, ncol = 2,r10temp, l10temp, r10chl, l10chl, r10slope, l10slope, r10percent_change, l10percent_change))
  
  
  
  
  ##### 2011 ####
  t11 = slopes %>%  filter(year == 2011, lake == "T")
  l11 = slopes %>%  filter(year == 2011, lake == "L")
  r11 = slopes %>%  filter(year == 2011, lake == "R")
  
  # Paul
  l11slope = ggplot(data = l11, aes( x = doy, y = chl_slope))+
    geom_line(color = "black", size = 1)+
    theme_classic()+
    ylim(-4.1, 4.5)+
    labs(y = "Chlorophyll slope (μg/L/day)")+
    xlim(140, 250)
  
  l11chl = ggplot(data = l11, aes(x = doy, y = manual_chl))+
    geom_line(color = "forestgreen", size = 1)+
    theme_classic()+
    labs(y = "Chlorophyll (μg/L)")+
    xlim(140, 250)
  
  l11percent_change = ggplot(data = l11, aes( x = doy, y = percent_change))+
    geom_line(size = 1, color = "black")+
    theme_classic()+
    xlim(138, 250)+
    labs(y = "% change in chlorophyll")+
    geom_rect(data=hw.l11, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
                                                  ymax= Inf), color="transparent", 
              fill="orange", alpha=0.3)+
    theme(legend.position="none")+
    geom_vline(data=hw.l11, inherit.aes=FALSE, aes(xintercept = yday(date_start)), linetype = "dashed")+
    geom_vline(data=hw.l11, inherit.aes=FALSE, aes(xintercept = yday(date_end)), linetype = "dashed")+
    geom_rect(data=hw.l11, inherit.aes=FALSE, aes(xmin=(start_analysis), xmax=(end_analysis), ymin= - Inf,
                                                  ymax= Inf), color="transparent", 
              fill="magenta", alpha=0.3)+
    theme(legend.position="none")+
    geom_vline(data=hw.l11, inherit.aes=FALSE, aes(xintercept = (start_analysis)), linetype = "dashed")+
    geom_vline(data=hw.l11, inherit.aes=FALSE, aes(xintercept = (end_analysis)), linetype = "dashed")+
    ylim(-200, 600)
  
  # l11temp = ggplot(data = l11, aes(x = doy, y = mean_temp))+
  #   geom_line(color = "steelblue", size = 1)+
  #   theme_classic()+
  #   labs(title = "Paul 2011")+
  #   annotate("rect", xmin = as.Date(199), xmax = as.Date(205), ymin = -Inf, ymax = Inf,
  #            fill = "red", alpha = 0.5)
  
  
  # Peter
  r11slope = ggplot(data = r11, aes( x = doy, y = chl_slope))+
    geom_line(color = "black", size = 1)+
    theme_classic()+
    ylim(-4.1, 4.5)+
    labs(y = "Chlorophyll slope (μg/L/day)")+
    xlim(140, 250)
  
  r11chl = ggplot(data = r11, aes(x = doy, y = manual_chl))+
    geom_line(color = "forestgreen", size = 1)+
    theme_classic()+
    labs(y = "Chlorophyll (μg/L)")+
    xlim(140, 250)
  
  r11percent_change = ggplot(data = r11, aes( x = doy, y = percent_change))+
    geom_line(size = 1, color = "black")+
    theme_classic()+
    xlim(138, 250)+
    labs(y = "% change in chlorophyll")+
    geom_rect(data=hw.r11, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
                                                  ymax= Inf), color="transparent", 
              fill="orange", alpha=0.3)+
    theme(legend.position="none")+
    geom_vline(data=hw.r11, inherit.aes=FALSE, aes(xintercept = yday(date_start)), linetype = "dashed")+
    geom_vline(data=hw.r11, inherit.aes=FALSE, aes(xintercept = yday(date_end)), linetype = "dashed")+
    geom_rect(data=hw.r11, inherit.aes=FALSE, aes(xmin=(start_analysis), xmax=(end_analysis), ymin= - Inf,
                                                  ymax= Inf), color="transparent", 
              fill="magenta", alpha=0.3)+
    theme(legend.position="none")+
    geom_vline(data=hw.r11, inherit.aes=FALSE, aes(xintercept = (start_analysis)), linetype = "dashed")+
    geom_vline(data=hw.r11, inherit.aes=FALSE, aes(xintercept = (end_analysis)), linetype = "dashed")+
    ylim(-200, 600)
  
  # r11temp = ggplot(data = r11, aes(x = doy, y = mean_temp))+
  #   geom_line(color = "steelblue", size = 1)+
  #   theme_classic()+
  #   labs(title = "Peter 2011")+
  #   annotate("rect", xmin = as.Date(199), xmax = as.Date(205), ymin = -Inf, ymax = Inf,
  #            fill = "red", alpha = 0.5)
  
  print(ggarrange(nrow = 4, ncol = 2, r11temp, l11temp, r11chl, l11chl, r11slope, l11slope, r11percent_change, l11percent_change))
  
  
  
  
  ##### 2013 ####
  t13 = slopes %>%  filter(year == 2013, lake == "T")
  l13 = slopes %>%  filter(year == 2013, lake == "L")
  r13 = slopes %>%  filter(year == 2013, lake == "R")
  
  # Tuesday
  t13slope = ggplot(data = t13, aes( x = doy, y = chl_slope))+
    geom_line(color = "black", size = 1)+
    theme_classic()+
    ylim(-4.7, 5.5)+
    labs(y = "Chlorophyll slope (μg/L/day)")+
    xlim(140, 250)
  
  t13chl = ggplot(data = t13, aes(x = doy, y = manual_chl))+
    geom_line(color = "forestgreen", size = 1)+
    theme_classic()+
    xlim(138, 250)+
    labs(y = "Chlorophyll (μg/L)")
  
  t13percent_change = ggplot(data = t13, aes( x = doy, y = percent_change))+
    geom_line(size = 1, color = "black")+
    theme_classic()+
    xlim(138, 250)+
    labs(y = "% change in chlorophyll")+
    geom_rect(data=hw.t13, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
                                                  ymax= Inf), color="transparent", 
              fill="orange", alpha=0.3)+
    theme(legend.position="none")+
    geom_vline(data=hw.t13, inherit.aes=FALSE, aes(xintercept = yday(date_start)), linetype = "dashed")+
    geom_vline(data=hw.t13, inherit.aes=FALSE, aes(xintercept = yday(date_end)), linetype = "dashed")+
    geom_rect(data=hw.t13, inherit.aes=FALSE, aes(xmin=(start_analysis), xmax=(end_analysis), ymin= - Inf,
                                                  ymax= Inf), color="transparent", 
              fill="magenta", alpha=0.3)+
    theme(legend.position="none")+
    geom_vline(data=hw.t13, inherit.aes=FALSE, aes(xintercept = (start_analysis)), linetype = "dashed")+
    geom_vline(data=hw.t13, inherit.aes=FALSE, aes(xintercept = (end_analysis)), linetype = "dashed")+
    ylim(-200, 600)
  
  # t13temp = ggplot(data = t13, aes(x = doy, y = mean_temp))+
  #   geom_line(color = "steelblue", size = 1)+
  #   theme_classic()+
  #   labs(title = "Tuesday 2013")+
  #   annotate("rect", xmin = as.Date(186), xmax = as.Date(190), ymin = -Inf, ymax = Inf,
  #            fill = "red", alpha = 0.5)+
  #   annotate("rect", xmin = as.Date(196), xmax = as.Date(201), ymin = -Inf, ymax = Inf,
  #            fill = "red", alpha = 0.5)
  
  
  # Paul
  l13slope = ggplot(data = l13, aes( x = doy, y = chl_slope))+
    geom_line(color = "black", size = 1)+
    theme_classic()+
    ylim(-4.7, 5.5)+
    labs(y = "Chlorophyll slope (μg/L/day)")+
    xlim(140, 250)
  
  l13chl = ggplot(data = l13, aes(x = doy, y = manual_chl))+
    geom_line(color = "forestgreen", size = 1)+
    theme_classic()+
    labs(y = "Chlorophyll (μg/L)")+
    xlim(140, 250)
  
  
  l13percent_change = ggplot(data = l13, aes( x = doy, y = percent_change))+
    geom_line(size = 1, color = "black")+
    theme_classic()+
    xlim(138, 250)+
    labs(y = "% change in chlorophyll")+
    geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
                                                  ymax= Inf), color="transparent", 
              fill="orange", alpha=0.3)+
    theme(legend.position="none")+
    geom_vline(data=hw.l13, inherit.aes=FALSE, aes(xintercept = yday(date_start)), linetype = "dashed")+
    geom_vline(data=hw.l13, inherit.aes=FALSE, aes(xintercept = yday(date_end)), linetype = "dashed")+
    geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=(start_analysis), xmax=(end_analysis), ymin= - Inf,
                                                  ymax= Inf), color="transparent", 
              fill="magenta", alpha=0.3)+
    theme(legend.position="none")+
    geom_vline(data=hw.l13, inherit.aes=FALSE, aes(xintercept = (start_analysis)), linetype = "dashed")+
    geom_vline(data=hw.l13, inherit.aes=FALSE, aes(xintercept = (end_analysis)), linetype = "dashed")+
    ylim(-200, 600)
  
  # l13temp = ggplot(data = l13, aes(x = doy, y = mean_temp))+
  #   geom_line(color = "steelblue", size = 1)+
  #   theme_classic()+
  #   labs(title = "Paul 2013")+
  #   annotate("rect", xmin = as.Date(196), xmax = as.Date(201), ymin = -Inf, ymax = Inf,
  #            fill = "red", alpha = 0.5)
  
  
  # Peter
  r13slope = ggplot(data = r13, aes( x = doy, y = chl_slope))+
    geom_line(color = "black", size = 1)+
    theme_classic()+
    ylim(-4.7, 5.5)+
    labs(y = "Chlorophyll slope (μg/L/day)")+
    xlim(140, 250)
  
  r13chl = ggplot(data = r13, aes(x = doy, y = manual_chl))+
    geom_line(color = "forestgreen", size = 1)+
    theme_classic()+
    labs(y = "Chlorophyll (μg/L)")+
    xlim(140, 250)
  
  r13percent_change = ggplot(data = r13, aes( x = doy, y = percent_change))+
    geom_line(size = 1, color = "black")+
    theme_classic()+
    xlim(138, 250)+
    labs(y = "% change in chlorophyll")+
    geom_rect(data=hw.r13, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
                                                  ymax= Inf), color="transparent", 
              fill="orange", alpha=0.3)+
    theme(legend.position="none")+
    geom_vline(data=hw.r13, inherit.aes=FALSE, aes(xintercept = yday(date_start)), linetype = "dashed")+
    geom_vline(data=hw.r13, inherit.aes=FALSE, aes(xintercept = yday(date_end)), linetype = "dashed")+
    geom_rect(data=hw.r13, inherit.aes=FALSE, aes(xmin=(start_analysis), xmax=(end_analysis), ymin= - Inf,
                                                  ymax= Inf), color="transparent", 
              fill="magenta", alpha=0.3)+
    theme(legend.position="none")+
    geom_vline(data=hw.r13, inherit.aes=FALSE, aes(xintercept = (start_analysis)), linetype = "dashed")+
    geom_vline(data=hw.r13, inherit.aes=FALSE, aes(xintercept = (end_analysis)), linetype = "dashed")+
    ylim(-200, 600)
  
  # r13temp = ggplot(data = r13, aes(x = doy, y = mean_temp))+
  #   geom_line(color = "steelblue", size = 1)+
  #   theme_classic()+
  #   labs(title = "Peter 2013")+
  #   annotate("rect", xmin = as.Date(196), xmax = as.Date(201), ymin = -Inf, ymax = Inf,
  #            fill = "red", alpha = 0.5)
  
  print(ggarrange(nrow = 4, ncol = 3, r13temp, l13temp, t13temp, r13chl, l13chl, t13chl, r13slope, l13slope, t13slope, r13percent_change, l13percent_change, t13percent_change))
  
  
  ##### 2014 ####
  t14 = slopes %>%  filter(year == 2014, lake == "T")
  l14 = slopes %>%  filter(year == 2014, lake == "L")
  r14 = slopes %>%  filter(year == 2014, lake == "R")
  
  # Tuesday
  t14slope = ggplot(data = t14, aes( x = doy, y = chl_slope))+
    geom_line(color = "black", size = 1)+
    theme_classic()+
    ylim(-4.7, 5.5)+
    labs(y = "Chlorophyll slope (μg/L/day)")+
    xlim(140, 250)
  
  t14chl = ggplot(data = t14, aes(x = doy, y = manual_chl))+
    geom_line(color = "forestgreen", size = 1)+
    theme_classic()+
    labs(y = "Chlorophyll (μg/L)")+
    xlim(140, 250)
  
  t14percent_change = ggplot(data = t14, aes( x = doy, y = percent_change))+
    geom_line(size = 1, color = "black")+
    theme_classic()+
    xlim(138, 250)+
    labs(y = "% change in chlorophyll")+
    geom_rect(data=hw.t14, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
                                                  ymax= Inf), color="transparent", 
              fill="orange", alpha=0.3)+
    theme(legend.position="none")+
    geom_vline(data=hw.t14, inherit.aes=FALSE, aes(xintercept = yday(date_start)), linetype = "dashed")+
    geom_vline(data=hw.t14, inherit.aes=FALSE, aes(xintercept = yday(date_end)), linetype = "dashed")+
    geom_rect(data=hw.t14, inherit.aes=FALSE, aes(xmin=(start_analysis), xmax=(end_analysis), ymin= - Inf,
                                                  ymax= Inf), color="transparent", 
              fill="magenta", alpha=0.3)+
    theme(legend.position="none")+
    geom_vline(data=hw.t14, inherit.aes=FALSE, aes(xintercept = (start_analysis)), linetype = "dashed")+
    geom_vline(data=hw.t14, inherit.aes=FALSE, aes(xintercept = (end_analysis)), linetype = "dashed")+
    ylim(-200, 600)
  
  # t14temp = ggplot(data = t14, aes(x = doy, y = mean_temp))+
  #   geom_line(color = "steelblue", size = 1)+
  #   theme_classic()+
  #   labs(title = "Tuesday 2014")+
  #   annotate("rect", xmin = as.Date(146), xmax = as.Date(154), ymin = -Inf, ymax = Inf,
  #            fill = "red", alpha = 0.5)
  
  
  # Paul
  l14slope = ggplot(data = l14, aes( x = doy, y = chl_slope))+
    geom_line(color = "black", size = 1)+
    theme_classic()+
    ylim(-4.1, 4.5)+
    labs(y = "Chlorophyll slope (μg/L/day)")+
    xlim(140, 250)
  
  l14chl = ggplot(data = l14, aes(x = doy, y = manual_chl))+
    geom_line(color = "forestgreen", size = 1)+
    theme_classic()+
    labs(y = "Chlorophyll (μg/L)")+
    xlim(140, 250)
  
  l14percent_change = ggplot(data = l14, aes( x = doy, y = percent_change))+
    geom_line(size = 1, color = "black")+
    theme_classic()+
    xlim(138, 250)+
    labs(y = "% change in chlorophyll")+
    geom_rect(data=hw.l14, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
                                                  ymax= Inf), color="transparent", 
              fill="orange", alpha=0.3)+
    theme(legend.position="none")+
    geom_vline(data=hw.l14, inherit.aes=FALSE, aes(xintercept = yday(date_start)), linetype = "dashed")+
    geom_vline(data=hw.l14, inherit.aes=FALSE, aes(xintercept = yday(date_end)), linetype = "dashed")+
    geom_rect(data=hw.l14, inherit.aes=FALSE, aes(xmin=(start_analysis), xmax=(end_analysis), ymin= - Inf,
                                                  ymax= Inf), color="transparent", 
              fill="magenta", alpha=0.3)+
    theme(legend.position="none")+
    geom_vline(data=hw.l14, inherit.aes=FALSE, aes(xintercept = (start_analysis)), linetype = "dashed")+
    geom_vline(data=hw.l14, inherit.aes=FALSE, aes(xintercept = (end_analysis)), linetype = "dashed")+
    ylim(-200, 600)
  
  # l14temp = ggplot(data = l14, aes(x = doy, y = mean_temp))+
  #   geom_line(color = "steelblue", size = 1)+
  #   theme_classic()+
  #   labs(title = "Paul 2014")
  
  
  # Peter
  r14slope = ggplot(data = r14, aes( x = doy, y = chl_slope))+
    geom_line(color = "black", size = 1)+
    theme_classic()+
    ylim(-4.1, 4.5)+
    labs(y = "Chlorophyll slope (μg/L/day)")+
    xlim(140, 250)
  
  r14chl = ggplot(data = r14, aes(x = doy, y = manual_chl))+
    geom_line(color = "forestgreen", size = 1)+
    theme_classic()+
    labs(y = "Chlorophyll (μg/L)")+
    xlim(140, 250)
  
  r14percent_change = ggplot(data = r14, aes( x = doy, y = percent_change))+
    geom_line(size = 1, color = "black")+
    theme_classic()+
    xlim(138, 250)+
    labs(y = "% change in chlorophyll")+
    geom_rect(data=hw.r14, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
                                                  ymax= Inf), color="transparent", 
              fill="orange", alpha=0.3)+
    theme(legend.position="none")+
    geom_vline(data=hw.r14, inherit.aes=FALSE, aes(xintercept = yday(date_start)), linetype = "dashed")+
    geom_vline(data=hw.r14, inherit.aes=FALSE, aes(xintercept = yday(date_end)), linetype = "dashed")+
    geom_rect(data=hw.r14, inherit.aes=FALSE, aes(xmin=(start_analysis), xmax=(end_analysis), ymin= - Inf,
                                                  ymax= Inf), color="transparent", 
              fill="magenta", alpha=0.3)+
    theme(legend.position="none")+
    geom_vline(data=hw.r14, inherit.aes=FALSE, aes(xintercept = (start_analysis)), linetype = "dashed")+
    geom_vline(data=hw.r14, inherit.aes=FALSE, aes(xintercept = (end_analysis)), linetype = "dashed")+
    ylim(-200, 600)
  
  # r14temp = ggplot(data = l14, aes(x = doy, y = mean_temp))+
  #   geom_line(color = "steelblue", size = 1)+
  #   theme_classic()+
  #   labs(title = "Peter 2014")+
  #   annotate("rect", xmin = as.Date(149), xmax = as.Date(153), ymin = -Inf, ymax = Inf,
  #            fill = "red", alpha = 0.5)
  
  print(ggarrange(nrow = 4, ncol = 3, r14temp, l14temp, t14temp, r14chl, l14chl, t14chl, r14slope, l14slope, t14slope, r14percent_change, l14percent_change, t14percent_change))
  
  
  
  
  ##### 2015 #####
  t15 = slopes %>%  filter(year == 2015, lake == "T")
  l15 = slopes %>%  filter(year == 2015, lake == "L")
  r15 = slopes %>%  filter(year == 2015, lake == "R")
  
  # Tuesday
  t15slope = ggplot(data = t15, aes( x = doy, y = chl_slope))+
    geom_line(color = "black", size = 1)+
    theme_classic()+
    ylim(-4.7, 5.5)+
    labs(y = "Chlorophyll slope (μg/L/day)")+
    xlim(140, 250)
  
  t15chl = ggplot(data = t15, aes(x = doy, y = manual_chl))+
    geom_line(color = "forestgreen", size = 1)+
    theme_classic()+
    labs(y = "Chlorophyll (μg/L)")+
    xlim(140, 250)
  
  
  t15percent_change = ggplot(data = t15, aes( x = doy, y = percent_change))+
    geom_line(size = 1, color = "black")+
    theme_classic()+
    xlim(138, 250)+
    labs(y = "% change in chlorophyll")+
    geom_rect(data=hw.t15, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
                                                  ymax= Inf), color="transparent", 
              fill="orange", alpha=0.3)+
    theme(legend.position="none")+
    geom_vline(data=hw.t15, inherit.aes=FALSE, aes(xintercept = yday(date_start)), linetype = "dashed")+
    geom_vline(data=hw.t15, inherit.aes=FALSE, aes(xintercept = yday(date_end)), linetype = "dashed")+
    geom_rect(data=hw.t15, inherit.aes=FALSE, aes(xmin=(start_analysis), xmax=(end_analysis), ymin= - Inf,
                                                  ymax= Inf), color="transparent", 
              fill="magenta", alpha=0.3)+
    theme(legend.position="none")+
    geom_vline(data=hw.t15, inherit.aes=FALSE, aes(xintercept = (start_analysis)), linetype = "dashed")+
    geom_vline(data=hw.t15, inherit.aes=FALSE, aes(xintercept = (end_analysis)), linetype = "dashed")+
    ylim(-200, 600)
  
  # t15temp = ggplot(data = l14, aes(x = doy, y = mean_temp))+
  #   geom_line(color = "steelblue", size = 1)+
  #   theme_classic()+
  #   labs(title = "Tuesday 2015")+
  #   annotate("rect", xmin = 206, xmax = as.Date(210), ymin = -Inf, ymax = Inf,
  #            fill = "red", alpha = 0.5)+
  #   annotate("rect", xmin = as.Date(226), xmax = as.Date(230), ymin = -Inf, ymax = Inf,
  #            fill = "red", alpha = 0.5)
  
  # Paul
  l15slope = ggplot(data = l15, aes( x = doy, y = chl_slope))+
    geom_line(color = "black", size = 1)+
    theme_classic()+
    ylim(-4.1, 4.5)+
    labs(y = "Chlorophyll slope (μg/L/day)")+
    xlim(140, 250)
  
  l15chl = ggplot(data = l15, aes(x = doy, y = manual_chl))+
    geom_line(color = "forestgreen", size = 1)+
    theme_classic()+
    labs(y = "Chlorophyll (μg/L)")+
    xlim(140, 250)
  
  l15percent_change = ggplot(data = l15, aes( x = doy, y = percent_change))+
    geom_line(size = 1, color = "black")+
    theme_classic()+
    xlim(138, 250)+
    labs(y = "% change in chlorophyll")+
    geom_rect(data=hw.l15, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
                                                  ymax= Inf), color="transparent", 
              fill="orange", alpha=0.3)+
    theme(legend.position="none")+
    geom_vline(data=hw.l15, inherit.aes=FALSE, aes(xintercept = yday(date_start)), linetype = "dashed")+
    geom_vline(data=hw.l15, inherit.aes=FALSE, aes(xintercept = yday(date_end)), linetype = "dashed")+
    geom_rect(data=hw.l15, inherit.aes=FALSE, aes(xmin=(start_analysis), xmax=(end_analysis), ymin= - Inf,
                                                  ymax= Inf), color="transparent", 
              fill="magenta", alpha=0.3)+
    theme(legend.position="none")+
    geom_vline(data=hw.l15, inherit.aes=FALSE, aes(xintercept = (start_analysis)), linetype = "dashed")+
    geom_vline(data=hw.l15, inherit.aes=FALSE, aes(xintercept = (end_analysis)), linetype = "dashed")+
    ylim(-200, 600)
  
  # l15temp = ggplot(data = l14, aes(x = doy, y = mean_temp))+
  #   geom_line(color = "steelblue", size = 1)+
  #   theme_classic()+
  #   labs(title = "Paul 2015")
  
  
  # Peter
  r15slope = ggplot(data = r15, aes( x = doy, y = chl_slope))+
    geom_line(color = "black", size = 1)+
    theme_classic()+
    ylim(-6, 4.5)+
    labs(y = "Chlorophyll slope (μg/L/day)")+
    xlim(140, 250)
  
  r15chl = ggplot(data = r15, aes(x = doy, y = manual_chl))+
    geom_line(color = "forestgreen", size = 1)+
    theme_classic()+
    labs(y = "Chlorophyll (μg/L)")+
    xlim(140, 250)
  
  r15percent_change = ggplot(data = r15, aes( x = doy, y = percent_change))+
    geom_line(size = 1, color = "black")+
    theme_classic()+
    xlim(138, 250)+
    labs(y = "% change in chlorophyll")+
    geom_rect(data=hw.r15, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
                                                  ymax= Inf), color="transparent", 
              fill="orange", alpha=0.3)+
    theme(legend.position="none")+
    geom_vline(data=hw.r15, inherit.aes=FALSE, aes(xintercept = yday(date_start)), linetype = "dashed")+
    geom_vline(data=hw.r15, inherit.aes=FALSE, aes(xintercept = yday(date_end)), linetype = "dashed")+
    geom_rect(data=hw.r15, inherit.aes=FALSE, aes(xmin=(start_analysis), xmax=(end_analysis), ymin= - Inf,
                                                  ymax= Inf), color="transparent", 
              fill="magenta", alpha=0.3)+
    theme(legend.position="none")+
    geom_vline(data=hw.r15, inherit.aes=FALSE, aes(xintercept = (start_analysis)), linetype = "dashed")+
    geom_vline(data=hw.r15, inherit.aes=FALSE, aes(xintercept = (end_analysis)), linetype = "dashed")+
    ylim(-200, 600)
  
  # r15temp = ggplot(data = l14, aes(x = doy, y = mean_temp))+
  #   geom_line(color = "steelblue", size = 1)+
  #   theme_classic()+
  #   labs(title = "Peter 2015")
  
  print(ggarrange(nrow = 4, ncol = 3, r15temp, l15temp, t15temp, r15chl, l15chl, t15chl, r15slope, l15slope, t15slope, r15percent_change, l15percent_change, t15percent_change))
  
  
  
  
  
  
  ##### 2018 ####
  t18 = slopes %>%  filter(year == 2018, lake == "T")
  l18 = slopes %>%  filter(year == 2018, lake == "L")
  r18 = slopes %>%  filter(year == 2018, lake == "R")
  
  # Paul
  l18slope = ggplot(data = l18, aes( x = doy, y = chl_slope))+
    geom_line(color = "black", size = 1)+
    theme_classic()+
    ylim(-4.7, 5.5)+
    labs(y = "Chlorophyll slope (μg/L/day)")+
    xlim(140, 250)
  
  l18chl = ggplot(data = l18, aes(x = doy, y = manual_chl))+
    geom_line(color = "forestgreen", size = 1)+
    theme_classic()+
    labs(y = "Chlorophyll (μg/L)")+
    xlim(140, 250)
  
  l18percent_change = ggplot(data = l18, aes( x = doy, y = percent_change))+
    geom_line(size = 1, color = "black")+
    theme_classic()+
    xlim(138, 250)+
    labs(y = "% change in chlorophyll")+
    geom_rect(data=hw.l18, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
                                                  ymax= Inf), color="transparent", 
              fill="orange", alpha=0.3)+
    theme(legend.position="none")+
    geom_vline(data=hw.l18, inherit.aes=FALSE, aes(xintercept = yday(date_start)), linetype = "dashed")+
    geom_vline(data=hw.l18, inherit.aes=FALSE, aes(xintercept = yday(date_end)), linetype = "dashed")+
    geom_rect(data=hw.l18, inherit.aes=FALSE, aes(xmin=(start_analysis), xmax=(end_analysis), ymin= - Inf,
                                                  ymax= Inf), color="transparent", 
              fill="magenta", alpha=0.3)+
    theme(legend.position="none")+
    geom_vline(data=hw.l18, inherit.aes=FALSE, aes(xintercept = (start_analysis)), linetype = "dashed")+
    geom_vline(data=hw.l18, inherit.aes=FALSE, aes(xintercept = (end_analysis)), linetype = "dashed")+
    ylim(-200, 600)
  
  # l18temp = ggplot(data = l18, aes(x = doy, y = mean_temp))+
  #   geom_line(color = "steelblue", size = 1)+
  #   theme_classic()+
  #   labs(title = "Paul 2018")+
  #   annotate("rect", xmin = as.Date(144), xmax = as.Date(152), ymin = -Inf, ymax = Inf,
  #            fill = "red", alpha = 0.5)+
  #   annotate("rect", xmin = as.Date(169), xmax = as.Date(175), ymin = -Inf, ymax = Inf,
  #            fill = "red", alpha = 0.5)+
  #   annotate("rect", xmin = as.Date(180), xmax = as.Date(188), ymin = -Inf, ymax = Inf,
  #            fill = "red", alpha = 0.5)+
  #   annotate("rect", xmin = as.Date(223), xmax = as.Date(231), ymin = -Inf, ymax = Inf,
  #            fill = "red", alpha = 0.5)
  
  # Peter
  r18slope = ggplot(data = r18, aes( x = doy, y = chl_slope))+
    geom_line(color = "black", size = 1)+
    theme_classic()+
    ylim(-4.7, 5.5)+
    labs(y = "Chlorophyll slope (μg/L/day)")+
    xlim(140, 250)
  
  r18chl = ggplot(data = r18, aes(x = doy, y = manual_chl))+
    geom_line(color = "forestgreen", size = 1)+
    theme_classic()+
    labs(y = "Chlorophyll (μg/L)")+
    xlim(140, 250)
  
  r18percent_change = ggplot(data = r18, aes( x = doy, y = percent_change))+
    geom_line(size = 1, color = "black")+
    theme_classic()+
    xlim(138, 250)+
    labs(y = "% change in chlorophyll")+
    geom_rect(data=hw.r18, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
                                                  ymax= Inf), color="transparent", 
              fill="orange", alpha=0.3)+
    theme(legend.position="none")+
    geom_vline(data=hw.r18, inherit.aes=FALSE, aes(xintercept = yday(date_start)), linetype = "dashed")+
    geom_vline(data=hw.r18, inherit.aes=FALSE, aes(xintercept = yday(date_end)), linetype = "dashed")+
    geom_rect(data=hw.r18, inherit.aes=FALSE, aes(xmin=(start_analysis), xmax=(end_analysis), ymin= - Inf,
                                                  ymax= Inf), color="transparent", 
              fill="magenta", alpha=0.3)+
    theme(legend.position="none")+
    geom_vline(data=hw.r18, inherit.aes=FALSE, aes(xintercept = (start_analysis)), linetype = "dashed")+
    geom_vline(data=hw.r18, inherit.aes=FALSE, aes(xintercept = (end_analysis)), linetype = "dashed")+
    ylim(-200, 600)
  
  # r18temp = ggplot(data = r18, aes(x = doy, y = mean_temp))+
  #   geom_line(color = "steelblue", size = 1)+
  #   theme_classic()+
  #   labs(title = "Peter 2018")+
  #   annotate("rect", xmin = as.Date(144), xmax = as.Date(152), ymin = -Inf, ymax = Inf,
  #            fill = "red", alpha = 0.5)+
  #   annotate("rect", xmin = as.Date(169), xmax = as.Date(175), ymin = -Inf, ymax = Inf,
  #            fill = "red", alpha = 0.5)+
  #   annotate("rect", xmin = as.Date(180), xmax = as.Date(192), ymin = -Inf, ymax = Inf,
  #            fill = "red", alpha = 0.5)+
  #   annotate("rect", xmin = as.Date(223), xmax = as.Date(232), ymin = -Inf, ymax = Inf,
  #            fill = "red", alpha = 0.5)
  
  print(ggarrange(nrow = 4, ncol = 2, r18temp, l18temp, r18chl, l18chl, r18slope, l18slope, r18percent_change, l18percent_change))
  
  
  
  
  
  ##### 2019 ####
  l19 = slopes %>%  filter(year == 2019, lake == "L")
  r19 = slopes %>%  filter(year == 2019, lake == "R")
  
  # Paul
  l19slope = ggplot(data = l19, aes( x = doy, y = chl_slope))+
    geom_line(color = "black", size = 1)+
    theme_classic()+
    ylim(-4.7, 5.5)+
    labs(y = "Chlorophyll slope (μg/L/day)")+
    xlim(140, 250)
  
  l19chl = ggplot(data = l19, aes(x = doy, y = manual_chl))+
    geom_line(color = "forestgreen", size = 1)+
    theme_classic()+
    labs(y = "Chlorophyll (μg/L)")+
    xlim(140, 250)
  
  l19percent_change = ggplot(data = l19, aes( x = doy, y = percent_change))+
    geom_line(size = 1, color = "black")+
    theme_classic()+
    xlim(138, 250)+
    labs(y = "% change in chlorophyll")+
    geom_rect(data=hw.l19, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
                                                  ymax= Inf), color="transparent", 
              fill="orange", alpha=0.3)+
    theme(legend.position="none")+
    geom_vline(data=hw.l19, inherit.aes=FALSE, aes(xintercept = yday(date_start)), linetype = "dashed")+
    geom_vline(data=hw.l19, inherit.aes=FALSE, aes(xintercept = yday(date_end)), linetype = "dashed")+
    geom_rect(data=hw.l19, inherit.aes=FALSE, aes(xmin=(start_analysis), xmax=(end_analysis), ymin= - Inf,
                                                  ymax= Inf), color="transparent", 
              fill="magenta", alpha=0.3)+
    theme(legend.position="none")+
    geom_vline(data=hw.l19, inherit.aes=FALSE, aes(xintercept = (start_analysis)), linetype = "dashed")+
    geom_vline(data=hw.l19, inherit.aes=FALSE, aes(xintercept = (end_analysis)), linetype = "dashed")+
    ylim(-200, 600)
    
  
  # l19temp = ggplot(data = l19, aes(x = doy, y = mean_temp))+
  #   geom_line(color = "steelblue", size = 1)+
  #   theme_classic()+
  #   labs(title = "Paul 2019")+
  #   annotate('rect', xmin = as.Date(196), xmax = as.Date(200), ymin = -Inf, ymax = Inf,
  #            fill = 'red', alpha = 0.5)
  
  
  # Peter
  r19slope = ggplot(data = r19, aes( x = doy, y = chl_slope))+
    geom_line(color = "black", size = 1)+
    theme_classic()+
    ylim(-4.7, 5.5)+
    labs(y = "Chlorophyll slope (μg/L/day)")+
    xlim(140, 250)
  
  r19chl = ggplot(data = r19, aes(x = doy, y = manual_chl))+
    geom_line(color = "forestgreen", size = 1)+
    theme_classic()+
    labs(y = "Chlorophyll (μg/L)")+
    xlim(140, 250)
  
  
  r19percent_change = ggplot(data = r19, aes( x = doy, y = percent_change))+
    geom_line(size = 1, color = "black")+
    theme_classic()+
    xlim(138, 250)+
    labs(y = "% change in chlorophyll")+
    geom_rect(data=hw.r19, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
                                                  ymax= Inf), color="transparent", 
              fill="orange", alpha=0.3)+
    theme(legend.position="none")+
    geom_vline(data=hw.r19, inherit.aes=FALSE, aes(xintercept = yday(date_start)), linetype = "dashed")+
    geom_vline(data=hw.r19, inherit.aes=FALSE, aes(xintercept = yday(date_end)), linetype = "dashed")+
    geom_rect(data=hw.r19, inherit.aes=FALSE, aes(xmin=(start_analysis), xmax=(end_analysis), ymin= - Inf,
                                                  ymax= Inf), color="transparent", 
              fill="magenta", alpha=0.3)+
    theme(legend.position="none")+
    geom_vline(data=hw.r19, inherit.aes=FALSE, aes(xintercept = (start_analysis)), linetype = "dashed")+
    geom_vline(data=hw.r19, inherit.aes=FALSE, aes(xintercept = (end_analysis)), linetype = "dashed")+
    ylim(-200, 600)
  
  # r19temp = ggplot(data = r19, aes(x = doy, y = mean_temp))+
  #   geom_line(color = "steelblue", size = 1)+
  #   theme_classic()+
  #   labs(title = "Peter 2019")+
  #   annotate('rect', xmin = as.Date(210), xmax = as.Date(219), ymin = -Inf, ymax = Inf,
  #            fill = 'red', alpha = 0.5)
  
  print(ggarrange(nrow = 4, ncol = 2, r19temp, l19temp, r19chl, l19chl, r19slope, l19slope, r19percent_change, l19percent_change))
  
  dev.off()
  
  
}




##### ASLO Figures ######
png("./figures/ASLO figures/Paul 2013 hw with heatwaves.png", height = 4.5, width = 8, units = "in", res = 300)

event_line_DKS("paul", 2013)+
  labs(title = "Paul 2013")+
  #theme(legend.position="none")+
  xlim(140, 250)+
  ylim(14, 32) +
  geom_point()+
  labs(x = "day of year")

dev.off()

paulHW = readRDS(file = "./results/heatwave modeled outputs/paul heatwave outputs modeled.rds")

climatology.L = paulHW$climatology %>% mutate(lake = "paul")
climatology.L$year = year(climatology.L$t)

climatology.L.2013 = climatology.L %>% filter(year == 2013)

#cur.climatology$thresh2 = cur.climatology$thresh * 2

#cur.climatology = cur.HW$climatology %>% filter(year(t) == year)

### Plot with thresholds no heatwaves ###
year = 2013

png("./figures/ASLO figures/Paul 2013 hw without heatwaves.png", height = 4.5, width = 8, units = "in", res = 300)

ggplot(climatology.L.2013, aes(x = doy, y = temp)) +
        geom_line(aes(x = doy, y = temp, color = "Temperature"), size = 0.8) +
        geom_point() +
        geom_line(aes(x = doy, y = seas, color = "Climatology", linetype = "solid"), size = 0.7) +
        geom_line(aes(x = doy, y = thresh, color = "Threshold", linetype = "solid"), size = 0.7) +
        # geom_line(aes(x = doy, y = thresh2, color = "2x Threshold", linetype = "solid")) +
        theme_classic() +
        labs(title = paste("Paul", year, sep = " "), y = "Temperature [°C]", x = "day of year") +
        ylim(14, 32) +
  xlim(140, 250)+
        scale_color_manual(values = c("black", "black", "forestgreen", "black"), 
                           guide = guide_legend(title = NULL, override.aes = list(linetype = "solid")))+
        guides(linetype = "none")

dev.off()





png("./figures/ASLO figures/Paul 2013 hw only temperature.png", height = 4.5, width = 8, units = "in", res = 300)

ggplot(climatology.L.2013, aes(x = doy, y = temp)) +
  geom_line(aes(x = doy, y = temp, color = "Temperature"), size = 0.8) +
  geom_point() +
 # geom_line(aes(x = doy, y = seas, color = "Climatology", linetype = "solid"), size = 0.7) +
#  geom_line(aes(x = doy, y = thresh, color = "Threshold", linetype = "solid"), size = 0.7) +
  # geom_line(aes(x = doy, y = thresh2, color = "2x Threshold", linetype = "solid")) +
  theme_classic() +
  labs(title = paste("Paul", year, sep = " "), y = "Temperature [°C]", x = "day of year") +
  ylim(14, 32) +
  xlim(140, 250)+
  scale_color_manual(values = c("black", "black", "forestgreen", "black"), 
                     guide = guide_legend(title = NULL, override.aes = list(linetype = "solid")))+
  guides(linetype = "none")

dev.off()









png("./figures/ASLO figures/Paul 2013 hw only temperature.png", height = 4.5, width = 8, units = "in", res = 300)

ggplot(climatology.L.2013, aes(x = doy, y = temp)) +
  geom_line(aes(x = doy, y = temp, color = "Temperature"), size = 0.8) +
  geom_point() +
  geom_line(aes(x = doy, y = seas, color = "Climatology", linetype = "solid"), size = 0.7) +
  geom_line(aes(x = doy, y = thresh, color = "Threshold", linetype = "solid"), size = 0.7) +
 # geom_line(aes(x = doy, y = thresh2, color = "2x Threshold", linetype = "solid")) +
  theme_classic() +
  labs(title = paste("Paul", year, sep = " "), y = "Temperature [°C]", x = "day of year") +
  ylim(14, 32) +
  xlim(140, 250)+
  scale_color_manual(values = c("black", "black", "forestgreen", "black"), 
                     guide = guide_legend(title = NULL, override.aes = list(linetype = "solid")))+
  guides(linetype = "none")

dev.off()


















###### METHODS EXAMPLE 2013 MANUSCRIPT UPDATED #######

heatwaves = read.csv("./formatted data/explanatory variables heatwaves/heatwaves with percent zoop color nutrients.csv")

# select just the columns we want
heatwaves = heatwaves %>% select(lake, year, date_start, date_end, event_no)

# calculate where the analysis is going to actually happen, including slopes picked and 
# the number of slopes included

heatwaves = heatwaves %>% mutate(start_analysis = yday(date_end)+ daysAfter - slopeLength)
heatwaves = heatwaves %>% mutate(end_analysis = yday(date_end) + daysAfter + numSlopes -1)

# create separate dataframes for each heatwave
hw.l13 = heatwaves %>% filter(lake == "L" & year == 2013)
hw.r13 = heatwaves %>% filter(lake == "R" & year == 2013)
hw.t13 = heatwaves %>% filter(lake == "T" & year == 2013)

## THIS DEPENDS ON THE MOST RECENT VERSION OF ALLSLOPES
slopes = allSlopes %>% filter(daysAfter == 2 & lake == "T" | daysAfter == 3 & lake == "R" | daysAfter == 3 & lake == "L")
slopes = slopes %>% dplyr::rename(doy = doyCat)

#scale_fill_manual(values = c("R" = "#60BFCC", "L" = "#D9EEF3", "T" = "#544C34")
                  

##### 2013 ####
t13 = slopes %>%  filter(year == 2013, lake == "T")
l13 = slopes %>%  filter(year == 2013, lake == "L")
r13 = slopes %>%  filter(year == 2013, lake == "R")

# make a couple version of the datasets merged
merged_data <- merge(hw.r13, r13, by = "doy")


# make temp plots
t13temp = event_line_DKS2("tuesday", 2013)+
  labs(title = "Tuesday 2013", y = "", x = "")+
  theme(legend.position="none")+
  xlim(190, 210)+
  geom_rect(data=hw.t13, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
                                                ymax= Inf), color="transparent", 
            fill="orange", alpha=0.3)+
  geom_point()+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))


l13temp = event_line_DKS2("paul", 2013)+
  labs(title = "Paul 2013", x = "")+
  theme(legend.position="none")+
  xlim(190, 210)+
  geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
                                                ymax= Inf), color="transparent", 
            fill="orange", alpha=0.3)+
  geom_point()+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))

r13temp = event_line_DKS2("peter", 2013)+
  labs(title = "Peter 2013", y = "", x = "")+
  theme(legend.position="none")+
  xlim(190, 210)+
  geom_rect(data=hw.r13, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
                                                ymax= Inf), color="transparent", 
            fill="orange", alpha=0.3)+
  geom_point()+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))

# Tuesday
t13slope = ggplot(data = t13, aes( x = doy, y = chl_slope))+
  geom_line(color = "black", size = 0.8)+
  theme_classic()+
  geom_point(size = 1)+
  ylim(-1, 1.8)+
  labs(y = "", x = "")+
  xlim(190, 210)+
  # annotate("point", x = 192, y = 	1.028482, fill = "#544C34", size = 4, shape = 21)+
  annotate("point", x = 204, y = 	0.8988908, fill = "#544C34", size = 5, shape = 21)+
  geom_rect(data=hw.t13, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
                                                ymax= Inf), color="transparent", 
            fill="orange", alpha=0.3)+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))
  # geom_rect(data=hw.t13, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
  #                                               ymax= Inf), color="transparent", 
  #           fill="orange", alpha=0.3)


t13chl = ggplot(data = t13, aes(x = doy, y = manual_chl))+
  geom_line(color = "forestgreen", size = 0.8)+
  theme_classic()+
  geom_point(size = 1)+
  xlim(190, 210)+
  ylim(1, 25)+
  labs(y = "", x = "")+
  geom_rect(data=hw.t13, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
                                                ymax= Inf), color="transparent", 
            fill="orange", alpha=0.3)+
  geom_rect(data=hw.t13, inherit.aes=FALSE, aes(xmin=yday(date_end)-5, xmax=yday(date_end)+2, ymin= - Inf,
                                                ymax= Inf), color="transparent", 
            fill="#544C34", alpha=0.7)+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))



# Ensure `date_start` and `date_end` in `hw.t13` are in Date format
hw.t13$date_start <- as.Date(hw.t13$date_start)
hw.t13$date_end <- as.Date(hw.t13$date_end)


# Define the rectangle x-range for the linear regression fit
rect_xmin <- 197
rect_xmax <- 205

# Filter data points within the rectangle's range
t13_rect_data <- t13 %>%
  filter(doy >= rect_xmin & doy <= rect_xmax)

# Plot with the regression line
t13chl = ggplot(data = t13, aes(x = doy, y = manual_chl))+
  theme_classic()+
  labs(y = "", x = "")+
  xlim(190, 210)+
  ylim(1, 25)+
  geom_rect(data=hw.t13, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
                                                ymax= Inf), color="transparent", 
            fill="orange", alpha=0.3)+
  geom_rect(data=hw.t13, inherit.aes=FALSE, aes(xmin=yday(date_end)-4, xmax=yday(date_end)+3, ymin= - Inf,
                                                ymax= Inf), color="transparent", 
            fill="#544C34", alpha=0.35)+
  geom_point(size = 1.2)+
  geom_line(color = "forestgreen", size = 1)+
  geom_smooth(data = t13_rect_data, aes(x = doy, y = manual_chl), method = "lm", color = "black", linetype = "solid", se = FALSE, linewidth = 1)+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))





t13percent_change = ggplot(data = t13, aes( x = doy, y = percent_change))+
  geom_line(size = 0.8, color = "black")+
  theme_classic()+
  geom_point(size = 1)+
  xlim(190, 210)+
  labs(y = "", x = "day of year")+
  # geom_rect(data=hw.t13, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
  #                                               ymax= Inf), color="transparent", 
  #           fill="orange", alpha=0.3)+
  theme(legend.position="none")+
  # geom_vline(data=hw.t13, inherit.aes=FALSE, aes(xintercept = yday(date_start)), linetype = "dashed")+
  # geom_vline(data=hw.t13, inherit.aes=FALSE, aes(xintercept = yday(date_end)), linetype = "dashed")+
  # geom_rect(data=hw.t13, inherit.aes=FALSE, aes(xmin=(start_analysis), xmax=(end_analysis), ymin= - Inf,
  #                                               ymax= Inf), color="transparent", 
  #           fill="magenta", alpha=0.3)+
  theme(legend.position="none")+
  # geom_vline(data=hw.t13, inherit.aes=FALSE, aes(xintercept = (start_analysis)), linetype = "dashed")+
  # geom_vline(data=hw.t13, inherit.aes=FALSE, aes(xintercept = (end_analysis)), linetype = "dashed")+
  ylim(-100, 250)+
  # annotate("point", x = 192, y = 	189.91973, fill = "#544C34", size = 4, shape = 21)+
  annotate("point", x = 204, y = 	110.7496, fill = "#544C34", size = 5, shape = 21)+
  geom_rect(data=hw.t13, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
                                                ymax= Inf), color="transparent", 
            fill="orange", alpha=0.3)+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))


# t13temp = ggplot(data = t13, aes(x = doy, y = mean_temp))+
#   geom_line(color = "steelblue", size = 1)+
#   theme_classic()+
#   labs(title = "Tuesday 2013")+
#   annotate("rect", xmin = as.Date(186), xmax = as.Date(190), ymin = -Inf, ymax = Inf,
#            fill = "red", alpha = 0.5)+
#   annotate("rect", xmin = as.Date(196), xmax = as.Date(201), ymin = -Inf, ymax = Inf,
#            fill = "red", alpha = 0.5)


# Paul
l13slope = ggplot(data = l13, aes( x = doy, y = chl_slope))+
  geom_line(color = "black", size = 0.8)+
  theme_classic()+
  geom_point(size = 1)+
  ylim(-1, 1.8)+
  labs(y = "Chlorophyll slope (μg/L/day)", x = "")+
  xlim(190, 210)+
  annotate("point", x = 204, y = 	0.262535019, fill = "#D9EEF3", size = 5, shape = 21)+
  geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
                                                ymax= Inf), color="transparent", 
            fill="orange", alpha=0.3)+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 11))
  # geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
  #                                               ymax= Inf), color="transparent", 
  #           fill="orange", alpha=0.3)

# l13chl = ggplot(data = l13, aes(x = doy, y = manual_chl))+
#   geom_line(color = "forestgreen", size = 0.8)+
#   theme_classic()+
#   geom_point(size = 1)+
#   labs(y = "Chlorophyll (μg/L)", x = "")+
#   xlim(190, 210)+
#   ylim(1, 4.5)+
#   geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
#                                                 ymax= Inf), color="transparent", 
#             fill="orange", alpha=0.3)+
#   geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=yday(date_end)-4, xmax=yday(date_end)+3, ymin= - Inf,
#                                                 ymax= 2.25), color="transparent", 
#             fill="#D9EEF3", alpha=0.7)
#   # annotate(data = hw.l13,  inherit.aes=FALSE, "segment", x = 204, xend = 210, y = 7, yend = 7, 
#   #          arrow = arrow(length = unit(0.1, "cm")), color = "black")



# Ensure `date_start` and `date_end` in `hw.l13` are in Date format
hw.l13$date_start <- as.Date(hw.l13$date_start)
hw.l13$date_end <- as.Date(hw.l13$date_end)

# Define the rectangle x-range for the linear regression fit
rect_xmin <- yday(min(hw.l13$date_end) - 4)
rect_xmax <- 204

# Filter data points within the rectangle's range
l13_rect_data <- l13 %>%
  filter(doy >= rect_xmin & doy <= rect_xmax)

# Plot with the regression line
l13chl = ggplot(data = l13, aes(x = doy, y = manual_chl))+
  theme_classic()+
  labs(y = "Chlorophyll (μg/L)", x = "")+
  xlim(190, 210)+
  ylim(1, 4.5)+
  geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
                                                ymax= Inf), color="transparent", 
            fill="orange", alpha=0.3)+
  geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=yday(date_end)-4, xmax=yday(date_end)+3, ymin= - Inf,
                                                ymax= Inf), color="transparent", 
            fill="#D9EEF3", alpha=0.7)+
  geom_point(size = 1.2)+
  geom_line(color = "forestgreen", size = 1)+
  geom_smooth(data = l13_rect_data, aes(x = doy, y = manual_chl), method = "lm", color = "black", linetype = "solid", se = FALSE)+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))




l13percent_change = ggplot(data = l13, aes( x = doy, y = percent_change))+
  geom_line(size = 0.8, color = "black")+
  theme_classic()+
  geom_point(size = 1)+
  xlim(190, 210)+
  labs(y = "% change in chlorophyll", x = "day of year")+
  # geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
  #                                               ymax= Inf), color="transparent", 
  #           fill="orange", alpha=0.3)+
  theme(legend.position="none")+
  # geom_vline(data=hw.l13, inherit.aes=FALSE, aes(xintercept = yday(date_start)), linetype = "dashed")+
  # geom_vline(data=hw.l13, inherit.aes=FALSE, aes(xintercept = yday(date_end)), linetype = "dashed")+
  # geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=(start_analysis), xmax=(end_analysis), ymin= - Inf,
  #                                               ymax= Inf), color="transparent", 
  #           fill="magenta", alpha=0.3)+
  theme(legend.position="none")+
  # geom_vline(data=hw.l13, inherit.aes=FALSE, aes(xintercept = (start_analysis)), linetype = "dashed")+
  # geom_vline(data=hw.l13, inherit.aes=FALSE, aes(xintercept = (end_analysis)), linetype = "dashed")+
  ylim(-100, 250)+
  annotate("point", x = 204, y = 	 144.5193669, fill = "#D9EEF3", size = 5, shape = 21)+
  geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
                                                ymax= Inf), color="transparent", 
            fill="orange", alpha=0.3)+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))


# l13temp = ggplot(data = l13, aes(x = doy, y = mean_temp))+
#   geom_line(color = "steelblue", size = 1)+
#   theme_classic()+
#   labs(title = "Paul 2013")+
#   annotate("rect", xmin = as.Date(196), xmax = as.Date(201), ymin = -Inf, ymax = Inf,
#            fill = "red", alpha = 0.5)


# Peter
r13slope = ggplot(data = r13, aes( x = doy, y = chl_slope))+
  geom_line(color = "black", size = 0.8)+
  theme_classic()+
  geom_point(size = 1)+
  ylim(-1, 1.8)+
  xlim(190, 210)+
  labs(y = "", x = "")+
  # geom_point(data=hw.r13, inherit.aes=FALSE, 
  #            aes(x = yday(date_end), y = 1),  # Map x to yday(date_end) dynamically
  #            color="#60BFCC", size=3)
  annotate("point", x = 204, y = 	 0.033689674, fill = "#60BFCC", size = 5, shape = 21)+
  geom_rect(data=hw.r13, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
                                                ymax= Inf), color="transparent", 
            fill="orange", alpha=0.3)+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))


  # xlim(140, 225)+  geom_rect(data=hw.r13, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
  #                                                                ymax= Inf), color="transparent", 
  #                            fill="orange", alpha=0.3)

# r13chl = ggplot(data = r13, aes(x = doy, y = manual_chl))+
#   geom_line(color = "forestgreen", size = 0.8)+
#   geom_point(size = 1)+
#   theme_classic()+
#   labs(y = "", x = "")+
#   xlim(190, 210)+
#   ylim(1, 5.5)+
#   geom_rect(data=hw.r13, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
#                                                                 ymax= Inf), color="transparent", 
#                             fill="orange", alpha=0.3)+
#   geom_rect(data=hw.r13, inherit.aes=FALSE, aes(xmin=yday(date_end)-4, xmax=yday(date_end)+3, ymin= - Inf,
#                                                 ymax= 2.75), color="transparent", 
#             fill="#60BFCC", alpha=0.7)
# 


# Ensure `date_start` and `date_end` in `hw.r13` are in Date format
hw.r13$date_start <- as.Date(hw.r13$date_start)
hw.r13$date_end <- as.Date(hw.r13$date_end)


# Define the rectangle x-range for the linear regression fit
rect_xmin <- yday(min(hw.r13$date_end) - 4)
rect_xmax <- 204

# Filter data points within the rectangle's range
r13_rect_data <- r13 %>%
  filter(doy >= rect_xmin & doy <= rect_xmax)

# Plot with the regression line
r13chl = ggplot(data = r13, aes(x = doy, y = manual_chl))+
  theme_classic()+
  labs(y = "", x = "")+
  xlim(190, 210)+
  ylim(1, 5.5)+
  geom_rect(data=hw.r13, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
                                                ymax= Inf), color="transparent", 
            fill="orange", alpha=0.3)+
  geom_rect(data=hw.r13, inherit.aes=FALSE, aes(xmin=yday(date_end)-4, xmax=yday(date_end)+3, ymin= - Inf,
                                                ymax= Inf), color="transparent", 
            fill="#60BFCC", alpha=0.7)+
  geom_point(size = 1.2)+
  geom_line(color = "forestgreen", size = 1)+
  geom_smooth(data = r13_rect_data, aes(x = doy, y = manual_chl), method = "lm", color = "black", linetype = "solid", se = FALSE, linewidth = 1)+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))
  
  



r13percent_change = ggplot(data = r13, aes( x = doy, y = percent_change))+
  geom_line(size = 0.8, color = "black")+
  theme_classic()+
  geom_point(size = 1)+
  xlim(190, 210)+
  labs(y = "", x = "day of year")+
  # geom_rect(data=hw.r13, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
  #                                               ymax= Inf), color="transparent", 
  #           fill="orange", alpha=0.3)+
  theme(legend.position="none")+
  # geom_vline(data=hw.r13, inherit.aes=FALSE, aes(xintercept = yday(date_start)), linetype = "dashed")+
  # geom_vline(data=hw.r13, inherit.aes=FALSE, aes(xintercept = yday(date_end)), linetype = "dashed")+
  # geom_rect(data=hw.r13, inherit.aes=FALSE, aes(xmin=(start_analysis), xmax=(end_analysis), ymin= - Inf,
  #                                               ymax= Inf), color="transparent", 
  #           fill="magenta", alpha=0.3)+
  theme(legend.position="none")+
  # geom_vline(data=hw.r13, inherit.aes=FALSE, aes(xintercept = (start_analysis)), linetype = "dashed")+
  # geom_vline(data=hw.r13, inherit.aes=FALSE, aes(xintercept = (end_analysis)), linetype = "dashed")+
  ylim(-100, 250)+
  annotate("point", x = 204, y = 	 7.701665, fill = "#60BFCC", size = 5, shape = 21)+
  geom_rect(data=hw.r13, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
                                                ymax= Inf), color="transparent", 
            fill="orange", alpha=0.3)+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))


# r13temp = ggplot(data = r13, aes(x = doy, y = mean_temp))+
#   geom_line(color = "steelblue", size = 1)+
#   theme_classic()+
#   labs(title = "Peter 2013")+
#   annotate("rect", xmin = as.Date(196), xmax = as.Date(201), ymin = -Inf, ymax = Inf,
#            fill = "red", alpha = 0.5)


png("./figures/manuscript draft 2024-11-11/methods figure 2024-11-11.png", height = 8, width = 8, units = "in", res = 300)
print(ggarrange(nrow = 4, ncol = 3, l13temp, r13temp, t13temp, l13chl, r13chl,  t13chl, l13slope, r13slope, t13slope, l13percent_change, r13percent_change,  t13percent_change))
dev.off()









###### 2013 supplemental figure ##########
t13 = slopes %>%  filter(year == 2013, lake == "T")
l13 = slopes %>%  filter(year == 2013, lake == "L")
r13 = slopes %>%  filter(year == 2013, lake == "R")

# make a couple version of the datasets merged
#merged_data <- merge(hw.r13, r13, by = "doy")


# make temp plots
t13temp = event_line_DKS2("tuesday", 2013)+
  labs(title = "Tuesday 2013", y = "", x = "")+
  theme(legend.position="none")+
  xlim(147, 244)+
  geom_rect(data=hw.t13, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
                                                ymax= Inf), color="transparent", 
            fill="orange", alpha=0.3)+
  #geom_point()+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))


l13temp = event_line_DKS2("paul", 2013)+
  labs(title = "Paul 2013", x = "")+
  theme(legend.position="none")+
  xlim(147, 244)+
  geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
                                                ymax= Inf), color="transparent", 
            fill="orange", alpha=0.3)+
  #geom_point()+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))

r13temp = event_line_DKS2("peter", 2013)+
  labs(title = "Peter 2013", y = "", x = "")+
  theme(legend.position="none")+
  xlim(147, 244)+
  geom_rect(data=hw.r13, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
                                                ymax= Inf), color="transparent", 
            fill="orange", alpha=0.3)+
 # geom_point()+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))

# Tuesday
t13slope = ggplot(data = t13, aes( x = doy, y = chl_slope))+
  geom_line(color = "black", size = 0.8)+
  theme_classic()+
  geom_point(size = 1)+
  #ylim(-1, 1.8)+
  labs(y = "", x = "")+
  xlim(147, 244)+
  annotate("point", x = 192, y = 	1.028482, fill = "#544C34", size = 4, shape = 21)+
  annotate("point", x = 204, y = 	0.8988908, fill = "#544C34", size = 4, shape = 21)+
  geom_rect(data=hw.t13, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
                                                ymax= Inf), color="transparent", 
            fill="orange", alpha=0.3)+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))
# geom_rect(data=hw.t13, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
#                                               ymax= Inf), color="transparent", 
#           fill="orange", alpha=0.3)


t13chl = ggplot(data = t13, aes(x = doy, y = manual_chl))+
  geom_line(color = "forestgreen", size = 0.8)+
  theme_classic()+
  geom_point(size = 1)+
  xlim(147, 244)+
  #ylim(1, 25)+
  labs(y = "", x = "")+
  geom_rect(data=hw.t13, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
                                                ymax= Inf), color="transparent", 
            fill="orange", alpha=0.3)+
  geom_rect(data=hw.t13, inherit.aes=FALSE, aes(xmin=yday(date_end)-5, xmax=yday(date_end)+2, ymin= - Inf,
                                                ymax= Inf), color="transparent", 
            fill="#544C34", alpha=0.7)+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))



# Ensure `date_start` and `date_end` in `hw.t13` are in Date format
hw.t13$date_start <- as.Date(hw.t13$date_start)
hw.t13$date_end <- as.Date(hw.t13$date_end)


# Define the rectangle x-range for the linear regression fit
rect_xmin <- 197
rect_xmax <- 205

# Filter data points within the rectangle's range
t13_rect_data <- t13 %>%
  filter(doy >= rect_xmin & doy <= rect_xmax)

# Define the rectangle x-range for the linear regression fit
rect_xmin2 <- 185
rect_xmax2 <- 193

# Filter data points within the rectangle's range
t13_rect_data2 <- t13 %>%
  filter(doy >= rect_xmin2 & doy <= rect_xmax2)

# Plot with the regression line
t13chl = ggplot(data = t13, aes(x = doy, y = manual_chl))+
  theme_classic()+
  labs(y = "", x = "")+
  xlim(147, 244)+
  #ylim(1, 25)+
  geom_rect(data=hw.t13, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
                                                ymax= Inf), color="transparent", 
            fill="orange", alpha=0.3)+
  geom_rect(data=hw.t13, inherit.aes=FALSE, aes(xmin=yday(date_end)-4, xmax=yday(date_end)+3, ymin= - Inf,
                                                ymax= Inf), color="transparent", 
            fill="#544C34", alpha=0.35)+
  geom_point(size = 1.2)+
  geom_line(color = "forestgreen", size = 1)+
  geom_smooth(data = t13_rect_data, aes(x = doy, y = manual_chl), method = "lm", color = "black", linetype = "solid", se = FALSE, linewidth = 1)+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))+
  geom_smooth(data = t13_rect_data2, aes(x = doy, y = manual_chl), method = "lm", color = "black", linetype = "solid", se = FALSE, linewidth = 1)+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))





t13percent_change = ggplot(data = t13, aes( x = doy, y = percent_change))+
  geom_line(size = 0.8, color = "black")+
  theme_classic()+
  geom_point(size = 1)+
  xlim(147, 244)+
  labs(y = "", x = "day of year")+
  # geom_rect(data=hw.t13, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
  #                                               ymax= Inf), color="transparent", 
  #           fill="orange", alpha=0.3)+
  theme(legend.position="none")+
  # geom_vline(data=hw.t13, inherit.aes=FALSE, aes(xintercept = yday(date_start)), linetype = "dashed")+
  # geom_vline(data=hw.t13, inherit.aes=FALSE, aes(xintercept = yday(date_end)), linetype = "dashed")+
  # geom_rect(data=hw.t13, inherit.aes=FALSE, aes(xmin=(start_analysis), xmax=(end_analysis), ymin= - Inf,
  #                                               ymax= Inf), color="transparent", 
  #           fill="magenta", alpha=0.3)+
  theme(legend.position="none")+
  # geom_vline(data=hw.t13, inherit.aes=FALSE, aes(xintercept = (start_analysis)), linetype = "dashed")+
  # geom_vline(data=hw.t13, inherit.aes=FALSE, aes(xintercept = (end_analysis)), linetype = "dashed")+
  #ylim(-100, 250)+
  annotate("point", x = 192, y = 	189.91973, fill = "#544C34", size = 4, shape = 21)+
  annotate("point", x = 204, y = 	110.7496, fill = "#544C34", size = 4, shape = 21)+
  geom_rect(data=hw.t13, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
                                                ymax= Inf), color="transparent", 
            fill="orange", alpha=0.3)+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))


# t13temp = ggplot(data = t13, aes(x = doy, y = mean_temp))+
#   geom_line(color = "steelblue", size = 1)+
#   theme_classic()+
#   labs(title = "Tuesday 2013")+
#   annotate("rect", xmin = as.Date(186), xmax = as.Date(190), ymin = -Inf, ymax = Inf,
#            fill = "red", alpha = 0.5)+
#   annotate("rect", xmin = as.Date(196), xmax = as.Date(201), ymin = -Inf, ymax = Inf,
#            fill = "red", alpha = 0.5)


# Paul
l13slope = ggplot(data = l13, aes( x = doy, y = chl_slope))+
  geom_line(color = "black", size = 0.8)+
  theme_classic()+
  geom_point(size = 1)+
  #ylim(-1, 1.8)+
  labs(y = "Chlorophyll slope (μg/L/day)", x = "")+
  xlim(147, 244)+
  annotate("point", x = 204, y = 	0.262535019, fill = "#D9EEF3", size = 4, shape = 21)+
  geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
                                                ymax= Inf), color="transparent", 
            fill="orange", alpha=0.3)+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 11))
# geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
#                                               ymax= Inf), color="transparent", 
#           fill="orange", alpha=0.3)

# l13chl = ggplot(data = l13, aes(x = doy, y = manual_chl))+
#   geom_line(color = "forestgreen", size = 0.8)+
#   theme_classic()+
#   geom_point(size = 1)+
#   labs(y = "Chlorophyll (μg/L)", x = "")+
#   xlim(147, 244)+
#   ylim(1, 4.5)+
#   geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
#                                                 ymax= Inf), color="transparent", 
#             fill="orange", alpha=0.3)+
#   geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=yday(date_end)-4, xmax=yday(date_end)+3, ymin= - Inf,
#                                                 ymax= 2.25), color="transparent", 
#             fill="#D9EEF3", alpha=0.7)
#   # annotate(data = hw.l13,  inherit.aes=FALSE, "segment", x = 204, xend = 210, y = 7, yend = 7, 
#   #          arrow = arrow(length = unit(0.1, "cm")), color = "black")



# Ensure `date_start` and `date_end` in `hw.l13` are in Date format
hw.l13$date_start <- as.Date(hw.l13$date_start)
hw.l13$date_end <- as.Date(hw.l13$date_end)

# Define the rectangle x-range for the linear regression fit
rect_xmin <- yday(min(hw.l13$date_end) - 4)
rect_xmax <- 204

# Filter data points within the rectangle's range
l13_rect_data <- l13 %>%
  filter(doy >= rect_xmin & doy <= rect_xmax)

# Plot with the regression line
l13chl = ggplot(data = l13, aes(x = doy, y = manual_chl))+
  theme_classic()+
  labs(y = "Chlorophyll (μg/L)", x = "")+
  xlim(147, 244)+
  #ylim(1, 4.5)+
  geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
                                                ymax= Inf), color="transparent", 
            fill="orange", alpha=0.3)+
  geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=yday(date_end)-4, xmax=yday(date_end)+3, ymin= - Inf,
                                                ymax= Inf), color="transparent", 
            fill="#D9EEF3", alpha=0.7)+
  geom_point(size = 1.2)+
  geom_line(color = "forestgreen", size = 1)+
  geom_smooth(data = l13_rect_data, aes(x = doy, y = manual_chl), method = "lm", color = "black", linetype = "solid", se = FALSE)+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))




l13percent_change = ggplot(data = l13, aes( x = doy, y = percent_change))+
  geom_line(size = 0.8, color = "black")+
  theme_classic()+
  geom_point(size = 1)+
  xlim(147, 244)+
  labs(y = "% change in chlorophyll", x = "day of year")+
  # geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
  #                                               ymax= Inf), color="transparent", 
  #           fill="orange", alpha=0.3)+
  theme(legend.position="none")+
  # geom_vline(data=hw.l13, inherit.aes=FALSE, aes(xintercept = yday(date_start)), linetype = "dashed")+
  # geom_vline(data=hw.l13, inherit.aes=FALSE, aes(xintercept = yday(date_end)), linetype = "dashed")+
  # geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=(start_analysis), xmax=(end_analysis), ymin= - Inf,
  #                                               ymax= Inf), color="transparent", 
  #           fill="magenta", alpha=0.3)+
  theme(legend.position="none")+
  # geom_vline(data=hw.l13, inherit.aes=FALSE, aes(xintercept = (start_analysis)), linetype = "dashed")+
  # geom_vline(data=hw.l13, inherit.aes=FALSE, aes(xintercept = (end_analysis)), linetype = "dashed")+
  #ylim(-100, 250)+
  annotate("point", x = 204, y = 	 147.5193669, fill = "#D9EEF3", size = 4, shape = 21)+
  geom_rect(data=hw.l13, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
                                                ymax= Inf), color="transparent", 
            fill="orange", alpha=0.3)+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))


# l13temp = ggplot(data = l13, aes(x = doy, y = mean_temp))+
#   geom_line(color = "steelblue", size = 1)+
#   theme_classic()+
#   labs(title = "Paul 2013")+
#   annotate("rect", xmin = as.Date(196), xmax = as.Date(201), ymin = -Inf, ymax = Inf,
#            fill = "red", alpha = 0.5)


# Peter
r13slope = ggplot(data = r13, aes( x = doy, y = chl_slope))+
  geom_line(color = "black", size = 0.8)+
  theme_classic()+
  geom_point(size = 1)+
  #ylim(-1, 1.8)+
  xlim(147, 244)+
  labs(y = "", x = "")+
  # geom_point(data=hw.r13, inherit.aes=FALSE, 
  #            aes(x = yday(date_end), y = 1),  # Map x to yday(date_end) dynamically
  #            color="#60BFCC", size=3)
  annotate("point", x = 204, y = 	 0.033689674, fill = "#60BFCC", size = 4, shape = 21)+
  geom_rect(data=hw.r13, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
                                                ymax= Inf), color="transparent", 
            fill="orange", alpha=0.3)+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))


# xlim(140, 225)+  geom_rect(data=hw.r13, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
#                                                                ymax= Inf), color="transparent", 
#                            fill="orange", alpha=0.3)

# r13chl = ggplot(data = r13, aes(x = doy, y = manual_chl))+
#   geom_line(color = "forestgreen", size = 0.8)+
#   geom_point(size = 1)+
#   theme_classic()+
#   labs(y = "", x = "")+
#   xlim(147, 244)+
#   ylim(1, 5.5)+
#   geom_rect(data=hw.r13, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
#                                                                 ymax= Inf), color="transparent", 
#                             fill="orange", alpha=0.3)+
#   geom_rect(data=hw.r13, inherit.aes=FALSE, aes(xmin=yday(date_end)-4, xmax=yday(date_end)+3, ymin= - Inf,
#                                                 ymax= 2.75), color="transparent", 
#             fill="#60BFCC", alpha=0.7)
# 


# Ensure `date_start` and `date_end` in `hw.r13` are in Date format
hw.r13$date_start <- as.Date(hw.r13$date_start)
hw.r13$date_end <- as.Date(hw.r13$date_end)


# Define the rectangle x-range for the linear regression fit
rect_xmin <- yday(min(hw.r13$date_end) - 4)
rect_xmax <- 204

# Filter data points within the rectangle's range
r13_rect_data <- r13 %>%
  filter(doy >= rect_xmin & doy <= rect_xmax)

# Plot with the regression line
r13chl = ggplot(data = r13, aes(x = doy, y = manual_chl))+
  theme_classic()+
  labs(y = "", x = "")+
  xlim(147, 244)+
  #ylim(1, 5.5)+
  geom_rect(data=hw.r13, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
                                                ymax= Inf), color="transparent", 
            fill="orange", alpha=0.3)+
  geom_rect(data=hw.r13, inherit.aes=FALSE, aes(xmin=yday(date_end)-4, xmax=yday(date_end)+3, ymin= - Inf,
                                                ymax= Inf), color="transparent", 
            fill="#60BFCC", alpha=0.7)+
  geom_point(size = 1.2)+
  geom_line(color = "forestgreen", size = 1)+
  geom_smooth(data = r13_rect_data, aes(x = doy, y = manual_chl), method = "lm", color = "black", linetype = "solid", se = FALSE, linewidth = 1)+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))





r13percent_change = ggplot(data = r13, aes( x = doy, y = percent_change))+
  geom_line(size = 0.8, color = "black")+
  theme_classic()+
  geom_point(size = 1)+
  xlim(147, 244)+
  labs(y = "", x = "day of year")+
  # geom_rect(data=hw.r13, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
  #                                               ymax= Inf), color="transparent", 
  #           fill="orange", alpha=0.3)+
  theme(legend.position="none")+
  # geom_vline(data=hw.r13, inherit.aes=FALSE, aes(xintercept = yday(date_start)), linetype = "dashed")+
  # geom_vline(data=hw.r13, inherit.aes=FALSE, aes(xintercept = yday(date_end)), linetype = "dashed")+
  # geom_rect(data=hw.r13, inherit.aes=FALSE, aes(xmin=(start_analysis), xmax=(end_analysis), ymin= - Inf,
  #                                               ymax= Inf), color="transparent", 
  #           fill="magenta", alpha=0.3)+
  theme(legend.position="none")+
  # geom_vline(data=hw.r13, inherit.aes=FALSE, aes(xintercept = (start_analysis)), linetype = "dashed")+
  # geom_vline(data=hw.r13, inherit.aes=FALSE, aes(xintercept = (end_analysis)), linetype = "dashed")+
 # ylim(-100, 250)+
  annotate("point", x = 204, y = 	 7.701665, fill = "#60BFCC", size = 4, shape = 21)+
  geom_rect(data=hw.r13, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
                                                ymax= Inf), color="transparent", 
            fill="orange", alpha=0.3)+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))


# r13temp = ggplot(data = r13, aes(x = doy, y = mean_temp))+
#   geom_line(color = "steelblue", size = 1)+
#   theme_classic()+
#   labs(title = "Peter 2013")+
#   annotate("rect", xmin = as.Date(196), xmax = as.Date(201), ymin = -Inf, ymax = Inf,
#            fill = "red", alpha = 0.5)


png("./figures/manuscript draft 2024-11-11/methods figure 2024-11-11 S4 2013.png", height = 8, width = 11, units = "in", res = 300)
print(ggarrange(nrow = 4, ncol = 3, l13temp, r13temp, t13temp, l13chl, r13chl,  t13chl, l13slope, r13slope, t13slope, l13percent_change, r13percent_change,  t13percent_change))
dev.off()








###### 2010 supplemental figure ##########

# create separate dataframes for each heatwave
hw.l10 = heatwaves %>% filter(lake == "L" & year == 2010)
hw.r10 = heatwaves %>% filter(lake == "R" & year == 2010)

## THIS DEPENDS ON THE MOST RECENT VERSION OF ALLSLOPES
slopes = allSlopes %>% filter(daysAfter == 2 & lake == "T" | daysAfter == 3 & lake == "R" | daysAfter == 3 & lake == "L")
slopes = slopes %>% dplyr::rename(doy = doyCat)


l10 = slopes %>%  filter(year == 2010, lake == "L")
r10 = slopes %>%  filter(year == 2010, lake == "R")

# make a couple version of the datasets merged
#merged_data <- merge(hw.r10, r10, by = "doy")


# make temp plots

l10temp = event_line_DKS2("paul", 2010)+
  labs(title = "Paul 2010", x = "")+
  theme(legend.position="none")+
  geom_rect(data=hw.l10, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
                                                ymax= Inf), color="transparent", 
            fill="orange", alpha=0.3)+
  #geom_point()+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))

r10temp = event_line_DKS2("peter", 2010)+
  labs(title = "Peter 2010", y = "", x = "")+
  theme(legend.position="none")+
  geom_rect(data=hw.r10, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
                                                ymax= Inf), color="transparent", 
            fill="orange", alpha=0.3)+
  # geom_point()+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))

# Paul
l10slope = ggplot(data = l10, aes( x = doy, y = chl_slope))+
  geom_line(color = "black", size = 0.8)+
  theme_classic()+
  geom_point(size = 1)+
  #ylim(-1, 1.8)+
  labs(y = "Chlorophyll slope (μg/L/day)", x = "")+
  annotate("point", x = 157, y = 	0.186260595, fill = "#D9EEF3", size = 4, shape = 21)+
  annotate("point", x = 230, y = 	0.256153333, fill = "#D9EEF3", size = 4, shape = 21)+
  geom_rect(data=hw.l10, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
                                                ymax= Inf), color="transparent", 
            fill="orange", alpha=0.3)+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 11))
# geom_rect(data=hw.l10, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
#                                               ymax= Inf), color="transparent", 
#           fill="orange", alpha=0.3)

# l10chl = ggplot(data = l10, aes(x = doy, y = manual_chl))+
#   geom_line(color = "forestgreen", size = 0.8)+
#   theme_classic()+
#   geom_point(size = 1)+
#   labs(y = "Chlorophyll (μg/L)", x = "")+
#   xlim(147, 244)+
#   ylim(1, 4.5)+
#   geom_rect(data=hw.l10, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
#                                                 ymax= Inf), color="transparent", 
#             fill="orange", alpha=0.3)+
#   geom_rect(data=hw.l10, inherit.aes=FALSE, aes(xmin=yday(date_end)-4, xmax=yday(date_end)+3, ymin= - Inf,
#                                                 ymax= 2.25), color="transparent", 
#             fill="#D9EEF3", alpha=0.7)
#   # annotate(data = hw.l10,  inherit.aes=FALSE, "segment", x = 204, xend = 210, y = 7, yend = 7, 
#   #          arrow = arrow(length = unit(0.1, "cm")), color = "black")



# Ensure `date_start` and `date_end` in `hw.l10` are in Date format
hw.l10$date_start <- as.Date(hw.l10$date_start)
hw.l10$date_end <- as.Date(hw.l10$date_end)

# Define the rectangle x-range for the linear regression fit
rect_xmin <- 149
rect_xmax <- 157

# Filter data points within the rectangle's range
l10_rect_data <- l10 %>%
  filter(doy >= rect_xmin & doy <= rect_xmax)


rect_xmin2 <- 222
rect_xmax2 <- 230

# Filter data points within the rectangle's range
l10_rect_data2 <- l10 %>%
  filter(doy >= rect_xmin2 & doy <= rect_xmax2)

# Plot with the regression line
l10chl = ggplot(data = l10, aes(x = doy, y = manual_chl))+
  theme_classic()+
  labs(y = "Chlorophyll (μg/L)", x = "")+
  #ylim(1, 4.5)+
  geom_rect(data=hw.l10, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
                                                ymax= Inf), color="transparent", 
            fill="orange", alpha=0.3)+
  geom_rect(data=hw.l10, inherit.aes=FALSE, aes(xmin=yday(date_end)-4, xmax=yday(date_end)+3, ymin= - Inf,
                                                ymax= Inf), color="transparent", 
            fill="#D9EEF3", alpha=0.7)+
  geom_point(size = 1.2)+
  geom_line(color = "forestgreen", size = 1)+
  geom_smooth(data = l10_rect_data, aes(x = doy, y = manual_chl), method = "lm", color = "black", linetype = "solid", se = FALSE)+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))+
  geom_smooth(data = l10_rect_data2, aes(x = doy, y = manual_chl), method = "lm", color = "black", linetype = "solid", se = FALSE)
  




l10percent_change = ggplot(data = l10, aes( x = doy, y = percent_change))+
  geom_line(size = 0.8, color = "black")+
  theme_classic()+
  geom_point(size = 1)+
  labs(y = "% change in chlorophyll", x = "day of year")+
  # geom_rect(data=hw.l10, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
  #                                               ymax= Inf), color="transparent", 
  #           fill="orange", alpha=0.3)+
  theme(legend.position="none")+
  # geom_vline(data=hw.l10, inherit.aes=FALSE, aes(xintercept = yday(date_start)), linetype = "dashed")+
  # geom_vline(data=hw.l10, inherit.aes=FALSE, aes(xintercept = yday(date_end)), linetype = "dashed")+
  # geom_rect(data=hw.l10, inherit.aes=FALSE, aes(xmin=(start_analysis), xmax=(end_analysis), ymin= - Inf,
  #                                               ymax= Inf), color="transparent", 
  #           fill="magenta", alpha=0.3)+
  theme(legend.position="none")+
  # geom_vline(data=hw.l10, inherit.aes=FALSE, aes(xintercept = (start_analysis)), linetype = "dashed")+
  # geom_vline(data=hw.l10, inherit.aes=FALSE, aes(xintercept = (end_analysis)), linetype = "dashed")+
  #ylim(-100, 250)+
  annotate("point", x = 157, y = 	 46.0169716, fill = "#D9EEF3", size = 4, shape = 21)+
  annotate("point", x = 230, y = 	  47.9676662, fill = "#D9EEF3", size = 4, shape = 21)+
  geom_rect(data=hw.l10, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
                                                ymax= Inf), color="transparent", 
            fill="orange", alpha=0.3)+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))


# l10temp = ggplot(data = l10, aes(x = doy, y = mean_temp))+
#   geom_line(color = "steelblue", size = 1)+
#   theme_classic()+
#   labs(title = "Paul 2010")+
#   annotate("rect", xmin = as.Date(196), xmax = as.Date(201), ymin = -Inf, ymax = Inf,
#            fill = "red", alpha = 0.5)


# Peter
r10slope = ggplot(data = r10, aes( x = doy, y = chl_slope))+
  geom_line(color = "black", size = 0.8)+
  theme_classic()+
  geom_point(size = 1)+
  #ylim(-1, 1.8)+
  labs(y = "", x = "")+
  # geom_point(data=hw.r10, inherit.aes=FALSE, 
  #            aes(x = yday(date_end), y = 1),  # Map x to yday(date_end) dynamically
  #            color="#60BFCC", size=3)
  annotate("point", x = 230, y = 	 0.116787857, fill = "#60BFCC", size = 4, shape = 21)+
  annotate("point", x = 157, y = 	 0.51458250, fill = "#60BFCC", size = 4, shape = 21)+
  geom_rect(data=hw.r10, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
                                                ymax= Inf), color="transparent", 
            fill="orange", alpha=0.3)+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))


# xlim(140, 225)+  geom_rect(data=hw.r10, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
#                                                                ymax= Inf), color="transparent", 
#                            fill="orange", alpha=0.3)

# r10chl = ggplot(data = r10, aes(x = doy, y = manual_chl))+
#   geom_line(color = "forestgreen", size = 0.8)+
#   geom_point(size = 1)+
#   theme_classic()+
#   labs(y = "", x = "")+
#   xlim(147, 244)+
#   ylim(1, 5.5)+
#   geom_rect(data=hw.r10, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
#                                                                 ymax= Inf), color="transparent", 
#                             fill="orange", alpha=0.3)+
#   geom_rect(data=hw.r10, inherit.aes=FALSE, aes(xmin=yday(date_end)-4, xmax=yday(date_end)+3, ymin= - Inf,
#                                                 ymax= 2.75), color="transparent", 
#             fill="#60BFCC", alpha=0.7)
# 


# Ensure `date_start` and `date_end` in `hw.r10` are in Date format
hw.r10$date_start <- as.Date(hw.r10$date_start)
hw.r10$date_end <- as.Date(hw.r10$date_end)


# Define the rectangle x-range for the linear regression fit
rect_xmin <- 149
rect_xmax <- 157

# Filter data points within the rectangle's range
r10_rect_data <- r10 %>%
  filter(doy >= rect_xmin & doy <= rect_xmax)

# Define the rectangle x-range for the linear regression fit
rect_xmin2 <- 222
rect_xmax2 <- 230

# Filter data points within the rectangle's range
r10_rect_data2 <- r10 %>%
  filter(doy >= rect_xmin2 & doy <= rect_xmax2)

# Plot with the regression line
r10chl = ggplot(data = r10, aes(x = doy, y = manual_chl))+
  theme_classic()+
  labs(y = "", x = "")+
  #ylim(1, 5.5)+
  geom_rect(data=hw.r10, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
                                                ymax= Inf), color="transparent", 
            fill="orange", alpha=0.3)+
  geom_rect(data=hw.r10, inherit.aes=FALSE, aes(xmin=yday(date_end)-4, xmax=yday(date_end)+3, ymin= - Inf,
                                                ymax= Inf), color="transparent", 
            fill="#60BFCC", alpha=0.7)+
  geom_point(size = 1.2)+
  geom_line(color = "forestgreen", size = 1)+
  geom_smooth(data = r10_rect_data, aes(x = doy, y = manual_chl), method = "lm", color = "black", linetype = "solid", se = FALSE, linewidth = 1)+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))+
  geom_smooth(data = r10_rect_data2, aes(x = doy, y = manual_chl), method = "lm", color = "black", linetype = "solid", se = FALSE, linewidth = 1)
  





r10percent_change = ggplot(data = r10, aes( x = doy, y = percent_change))+
  geom_line(size = 0.8, color = "black")+
  theme_classic()+
  geom_point(size = 1)+
  labs(y = "", x = "day of year")+
  # geom_rect(data=hw.r10, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
  #                                               ymax= Inf), color="transparent", 
  #           fill="orange", alpha=0.3)+
  theme(legend.position="none")+
  # geom_vline(data=hw.r10, inherit.aes=FALSE, aes(xintercept = yday(date_start)), linetype = "dashed")+
  # geom_vline(data=hw.r10, inherit.aes=FALSE, aes(xintercept = yday(date_end)), linetype = "dashed")+
  # geom_rect(data=hw.r10, inherit.aes=FALSE, aes(xmin=(start_analysis), xmax=(end_analysis), ymin= - Inf,
  #                                               ymax= Inf), color="transparent", 
  #           fill="magenta", alpha=0.3)+
  theme(legend.position="none")+
  # geom_vline(data=hw.r10, inherit.aes=FALSE, aes(xintercept = (start_analysis)), linetype = "dashed")+
  # geom_vline(data=hw.r10, inherit.aes=FALSE, aes(xintercept = (end_analysis)), linetype = "dashed")+
  # ylim(-100, 250)+
  annotate("point", x = 230, y = 	 20.93080193, fill = "#60BFCC", size = 4, shape = 21)+
  annotate("point", x = 157, y = 	 150.67364037, fill = "#60BFCC", size = 4, shape = 21)+
  geom_rect(data=hw.r10, inherit.aes=FALSE, aes(xmin=yday(date_start), xmax=yday(date_end), ymin= - Inf,
                                                ymax= Inf), color="transparent", 
            fill="orange", alpha=0.3)+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))


# r10temp = ggplot(data = r10, aes(x = doy, y = mean_temp))+
#   geom_line(color = "steelblue", size = 1)+
#   theme_classic()+
#   labs(title = "Peter 2010")+
#   annotate("rect", xmin = as.Date(196), xmax = as.Date(201), ymin = -Inf, ymax = Inf,
#            fill = "red", alpha = 0.5)


png("./figures/manuscript draft 2024-11-11/methods figure 2024-11-11 S4 2010.png", height = 8, width = 7.5, units = "in", res = 300)
print(ggarrange(nrow = 4, ncol = 2, l10temp, r10temp, l10chl, r10chl, l10slope, r10slope, l10percent_change, r10percent_change))
dev.off()





## Tuesday days after is 2
## Peter and Paul is 3
