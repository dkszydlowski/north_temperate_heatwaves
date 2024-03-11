# make a plot of the slopes over time and the raw chl data
# updated for event_line plots

library(dplyr)
library(ggpubr)

# slopes = read.csv("./formatted data/slopes.csv")
# slopes = read.csv("./formatted data/slopes_3day.csv")

# slopes = slopes %>% dplyr::rename(doy = doyCat)

# pdf("./figures/Heatwave raw data plots/temp_chl_heatwaves_slopes_7day.pdf", onefile = TRUE)

# set the number of days after heatwave and number of days to include
#daysAfter = 1 # 3 days after heatwave
#numSlopes = 5 # 5-day numSlopes

makePDFrawPlots <- function(allSlopes, daysAfter, numSlopes, metadata_plot, runNumber){
  
  curDate = cur_date_time = format(Sys.Date(), "%Y_%m_%d")
  pdfName = paste(runNumber, "_TIME_SERIES_date_", curDate, "_slopeLength_", slopeLength, "_daysAfter_", daysAfter, "_numSlopes_", numSlopes, ".pdf", sep = "")
  
  
  pdf(paste("./figures/sensitivity tests/" , pdfName, sep = ""), height = 6, width = 8)
  
  print(metadata_plot)
  
  slopes = allSlopes
  slopes = slopes %>% dplyr::rename(doy = doyCat)
  
  
  # save to a pdf with a similar name to the data outputs
  
  ##### 2008 ####
  t08 = slopes %>%  filter(year == 2008, lake == "T")
  l08 = slopes %>%  filter(year == 2008, lake == "L")
  r08 = slopes %>%  filter(year == 2008, lake == "R")
  
  # Paul
  l08slope = ggplot(data = l08, aes( x = doy, y = chl_slope))+
    geom_line(color = "black", size = 1)+
    theme_classic()+
    ylim(-4.1, 4.5)
  
  l08chl = ggplot(data = l08, aes(x = doy, y = manual_chl))+
    geom_line(color = "forestgreen", size = 1)+
    theme_classic()
  
  l08temp = ggplot(data = l08, aes(x = doy, y = mean_temp))+
    geom_line(color = "steelblue", size = 1)+
    theme_classic()+
    labs(title = "Paul 2008")
  
  # Peter
  r08slope = ggplot(data = r08, aes( x = doy, y = chl_slope))+
    geom_line(color = "black", size = 1)+
    theme_classic()+
    ylim(-4.1, 4.5)
  
  r08chl = ggplot(data = r08, aes(x = doy, y = manual_chl))+
    geom_line(color = "forestgreen", size = 1)+
    theme_classic()
  
  r08temp = ggplot(data = r08, aes(x = doy, y = mean_temp))+
    geom_line(color = "steelblue", size = 1)+
    theme_classic()+
    labs(title = "Peter 2008")
  
  print(ggarrange(nrow = 3, ncol = 2, r08temp, l08temp, r08chl, l08chl, r08slope, l08slope))
  
  
  
  ##### 2009 ####
  l09 = slopes %>%  filter(year == 2009, lake == "L")
  r09 = slopes %>%  filter(year == 2009, lake == "R")
  
  # Paul
  l09slope = ggplot(data = l09, aes( x = doy, y = chl_slope))+
    geom_line(color = "black", size = 1)+
    theme_classic()+
    ylim(-4.7, 5.5)+
    annotate("rect", xmin = as.Date(171), xmax = as.Date(178), ymin = -Inf, ymax = Inf,
             fill = "red", alpha = 0.5)+
    annotate("rect", xmin = as.Date(178+daysAfter - slopeLength), xmax = as.Date(178+daysAfter+numSlopes), ymin = -Inf, ymax = Inf,
             fill = "steelblue", alpha = 0.5)
  
  l09chl = ggplot(data = l09, aes(x = doy, y = manual_chl))+
    geom_line(color = "forestgreen", size = 1)+
    theme_classic()
  
  l09temp = ggplot(data = l09, aes(x = doy, y = mean_temp))+
    geom_line(color = "steelblue", size = 1)+
    theme_classic()+
    labs(title = "Paul 2009")+
    annotate("rect", xmin = as.Date(171), xmax = as.Date(178), ymin = -Inf, ymax = Inf,
             fill = "red", alpha = 0.5)
  
  
  # Peter
  r09slope = ggplot(data = r09, aes( x = doy, y = chl_slope))+
    geom_line(color = "black", size = 1)+
    theme_classic()+
    ylim(-4.7, 5.5)+
    annotate("rect", xmin = as.Date(171), xmax = as.Date(178), ymin = -Inf, ymax = Inf,
             fill = "red", alpha = 0.5)+
    annotate("rect", xmin = as.Date(178+daysAfter - slopeLength), xmax = as.Date(178+daysAfter+numSlopes), ymin = -Inf, ymax = Inf,
             fill = "steelblue", alpha = 0.5)
  
  r09chl = ggplot(data = r09, aes(x = doy, y = manual_chl))+
    geom_line(color = "forestgreen", size = 1)+
    theme_classic()
  
  
  r09temp = ggplot(data = r09, aes(x = doy, y = mean_temp))+
    geom_line(color = "steelblue", size = 1)+
    theme_classic()+
    labs(title = "Peter 2009")+
    annotate("rect", xmin = as.Date(171), xmax = as.Date(178), ymin = -Inf, ymax = Inf,
             fill = "red", alpha = 0.5)
  
  print(ggarrange(nrow = 3, ncol = 2, r09temp, l09temp, r09chl, l09chl, r09slope, l09slope))
  
  
  ##### 2010 ####
  t10 = slopes %>%  filter(year == 2010, lake == "T")
  l10 = slopes %>%  filter(year == 2010, lake == "L")
  r10 = slopes %>%  filter(year == 2010, lake == "R")
  
  # Paul
  l10slope = ggplot(data = l10, aes( x = doy, y = chl_slope))+
    geom_line(color = "black", size = 1)+
    theme_classic()+
    ylim(-4.7, 5.5)+
    annotate("rect", xmin = as.Date(144), xmax = as.Date(154), ymin = -Inf, ymax = Inf,
             fill = "red", alpha = 0.5)+
    annotate("rect", xmin = as.Date(221), xmax = as.Date(227), ymin = -Inf, ymax = Inf,
             fill = "red", alpha = 0.5)+
    annotate("rect", xmin = as.Date(154+daysAfter - slopeLength), xmax = as.Date(154+daysAfter+numSlopes), ymin = -Inf, ymax = Inf,
             fill = "steelblue", alpha = 0.5)+
    annotate("rect", xmin = as.Date(227+daysAfter - slopeLength), xmax = as.Date(227+daysAfter+numSlopes), ymin = -Inf, ymax = Inf,
             fill = "steelblue", alpha = 0.5)
  
  l10chl = ggplot(data = l10, aes(x = doy, y = manual_chl))+
    geom_line(color = "forestgreen", size = 1)+
    theme_classic()
  
  l10temp = ggplot(data = l10, aes(x = doy, y = mean_temp))+
    geom_line(color = "steelblue", size = 1)+
    theme_classic()+
    labs(title = "Paul 2010")+
    annotate("rect", xmin = as.Date(144), xmax = as.Date(154), ymin = -Inf, ymax = Inf,
             fill = "red", alpha = 0.5)+
    annotate("rect", xmin = as.Date(221), xmax = as.Date(227), ymin = -Inf, ymax = Inf,
             fill = "red", alpha = 0.5)
  
  
  # Peter
  r10slope = ggplot(data = r10, aes( x = doy, y = chl_slope))+
    geom_line(color = "black", size = 1)+
    theme_classic()+
    ylim(-4.7, 5.5)+
    annotate("rect", xmin = as.Date(144), xmax = as.Date(154), ymin = -Inf, ymax = Inf,
             fill = "red", alpha = 0.5)+
    annotate("rect", xmin = as.Date(221), xmax = as.Date(227), ymin = -Inf, ymax = Inf,
             fill = "red", alpha = 0.5)+
    annotate("rect", xmin = as.Date(154+daysAfter - slopeLength), xmax = as.Date(154+daysAfter+numSlopes), ymin = -Inf, ymax = Inf,
             fill = "steelblue", alpha = 0.5)+
    annotate("rect", xmin = as.Date(227+daysAfter - slopeLength), xmax = as.Date(227+daysAfter+numSlopes), ymin = -Inf, ymax = Inf,
             fill = "steelblue", alpha = 0.5)
  
  
  r10chl = ggplot(data = r10, aes(x = doy, y = manual_chl))+
    geom_line(color = "forestgreen", size = 1)+
    theme_classic()
  
  
  r10temp = ggplot(data = r10, aes(x = doy, y = mean_temp))+
    geom_line(color = "steelblue", size = 1)+
    theme_classic()+
    labs(title = "Peter 2010")+
    annotate("rect", xmin = as.Date(144), xmax = as.Date(154), ymin = -Inf, ymax = Inf,
             fill = "red", alpha = 0.5)+
    annotate("rect", xmin = as.Date(221), xmax = as.Date(227), ymin = -Inf, ymax = Inf,
             fill = "red", alpha = 0.5)
  
  
  print(ggarrange(nrow = 3, ncol = 2,r10temp, l10temp, r10chl, l10chl, r10slope, l10slope))
  
  
  
  
  ##### 2011 ####
  t11 = slopes %>%  filter(year == 2011, lake == "T")
  l11 = slopes %>%  filter(year == 2011, lake == "L")
  r11 = slopes %>%  filter(year == 2011, lake == "R")
  
  # Paul
  l11slope = ggplot(data = l11, aes( x = doy, y = chl_slope))+
    geom_line(color = "black", size = 1)+
    theme_classic()+
    ylim(-4.1, 4.5)+
    annotate("rect", xmin = as.Date(199), xmax = as.Date(205), ymin = -Inf, ymax = Inf,
             fill = "red", alpha = 0.5)+
    annotate("rect", xmin = as.Date(205+daysAfter - slopeLength), xmax = as.Date(205+daysAfter+numSlopes), ymin = -Inf, ymax = Inf,
             fill = "steelblue", alpha = 0.5)
  
  l11chl = ggplot(data = l11, aes(x = doy, y = manual_chl))+
    geom_line(color = "forestgreen", size = 1)+
    theme_classic()
  
  
  l11temp = ggplot(data = l11, aes(x = doy, y = mean_temp))+
    geom_line(color = "steelblue", size = 1)+
    theme_classic()+
    labs(title = "Paul 2011")+
    annotate("rect", xmin = as.Date(199), xmax = as.Date(205), ymin = -Inf, ymax = Inf,
             fill = "red", alpha = 0.5)
  
  
  # Peter
  r11slope = ggplot(data = r11, aes( x = doy, y = chl_slope))+
    geom_line(color = "black", size = 1)+
    theme_classic()+
    ylim(-4.1, 4.5)+
    annotate("rect", xmin = as.Date(199), xmax = as.Date(205), ymin = -Inf, ymax = Inf,
             fill = "red", alpha = 0.5)+
    annotate("rect", xmin = as.Date(205+daysAfter - slopeLength), xmax = as.Date(205+daysAfter+numSlopes), ymin = -Inf, ymax = Inf,
             fill = "steelblue", alpha = 0.5)
  
  r11chl = ggplot(data = r11, aes(x = doy, y = manual_chl))+
    geom_line(color = "forestgreen", size = 1)+
    theme_classic()
  
  r11temp = ggplot(data = r11, aes(x = doy, y = mean_temp))+
    geom_line(color = "steelblue", size = 1)+
    theme_classic()+
    labs(title = "Peter 2011")+
    annotate("rect", xmin = as.Date(199), xmax = as.Date(205), ymin = -Inf, ymax = Inf,
             fill = "red", alpha = 0.5)
  
  print(ggarrange(nrow = 3, ncol = 2, r11temp, l11temp, r11chl, l11chl, r11slope, l11slope))
  
  
  
  
  ##### 2013 ####
  t13 = slopes %>%  filter(year == 2013, lake == "T")
  l13 = slopes %>%  filter(year == 2013, lake == "L")
  r13 = slopes %>%  filter(year == 2013, lake == "R")
  
  # Tuesday
  t13slope = ggplot(data = t13, aes( x = doy, y = chl_slope))+
    geom_line(color = "black", size = 1)+
    theme_classic()+
    ylim(-4.7, 5.5)+
    annotate("rect", xmin = as.Date(186), xmax = as.Date(190), ymin = -Inf, ymax = Inf,
             fill = "red", alpha = 0.5)+
    annotate("rect", xmin = as.Date(196), xmax = as.Date(201), ymin = -Inf, ymax = Inf,
             fill = "red", alpha = 0.5)+
    annotate("rect", xmin = as.Date(190 + daysAfter), xmax = as.Date(190+daysAfter+numSlopes), ymin = -Inf, ymax = Inf,
             fill = "steelblue", alpha = 0.5)+
    annotate("rect", xmin = as.Date(201+daysAfter - slopeLength), xmax = as.Date(201+daysAfter+numSlopes), ymin = -Inf, ymax = Inf,
             fill = "steelblue", alpha = 0.5)
  
  t13chl = ggplot(data = t13, aes(x = doy, y = manual_chl))+
    geom_line(color = "forestgreen", size = 1)+
    theme_classic()
  
  t13temp = ggplot(data = t13, aes(x = doy, y = mean_temp))+
    geom_line(color = "steelblue", size = 1)+
    theme_classic()+
    labs(title = "Tuesday 2013")+
    annotate("rect", xmin = as.Date(186), xmax = as.Date(190), ymin = -Inf, ymax = Inf,
             fill = "red", alpha = 0.5)+
    annotate("rect", xmin = as.Date(196), xmax = as.Date(201), ymin = -Inf, ymax = Inf,
             fill = "red", alpha = 0.5)
  
  
  # Paul
  l13slope = ggplot(data = l13, aes( x = doy, y = chl_slope))+
    geom_line(color = "black", size = 1)+
    theme_classic()+
    ylim(-4.7, 5.5)+
    annotate("rect", xmin = as.Date(196), xmax = as.Date(201), ymin = -Inf, ymax = Inf,
             fill = "red", alpha = 0.5)+
    annotate("rect", xmin = as.Date(201+daysAfter - slopeLength), xmax = as.Date(201+daysAfter+numSlopes), ymin = -Inf, ymax = Inf,
             fill = "steelblue", alpha = 0.5)
  
  l13chl = ggplot(data = l13, aes(x = doy, y = manual_chl))+
    geom_line(color = "forestgreen", size = 1)+
    theme_classic()
  
  l13temp = ggplot(data = l13, aes(x = doy, y = mean_temp))+
    geom_line(color = "steelblue", size = 1)+
    theme_classic()+
    labs(title = "Paul 2013")+
    annotate("rect", xmin = as.Date(196), xmax = as.Date(201), ymin = -Inf, ymax = Inf,
             fill = "red", alpha = 0.5)
  
  
  # Peter
  r13slope = ggplot(data = r13, aes( x = doy, y = chl_slope))+
    geom_line(color = "black", size = 1)+
    theme_classic()+
    ylim(-4.7, 5.5)+
    annotate("rect", xmin = as.Date(196), xmax = as.Date(201), ymin = -Inf, ymax = Inf,
             fill = "red", alpha = 0.5)+
    annotate("rect", xmin = as.Date(201+daysAfter - slopeLength), xmax = as.Date(201+daysAfter+numSlopes), ymin = -Inf, ymax = Inf,
             fill = "steelblue", alpha = 0.5)
  
  r13chl = ggplot(data = r13, aes(x = doy, y = manual_chl))+
    geom_line(color = "forestgreen", size = 1)+
    theme_classic()
  
  r13temp = ggplot(data = r13, aes(x = doy, y = mean_temp))+
    geom_line(color = "steelblue", size = 1)+
    theme_classic()+
    labs(title = "Peter 2013")+
    annotate("rect", xmin = as.Date(196), xmax = as.Date(201), ymin = -Inf, ymax = Inf,
             fill = "red", alpha = 0.5)
  
  print(ggarrange(nrow = 3, ncol = 3, r13temp, l13temp, t13temp, r13chl, l13chl, t13chl, r13slope, l13slope, t13slope))
  
  
  ##### 2014 ####
  t14 = slopes %>%  filter(year == 2014, lake == "T")
  l14 = slopes %>%  filter(year == 2014, lake == "L")
  r14 = slopes %>%  filter(year == 2014, lake == "R")
  
  # Tuesday
  t14slope = ggplot(data = t14, aes( x = doy, y = chl_slope))+
    geom_line(color = "black", size = 1)+
    theme_classic()+
    ylim(-4.7, 5.5)+
    annotate("rect", xmin = as.Date(146), xmax = as.Date(154), ymin = -Inf, ymax = Inf,
             fill = "red", alpha = 0.5)+
    annotate("rect", xmin = as.Date(154+daysAfter - slopeLength), xmax = as.Date(154+daysAfter+numSlopes), ymin = -Inf, ymax = Inf,
             fill = "steelblue", alpha = 0.5)
  
  t14chl = ggplot(data = t14, aes(x = doy, y = manual_chl))+
    geom_line(color = "forestgreen", size = 1)+
    theme_classic()
  
  t14temp = ggplot(data = t14, aes(x = doy, y = mean_temp))+
    geom_line(color = "steelblue", size = 1)+
    theme_classic()+
    labs(title = "Tuesday 2014")+
    annotate("rect", xmin = as.Date(146), xmax = as.Date(154), ymin = -Inf, ymax = Inf,
             fill = "red", alpha = 0.5)
  
  
  # Paul
  l14slope = ggplot(data = l14, aes( x = doy, y = chl_slope))+
    geom_line(color = "black", size = 1)+
    theme_classic()+
    ylim(-4.1, 4.5)
  
  l14chl = ggplot(data = l14, aes(x = doy, y = manual_chl))+
    geom_line(color = "forestgreen", size = 1)+
    theme_classic()
  
  l14temp = ggplot(data = l14, aes(x = doy, y = mean_temp))+
    geom_line(color = "steelblue", size = 1)+
    theme_classic()+
    labs(title = "Paul 2014")
  
  
  # Peter
  r14slope = ggplot(data = r14, aes( x = doy, y = chl_slope))+
    geom_line(color = "black", size = 1)+
    theme_classic()+
    ylim(-4.1, 4.5)+
    annotate("rect", xmin = as.Date(149), xmax = as.Date(153), ymin = -Inf, ymax = Inf,
             fill = "red", alpha = 0.5)+
    annotate("rect", xmin = as.Date(153+daysAfter - slopeLength), xmax = as.Date(153+daysAfter+numSlopes), ymin = -Inf, ymax = Inf,
             fill = "steelblue", alpha = 0.5)
  
  r14chl = ggplot(data = r14, aes(x = doy, y = manual_chl))+
    geom_line(color = "forestgreen", size = 1)+
    theme_classic()
  
  r14temp = ggplot(data = l14, aes(x = doy, y = mean_temp))+
    geom_line(color = "steelblue", size = 1)+
    theme_classic()+
    labs(title = "Peter 2014")+
    annotate("rect", xmin = as.Date(149), xmax = as.Date(153), ymin = -Inf, ymax = Inf,
             fill = "red", alpha = 0.5)
  
  print(ggarrange(nrow = 3, ncol = 3, r14temp, l14temp, t14temp, r14chl, l14chl, t14chl, r14slope, l14slope, t14slope))
  
  
  
  
  ##### 2015 #####
  t15 = slopes %>%  filter(year == 2015, lake == "T")
  l15 = slopes %>%  filter(year == 2015, lake == "L")
  r15 = slopes %>%  filter(year == 2015, lake == "R")
  
  # Tuesday
  t15slope = ggplot(data = t15, aes( x = doy, y = chl_slope))+
    geom_line(color = "black", size = 1)+
    theme_classic()+
    ylim(-4.7, 5.5)+
    annotate("rect", xmin = 206, xmax = as.Date(210), ymin = -Inf, ymax = Inf,
             fill = "red", alpha = 0.5)+
    annotate("rect", xmin = as.Date(226), xmax = as.Date(230), ymin = -Inf, ymax = Inf,
             fill = "red", alpha = 0.5)+
    annotate("rect", xmin = as.Date(210+daysAfter - slopeLength), xmax = as.Date(210+daysAfter+numSlopes), ymin = -Inf, ymax = Inf,
             fill = "steelblue", alpha = 0.5)+
    annotate("rect", xmin = as.Date(230+daysAfter - slopeLength), xmax = as.Date(230+daysAfter+numSlopes), ymin = -Inf, ymax = Inf,
             fill = "steelblue", alpha = 0.5)
  
  t15chl = ggplot(data = t15, aes(x = doy, y = manual_chl))+
    geom_line(color = "forestgreen", size = 1)+
    theme_classic()
  
  t15temp = ggplot(data = l14, aes(x = doy, y = mean_temp))+
    geom_line(color = "steelblue", size = 1)+
    theme_classic()+
    labs(title = "Tuesday 2015")+
    annotate("rect", xmin = 206, xmax = as.Date(210), ymin = -Inf, ymax = Inf,
             fill = "red", alpha = 0.5)+
    annotate("rect", xmin = as.Date(226), xmax = as.Date(230), ymin = -Inf, ymax = Inf,
             fill = "red", alpha = 0.5)
  
  # Paul
  l15slope = ggplot(data = l15, aes( x = doy, y = chl_slope))+
    geom_line(color = "black", size = 1)+
    theme_classic()+
    ylim(-4.1, 4.5)
  
  l15chl = ggplot(data = l15, aes(x = doy, y = manual_chl))+
    geom_line(color = "forestgreen", size = 1)+
    theme_classic()
  
  l15temp = ggplot(data = l14, aes(x = doy, y = mean_temp))+
    geom_line(color = "steelblue", size = 1)+
    theme_classic()+
    labs(title = "Paul 2015")
  
  
  # Peter
  r15slope = ggplot(data = r15, aes( x = doy, y = chl_slope))+
    geom_line(color = "black", size = 1)+
    theme_classic()+
    ylim(-6, 4.5)
  
  r15chl = ggplot(data = r15, aes(x = doy, y = manual_chl))+
    geom_line(color = "forestgreen", size = 1)+
    theme_classic()
  
  r15temp = ggplot(data = l14, aes(x = doy, y = mean_temp))+
    geom_line(color = "steelblue", size = 1)+
    theme_classic()+
    labs(title = "Peter 2015")
  
  print(ggarrange(nrow = 3, ncol = 3, r15temp, l15temp, t15temp, r15chl, l15chl, t15chl, r15slope, l15slope, t15slope))
  
  
  
  
  
  
  ##### 2018 ####
  t18 = slopes %>%  filter(year == 2018, lake == "T")
  l18 = slopes %>%  filter(year == 2018, lake == "L")
  r18 = slopes %>%  filter(year == 2018, lake == "R")
  
  # Paul
  l18slope = ggplot(data = l18, aes( x = doy, y = chl_slope))+
    geom_line(color = "black", size = 1)+
    theme_classic()+
    ylim(-4.7, 5.5)+
    annotate("rect", xmin = as.Date(144), xmax = as.Date(152), ymin = -Inf, ymax = Inf,
             fill = "red", alpha = 0.5)+
    annotate("rect", xmin = as.Date(169), xmax = as.Date(175), ymin = -Inf, ymax = Inf,
             fill = "red", alpha = 0.5)+
    annotate("rect", xmin = as.Date(180), xmax = as.Date(188), ymin = -Inf, ymax = Inf,
             fill = "red", alpha = 0.5)+
    annotate("rect", xmin = as.Date(223), xmax = as.Date(231), ymin = -Inf, ymax = Inf,
             fill = "red", alpha = 0.5)+
    annotate("rect", xmin = as.Date(152+daysAfter - slopeLength), xmax = as.Date(152+daysAfter+numSlopes), ymin = -Inf, ymax = Inf,
             fill = "steelblue", alpha = 0.5)+
    annotate("rect", xmin = as.Date(175+daysAfter - slopeLength), xmax = as.Date(175+daysAfter+numSlopes), ymin = -Inf, ymax = Inf,
             fill = "steelblue", alpha = 0.5)+
    annotate("rect", xmin = as.Date(188+daysAfter - slopeLength), xmax = as.Date(188+daysAfter+numSlopes), ymin = -Inf, ymax = Inf,
             fill = "steelblue", alpha = 0.5)+
    annotate("rect", xmin = as.Date(231+daysAfter - slopeLength), xmax = as.Date(231+daysAfter+numSlopes), ymin = -Inf, ymax = Inf,
             fill = "steelblue", alpha = 0.5)
  
  l18chl = ggplot(data = l18, aes(x = doy, y = manual_chl))+
    geom_line(color = "forestgreen", size = 1)+
    theme_classic()
  
  l18temp = ggplot(data = l18, aes(x = doy, y = mean_temp))+
    geom_line(color = "steelblue", size = 1)+
    theme_classic()+
    labs(title = "Paul 2018")+
    annotate("rect", xmin = as.Date(144), xmax = as.Date(152), ymin = -Inf, ymax = Inf,
             fill = "red", alpha = 0.5)+
    annotate("rect", xmin = as.Date(169), xmax = as.Date(175), ymin = -Inf, ymax = Inf,
             fill = "red", alpha = 0.5)+
    annotate("rect", xmin = as.Date(180), xmax = as.Date(188), ymin = -Inf, ymax = Inf,
             fill = "red", alpha = 0.5)+
    annotate("rect", xmin = as.Date(223), xmax = as.Date(231), ymin = -Inf, ymax = Inf,
             fill = "red", alpha = 0.5)
  
  # Peter
  r18slope = ggplot(data = r18, aes( x = doy, y = chl_slope))+
    geom_line(color = "black", size = 1)+
    theme_classic()+
    ylim(-4.7, 5.5)+
    annotate("rect", xmin = as.Date(144), xmax = as.Date(152), ymin = -Inf, ymax = Inf,
             fill = "red", alpha = 0.5)+
    annotate("rect", xmin = as.Date(169), xmax = as.Date(175), ymin = -Inf, ymax = Inf,
             fill = "red", alpha = 0.5)+
    annotate("rect", xmin = as.Date(180), xmax = as.Date(192), ymin = -Inf, ymax = Inf,
             fill = "red", alpha = 0.5)+
    annotate("rect", xmin = as.Date(223), xmax = as.Date(232), ymin = -Inf, ymax = Inf,
             fill = "red", alpha = 0.5)+
    annotate("rect", xmin = as.Date(152+daysAfter - slopeLength), xmax = as.Date(152+daysAfter+numSlopes), ymin = -Inf, ymax = Inf,
             fill = "steelblue", alpha = 0.5)+
    annotate("rect", xmin = as.Date(175+daysAfter - slopeLength), xmax = as.Date(175+daysAfter+numSlopes), ymin = -Inf, ymax = Inf,
             fill = "steelblue", alpha = 0.5)+
    annotate("rect", xmin = as.Date(192+daysAfter - slopeLength), xmax = as.Date(192+daysAfter+numSlopes), ymin = -Inf, ymax = Inf,
             fill = "steelblue", alpha = 0.5)+
    annotate("rect", xmin = as.Date(232+daysAfter - slopeLength), xmax = as.Date(232+daysAfter+numSlopes), ymin = -Inf, ymax = Inf,
             fill = "steelblue", alpha = 0.5)
  
  r18chl = ggplot(data = r18, aes(x = doy, y = manual_chl))+
    geom_line(color = "forestgreen", size = 1)+
    theme_classic()
  
  r18temp = ggplot(data = r18, aes(x = doy, y = mean_temp))+
    geom_line(color = "steelblue", size = 1)+
    theme_classic()+
    labs(title = "Peter 2018")+
    annotate("rect", xmin = as.Date(144), xmax = as.Date(152), ymin = -Inf, ymax = Inf,
             fill = "red", alpha = 0.5)+
    annotate("rect", xmin = as.Date(169), xmax = as.Date(175), ymin = -Inf, ymax = Inf,
             fill = "red", alpha = 0.5)+
    annotate("rect", xmin = as.Date(180), xmax = as.Date(192), ymin = -Inf, ymax = Inf,
             fill = "red", alpha = 0.5)+
    annotate("rect", xmin = as.Date(223), xmax = as.Date(232), ymin = -Inf, ymax = Inf,
             fill = "red", alpha = 0.5)
  
  print(ggarrange(nrow = 3, ncol = 2, r18temp, l18temp, r18chl, l18chl, r18slope, l18slope))
  
  
  
  
  
  ##### 2019 ####
  l19 = slopes %>%  filter(year == 2019, lake == "L")
  r19 = slopes %>%  filter(year == 2019, lake == "R")
  
  # Paul
  l19slope = ggplot(data = l19, aes( x = doy, y = chl_slope))+
    geom_line(color = "black", size = 1)+
    theme_classic()+
    ylim(-4.7, 5.5)+
    annotate('rect', xmin = as.Date(196), xmax = as.Date(200), ymin = -Inf, ymax = Inf,
             fill = 'red', alpha = 0.5)+
    annotate('rect', xmin = as.Date(200+daysAfter - slopeLength), xmax = as.Date(200+daysAfter+numSlopes), ymin = -Inf, ymax = Inf,
             fill = 'steelblue', alpha = 0.5)
  
  l19chl = ggplot(data = l19, aes(x = doy, y = manual_chl))+
    geom_line(color = "forestgreen", size = 1)+
    theme_classic()
  
  l19temp = ggplot(data = l19, aes(x = doy, y = mean_temp))+
    geom_line(color = "steelblue", size = 1)+
    theme_classic()+
    labs(title = "Paul 2019")+
    annotate('rect', xmin = as.Date(196), xmax = as.Date(200), ymin = -Inf, ymax = Inf,
             fill = 'red', alpha = 0.5)
  
  
  # Peter
  r19slope = ggplot(data = r19, aes( x = doy, y = chl_slope))+
    geom_line(color = "black", size = 1)+
    theme_classic()+
    ylim(-4.7, 5.5)+
    annotate("rect", xmin = as.Date(215), xmax = as.Date(219), ymin = -Inf, ymax = Inf,
             fill = "red", alpha = 0.5)+
    annotate("rect", xmin = as.Date(219+daysAfter - slopeLength), xmax = as.Date(219+daysAfter+numSlopes), ymin = -Inf, ymax = Inf,
             fill = "steelblue", alpha = 0.5)
  
  r19chl = ggplot(data = r19, aes(x = doy, y = manual_chl))+
    geom_line(color = "forestgreen", size = 1)+
    theme_classic()
  
  r19temp = ggplot(data = r19, aes(x = doy, y = mean_temp))+
    geom_line(color = "steelblue", size = 1)+
    theme_classic()+
    labs(title = "Peter 2019")+
    annotate('rect', xmin = as.Date(215), xmax = as.Date(219), ymin = -Inf, ymax = Inf,
             fill = 'red', alpha = 0.5)
  
  print(ggarrange(nrow = 3, ncol = 2, r19temp, l19temp, r19chl, l19chl, r19slope, l19slope))
  
  dev.off()
  
  
}
