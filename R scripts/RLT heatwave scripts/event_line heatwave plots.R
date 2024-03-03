# Script for making heatwave plots using event_line from heatwaveR

library(tidyverse)

# read in the heatwave outputs
peterHW = readRDS(file = "./results/heatwave modeled outputs/peter heatwave outputs modeled.rds")
paulHW = readRDS(file = "./results/heatwave modeled outputs/paul heatwave outputs modeled.rds")
tuesdayHW = readRDS(file = "./results/heatwave modeled outputs/tuesday heatwave outputs modeled.rds")


event_line(tuesdayHW, category = TRUE, start_date = "2013-05-15", end_date= "2013-09-15")+geom_point()+
  theme_classic()
