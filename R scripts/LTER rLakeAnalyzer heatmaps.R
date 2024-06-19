#' LTER heatmaps of temp
#' Read in the daily average LTER temp data
#' 
library(tidyverse)

TR = read.csv("./formatted data/LTER daily temperature/Trout Lake daily temperature all depths.csv")


TR = TR %>% filter(!is.na(wtemp))


