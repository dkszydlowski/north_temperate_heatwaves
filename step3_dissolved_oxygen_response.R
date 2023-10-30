# Re-run step 3 but with the dissolved oxygen data

# step 3, calculate the response of chl to the heatwaves
#install and load slider library for rolling window analysis
if (!require(slider)) install.packages('slider')
library(slider)

if (!require(plyr)) install.packages('plyr')
library(plyr)

if (!require(tidyr)) install.packages('tidyr')
library(tidyr)

if (!require(dplyr)) install.packages('dplyr')
library(dplyr)
library(tidyverse)

if (!require(zoo)) install.packages('zoo')
library(zoo)

if (!require(gganimate)) install.packages('gganimate')
library(gganimate)

if (!require(transformr)) install.packages('transformr')
library(transformr)

if (!require(ggridges)) install.packages('ggridges')
library(ggridges)


# read in the heatwaves data calculated in the previous step
heatwaves = read.csv("./formatted data/heatwavesdata.csv")

# read in the sonde data from step 1
allData = read.csv("./formatted data/full raw data manual and sonde chlorophyll.csv")
allData$date = as.Date(allData$date)

