# step 2, calculate the response of chl to the heatwaves
#install and load slider library for rolling window analysis
if (!require(slider)) install.packages('slider')
library(slider)

# maybe a function that calculates the slope following a heatwave