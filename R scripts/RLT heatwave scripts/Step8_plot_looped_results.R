#### Script for plotting the looped results #####
library(tidyverse)

# this gives us heatmaps of how the results change with daysAfter, numSlopes, and slopeLength


plot.looped.results = function(df, lake, set.var, set.var.value, period, orientation){
  
  df = df %>% filter({{orientation}} == orientation)
  
if(period == "after heatwave"){
  if(lake == "T"){
 print( ggplot(data = df, aes_string(x = "daysAfter", y = "numSlopes", fill = "T.after.heatwave")) +
    geom_tile(color = "black") +
    scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse") +
    labs(title = "Tuesday after heatwave") +
    theme_classic())}
  
  
  if(lake == "R"){
    print(ggplot(data = df, aes_string(x = "daysAfter", y = "numSlopes", fill = "R.after.heatwave")) +
      geom_tile(color = "black") +
      scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse") +
      labs(title = "Peter after heatwave") +
      theme_classic())}
  
  
  if(lake == "L"){
    print(ggplot(data = df, aes_string(x = "daysAfter", y = "slopeLength", fill = "L.after.heatwave")) +
      geom_tile(color = "black") +
      scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse") +
      labs(title = "Paul after heatwave") +
      theme_classic())}
}
  
  
  
  if(period == "all other days"){
    if(lake == "T"){
      print( ggplot(data = df, aes_string(x = "daysAfter", y = "slopeLength", fill = "T.all.other.days")) +
               geom_tile(color = "black") +
               scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse") +
               labs(title = "Tuesday all other days") +
               theme_classic())}
    
    
    if(lake == "R"){
      print(ggplot(data = df, aes_string(x = "daysAfter", y = "numSlopes", fill = "R.all.other.days")) +
              geom_tile(color = "black") +
              scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse") +
              labs(title = "Peter all other days") +
              theme_classic())}
    
    
    if(lake == "L"){
      print(ggplot(data = df, aes_string(x = "numSlopes", y = "slopeLength", fill = "L.all.other.days")) +
              geom_tile(color = "black") +
              scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse", limits = c(80, -80), oob = scales::squish) +
              labs(title = "Paul all other days") +
              theme_classic())}
  }
  
  
  if(lake == "all"){
    
  }
  
  
}


df = looped.results
lake = "T"

plot.looped.results(df = looped.results, lake = "L", set.var = "daysAfter", set.var.value = 0, period = "all other days", orientation = "start")
