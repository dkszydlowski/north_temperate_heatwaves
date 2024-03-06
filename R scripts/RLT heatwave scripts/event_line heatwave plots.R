# Script for making heatwave plots using event_line from heatwaveR

library(tidyverse)

# read in the heatwave outputs
peterHW = readRDS(file = "./results/heatwave modeled outputs/peter heatwave outputs modeled.rds")
paulHW = readRDS(file = "./results/heatwave modeled outputs/paul heatwave outputs modeled.rds")
tuesdayHW = readRDS(file = "./results/heatwave modeled outputs/tuesday heatwave outputs modeled.rds")

climatology.R = peterHW$climatology %>% mutate(lake = "peter")
climatology.L = paulHW$climatology %>% mutate(lake = "paul")
climatology.T = tuesdayHW$climatology %>% mutate(lake = "tuesday")

climatology.all = rbind(climatology.R, climatology.L, climatology.T)

climatology.all = climatology.all %>% mutate(year = year(t), lake_year = paste(lake, year(t), sep = "_"))

lakes = unique(climatology.all$lake)
years = unique(climatology.all$year)
i = 1
j = 1

pdf("./figures/event_line plots/event_line plots modeled temp 20240304.pdf", width = 6, height = 4)

for(i in 1:length(years)){
  for(j in 1:length(lakes)){
    
    lake_year = paste(lakes[j], years[i], sep = "_")
    
    if(!(lake_year %in% c("tuesday_2008", "tuesday_2009", "tuesday_2010", "tuesday_2011", "tuesday_2019", "tuesday_2018"))){
    
    # set the start and end dat for event_line
    start_date = paste(years[i], "-05-15", sep = "")
    end_date = paste(years[i], "-09-15", sep = "")
    
    cur.lake = lakes[j]
    
    # grab the climatology of the current lake_year combination
    if(cur.lake == "peter"){
      cur.HW = peterHW
    }
    if(cur.lake == "paul"){
      cur.HW = paulHW
    }
    if(cur.lake == "tuesday"){
      cur.HW = tuesdayHW
    }
    
    # save the current climatology for ggplot if event_line fails
    cur.climatology = cur.HW$climatology %>% filter(year(t) == years[i])
    
 result <- tryCatch({
      
   print(event_line(cur.HW, category = TRUE, start_date = start_date, end_date = end_date)+
     geom_point()+
     theme_classic()+
     labs(title = paste(cur.lake, years[i], sep = " ")))
   
    }, error =function(err) {
      
      cur.climatology$thresh2 = cur.climatology$thresh * 2
      
      print(ggplot(cur.climatology, aes(x = doy, y = temp)) +
        geom_line(aes(x = doy, y = temp, color = "Temperature")) +
        geom_point() +
        geom_line(aes(x = doy, y = seas, color = "Climatology", linetype = "solid")) +
        geom_line(aes(x = doy, y = thresh, color = "Threshold", linetype = "solid")) +
        geom_line(aes(x = doy, y = thresh2, color = "2x Threshold", linetype = "solid")) +
        theme_classic() +
        labs(title = paste(cur.lake, years[i], sep = " ")) +
        ylim(12, 30) +
        scale_color_manual(values = c("black", "black", "forestgreen", "black")) +
      #  scale_linetype_manual(values = c("solid", "solid", "dotted", "dashed")) +
        guides(color = guide_legend(title = "", override.aes = list(linetype = "solid"))))
      
      
    })
    
    }
  }
}

dev.off()


#### Plot the climatology of the three lakes to compare #####
climatology.2015 = climatology.all %>% filter(year == "2015")

ggplot(climatology.2015, aes(x = doy, y = seas, color = lake))+
  geom_line()+
  theme_classic()

ggplot(climatology.2015, aes(x = doy, y = temp, color = lake))+
  geom_line()+
  theme_classic()

##### Function that can be called to plot desired lake and year ######


