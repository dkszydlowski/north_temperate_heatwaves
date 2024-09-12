#### function for taking data before, during, and after heatwaves

# this can be applied to any explanatory variable datasets

# requires the data to have lakeid, year4, and daynum columns


# read in the nutrient data
nuts = read.csv("./formatted data/explanatory variables heatwaves/nutrients 2008 to 2016.csv")


heatwaves = hw.exp

before.during.after.hw = function(data, heatwaves){
  

  for(i in 1:nrow(heatwaves)){
    
    # save the start date of current heatwave
    targ.date = heatwaves$date_start[i]
    targ.date.end = heatwaves$date_end[i]
    targ.doy.end = yday(targ.date.end)
    targ.doy = heatwaves$doy[i] # doy at the start of the heatwave
    print(targ.date)
    print(targ.date.end)
    
    # save the lake of the current heatwave
    targ.lake = heatwaves$lake[i]
    targ.year = heatwaves$year[i]
    
    # filter the explanatory dataframe to match each category
    cur.casc.exp.before = data %>% filter(lakeid == targ.lake, year4 == targ.year, daynum < targ.doy)
    cur.casc.exp.during = data %>% filter(lakeid == targ.lake, year4 == targ.year, daynum <= targ.doy.end & daynum >= targ.doy)
    cur.casc.exp.after = data %>% filter(lakeid == targ.lake, year4 == targ.year, daynum > targ.doy.end)
    
    
    
    # total biomass before, during, and after the heatwave
    cur.casc.exp.before = cur.casc.exp.before %>% filter(daynum == max(daynum))
    if(nrow(cur.casc.exp.before) > 0){ heatwaves$total.biomass.before[i] = sum(cur.casc.exp.before$biomass, na.rm = TRUE)
    heatwaves$manual.chl.before[i] = mean(cur.casc.exp.before$manual_chl, na.rm = TRUE)
    heatwaves$zoop.days.before[i] = targ.doy - max(cur.casc.exp.before$daynum)}
    
    cur.casc.exp.during = cur.casc.exp.during %>% filter(daynum == min(daynum))
    if(nrow(cur.casc.exp.during) > 0){ heatwaves$total.biomass.during[i] = sum(cur.casc.exp.during$biomass, na.rm = TRUE)
    heatwaves$manual.chl.during[i] = mean(cur.casc.exp.during$manual_chl, na.rm = TRUE)
    heatwaves$zoop.days.after.start.during.hw[i] = max(cur.casc.exp.before$daynum) - targ.doy}
    
    cur.casc.exp.after = cur.casc.exp.after %>% filter(daynum == min(daynum))
    if(nrow(cur.casc.exp.after) > 0){ heatwaves$total.biomass.after[i] = sum(cur.casc.exp.after$biomass, na.rm = TRUE)
    heatwaves$manual.chl.after[i] = mean(cur.casc.exp.after$manual_chl, na.rm = TRUE)
    heatwaves$zoop.days.after.hw[i] = min(cur.casc.exp.before$daynum) - targ.doy.end}
    
    # save the whole zooplankton community data
    if(i == 1){zoop.before.all = cur.casc.exp.before}
    if(i > 1){  zoop.before.all = rbind(cur.casc.exp.before, zoop.before.all)}
    
    if(i == 1){zoop.during.all = cur.casc.exp.during}
    if(i > 1){  zoop.during.all = rbind(cur.casc.exp.during, zoop.during.all)}
    
    if(i == 1){zoop.after.all = cur.casc.exp.after}
    if(i > 1){  zoop.after.all = rbind(cur.casc.exp.after, zoop.after.all)}
    
    # Daphnia biomass before, during, and after the heatwave
    cur.casc.exp.before = cur.casc.exp.before %>% filter(daynum == max(daynum))
    if(nrow(cur.casc.exp.before) > 0){ 
      
      heatwaves$daphnia.biomass.before[i] = sum(cur.casc.exp.before %>% filter(taxon_name == "Daphnia" | 
                                                                              taxon_name == "Holopedium gibberum" |
                                                                              taxon_name == "Ceriodaphnia") %>% pull(biomass), na.rm = TRUE)
      
      heatwaves$daphnia.length.before[i] = mean(cur.casc.exp.before %>% filter(taxon_name == "Daphnia") %>% pull(mean_length), na.rm = TRUE)
      
    }
    
    cur.casc.exp.during = cur.casc.exp.during %>% filter(daynum == min(daynum))
    if(nrow(cur.casc.exp.during) > 0){
      
      heatwaves$daphnia.biomass.during[i] = sum(cur.casc.exp.during %>% filter(taxon_name == "Daphnia" |
                                                                              taxon_name == "Holopedium gibberum" |
                                                                              taxon_name == "Ceriodaphnia") %>% pull(biomass), na.rm = TRUE)
      
      
      heatwaves$daphnia.length.during[i] = mean(cur.casc.exp.during %>% filter(taxon_name == "Daphnia" 
      ) %>% pull(mean_length), na.rm = TRUE)
      
    }
    
    cur.casc.exp.after = cur.casc.exp.after %>% filter(daynum == min(daynum))
    if(nrow(cur.casc.exp.after) > 0){ 
      
      heatwaves$daphnia.biomass.after[i] = sum(cur.casc.exp.after %>% filter(taxon_name == "Daphnia" | taxon_name == "Holopedium gibberum" |
                                                                            taxon_name == "Ceriodaphnia" ) %>% pull(biomass), na.rm = TRUE)
      
      
      heatwaves$daphnia.length.after[i] = mean(cur.casc.exp.after %>% filter(taxon_name == "Daphnia" ) %>% pull(mean_length), na.rm = TRUE)
      
      
      
    }
    
    
  }
  
  
  # combine community data into one and save
  zoop.before.all = zoop.before.all %>% mutate(period = "before")
  zoop.during.all = zoop.during.all %>% mutate(period = "during")
  zoop.after.all = zoop.after.all %>% mutate(period = "after")
  
  zoop.all.hw = rbind(zoop.before.all, zoop.during.all, zoop.after.all)
  
  
  write.csv(zoop.all.hw, "./formatted data/zooplankton/zooplankton community heatwaves.csv", row.names= FALSE)
  
  ### compare new changes in zooplankton to changes in chlorophyll
  # if we look at the last sampling date during the heatwave, and the closest sampling to before it began, zooplankton decrease
  
  
  
  heatwaves = heatwaves %>% mutate(pchange.total.zoop = 100*(total.biomass.during - total.biomass.before)/total.biomass.before,
                             abschange.total.zoop = total.biomass.during- total.biomass.before,
                             pchange.total.zoop.during.to.after = 100*(total.biomass.after - total.biomass.during)/total.biomass.during,
                             pchange.daphnia.length = 100*(daphnia.length.before- daphnia.length.during)/daphnia.length.during)
  
    
  
  
}
