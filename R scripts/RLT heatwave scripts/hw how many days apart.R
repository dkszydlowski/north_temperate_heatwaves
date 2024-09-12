##### calculate timing of heatwaves relative to each other

heatwaves = read.csv("./results/heatwave modeled outputs/heatwave events LRT.csv")

heatwaves$days.apart = NA

l.hw = heatwaves %>% filter(lake == "L")
r.hw = heatwaves %>% filter(lake == "R")
t.hw = heatwaves %>% filter(lake == "T")

for(i in 2:nrow(l.hw)){
  l.hw$days.apart[i] = as.Date(l.hw$date_start[i]) - as.Date(l.hw$date_end[i-1])
  
}


for(i in 2:nrow(r.hw)){
  r.hw$days.apart[i] = as.Date(r.hw$date_start[i]) - as.Date(r.hw$date_end[i-1])
  
}

for(i in 2:nrow(t.hw)){
  t.hw$days.apart[i] = as.Date(t.hw$date_start[i]) - as.Date(t.hw$date_end[i-1])
  
}


hw.days.apart = rbind(l.hw, r.hw, t.hw)

