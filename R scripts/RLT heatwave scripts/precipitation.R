### check if precipitation is a predictor (rain in the week leading up to a heatwave)

# read in the precipitation data from Steve


load(file="./formatted data/precipitation/Daily_PPT_combo.Rdata")
print(PPT0[1,])


precip = PPT0

precip = precip %>% filter(year >= 2008)


# read in the heatwaves
paulHW = readRDS(file = "./results/heatwave modeled outputs/paul heatwave outputs modeled categories.rds")
peterHW = readRDS(file = "./results/heatwave modeled outputs/peter heatwave outputs modeled categories.rds")
tuesdayHW = readRDS(file = "./results/heatwave modeled outputs/tuesday heatwave outputs modeled categories.rds")

heatwave.char = read.csv("./formatted data/explanatory variables heatwaves/heatwaves with percent.csv")


heatwave.char$precip = NA


for(i in 1:nrow(heatwave.char)){
  
  cur.hw.date = heatwave.char$date_start[i]
  year.hw = year(cur.hw.date)
  doy = yday(cur.hw.date)
  
  temp.precip = precip %>% filter(year == year.hw & (DoY <= doy & DoY >= doy -7))
  print(sum(temp.precip$best))
  heatwave.char$precip[i] = sum(temp.precip$best)
  
}



heatwave.char = heatwave.char %>% filter(precip > 0)


ggplot(heatwave.char, aes(x = precip, y = percentChange, fill = lake))+
  geom_point(size = 3, pch = 21)+
  labs(x = "precipitation week before heatwave (mm)")+
  scale_fill_manual(values = c("R" = "#4AB5C4", "L" = "#ADDAE3", "T" = "#BAAD8D"))+
  theme_bw()
  

