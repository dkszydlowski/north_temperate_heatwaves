# format LTER temperature data

library(tidyverse)

SP.temp = read.csv("./formatted data/LTER daily temperature/Sparkling Lake daily temperature all depths.csv")
TR.temp = read.csv("./formatted data/LTER daily temperature/Trout Lake daily temperature all depths.csv")
TB.temp = read.csv("./formatted data/LTER daily temperature/Trout Bog daily temperature all depths.csv")
CB.temp = read.csv("./formatted data/LTER daily temperature/Crystal Bog daily temperature all depths.csv")

# investigate sampling depths

SP.depths <- SP.temp %>%
  filter(depth < 2) %>% 
  group_by(year4) %>%
  summarise(sampling_depths = toString(sort(unique(depth)))) %>% 
  ungroup() %>% 
  mutate(lake = "Sparkling Lake")

TR.depths <- TR.temp %>%
  filter(depth < 2) %>% 
  group_by(year4) %>%
  summarise(sampling_depths = toString(sort(unique(depth)))) %>% 
  ungroup() %>% 
  mutate(lake = "Trout Lake")

CB.depths <- CB.temp %>%
  filter(depth < 2) %>% 
  mutate(year4 = year(sampledate)) %>% 
  group_by(year4) %>%
  summarise(sampling_depths = toString(sort(unique(depth)))) %>% 
  ungroup() %>% 
  mutate(lake = "Crystal Bog")

TB.depths <- TB.temp %>%
  filter(depth < 2) %>% 
  mutate(year4 = year(sampledate)) %>% 
  group_by(year4) %>%
  summarise(sampling_depths = toString(sort(unique(depth)))) %>% 
  ungroup() %>% 
  mutate(lake = "Trout Bog")


### Format the data for heatwave analysis ###
# Sparkling Lake
SP.surface = SP.temp %>%
  filter(depth == 0 | depth == 0.01) %>% 
  rename(year = year4, doy = daynum) %>% 
  mutate(year = as.factor(year)) %>% 
  filter(doy > 152 & doy <259)

png("./figures/")



SP.surface.HWR = SP.surface %>% select(sampledate, wtemp) %>% 
  rename(t = sampledate, temp = wtemp)



# Trout Lake
TR.surface = TR.temp %>%
  filter(depth == 0) %>% 
  rename(year = year4, doy = daynum) %>% 
  mutate(year = as.factor(year)) %>% 
  filter(doy > 152 & doy <259)

  

TR.surface.HWR = TR.surface %>% select(sampledate, wtemp) %>% 
  rename(t = sampledate, temp = wtemp)



# Crystal Bog
CB.temp = CB.temp %>% mutate(year4 = year(sampledate), daynum = yday(sampledate))

CB.surface = CB.temp %>%
  rename(year = year4, doy = daynum) %>% 
  group_by(year) %>% 
  mutate(year = as.factor(year)) %>% 
  slice_min(order_by = depth) %>% 
  ungroup() %>% 
  filter(doy > 152 & doy <259)



CB.surface.HWR = CB.surface %>% select(sampledate, wtemp) %>% 
  rename(t = sampledate, temp = wtemp)




# Trout Bog
TB.temp = TB.temp %>% mutate(year4 = year(sampledate), daynum = yday(sampledate))

TB.surface = TB.temp %>%
  rename(year = year4, doy = daynum) %>% 
  group_by(year) %>% 
  mutate(year = as.factor(year)) %>% 
  slice_min(order_by = depth) %>% 
  ungroup() %>% 
  filter(doy > 152 & doy < 300)


TB.surface.HWR = TB.surface %>% select(sampledate, wtemp) %>% 
  rename(t = sampledate, temp = wtemp)


### Save each of the HWR formatted dataframes
write.csv(TB.surface.HWR, "./formatted data/LTER daily temperature/TB surface for heatwaveR.csv", row.names = FALSE)
write.csv(SP.surface.HWR, "./formatted data/LTER daily temperature/SP surface for heatwaveR.csv", row.names = FALSE)
write.csv(TR.surface.HWR, "./formatted data/LTER daily temperature/TR surface for heatwaveR.csv", row.names = FALSE)
write.csv(CB.surface.HWR, "./formatted data/LTER daily temperature/CB surface for heatwaveR.csv", row.names = FALSE)

### Plot the available data ###
pdf(onefile = TRUE, "./figures/LTER heatwaves/LTER temp data/LTER temp data availability.pdf", height = 7, width = 8)


ggplot(data = SP.surface, aes(x =doy, y = wtemp, color = year))+
  geom_line(size = 1, alpha = 0.5)+
  theme_classic()+
  labs(title = "Sparkling Lake temperature")+
  facet_wrap(~year)+
  geom_text(data = SP.surface %>% group_by(year) %>% summarize(depth = first(depth)),
            aes(x = Inf, y = -Inf, label = paste("Depth:", depth)),
            hjust = 1, vjust = -0.4, size = 3)+
  theme(legend.position = "none")


ggplot(data = TR.surface, aes(x =doy, y = wtemp, color = year))+
  geom_line(size = 1, alpha = 0.5)+
  theme_classic()+
  labs(title = "Trout Lake temperature")+
  facet_wrap(~year)+
  geom_text(data = TR.surface %>% group_by(year) %>% summarize(depth = first(depth)),
            aes(x = Inf, y = -Inf, label = paste("Depth:", depth)),
            hjust = 1, vjust = -0.4, size = 3)+
  theme(legend.position = "none")


ggplot(data = CB.surface, aes(x =doy, y = wtemp, color = year))+
  geom_line(size = 1, alpha = 0.5)+
  theme_classic()+
  labs(title = "Crystal Bog temperature")+
  facet_wrap(~year)+
  geom_text(data = CB.surface %>% group_by(year) %>% summarize(depth = first(depth)),
            aes(x = Inf, y = -Inf, label = paste("Depth:", depth)),
            hjust = 1, vjust = -0.4, size = 3)+
  theme(legend.position = "none")


ggplot(data = TB.surface, aes(x =doy, y = wtemp, color = year))+
  geom_line(size = 1, alpha = 0.5)+
  theme_classic()+
  labs(title = "Trout Bog temperature")+
  facet_wrap(~year)+
  geom_text(data = TB.surface %>% group_by(year) %>% summarize(depth = first(depth)),
            aes(x = Inf, y = -Inf, label = paste("Depth:", depth)),
            hjust = 1, vjust = -0.4, size = 3)+
  theme(legend.position = "none")

dev.off()
