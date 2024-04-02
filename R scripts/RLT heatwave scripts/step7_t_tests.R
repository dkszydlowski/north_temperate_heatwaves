####  t-tests and plotting boxplots of the results 
# run after running step3_manual_chl_response

slopes.0 = allSlopes %>% filter(daysAfter == 0)


slopes.0 = slopes.0 %>% mutate(period = replace(period, period == "after heatwave", "during heatwave"))
desired_order <- c("during heatwave", "all other days")

png("./figures/boxplots/days after 0 chl a t tests.png", height = 4, width = 7, res = 300, units = "in")

ggplot(slopes.0, aes(x = period, y = percent_change, fill = lake))+
  geom_boxplot()+
  facet_wrap(~lake)+
  theme_classic()+
  labs(x = "", y = "percent change in surface chl a")+
  scale_fill_manual(values = c("R"=  "#4AB5C4", "L" = "#ADDAE3", "T"=  "#BAAD8D"))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  
dev.off()

### Overall t-test
slopes.0.during = slopes.0 %>% filter(period == "during heatwave")
slopes.0.other = slopes.0 %>% filter(period == "all other days")

t.test(slopes.0.during$percent_change, slopes.0.other$percent_change)


### Peter t-test ###
slopes.0.R = slopes.0 %>% filter(lake == "R")

slopes.0.R.during = slopes.0.R %>% filter(period == "during heatwave")
slopes.0.R.other = slopes.0.R %>% filter(period == "all other days")

t.test(slopes.0.R.during$percent_change, slopes.0.R.other$percent_change)


### Paul t-test ###
slopes.0.L = slopes.0 %>% filter(lake == "L")

slopes.0.L.during = slopes.0.L %>% filter(period == "during heatwave")
slopes.0.L.other = slopes.0.L %>% filter(period == "all other days")

t.test(slopes.0.L.during$percent_change, slopes.0.L.other$percent_change)


### Tuesday t-test ###
slopes.0.T = slopes.0 %>% filter(lake == "T")

slopes.0.T.during = slopes.0.T %>% filter(period == "during heatwave")
slopes.0.T.other = slopes.0.T %>% filter(period == "all other days")

t.test(slopes.0.T.during$percent_change, slopes.0.T.other$percent_change)

library(tidyverse)

