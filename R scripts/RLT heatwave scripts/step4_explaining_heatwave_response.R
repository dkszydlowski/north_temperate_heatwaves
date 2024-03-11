# explaining the variation in heatwaves
# compare to color and TP

library(readxl)
library(lme4)


### Old code with old values 
exp = read_xlsx("./formatted data/explanatory_variables_heatwaves.xlsx")

exp$percent_change = as.numeric(exp$percent_change)


ggplot(data = exp, aes(x = p_loading_mg_m2, y = abs(percent_change)))+
  geom_point(size = 3, color = "steelblue2")+
  theme_classic()


ggplot(data = exp, aes(x = color_m_1, y = abs(percent_change)))+
  geom_point(size = 3, color = "steelblue2")+
  theme_classic()


# plot of the percent change in chlorophyll seasonally

exp = exp %>% mutate(doy.start = yday(start_date))

png("./figures/seasonality/seasonality_of_percent_change.png", width = 4, height = 4, units = "in", res = 600)

ggplot(data = exp, aes(x = doy.start, y = abs(percent_change), fill = lake))+
  geom_point(size = 3, color = "black", shape = 21, stroke = 1, alpha = 0.7)+
  theme_classic()+
  labs(y = "percent change in surface chlorophyll", x = "day of year of heatwave")+
  scale_fill_manual(values = c("L" = "steelblue2", "R" = "black", "T" = "white"))

dev.off()




##### Make a new version of the dataset for the modeled heatwaves #####

peterHW = readRDS(file = "./results/heatwave modeled outputs/peter heatwave outputs modeled.rds")
paulHW = readRDS(file = "./results/heatwave modeled outputs/paul heatwave outputs modeled.rds")
tuesdayHW = readRDS(file = "./results/heatwave modeled outputs/tuesday heatwave outputs modeled.rds")

paulHW = readRDS(file = "./results/heatwave modeled outputs/paul heatwave outputs modeled categories.rds")
peterHW = readRDS(file = "./results/heatwave modeled outputs/peter heatwave outputs modeled categories.rds")
tuesdayHW = readRDS(file = "./results/heatwave modeled outputs/tuesday heatwave outputs modeled categories.rds")

peterHW = peterHW %>% mutate(lake = "R")
paulHW = paulHW %>% mutate(lake = "L")
tuesdayHW = tuesdayHW %>% mutate(lake = "T")

hw.all = rbind(peterHW, paulHW, tuesdayHW) %>% select(-season)

hw.all = hw.all %>% mutate(year = year(date_start), lake_year = paste(lake, year, sep = "_"))

write.csv(hw.all, "./results/heatwave modeled outputs/heatwave events LRT.csv", row.names = FALSE)







# check if heatwave characteristics are at all related to heatwave response
heatwave.char = read.csv("./formatted data/explanatory variables heatwaves/heatwaves with percent.csv")


global.model = lm(data = heatwave.char, percentChange ~ intensity_max+     +   intensity_mean    +                        +   intensity_max    +                         +   intensity_var    +                    
      +   intensity_cumulative    +                  +   intensity_mean_relThresh    +              +   intensity_max_relThresh    +          
       +   intensity_var_relThresh    +               +   intensity_cumulative_relThresh    +        +   intensity_mean_abs    +               
        +   intensity_max_abs    +                     +   intensity_var_abs    +                     +   intensity_cumulative_abs    +         
       +   rate_onset    +                            +   rate_decline,    na.action = na.pass)

summary(lm(data = heatwave.char, ))


summary(global.model)
dredge(global.model)








#### Gather stats on the lakes as a whole during this time period