# create a dataframe that has daily cumulative and single day P-load for
# each of the years I am using in analysis

library(tidyverse)

# create a dataframe with each of the lake_year combinations and all of the 
# days of the year empty

# get the lake_years and days from the chlorophyll data
chl = read.csv("./formatted data/manual_chlorophyll.csv")

nut.load = chl %>% mutate(cumulative.load = 0, daily.load = 0)

nut.load <- nut.load %>%
  dplyr::rename(doy = doyCat)


# Need nutrient loading rates for Tuesday in 2013, 2014, and 2015
# and Peter in 2013, 2014, 2015, and 2019

# From Wilkinson et al. 2018 (Ecological Monographs):

# 2013: 0.5 mg P*m-2*d-1 increasing weekly by 0.3125 mg P*m-2*d-1 until doy 203,
  # after which the weekly increase was 0.625 

# 2014: R and T had an aerial P loading rate of 3 mg P*m-2*d-1 from doy 153-241

# 2015: R had an aerial P loading rate of 3 mg P*m-2*d-1 from doy 152-180
#       T had an aerial P loading rate of 3 mg P*m-2*d-1 from doy 152-240
# 

# From Buelo et al. 2022 Ecological Applications:

# "In the second year, 2019, nutrients 
# were added to Peter Lake daily starting 
# on day of year 161 and ending on day of year 237. 
# Solutions of phosphoric acid and ammonium nitrate 
# were prepared and distributed by pumping them into the
# prop-wash of a boat propelled by an electric motor.
# Nutrients were added at a fixed 15:1 molar ratio of N:P, 
# with the loading rate starting at 0.5 mg P m−2 day−1 for the first week.
# Every 7 days, the nutrient loading rate was increased by 0.5 mg P m−2 day−1 
# until reaching a rate of 5 mg P m−2 day−1 in week 10, which was then maintained
# for 7 additional days before stopping nutrient additions."

# calculate the daily load for 2019
calculate_daily_load_2019 <- function(doy) {
  start_doy <- 161
  end_doy <- 237
  initial_load <- 0.5
  final_load <- 5
  load_increase <- 0.5
  weeks <- ((doy - start_doy) %/% 7) + 1
  
  ifelse(doy < start_doy | doy > end_doy,
         0,
         ifelse(weeks <= 10,
                initial_load + load_increase * (weeks - 1),
                final_load))
}

nut.load <- nut.load %>%
  mutate(daily.load = case_when(
    year == 2013 & (lake == "R" | lake == "T") & doy >= 154 ~ 0.5 + 0.3125 * ((doy - 154) %/% 7),
    year == 2013 & (lake == "R" | lake == "T") & doy < 154 ~ 0,
    year == 2013 & (lake == "R" | lake == "T") & doy > 203 ~ 0.5 + 0.625 * ((doy - 203) %/% 7),
    (lake == "R" | lake == "T") & year == 2014 & doy >= 153 & doy <= 241 ~ 3,
    (lake == "R") & year == 2015 & doy >= 152 & doy <= 180 ~ 3,
    (lake == "T") & year == 2015 & doy >= 152 & doy <= 240 ~ 3,
    (lake == "R") & year == 2019 & doy >= 161 & doy <= 237 ~ calculate_daily_load_2019(doy),
    TRUE ~ daily.load
  ))

nut.load = nut.load %>% mutate(year = as.factor(year))

# calculate the cumulative loading

nut.load = nut.load %>% 
  arrange(lake, year) %>%
  dplyr::group_by(lake, year) %>% 
  dplyr::mutate(cumulative.load = cumsum(daily.load))

nut.load.13.15 = nut.load %>% filter(year == 2013 | year == 2014 | year == 2015 | year == 2019)

ggplot(nut.load.13.15 %>% filter(lake != "L"), aes(x = doy, y = cumulative.load, fill = lake))+
  geom_area()+
  geom_line(color = "black", size = 1)+
  facet_wrap(lake~year)+
  theme_classic()

ggplot(nut.load.13.15 %>% filter(lake != "L"), aes(x = doy, y = daily.load, fill = lake))+
  geom_area()+
  geom_line(color = "black", size = 1)+
  facet_wrap(lake~year)+
  theme_classic()


# save the nutrient loading data to a file
write.csv(nut.load, "./formatted data/explanatory variables heatwaves.csv", row.names = FALSE)

