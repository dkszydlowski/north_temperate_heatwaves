### clean tchains for rLakeAnalyzer and lake stability ###


library(tidyverse)
library(rLakeAnalyzer)
library(ggpubr)
library(zoo)
library(readxl)


# read in the 2009-2011 tchain data
tchain = read_xlsx("./formatted data/Cascade hourly temp sensor data/T-Chain Data 2009-2011.xlsx")

tchain = tchain %>% select(-contains("stability"), -Zmix)

# format datetime
tchain = tchain %>% mutate(datetime = as.POSIXct(paste(Date, format(TIme, "%H:%M:%S")), format = "%Y-%m-%d %H:%M:%S"))

tchain = tchain %>% mutate(datetime = format(datetime, "%Y-%m-%d %H:%M:%S"))

# reorder columns
tchain = tchain %>% select(-Date, -TIme, - Dayfrac)  %>% 
  select(datetime, everything())

# rename to make not capitalized
# and to format water correctly
tchain = tchain %>% rename(lake = Lake, year = Year)

tchain = tchain %>% rename_with(~ gsub("Temp_", "wtr_", .) %>%
                                  sub("([0-9]{1})([0-9])$", "\\1.\\2", .))


# split into separate dataframes and save
tchainL09 = tchain %>% filter(year == 2009 & lake == "L") %>% select(-lake, -year)
tchainL10 = tchain %>% filter(year == 2010 & lake == "L") %>% select(-lake, -year)
tchainL11 = tchain %>% filter(year == 2011 & lake == "L") %>% select(-lake, -year)

tchainR09 = tchain %>% filter(year == 2009 & lake == "R") %>% select(-lake, -year)
tchainR10 = tchain %>% filter(year == 2010 & lake == "R") %>% select(-lake, -year)
tchainR11 = tchain %>% filter(year == 2011 & lake == "R") %>% select(-lake, -year)

write.table(tchainL09, "./formatted data/LRT temp chains rLakeAnalyzer/Paul_temperature_2009_rLakeAnalyzer.csv", row.names = FALSE, sep = "\t")
write.table(tchainL10, "./formatted data/LRT temp chains rLakeAnalyzer/Paul_temperature_2010_rLakeAnalyzer.csv", row.names = FALSE, sep = "\t")
write.table(tchainL11, "./formatted data/LRT temp chains rLakeAnalyzer/Paul_temperature_2011_rLakeAnalyzer.csv", row.names = FALSE, sep = "\t")

write.table(tchainR09, "./formatted data/LRT temp chains rLakeAnalyzer/Peter_temperature_2009_rLakeAnalyzer.csv", row.names = FALSE, sep = "\t")
write.table(tchainR10, "./formatted data/LRT temp chains rLakeAnalyzer/Peter_temperature_2010_rLakeAnalyzer.csv", row.names = FALSE, sep = "\t")
write.table(tchainR11, "./formatted data/LRT temp chains rLakeAnalyzer/Peter_temperature_2011_rLakeAnalyzer.csv", row.names = FALSE, sep = "\t")


