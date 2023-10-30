# explaining the variation in heatwaves
# compare to color and TP

library(readxl)


exp = read_xlsx("./formatted data/explanatory_variables_heatwaves.xlsx")

exp$percent_change = as.numeric(exp$percent_change)


ggplot(data = exp, aes(x = p_loading_mg_m2, y = abs(percent_change)))+
  geom_point(size = 3, color = "steelblue2")+
  theme_classic()




ggplot(data = exp, aes(x = color_m_1, y = abs(percent_change)))+
  geom_point(size = 3, color = "steelblue2")+
  theme_classic()
