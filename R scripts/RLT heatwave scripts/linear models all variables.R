#### R script for systematically fitting linear models to each of 
# the covariates we are interested in 

library(lme4)
library(MuMIn)

# read in the explanatory dataset
hw.exp = read.csv("./formatted data/master explanatory dataset/heatwaves explained var6.csv")

hw.exp = hw.exp %>% mutate(lake = as.factor(lake))

exp.vars = c("duration", "intensity_mean", "intensity_max", "intensity_var", "intensity_cumulative", 
             "intensity_mean_relThresh", "intensity_max_relThresh", "intensity_var_relThresh", "intensity_cumulative_relThresh", 
             "intensity_mean_abs", "intensity_max_abs", "intensity_var_abs", "intensity_cumulative_abs", 
             "rate_onset", "rate_decline", "precip", "doy", "PML.g440", "biomass", "biomass.during", "cumulative.load", "daily.load", 
  "stability.before", "stability.during", "stability.after", "daphnia.biomass.before", 
  "daphnia.biomass.during", "daphnia.biomass.after", "total.biomass.before", "total.biomass.during", "total.biomass.after", 
  "manual.chl.before", "manual.chl.during", "zoop.days.before", 
 "daphnia.length.before", "daphnia.length.during", 
  "daphnia.length.after","pchange.total.zoop", "abschange.total.zoop", "pchange.total.zoop.during.to.after", 
  "pchange.daphnia.length", "abschange.daphnia.zoop", "abschange.daphnia.zoop.during.to.after", "tp_ugL.before", 
 "tp_ugL.after", "tp_ugL.during","tn_ugL.before", 
 "tn_ugL.after", "tn_ugL.during")



pdf("./figures/explanatory variables/all variables.pdf", height = 4, width= 6, onefile = TRUE)


num.vars = length(exp.vars)

model.no.random <- data.frame(
  predictor = rep(NA, num.vars),     # Predictor variable names
  slope = rep(NA, num.vars),         # Slope of the model
  SE = rep(NA, num.vars),            # Standard error of the slope
  n = rep(NA, num.vars),             # Number of observations
  p_value = rep(NA, num.vars),       # P-value of the slope
  stringsAsFactors = FALSE
)


pdf("./figures/explanatory variables/all variables.pdf", height = 4, width= 6, onefile = TRUE)


for(i in 1:num.vars){
  
  var = exp.vars[i]
  formula <- as.formula(paste("percentChange ~", var))
  model = lm(formula, data = hw.exp)
  
  print(summary(model))
  
  model.no.random$predictor[i] = var
  model.no.random$p_value[i] = summary(model)$coefficients[2, "Pr(>|t|)"]
  model.no.random$r_squared[i] = round(summary(model)$r.squared, 2)
  model.no.random$n[i] = length(model$model[[1]])
  model.no.random$SE[i] = summary(model)$coefficients[2, "Std. Error"]
  model.no.random$slope[i] = summary(model)$coefficients[2, "Estimate"]
  
  
  
  
  print(ggplot(hw.exp, aes_string(x = var, y = "percentChange", fill = "lake"))+
          geom_point(size = 5, color = "black", shape = 21, stroke = 1, alpha = 0.9)+
          labs(y = "% change in chlorophyll-a")+
          theme_classic()+
          scale_fill_manual(values = c("R" = "#60BFCC", "L" = "#D9EEF3", "T" = "#544C34"),
                            labels = c("R" = "Peter", "L" = "Paul", "T" = "Tuesday"))+
          theme(legend.text = element_text(size = 16)) + 
          guides(fill = guide_legend(title = NULL), fill = guide_legend(title = NULL)))
  # guides(fill = guide_legend(override.aes = list(shape = 22), title = NULL)) )
  
  
}

dev.off()



#======================================================================================#
#### random effect for lake ####

num.vars = length(exp.vars)

model.with.random <- data.frame(
  predictor = rep(NA, num.vars),     # Predictor variable names
  slope = rep(NA, num.vars),         # Slope of the model
  SE = rep(NA, num.vars),            # Standard error of the slope
  n = rep(NA, num.vars),             # Number of observations
  p_value = rep(NA, num.vars),       # P-value of the slope
  marginal_R2 = rep(NA, num.vars),   # Marginal R-squared
  conditional_R2 = rep(NA, num.vars),   # Marginal R-squared
  stringsAsFactors = FALSE
)


for(i in 1:num.vars){
  
  var = exp.vars[i]
  formula <- as.formula(paste("percentChange ~", var, "+ (1 | lake)"))
  model = lmer(formula, data = hw.exp)
  
  # Extract fixed effect summary and p-value
  fixed_effects <- summary(model)$coefficients
  
  model.with.random$predictor[i] = var
  model.with.random$p_value[i] = coef(summary(model))[2, "Pr(>|t|)"]
  model.with.random$marginal_R2[i] = round(r.squaredGLMM(model)[1], 2)  # R2m (marginal R-squared) for fixed effects
  model.with.random$conditional_R2[i] = round(r.squaredGLMM(model)[2], 2)  # R2m (marginal R-squared) for fixed effects
  model.with.random$n[i] = length(model@frame$percentChange)
  model.with.random$SE[i] = fixed_effects[2, "Std. Error"]
  model.with.random$slope[i] = fixed_effects[2, "Estimate"]
  

}

dev.off()









### play around with models ###
formula <- as.formula(paste("percentChange ~ stability.during + pchange.total.zoop.during.to.after + cumulative.load", "+ (1 | lake)"))
model = lmer(formula, data = hw.exp)

summary(model)
r.squaredGLMM(model)


formula <- as.formula(paste("percentChange ~ cumulative.load + intensity_mean_abs:doy", "+ (1 | lake)"))
model = lmer(formula, data = hw.exp)

summary(model)
r.squaredGLMM(model)


### dredge based on a couple of top individual predictors, and those with 29 datapoints ####

model.29 = model.with.random %>% filter(n >= 29)

model.29.vars = model.29$predictor


global_model <- lmer(percentChange ~ total.biomass.after + duration + intensity_mean + intensity_max +
                       intensity_var + intensity_cumulative + intensity_mean_relThresh +
                       intensity_max_relThresh + intensity_var_relThresh + 
                       intensity_cumulative_relThresh + intensity_mean_abs + 
                       intensity_max_abs + intensity_var_abs + intensity_cumulative_abs +
                       rate_onset + rate_decline + precip + doy + PML.g440 + 
                       cumulative.load + daily.load + daphnia.biomass.after + 
                       (1 | lake), data = hw.exp)
summary(global_model)


model_set <- dredge(global_model)

hw.exp = hw.exp %>% filter(!is.na(percentChange))

global_model_simple <- lmer(percentChange ~ total.biomass.after + duration + intensity_mean +
                             rate_onset + rate_decline + precip + doy + 
                              cumulative.load + (1 | lake), data = hw.exp, na.action = "na.fail")

model_set <- dredge(global_model_simple)


test = lmer(percentChange ~ total.biomass.after + duration + intensity_mean +
       rate_onset + rate_decline + precip + doy + 
        (1 | lake), data = hw.exp, na.action = "na.fail")


test = lmer(percentChange ~ total.biomass.after + duration + intensity_mean +
       rate_onset + rate_decline + precip + doy + 
        (1 | lake), data = hw.exp, na.action = "na.fail")

summary(test)
r.squaredGLMM(test)





##### format tables #####

# read in the description information

desc = read.csv("./formatted data/master explanatory dataset/explanatory variables names.csv")

desc = desc %>% rename(code.name = predictor, predictor = code.name)

model.with.random = model.with.random %>% full_join(desc, by = c("predictor"))

model.with.random = model.with.random %>% select(code.name, n, slope, SE, p_value, marginal_R2, conditional_R2, include, table)

model.with.random = model.with.random %>% filter(include == TRUE)

model.with.random = model.with.random %>% mutate(p_value = round(p_value, 3), slope = round(slope, 2), SE = round(SE, 2))

model.with.random.hw = model.with.random %>% filter(table == "heatwave")
model.with.random.other = model.with.random %>% filter(table != "heatwave")



#### save the model results #####
write.csv(model.with.random.hw, "./results/explanatory model outputs/models with random effects heatwave variables.csv", row.names = FALSE)
write.csv(model.with.random.other, "./results/explanatory model outputs/models with random effects non-heatwave variables.csv", row.names = FALSE)



### make plots of variables that are statistically significant with the random effect
# absolute mean intensity
# absolute max intensity
# Schmidt stability during the heatwave
# day of year
# stability before the heatwave
# percent change in total zoop biomass after heatwave
# daphnia length following heatwave
# Schmidt stability following heatwave
# cumulative phosphorus added


a = ggplot(hw.exp, aes(x = (intensity_max_abs), y = (percentChange), fill = lake))+
  geom_point(size = 4, color = "black", shape = 21, stroke = 1, alpha = 0.95)+
  labs(x = expression("absolute max intensity (°C)"), y = "% change in chlorophyll-a")+
  theme_classic()+
  scale_fill_manual(values = c("R" = "#60BFCC", "L" = "#D9EEF3", "T" = "#544C34"),
                    labels = c("R" = "Peter", "L" = "Paul", "T" = "Tuesday"))+
  theme(legend.text = element_text(size = 16),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10)) + 
  guides(fill = guide_legend(title = NULL), fill = guide_legend(title = NULL))+
  guides(fill = guide_legend(override.aes = list(shape = 22), title = NULL)) 

b = ggplot(hw.exp, aes(x = (intensity_mean_abs), y = (percentChange), fill = lake))+
  geom_point(size = 4, color = "black", shape = 21, stroke = 1, alpha = 0.95)+
  labs(x = expression("absolute mean intensity (°C)"), y = "")+
  theme_classic()+
  scale_fill_manual(values = c("R" = "#60BFCC", "L" = "#D9EEF3", "T" = "#544C34"),
                    labels = c("R" = "Peter", "L" = "Paul", "T" = "Tuesday"))+
  theme(legend.text = element_text(size = 16),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10)) + 
  guides(fill = guide_legend(title = NULL), fill = guide_legend(title = NULL))+
  guides(fill = guide_legend(override.aes = list(shape = 22), title = NULL)) 

c = ggplot(hw.exp, aes(x = (stability.during), y = (percentChange), fill = lake))+
  geom_point(size = 4, color = "black", shape = 21, stroke = 1, alpha = 0.95)+
  labs(x = expression("Schmidt stability during the heatwave (J/m"^2*")"), y = "")+
  theme_classic()+
  scale_fill_manual(values = c("R" = "#60BFCC", "L" = "#D9EEF3", "T" = "#544C34"),
                    labels = c("R" = "Peter", "L" = "Paul", "T" = "Tuesday"))+
  theme(legend.text = element_text(size = 16),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10)) + 
  guides(fill = guide_legend(title = NULL), fill = guide_legend(title = NULL))+
  guides(fill = guide_legend(override.aes = list(shape = 22), title = NULL)) 


e = ggplot(hw.exp, aes(x = (doy), y = (percentChange), fill = lake))+
  geom_point(size = 4, color = "black", shape = 21, stroke = 1, alpha = 0.95)+
  labs(x = expression("Day of year"), y = "")+
  theme_classic()+
  scale_fill_manual(values = c("R" = "#60BFCC", "L" = "#D9EEF3", "T" = "#544C34"),
                    labels = c("R" = "Peter", "L" = "Paul", "T" = "Tuesday"))+
  theme(legend.text = element_text(size = 16),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10)) + 
  guides(fill = guide_legend(title = NULL), fill = guide_legend(title = NULL))+
  guides(fill = guide_legend(override.aes = list(shape = 22), title = NULL))


d = ggplot(hw.exp, aes(x = (stability.before), y = (percentChange), fill = lake))+
  geom_point(size = 4, color = "black", shape = 21, stroke = 1, alpha = 0.95)+
  labs(x = expression("Schmidt stability before the heatwave (J/m"^2*")"), y = "% change in chlorophyll-a")+
  theme_classic()+
  scale_fill_manual(values = c("R" = "#60BFCC", "L" = "#D9EEF3", "T" = "#544C34"),
                    labels = c("R" = "Peter", "L" = "Paul", "T" = "Tuesday"))+
  theme(legend.text = element_text(size = 16),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10)) + 
  guides(fill = guide_legend(title = NULL), fill = guide_legend(title = NULL))+
  guides(fill = guide_legend(override.aes = list(shape = 22), title = NULL))



f = ggplot(hw.exp, aes(x = (pchange.total.zoop.during.to.after), y = (percentChange), fill = lake))+
  geom_point(size = 4, color = "black", shape = 21, stroke = 1, alpha = 0.95)+
  labs(x = expression("% change in total zoop biomass after heatwave"), y = "% change in chlorophyll-a")+
  theme_classic()+
  scale_fill_manual(values = c("R" = "#60BFCC", "L" = "#D9EEF3", "T" = "#544C34"),
                    labels = c("R" = "Peter", "L" = "Paul", "T" = "Tuesday"))+
  theme(legend.text = element_text(size = 16),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10)) + 
  guides(fill = guide_legend(title = NULL), fill = guide_legend(title = NULL))+
  guides(fill = guide_legend(override.aes = list(shape = 22), title = NULL))


g = ggplot(hw.exp, aes(x = (daphnia.length.after), y = (percentChange), fill = lake))+
  geom_point(size = 4, color = "black", shape = 21, stroke = 1, alpha = 0.95)+
  labs(x = expression("Daphnia length following the heatwave (mm)"), y = "")+
  theme_classic()+
  scale_fill_manual(values = c("R" = "#60BFCC", "L" = "#D9EEF3", "T" = "#544C34"),
                    labels = c("R" = "Peter", "L" = "Paul", "T" = "Tuesday"))+
  theme(legend.text = element_text(size = 16),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10)) + 
  guides(fill = guide_legend(title = NULL), fill = guide_legend(title = NULL))+
  guides(fill = guide_legend(override.aes = list(shape = 22), title = NULL))



h = ggplot(hw.exp, aes(x = (stability.after), y = (percentChange), fill = lake))+
  geom_point(size = 4, color = "black", shape = 21, stroke = 1, alpha = 0.95)+
  labs(x = expression("Schmidt stability following the heatwave (J/m"^2*")"), y = "")+
  theme_classic()+
  scale_fill_manual(values = c("R" = "#60BFCC", "L" = "#D9EEF3", "T" = "#544C34"),
                    labels = c("R" = "Peter", "L" = "Paul", "T" = "Tuesday"))+
  theme(legend.text = element_text(size = 16),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10)) + 
  guides(fill = guide_legend(title = NULL), fill = guide_legend(title = NULL))+
  guides(fill = guide_legend(override.aes = list(shape = 22), title = NULL))



i = ggplot(hw.exp, aes(x = (cumulative.load), y = (percentChange), fill = lake))+
  geom_point(size = 4, color = "black", shape = 21, stroke = 1, alpha = 0.95)+
  labs(x = expression("cumulative P added (mg/m"^2*")"), y = "")+
  theme_classic()+
  scale_fill_manual(values = c("R" = "#60BFCC", "L" = "#D9EEF3", "T" = "#544C34"),
                    labels = c("R" = "Peter", "L" = "Paul", "T" = "Tuesday"))+
  theme(legend.text = element_text(size = 16),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10)) + 
  guides(fill = guide_legend(title = NULL), fill = guide_legend(title = NULL))+
  guides(fill = guide_legend(override.aes = list(shape = 22), title = NULL))



j = ggplot(hw.exp, aes(x = (tp_ugL.after), y = (percentChange), fill = lake))+
  geom_point(size = 4, color = "black", shape = 21, stroke = 1, alpha = 0.95)+
  labs(x = expression("TP after heatwave (ug/m"^2*")"), y = "")+
  theme_classic()+
  scale_fill_manual(values = c("R" = "#60BFCC", "L" = "#D9EEF3", "T" = "#544C34"),
                    labels = c("R" = "Peter", "L" = "Paul", "T" = "Tuesday"))+
  theme(legend.text = element_text(size = 16),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10)) + 
  guides(fill = guide_legend(title = NULL), fill = guide_legend(title = NULL))+
  guides(fill = guide_legend(override.aes = list(shape = 22), title = NULL))


png("./figures/manuscript draft 2024-11-11/S4 plots.png", height = 8, width = 11, res = 300, units = "in")
ggarrange(nrow = 4, ncol = 3, d, c, h, a, b, e, f, g, i, j, common.legend = TRUE)
dev.off()

