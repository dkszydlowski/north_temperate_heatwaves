### try models with multiple predictors 
library(MuMIn)

global_model <- lmer(percentChange ~ PML.g440 + daily.load + cumulative.load + biomass + doy + duration + rate_onset + intensity_mean + (1 | lake), data = heatwaves.exp , na.action = "na.fail")


dredge_results <- dredge(global_model)


model <- lmer(percentChange ~ cumulative.load *doy + PML.g440 + duration + biomass + (1 | lake), data = heatwaves.exp)

r.squaredGLMM(model)

plot(model)

summary(model)


model <- lmer(percentChange ~ daily.load *intensity_mean*doy +(1 | lake), data = heatwaves.exp)
summary(model)
r.squaredGLMM(model)
