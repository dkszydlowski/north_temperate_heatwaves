# random sampling of slopes
# sensitivity test to determine at what n the mean converges to 0

slopes = slopes %>% mutate(random.period = "all other days")

mean.randomized.slopes <- data.frame(
  numIncluded = rep(NA, nrow(slopes)),
  mean.slopes = rep(NA, nrow(slopes))
)

sequence <- seq(1, nrow(slopes))

for(j in 1:10000){
set.seed(j)

for(i in 1:168){
  
  
  # Shuffle the sequence
  shuffled_sequence <- sample(sequence, i)
  
  slopes.random = slopes[shuffled_sequence, ]
  
 # print(mean(slopes.random$percent_change, na.rm = TRUE))
  
  mean.randomized.slopes$mean.slopes[i] = mean(slopes.random$percent_change, na.rm = TRUE)
  mean.randomized.slopes$numIncluded[i] = i
}

if(mean.randomized.slopes$mean.slopes[26] > 60){print(TRUE)
  print(j)
  break}

}
}


ggplot(mean.randomized.slopes, aes(x = numIncluded, y = mean.slopes))+
  geom_line()+
  theme_classic()+
  geom_smooth(method = "lm")+
  xlim(0, 500)+
  geom_vline(xintercept = 100, color = "red", size = 1)
