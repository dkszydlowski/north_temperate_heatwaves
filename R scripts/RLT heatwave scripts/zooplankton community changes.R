### evaluate if zooplankton community composition changes during heatwaves

library(tidyverse)
library(vegan)

# read in the community data
zoop.community = read.csv("./formatted data/zooplankton/zooplankton community heatwaves.csv")

# remove duplicated rows
zoop.community = zoop.community %>% distinct()

zoop.community = zoop.community %>% pivot_wider(names_from = taxon_name, values_from = biomass, id_cols = c("lakeid", "year4", "daynum", "period"))


# Convert list elements to numeric, replace NULL with NA
zoop.community <- zoop.community %>% 
  mutate(across(where(is.list), ~map_dbl(.x, ~ifelse(is.null(.x), NA, .x))))

# Drop non-numeric columns (lake, year, period, daynum)
zoop_matrix <- zoop.community %>% 
  select(-lakeid, -year4, -period, -daynum) %>% 
  as.data.frame() %>% 
  as.matrix()

# Replace NAs with zeros (if appropriate for your analysis)
zoop_matrix[is.na(zoop_matrix)] <- 0

# Calculate the Distance Matrix
zoop_dist <- vegdist(zoop_matrix, method = "bray")

# Perform NMDS
zoop_nmds <- metaMDS(zoop_dist, k = 2, trymax = 100)

# Basic plot
plot(zoop_nmds)

# Enhanced plot with ggplot2
zoop_nmds_scores <- as.data.frame(scores(zoop_nmds))
zoop_nmds_scores <- cbind(zoop.community[, c("lakeid", "year4", "period", "daynum")], zoop_nmds_scores)

ggplot(zoop_nmds_scores %>% filter(period != "after"), aes(x = NMDS1, y = NMDS2, color = lakeid, shape = period)) +
  geom_point(size = 4) +
  theme_minimal() +
  labs(title = "NMDS of Zooplankton Community Data",
       x = "NMDS1",
       y = "NMDS2") +
  scale_color_manual(values = c("R" = "#4AB5C4", "L" = "#ADDAE3", "T" = "#BAAD8D")) +
  theme(legend.position = "right")
