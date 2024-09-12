### bar charts of zooplankton before and during heatwave

library(tidyverse)

# read in the community zooplankton dataset
zoop.com = read.csv("./formatted data/zooplankton/zooplankton community heatwaves.csv")


str(zoop.com)



# Summarize the data
summary_data <- zoop.com %>%
  group_by(event_index, taxon_name, lakeid, period) %>%
  summarise(total_biomass = sum(biomass, na.rm = TRUE)) %>%
  ungroup()

# Plot the data
ggplot(summary_data %>% filter(period != "after"), aes(x = factor(event_index), y = total_biomass, fill = taxon_name)) +
  facet_wrap(period~lakeid)+
  geom_bar(stat = "identity") +
  labs(x = "Event Index", y = "Total Biomass", fill = "Taxon Name") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))






# Create a separate data frame for each period
before_data <- summary_data %>% filter(period == "before")
after_data <- summary_data %>% filter(period == "during")

# Combine the data frames for plotting
plot_data <- bind_rows(
  before_data %>% mutate(position = -total_biomass),
  after_data %>% mutate(position = total_biomass)
)

# Plot the data
ggplot(plot_data, aes(x = position, y = factor(event_index), fill = taxon_name)) +
  facet_wrap(lakeid~period) +
  geom_bar(stat = "identity") +
  labs(x = "Event Index", y = "Total Biomass", fill = "Taxon Name") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))








#####



# Summarize the data
summary_data <- zoop.com %>%
  group_by(start.date.group, taxon_name, lakeid, period) %>%
  summarise(total_biomass = sum(biomass, na.rm = TRUE)) %>%
  ungroup()

# Prepare data for plotting
plot_data_log <- summary_data %>%
  mutate(Freq = ifelse(period == "before", -log10(total_biomass), log10(total_biomass)))

plot_data <- summary_data %>%
  mutate(Freq = ifelse(period == "before", -total_biomass, total_biomass))

# Define order of event_index if necessary
the_order <- unique(plot_data$start.date.group)


# log-transformed plot

# Plot the data
plot_data_log %>% filter(period != "after") %>% 
  ggplot(aes(x = factor(start.date.group), y = Freq, group = taxon_name, fill = taxon_name)) +
  geom_bar(stat = "identity", width = 0.75) +
 coord_flip() +
  #scale_x_discrete(limits = the_order) +
  # Scale y-axis and label with absolute values
  #scale_y_continuous(breaks = seq(-max(abs(plot_data$Freq)), max(abs(plot_data$Freq)), by = 1), 
  #                   labels = abs(seq(-max(abs(plot_data$Freq)), max(abs(plot_data$Freq)), by = 1))) +
  labs(x = "heatwave start date", y = "log-transformed total biomass", title = "Zooplankton biomass before and during heatwaves") +
  theme_classic() +
 facet_wrap(~lakeid)+
  theme(legend.position = "right",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        #panel.background = element_rect(fill =  "grey90"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed")
  # Custom colors if needed, change to your preference
# scale_fill_manual(values = c("red", "blue", "green", "purple", "orange", "brown", "blue", 
    #                        "blue", "blue", "blue"))




# normal plot


# Plot the data
plot_data %>% filter(period != "after") %>% 
  ggplot(aes(x = factor(start.date.group), y = Freq, group = taxon_name, fill = taxon_name)) +
  geom_bar(stat = "identity", width = 0.75) +
  coord_flip() +
  #scale_x_discrete(limits = the_order) +
  # Scale y-axis and label with absolute values
  #scale_y_continuous(breaks = seq(-max(abs(plot_data$Freq)), max(abs(plot_data$Freq)), by = 1), 
  #                   labels = abs(seq(-max(abs(plot_data$Freq)), max(abs(plot_data$Freq)), by = 1))) +
  labs(x = "heatwave start date", y = "total biomass", title = "Zooplankton biomass before and during heatwaves") +
  theme_classic() +
  facet_wrap(~lakeid)+
  theme(legend.position = "right",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        #panel.background = element_rect(fill =  "grey90"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed")
# Custom colors if needed, change to your preference
# scale_fill_manual(values = c("red", "blue", "green", "purple", "orange", "brown", "blue", 
#                        "blue", "blue", "blue"))

