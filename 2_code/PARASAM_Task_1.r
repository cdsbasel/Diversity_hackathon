# dependencies
library(tidyverse)

# read data
data <- read_csv("1_data/unibas_diversity.csv")

# gender proportion over time


data %>% 
  select(year, gender_proportion) %>% 
  ggplot(aes(x = year, y = gender_proportion)) +
  geom_jitter(alpha = 0.1)

data %>% 
  select(year, gender_proportion) %>% 
  group_by(year) %>% 
  summarize(prop_fem = mean(gender_proportion, na.rm = TRUE)) %>% 
  ggplot(aes(x = year, y = prop_fem)) +
  geom_point()