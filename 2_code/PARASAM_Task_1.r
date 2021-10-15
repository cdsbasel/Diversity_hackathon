# dependencies
library(tidyverse)

# read data
data <- read_csv("1_data/unibas_diversity.csv")

# gender proportion over time
data %>% 
  select(year, gender_proportion) %>% 
  ggplot(aes(x = year, y = gender_proportion)) +
  geom_jitter(width = 5, alpha = 0.1)

data %>% 
  select(year, gender_proportion) %>% 
  group_by(year) %>% 
  summarize(prop_fem = mean(gender_proportion, na.rm = TRUE)) %>% 
  ggplot(aes(x = year, y = prop_fem)) +
  geom_point()

# age over time
data %>% 
  select(year, age_average) %>% 
  group_by(year) %>% 
  summarize(age = mean(age_average, na.rm = TRUE)) %>% 
  ggplot(aes(x = year, y = age)) +
  geom_point()

# nationality over time
data %>% 
  select(year, nation_proportion) %>% 
  group_by(year) %>% 
  summarize(western = mean(nation_proportion, na.rm = TRUE)) %>% 
  ggplot(aes(x = year, y = western)) +
  geom_point()







