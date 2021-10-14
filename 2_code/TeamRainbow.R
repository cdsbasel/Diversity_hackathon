### CDS Diversity Hackathon #####
### Script to visualize diversity gaps over time #####

rm(list=ls())

#loading packages
library(tidyverse)

#loading file
diversity <- read.csv("1_data/unibas_diversity.csv")
head(diversity)
names(diversity)

### AGE GAP
ggplot(diversity, aes(age_first)) +
  geom_histogram(color = "white", fill = "orange") +
  theme_bw()

ggplot(diversity, aes(age_last)) +
  geom_histogram(color = "white", fill = "green") +
  theme_bw()

diversity %>% 
  select("year","age_first","age_last") %>% 
  group_by(year) %>% 
  summarize(meanage_first = mean(age_first, na.rm=T),
            meanage_last = mean(age_last, na.rm=T)) %>% 
  gather(position,age,-year) %>% 
  na.omit() %>% 
  mutate(pos = factor(position)) %>% 
  ggplot(aes(x=year,y=age, color=pos)) +
   geom_point() +
   ylim(c(35,60)) +
   labs(x='Publication year',y='Mean age', title='Mean age ~ author position over time') +
   theme_bw() +
  ggsave("3_figures/Age_Position_years.pdf")

### GENDER GAP

### NATIONALITY GAP

