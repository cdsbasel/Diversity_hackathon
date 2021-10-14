library(dbplyr)
library(tidyverse)
dat <- read_csv('1_data/unibas_diversity.csv')

## Part 1 ## 

p1 <- dat %>% 
  group_by(year) %>% 
  summarise(gender_prop= mean(gender_proportion, na.rm=TRUE)) %>% 
  ggplot(aes(x=year, y=gender_prop)) + 
  geom_point(color="#FF7F7F") + 
  geom_smooth(se=FALSE, method='lm', color="#A5D7D2") +
  labs(title='Gender Proportion Over Time', y='Proportion of Female Authors', x='Year') + 
  theme_light()
  
p2 = dat %>% 
  group_by(year) %>% 
  summarise(mean_age= mean(age_average, na.rm=TRUE)) %>% 
  ggplot(aes(x=year, y=mean_age)) + 
  geom_point() + 
  geom_smooth(se=FALSE, method='lm', color="#A5D7D2") +
  labs(title='Average Age Over Time', y='Average Age', x='Year') + 
  theme_light()

p3 = dat %>% 
  group_by(year) %>% 
  summarise(mean_nation= mean(nation_proportion, na.rm=TRUE)) %>% 
  ggplot(aes(x=year, y=mean_nation)) + 
  geom_point(color="#FF7F7F") + 
  geom_smooth(se=FALSE, method='lm', color="#A5D7D2") +
  labs(title='Proportion of Western Authors Over Times', y='Proportion of Western Authors', x='Year') + 
  theme_light()


p = p1 + p2 + p3

ggsave('Proportions Over Time.pdf', plot=p, path = '3_figures', width = 10, height=4)


## Part 2: Moderators ## 




