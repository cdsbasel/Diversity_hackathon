library(dbplyr)
library(tidyverse)
dat <- read_csv('1_data/unibas_diversity.csv')

summary(dat$n_authors)
hist(dat$n_authors, breaks = 2000, xlim = c(0,10))

# groups = (1, 2, 3, 4, 5, 5>)

dat$author_groups[dat$n_authors == 1] = '1'
dat$author_groups[dat$n_authors == 2] = '2'
dat$author_groups[dat$n_authors == 3] = '3'
dat$author_groups[dat$n_authors == 4] = '4'
dat$author_groups[dat$n_authors == 5] = '5'
dat$author_groups[dat$n_authors > 5] = '>5'

dat %>% 
  group_by(year) %>% 
  summarise(gender_prop= mean(gender_proportion, na.rm=TRUE)) %>% 
  ggplot(aes(x=year, y=gender_prop, color=author_groups)) + 
  geom_point() + 
  geom_smooth(se=FALSE, method='lm') +
  labs(title='Gender Proportion Over Time', y='Proportion of Female Authors', x='Year') + 
  theme_light()

