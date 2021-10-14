library(dbplyr)
library(tidyverse)
dat <- read_csv('1_data/unibas_diversity.csv')


ggplot(data=dat, aes(x=year, y=gender_proportion)) + geom_smooth() + labs(title='Gender Proportion Over Time', y='Proportion of Female Authors', x='Year')
ggplot(data=dat, aes(x=year, y=age_average)) + geom_smooth() + labs(title='Average Age Over Time', y='Average Age', x='Year')
ggplot(data=dat, aes(x=year, y=nation_proportion)) + geom_smooth() + labs(title='Proportion of Western Authors Over Times', y='Proportion of Western Authors', x='Year')

ggsave('Proportions Over Time.pdf')
