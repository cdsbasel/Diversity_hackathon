##### gender_position_years #####
### Script group TheRoundGlasses 

rm(list=ls())

library(tidyverse)

dat <- read_csv('1_data/unibas_diversity.csv')
head(dat)
names(dat)
View(dat)

dat %>% 
  select("year","gender_proportion","gender_first","gender_last") %>% 
  na.omit() %>% 
  group_by(year) %>% 
   summarize(Author_first = mean(gender_first),
             Author_last = mean(gender_last)) %>% 
  pivot_longer(-year,names_to = "pos",values_to = "mean_prop_gender") %>% 
  ggplot(aes(x=year,y=mean_prop_gender, col=pos)) +
  geom_line() +
  geom_smooth(method=lm, size=0.2, alpha=0.3, linetype=20) +
  labs(x='Publication year', y='Proportion female authors', title='Gender split by position across time') +
  ylim(c(0,1)) +
  theme_bw() +
  ggsave("GenderSplit_Position_Time_RoundGlasses.pdf")

dat %>% 
  pivot_longer(starts_with("Org-"),names_to = "organisation",values_to = "present",names_prefix = "Org-") %>% 
  filter(present) %>% 
  select("year","gender_first","gender_last","organisation") %>% 
  na.omit() %>% 
  group_by(year, organisation) %>% 
  summarize(Author_first = mean(gender_first),
            Author_last = mean(gender_last),
            n=n()) %>% 
  filter(n>10) %>% 
  pivot_longer(c(Author_first, Author_last),names_to='pos', values_to = 'mean_prop_gender') %>% 
  na.omit() %>% 
  ggplot(aes(x=year,y=mean_prop_gender, col=pos)) +
  geom_line() +
  #geom_smooth(method=lm, size=0.2, alpha=0.3, linetype=20) +
  labs(x='Publication year', y='Proportion female authors', title='Gender split by position and organization across time') +
  ylim(c(0,1)) +
  theme_bw() +
  facet_wrap(~organisation, ncol=4) +
  theme(strip.text = element_text(size=5),
        axis.text = element_text(size=6)) +
  ggsave("GenderSplit_Position_Time_Organization_RoundGlasses.pdf")



