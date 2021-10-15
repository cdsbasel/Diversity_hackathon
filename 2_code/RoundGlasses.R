##### gender_position_years #####
### Script group TheRoundGlasses 

library(tidyverse)
library(ggpubr)

rm(list=ls())

dat <- read_csv('1_data/unibas_diversity.csv')
head(dat)
names(dat)
#View(dat)

p1 <- dat %>% 
  select("year","gender_proportion","gender_first","gender_last") %>% 
  na.omit() %>% 
  group_by(year) %>% 
   summarize(Author_first = mean(gender_first),
             Author_last = mean(gender_last)) %>% 
  pivot_longer(-year,names_to = "Position",values_to = "mean_prop_gender") %>% 
  ggplot(aes(x=year,y=mean_prop_gender, col=Position)) +
  geom_line(size=1) +
  geom_smooth(method=lm, size=0.4, alpha=0.4, linetype=20) +
  labs(x='Publication year', y='Proportion female authors', title='Gender split by author position across time', subtitle = 'All publications') +
  ylim(c(0,1)) +
  theme_bw() +
  theme(legend.position = 'bottom',
        title = element_text(size=9),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)) ,
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) 
        #ggsave("3_figures/GenderSplit_Position_Time_RoundGlasses.pdf")

p2 <- dat %>% 
  select("year","gender_proportion","gender_first","gender_last", "n_authors") %>% 
  na.omit() %>% 
  filter(n_authors > 1) %>% 
  group_by(year) %>% 
  summarize(Author_first = mean(gender_first),
            Author_last = mean(gender_last)) %>% 
  pivot_longer(-year,names_to = "Position",values_to = "mean_prop_gender") %>% 
  ggplot(aes(x=year,y=mean_prop_gender, col=Position)) +
  geom_line(size=1) +
  geom_smooth(method=lm, size=0.4, alpha=0.4, linetype=20) +
  labs(x='Publication year', y='Proportion female authors', title='Gender split by author position across time', subtitle = "Single-author publications omitted") +
  ylim(c(0,1)) +
  theme_bw() +
  theme(legend.position = 'bottom',
        title = element_text(size=9),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)) ,
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) 
  #ggsave("3_figures/GenderSplit_Position_Time_nosingle_RoundGlasses.pdf")

ggarrange(
  p1, p2, labels = c("A", "B"),
  common.legend = TRUE, legend = "bottom"
)
ggsave('3_figures/GenderSplit_Position_Time_pub_TeamRoundGlasses.pdf')


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
  pivot_longer(c(Author_first, Author_last),names_to='Position', values_to = 'mean_prop_gender') %>% 
  na.omit() %>% 
  ggplot(aes(x=year,y=mean_prop_gender, col=Position)) +
  geom_line() +
  labs(x='Publication year', y='Proportion female authors', title='Gender split by position and organization across time', subtitle = 'All publications') +
  ylim(c(0,1)) +
  theme_bw() +
  facet_wrap(~organisation, ncol=4) +
  theme(strip.text = element_text(size=4),
        axis.text = element_text(size=6),
        legend.position = 'bottom',
        title = element_text(size=10),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)) ,
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
  ggsave("3_figures/GenderSplit_Position_Time_Organization_RoundGlasses.pdf")

dat %>% 
  filter(n_authors > 1) %>% 
  pivot_longer(starts_with("Org-"),names_to = "organisation",values_to = "present",names_prefix = "Org-") %>% 
  filter(present) %>% 
  select("year","gender_first","gender_last","organisation") %>% 
  na.omit() %>% 
  group_by(year, organisation) %>% 
  summarize(Author_first = mean(gender_first),
            Author_last = mean(gender_last),
            n=n()) %>% 
  filter(n>10) %>% 
  pivot_longer(c(Author_first, Author_last),names_to='Position', values_to = 'mean_prop_gender') %>% 
  na.omit() %>% 
  ggplot(aes(x=year,y=mean_prop_gender, col=Position)) +
  geom_line() +
  #geom_smooth(method=lm, size=0.2, alpha=0.3, linetype=20) +
  labs(x='Publication year', y='Proportion female authors', title='Gender split by position and organization across time', subtitle = 'Single-author publications omitted') +
  ylim(c(0,1)) +
  theme_bw() +
  facet_wrap(~organisation, ncol=4) +
  theme(strip.text = element_text(size=4),
        axis.text = element_text(size=6),
        legend.position = 'bottom',
        title = element_text(size=10),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)) ,
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
  ggsave("3_figures/GenderSplit_Position_Time_Organization_nosingle_RoundGlasses.pdf")



