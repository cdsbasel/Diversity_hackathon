###

data <- read_csv(file.choose("~unibas_diversity.csv") )
library(tidyverse)
library(stringr)


## type of publication, gender across time

data.gender <- data %>% 
  group_by(year, category) %>%
  summarise(female = mean(gender_proportion, na.rm=TRUE) )

data.gender$male <- c(1-data.gender$female)

data.gender.long <- data.gender %>%
  pivot_longer(cols=c( female,male), names_to="gender", values_to ="proportion" )

ggplot(data=data.gender.long, 
       aes(x=year, y=proportion, color=gender)) +
  geom_point() + 
  facet_wrap(~category) + 
  ylim(0,1) +
  theme_minimal()

#### organizational unit

data.long <- data %>%
  pivot_longer(starts_with("Org."), names_to="Org", values_to="Org.value") %>%
  filter(Org.value==TRUE)

data.gender.org <- data.long %>% 
  group_by(year, category,Org) %>%
  summarise(female = mean(gender_proportion, na.rm=TRUE) )

data.gender.org$male <- c(1-data.gender.org$female)

data.gender.org.l <- data.gender.org %>%
  pivot_longer(cols=c( female,male), names_to="gender", values_to ="proportion" )
data.gender.org.l$Org <- str_remove(data.gender.org.l$Org,"Org." )


data.gender.org.l <- data.gender.org.l %>%
  filter(!Org=="Basel.Institute.on.Governance") #because little temporal data
  
ggplot(data=data.gender.org.l, 
       aes(x=year, y=proportion, color=gender)) +
  geom_point(size=.5) + 
  facet_grid(Org~category) + 
  ylim(0,1) +
  theme_minimal() +
  theme(strip.text.y = element_text(angle = 0, hjust=0), axis.text.x = element_text(angle=90, hjust=1)) +
  scale_y_continuous(breaks = c(0,1) ) + 
  scale_x_continuous(breaks= c(2000,2020)) 



