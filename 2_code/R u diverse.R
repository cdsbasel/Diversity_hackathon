### r-skript data vis part 1
##

data <- read.csv(file.choose("~unibas_diversity.csv") )

library(tidyverse)


data_1 <- data %>% filter(n_authors==1)

## 1=female ,  0=male

# Anzahl publications pro jahr und gender
table(data_1$year, data_1$gender_proportion)

data_year <- data_1 %>% group_by(year) %>% summarise(gender= mean(gender_proportion, na.rm=TRUE)  )

### nur 1 author
ggplot(data=data_year ,aes(x=year, y=gender)) +
  geom_line() + theme_bw () + ylim(0,1) +
  geom_hline(yintercept=0.5, color="black",linetype="dashed", size= .5) 


gender <- data %>% group_by(year) %>% summarise(gender= mean(gender_proportion, na.rm=TRUE)  )

## alle
ggplot(data=gender ,aes(x=year, y=gender)) +
  geom_line() + theme_bw () + ylim(0,1) +
  geom_hline(yintercept=0.5, color="black",linetype="dashed", size= .5) +
  ylab("proportion female") + labs( title="Gender gap in Unibas publications", subtitle="from 2000 to 2020") 

### age

data_age <- data %>% group_by(year) %>% summarise(age= mean(age_average, na.rm=TRUE), age.sd= sd(age_average, na.rm=TRUE)  )

ggplot(data=data_age ,aes(x=year, y=age)) +
  geom_point () + theme_bw() + ylim(20,70) + 
  ylab("average age") + labs( title="Age in Unibas publications", subtitle="from 2000 to 2020") 

data$age.cat[data$age_average <36] <- 1
data$age.cat[data$age_average >35 &data$age_average <51] <- 2
data$age.cat[data$age_average >50 &data$age_average <66] <- 3
data$age.cat[data$age_average >65] <- 4


data_age2 <- data %>% group_by(year, age.cat) %>% tally()

ggplot(data=data_age2 ,aes(x=year, y=n, color=as.factor(age.cat)) ) +
  geom_point () + theme_bw() + 
  ylab("number of publications") + labs( title="Age categories in Unibas publications", subtitle="from 2000 to 2020") 


### nationality
# 1= western

nation <- data %>% group_by(year) %>% summarise(western= mean(nation_proportion, na.rm=TRUE)  )

ggplot(data=nation ,aes(x=year, y=western)) +
  geom_line() + theme_bw () + ylim(0,1) +
  ylab("proportion western authors") + labs( title="Nationality gap in Unibas publications", subtitle="from 2000 to 2020") 




