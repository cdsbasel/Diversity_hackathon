require(tidyverse)


pub_data <- read_csv("1_data/unibas_diversity.csv")

pub_data$author_title = paste0(pub_data$author, "_", pub_data$title)
pub_data$author_year_title = paste0(pub_data$author, "_", pub_data$year, "_", pub_data$title)

pub_data_clean = pub_data %>% filter(!duplicated(author_year_title))

write_csv(pub_data_clean,"1_data/unibas_diversity_noduplicates.csv")


table(pub_data$author_year_title) %>% table()

tab = table(pub_data$author_year_title) 

a = pub_data %>% filter(author_year_title %in% names(tab)[tab == 2]) %>% 
  select(1:3, unibas, organization, category) %>% 
  arrange(author)

a %>% print(n = 20)

a$organization[1:4]
