### CDS Diversity Hackathon #####
### Script to visualize diversity gaps over time #####

#loading packages
library(tidyverse)

#loading file
diversity <- read.csv("1_data/unibas_diversity.csv")
head(diversity)

### AGE GAP

### GENDER GAP

### NATIONALITY GAP
nation <- diversity %>% 
  select(year, nation_proportion)

nation$nat2 <- 1 - nation$nation_proportion

nation <- nation %>% 
  rename( western = nation_proportion,
          nonwestern = nat2
         )

nation <- nation %>% 
  group_by(year) %>% 
  summarise(western = mean(western, na.rm = TRUE),
            nonwestern = mean(nonwestern, na.rm = TRUE)
            )

nation <- nation %>% 
  pivot_longer( cols = c("western", "nonwestern"),
                names_to = "nationality",
                values_to = "percentage"
                )
barplot_n <- nation %>% 
  ggplot() +
  geom_bar(aes(x = year,
               y = percentage,
               fill = nationality,
               color = nationality),
           stat = "identity",
           position = position_dodge()
           )

barplot_n
