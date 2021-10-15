# dependencies
library(tidyverse)

# read data
data <- read_csv("1_data/unibas_diversity.csv")

# gender proportion over time, by organizational unit
data %>% 
  pivot_longer(starts_with("Org-"), names_to = "org_unit") %>%
  filter(value == TRUE) %>%
  group_by(year, org_unit) %>% 
  summarize(prop_female = mean(gender_proportion, na.rm = TRUE)) %>% 
  ggplot(aes(x = year, y = prop_female, group = org_unit, color = org_unit)) +
  geom_line() +
  geom_point()

# select units with more than 1k publications
unit_larger <- data %>% 
  pivot_longer(starts_with("Org-"), names_to = "org_unit") %>%
  filter(value == TRUE) %>%
  count(org_unit) %>% 
  filter(n > 1000) %>% 
  pull(org_unit)

# plot only for larger organizational units
data %>% 
  pivot_longer(starts_with("Org-"), names_to = "org_unit") %>%
  filter(value == TRUE) %>%
  filter(org_unit %in% unit_larger) %>% 
  mutate(org_unit = str_remove(org_unit, "Org-")) %>% 
  group_by(year, org_unit) %>% 
  summarize(prop_female = mean(gender_proportion, na.rm = TRUE)) %>% 
  ggplot(aes(x = year, y = prop_female, group = org_unit, color = org_unit)) +
  geom_line() +
  geom_point() + 
  labs(
    title = "Proportion of female authors by organizational unit",
    subtitle = "only organizational units with more than 1000 publications considered",
    color = "Organizational unit",
    x = "Year",
    y = "Proportion of female authors"
  ) + 
  theme_minimal()
ggsave(
  "3_figures/PARASAM_author_gender_by_year_and_organizational_unit.pdf", 
  height = 10, 
  width = 15, 
  scale = 0.6
  )

