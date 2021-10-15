# dependencies
library(tidyverse)
library(patchwork)

# read data
data <- read_csv("1_data/unibas_diversity.csv")

# select units with more than 1k publications
unit_larger <- data %>% 
  pivot_longer(starts_with("Org-"), names_to = "org_unit") %>%
  filter(value == TRUE) %>%
  count(org_unit) %>% 
  filter(n > 1000) %>% 
  pull(org_unit)

data %>% 
  pivot_longer(starts_with("Org-"), names_to = "org_unit") %>%
  filter(value == TRUE) %>%
  filter(org_unit %in% unit_larger) %>% 
  mutate(org_unit = str_remove(org_unit, "Org-")) %>% 
  group_by(year, org_unit) %>% 
  summarize(
    prop_female_all = mean(gender_proportion, na.rm = TRUE),
    prop_female_first = mean(gender_first, na.rm = TRUE),
    prop_female_last = mean(gender_last, na.rm = TRUE)
  ) %>% 
  pivot_longer(starts_with("prop_female_"), 
               names_to = "author_position",
               values_to = "prop_female") %>% 
  ggplot(
    aes(
      x = year, 
      y = prop_female, 
      group = author_position, 
      color = author_position
    )
  ) +
  geom_line() +
  geom_point() +
  facet_wrap(~org_unit)

# nationality
data %>% 
  pivot_longer(starts_with("Org-"), names_to = "org_unit") %>%
  filter(value == TRUE) %>%
  filter(org_unit %in% unit_larger) %>% 
  mutate(org_unit = str_remove(org_unit, "Org-")) %>% 
  group_by(year, org_unit) %>% 
  summarize(
    prop_western_all = mean(nation_proportion, na.rm = TRUE),
    prop_western_first = mean(nation_first, na.rm = TRUE),
    prop_western_last = mean(nation_last, na.rm = TRUE)
  ) %>% 
  pivot_longer(starts_with("prop_western_"), 
               names_to = "author_position",
               values_to = "prop_western") %>% 
  ggplot(
    aes(
      x = year, 
      y = prop_western, 
      group = author_position, 
      color = author_position
    )
  ) +
  geom_line() +
  geom_point() +
  facet_wrap(~org_unit)

# age
data %>% 
  pivot_longer(starts_with("Org-"), names_to = "org_unit") %>%
  filter(value == TRUE) %>%
  filter(org_unit %in% unit_larger) %>% 
  mutate(org_unit = str_remove(org_unit, "Org-")) %>% 
  group_by(year, org_unit) %>% 
  summarize(
    mean_age_all = mean(age_average, na.rm = TRUE),
    mean_age_first = mean(age_first, na.rm = TRUE),
    mean_age_last = mean(age_last, na.rm = TRUE)
  ) %>% 
  pivot_longer(starts_with("mean_age_"), 
               names_to = "author_position",
               values_to = "mean_age") %>% 
  ggplot(
    aes(
      x = year, 
      y = mean_age, 
      group = author_position, 
      color = author_position
    )
  ) +
  geom_line() +
  geom_point() +
  facet_wrap(~org_unit)


# combined plot
data_gender <- data %>% 
  pivot_longer(starts_with("Org-"), names_to = "org_unit") %>%
  filter(value == TRUE) %>%
  filter(org_unit %in% unit_larger) %>% 
  mutate(org_unit = str_remove(org_unit, "Org-")) %>% 
  group_by(year, org_unit) %>% 
  summarize(
    prop_female_all = mean(gender_proportion, na.rm = TRUE),
    prop_female_first = mean(gender_first, na.rm = TRUE),
    prop_female_last = mean(gender_last, na.rm = TRUE)
  ) %>% 
  pivot_longer(starts_with("prop_female_"), 
               names_to = "author_position",
               values_to = "prop_female") %>% 
  mutate(diversity_type = "gender") %>% 
  rename(diversity_value = prop_female) %>% 
  mutate(author_position = str_remove(author_position, "prop_female_"))

data_age <- data %>% 
  pivot_longer(starts_with("Org-"), names_to = "org_unit") %>%
  filter(value == TRUE) %>%
  filter(org_unit %in% unit_larger) %>% 
  mutate(org_unit = str_remove(org_unit, "Org-")) %>% 
  group_by(year, org_unit) %>% 
  summarize(
    mean_age_all = mean(age_average, na.rm = TRUE),
    mean_age_first = mean(age_first, na.rm = TRUE),
    mean_age_last = mean(age_last, na.rm = TRUE)
  ) %>% 
  pivot_longer(starts_with("mean_age_"), 
               names_to = "author_position",
               values_to = "mean_age") %>% 
  mutate(diversity_type = "age") %>% 
  rename(diversity_value = mean_age) %>% 
  mutate(author_position = str_remove(author_position, "mean_age_"))

data_nationality <- data %>% 
  pivot_longer(starts_with("Org-"), names_to = "org_unit") %>%
  filter(value == TRUE) %>%
  filter(org_unit %in% unit_larger) %>% 
  mutate(org_unit = str_remove(org_unit, "Org-")) %>% 
  group_by(year, org_unit) %>% 
  summarize(
    prop_western_all = mean(nation_proportion, na.rm = TRUE),
    prop_western_first = mean(nation_first, na.rm = TRUE),
    prop_western_last = mean(nation_last, na.rm = TRUE)
  ) %>% 
  pivot_longer(starts_with("prop_western_"), 
               names_to = "author_position",
               values_to = "prop_western") %>% 
  mutate(diversity_type = "nationality") %>% 
  rename(diversity_value = prop_western) %>% 
  mutate(author_position = str_remove(author_position, "prop_western_"))

data_diversity <- data_gender %>% 
  bind_rows(data_age) %>% 
  bind_rows(data_nationality)

data_diversity %>% 
  ggplot(
    aes(
      x = year, 
      y = diversity_value, 
      group = author_position, 
      color = author_position)) +
  geom_line() +
  geom_point() +
  facet_grid(
    rows = vars(diversity_type),
    cols = vars(org_unit),
    scales = "free_y"
  ) +
  labs(
    title = "Diversity measures by organizational unit",
    y = "diversity",
    colour = "author position"
  ) +
  theme(
    strip.text.x = element_text(angle = 90, hjust = 0),
    strip.background.x = element_rect(fill = NA, colour = NA)
  )
ggsave(
  "3_figures/PARASAM_task1mail_alternative_less_inst.pdf",
  width = 18,
  height = 10
)

plot_gender <- data_diversity %>% 
  filter(diversity_type == "gender") %>% 
  ggplot(
    aes(
      x = year, 
      y = diversity_value, 
      group = author_position, 
      color = author_position)) +
  geom_line() +
  geom_point() +
  facet_grid(
    cols = vars(diversity_type),
    rows = vars(org_unit),
  ) +
  theme(
    strip.text.y = element_blank()
  ) +
  labs(
    y = "diversity",
    colour = "author position"
  )
plot_nationality <- data_diversity %>% 
  filter(diversity_type == "nationality") %>% 
  ggplot(
    aes(
      x = year, 
      y = diversity_value, 
      group = author_position, 
      color = author_position)) +
  geom_line() +
  geom_point() +
  facet_grid(
    cols = vars(diversity_type),
    rows = vars(org_unit),
  )  +
  theme(
    axis.title.y = element_blank(),
    strip.text.y = element_blank()
  ) +
  labs(
    colour = "author position"
  )
plot_age <- data_diversity %>% 
  filter(diversity_type == "age") %>% 
  ggplot(
    aes(
      x = year, 
      y = diversity_value, 
      group = author_position, 
      color = author_position)) +
  geom_line() +
  geom_point() +
  facet_grid(
    cols = vars(diversity_type),
    rows = vars(org_unit),
  ) +
  theme(
    axis.title.y = element_blank(),
    strip.text.y = element_text(angle = 0, hjust = 0),
    strip.background.y = element_rect(colour = NA, fill = NA)
  ) +
  labs(
    colour = "author position"
  )
plot_gender + plot_nationality + plot_age +
  plot_layout(guides = 'collect') +
  plot_annotation(
    title = "Diversity measures by organizational unit"
  )
ggsave(
  "3_figures/PARASAM_task1mail_large_less_inst.pdf",
  width = 15,
  height = 15
)
