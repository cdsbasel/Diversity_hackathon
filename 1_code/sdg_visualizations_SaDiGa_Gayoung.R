# SDG hackathon
# Group: SaDiGa

library(tidyverse)

# read data
publications <- read_csv('0_data/publications_sdg.csv')
projects <- read_csv('0_data/projects_sdg.csv')


## Task 1 ----------------------------------------------------------------------
# data wrangling
publications_new <- publications %>%
  select(29:45) %>%
  colSums() %>%
  t() %>%
  as_tibble() %>%
  pivot_longer(everything())

projects_new <- projects %>%
  select(starts_with("SDG")) %>%
  colSums() %>%
  t() %>%
  as_tibble() %>%
  pivot_longer(everything())

data <- right_join(publications_new, projects_new, by = "name")

data <- data %>%
  rename(Publications = value.x,
         Projects = value.y) %>%
  pivot_longer(cols = Publications:Projects,
               names_to = "type",
               values_to = "frequency")

# recode SDGs
data$name <- recode(data$name,
                    "SDG-01" = "Poverty",
                    "SDG-02" = "Hunger",
                    "SDG-03" = "Health and Well-Being",
                    "SDG-04" = "Education",
                    "SDG-05" = "Gender Equality",
                    "SDG-06" = "Clean water",
                    "SDG-07" = "Clean energy",
                    "SDG-08" = "Employment",
                    "SDG-09" = "Innovation",
                    "SDG-10" = "Inequality",
                    "SDG-11" = "Sustainable Housing",
                    "SDG-12" = "Sustainable Consumption",
                    "SDG-13" = "Climate Action",
                    "SDG-14" = "Marine Resources",
                    "SDG-15" = "Life on Land",
                    "SDG-16" = "Institutions",
                    "SDG-17" = "Global Partnership")



# visualization 
ggplot(data, aes(fill=type, y=frequency, x=name)) + 
  geom_bar(position="dodge", stat="identity") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
  labs(
    title = "SDG Frequencies",
    subtitle = "Frequency of Publications and Projects related to SDGs at University of Basel",
    x = "SDG",
    y = "Frequency"
  ) +
  scale_fill_discrete("Type")


## Task 2 ----------------------------------------------------------------------

publications_psy <- publications %>%
  pivot_longer(starts_with("Org-")) %>%
  filter(value == TRUE) %>%
  group_by(name) %>%
  summarise_each(sum(12:27))


