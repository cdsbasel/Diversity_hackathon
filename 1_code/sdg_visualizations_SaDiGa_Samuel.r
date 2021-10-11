# SDG hackathon
# Group: GROUPPSEUDONYM

library(tidyverse)

# read data
publications <- read_csv('0_data/publications_sdg.csv')
projects <- read_csv('0_data/projects_sdg.csv')

##########
# plot 1
##########

# count occurences
pub = publications %>% select(starts_with("SDG")) %>% colSums() %>% t() %>% as_tibble() %>% pivot_longer(everything()) %>% mutate(type = "publication")
pro = projects %>% select(starts_with("SDG")) %>% colSums() %>% t() %>% as_tibble() %>% pivot_longer(everything()) %>% mutate(type = "project")
alltypes = bind_rows(pub, pro)

# plot publication frequencies
ggplot(data = pub) +
  geom_col(mapping = aes(x = name, y = value, fill = name)) +
  theme_minimal() +
  scale_fill_viridis_d() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    title = "SDG Frequencies",
    subtitle = "Number of publications related to SDGs at University of Basel",
    x = "SDG",
    y = "Frequency"
  )
ggsave(filename = '2_figures/sdg_frequencies_SaDiGa_publications.pdf')

# plot project frequencies
ggplot(data = pro) +
  geom_col(mapping = aes(x = name, y = value, fill = name)) +
  theme_minimal() +
  scale_fill_viridis_d() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    title = "SDG Frequencies",
    subtitle = "Number of projects related to SDGs at University of Basel",
    x = "SDG",
    y = "Frequency"
  )
ggsave(filename = '2_figures/sdg_frequencies_SaDiGa_projects.pdf')

# plot all frequencies
ggplot(data = alltypes) +
  geom_col(mapping = aes(x = name, y = value, fill = name)) +
  theme_minimal() +
  scale_fill_viridis_d() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    title = "SDG Frequencies",
    subtitle = "Number of projects and publications related to SDGs at University of Basel",
    x = "SDG",
    y = "Frequency"
  ) +
  facet_wrap(~type)
ggsave(filename = '2_figures/sdg_frequencies_SaDiGa_alltypes.pdf')


##########
# plot 2
##########


##########
# plot 3
##########

publications_all <- publications %>%
  pivot_longer(starts_with("Org-"), names_to = "Organisations") %>%
  filter(value == TRUE) %>%
  group_by(Organisations) %>%
  select(`SDG-01`:`SDG-17`) %>%
  summarise_all(sum) 

publications_total <- publications_all %>%
  pivot_longer(`SDG-01`:`SDG-17`, names_to = "SDG") %>% 
  group_by(Organisations) %>%
  mutate(value = value/sum(value))

# visualization for publications
ggplot(publications_total, aes(x= SDG, y=Organisations, fill = value)) +
  geom_tile() +
  theme_minimal() +
  scale_fill_viridis_c() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  labs(
    title = "Proportion of SDG Publications by Organisation"
  )
ggsave(filename = '2_figures/sdg_tileplot_SaDiGa_publications.pdf')

projects_test <- projects %>% 
  pivot_longer(starts_with("Org-"), names_to = "Organisations") %>% 
  filter(value == TRUE) %>% 
  group_by(Organisations) %>% 
  select(`SDG-01` : `SDG-17`) %>% 
  summarise_all(sum) %>% 
  ungroup() 

projects_test2 <- projects_test %>%   
  pivot_longer(`SDG-01` : `SDG-17`, names_to = "SDG") %>% 
  group_by(Organisations) %>% 
  mutate(value = value/sum(value))

ggplot(projects_test2, aes(x= SDG, y= Organisations, fill = value)) +
  geom_tile() +
  theme_minimal() +
  scale_fill_viridis_c() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  labs(
    title = "Proportion of SDG Projects by Organisation",
    x = "SDG",
    y = "Organisation"
  )
ggsave(filename = '2_figures/sdg_tileplot_SaDiGa_projects.pdf')



