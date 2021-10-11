# SDG hackathon
# Group: syr

library(tidyverse)
library(viridis)
#library(ggplot2)

# read data
publications <- read_csv('0_data/publications_sdg.csv')
projects <- read_csv('0_data/projects_sdg.csv')

# PLOT PUBLICATIONS
publications <- publications %>%
  select(`SDG-01`:`SDG-17`) %>%
  summarise_all(sum)

publications_long <- publications %>%
  pivot_longer(starts_with("SDG"),names_to="SDG",values_to="count")
  
pdf('2_figures/sdg_frequencies_publications_syr.pdf')
ggplot(publications_long, aes(x=SDG, y=count, fill=SDG))+
  geom_bar(stat = "identity")+
  labs(title="Number of Publications by SDG University of Basel",x="",y="Number of publications")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45)) +
  scale_colour_viridis_d() +
  theme(legend.position = "none")
dev.off()

# PLOT PROJECTS

projects <- projects %>%
  select(`SDG-01`:`SDG-17`) %>%
  summarise_all(sum)

projects_long <- projects %>%
  pivot_longer(starts_with("SDG"),names_to="SDG",values_to="count")

pdf('2_figures/sdg_frequencies_projects_syr.pdf')
ggplot(projects_long, aes(x=SDG, y=count, fill=SDG))+
  geom_bar(stat = "identity")+
  labs(title="Number of Projects by SDG University of Basel",x="",y="Number of publications")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45)) +
  scale_colour_viridis_d() +
  theme(legend.position = "none")
dev.off()