# SDG hackathon
# Group: GROUPPSEUDONYM

library(tidyverse)
library(waffle)

# read data
publications <- read_csv('0_data/publications_sdg.csv')
projects <- read_csv('0_data/projects_sdg.csv')


# select columns of interest
publications <- publications %>% select(type, doc_id, `SDG-01`:`SDG-17`) %>% mutate(type = "publications")
projects <- projects %>% select(type, id, `SDG-01`:`SDG-17`) %>% rename(doc_id = id) %>% mutate(type = "projects")

data <- rbind(publications, projects) 
data <- data %>% pivot_longer(cols =`SDG-01`:`SDG-17`, names_to = "sdg", values_to = "present") 

data <- data %>% group_by(type, sdg) %>% summarise(total_present = sum(present)) %>% ungroup() %>% 
  mutate(sdg = case_when(sdg == "SDG-01" ~ "Poverty",
                         sdg == "SDG-02" ~ "Hunger",
                         sdg == "SDG-03" ~ "Health",
                         sdg == "SDG-04" ~ "Edu",
                         sdg == "SDG-05" ~ "Gender",
                         sdg == "SDG-06" ~ "Water",
                         sdg == "SDG-07" ~ "Energy",
                         sdg == "SDG-08" ~ "Econ",
                         sdg == "SDG-09" ~ "Infra",
                         sdg == "SDG-10" ~ "Equal",
                         sdg == "SDG-11" ~ "Sustain.",
                         sdg == "SDG-12" ~ "Prod.",
                         sdg == "SDG-13" ~ "Climate",
                         sdg == "SDG-14" ~ "W.Life",
                         sdg == "SDG-15" ~ "L-Life",
                         sdg == "SDG-16" ~ "Peace",
                         sdg == "SDG-17" ~ "Patern"))
p <- data %>% 
  ggplot(aes(fill = type, values = total_present)) +
  theme_minimal() +
  geom_waffle(color = "light grey", size = .15, n_rows = 10, flip = T) +
  facet_wrap(~sdg, nrow = 1, strip.position = "bottom") +
  scale_x_discrete() + 
  labs(title = "Frequency of SDG-Related projects and publications at UNIBAS",
       x = "SDG",
       fill = "Type of publication",
       y = "Number of hits",
       caption = "number of projects and publications: 19'498"
       ) +
  scale_fill_manual(values = c("#A5D7D2", "#1EA5A5"))+
  theme(plot.title = element_text(size = 20, face = "bold", margin = margin(t = 20, b = 30)),
        axis.title.y = element_text(face = "bold", margin = margin(r = 10, l = 10)),
        axis.title.x = element_text(face = "bold", margin = margin(t = 10, b = 10)),
        axis.text.x = element_text( size = 12),
        legend.position = "top") +
  scale_y_continuous(labels = function(x) x * 10, # make this multiplyer the same as n_rows
                     expand = c(0,0))


ggsave(p, file="2_figures/sdg_frequencies_GirlsClub.pdf", device="pdf",dpi = 600,width = 45, height = 20, units = "cm") 


