
# DESCRIPTION -------------------------------------------------------------

# This script produces different visualizations that illustrate three diversity gaps (i.e., gender, age, nationality)
# in number of publications over time (2000-2020);

# DATA --------------------------------------------------------------------
library(tidyverse)
library(ggtext)
library(patchwork)
pub_data <- read_csv("1_data/unibas_diversity.csv")

# OVER TIME GENDER BY FACULTY ----------------------------------------------------------------

# data
gender_inst_time <- pub_data %>% 
  select(year, gender_proportion, `Org-Swiss Tropical and Public Health Institute`: `Org-Basel Institute on Governance`) %>%
  pivot_longer(!year:gender_proportion,names_to = "organisation_name", values_to = "applies") %>% 
  filter(applies == 1) %>% #only select those rows that could be classified
  mutate(organisation_name = case_when(organisation_name == "Org-Faculty of Theology" ~ "Faculty of\nTheology",
                                       organisation_name == "Org-Faculty of Science" ~ "Faculty of\nScience",
                                       organisation_name == "Org-Faculty of Law" ~ "Faculty of\nLaw",
                                       organisation_name == "Org-Faculty of Humanties and Social Sciences" ~ "Faculty of\nHumanties & Soc. Sci.",
                                       organisation_name == "Org-Faculty of Medicine"  ~ "Faculty of\nMedicine",
                                       organisation_name == "Org-Faculty of Business and Economics" ~ "Faculty of\nBusiness & Econ.",
                                       organisation_name == "Org-Faculty of Psychology" ~ "Faculty of\nPsychology")) %>% 
  filter(!is.na(organisation_name)) %>% # ignoring non-faculty organizations
  group_by(organisation_name, year) %>% 
  summarise(FEMALE = 100*mean(gender_proportion, na.rm = TRUE),
            MALE = 100 - FEMALE) %>%
  pivot_longer(FEMALE:MALE, names_to = "gender", values_to = "publication")

gender_inst_time$gender = factor(gender_inst_time$gender,levels = c("MALE","FEMALE"))


# Plot

charcoal <-  "#31424B"

label_num <- data.frame(organisation_name= unique(gender_inst_time$organisation_name), label= c("50%", rep("", 6)))
label_f <- data.frame(organisation_name= unique(gender_inst_time$organisation_name), label= c("FEMALE", rep("", 6)))
label_m <- data.frame(organisation_name= unique(gender_inst_time$organisation_name), label= c("MALE", rep("", 6)))

pA1 <- ggplot(gender_inst_time, aes(x= year, y=publication, fill=gender)) +
  geom_bar(width = 1,position = "stack", stat = "identity") +
  scale_fill_manual(values = c("FEMALE" = "#4ADABF", "MALE" = "#4E4BD8"))+
  geom_hline(yintercept = 50, linetype = "dashed", color = "white") +
  theme_void() +
  geom_label(data = label_num, aes(label=label), fill = NA,
             x = 2001, y = 51, hjust= 0, vjust = 0,  label.size = NA,
             family = "Oswald Bold", size = 5, color = "white" ,
             inherit.aes = FALSE) +
  geom_label(data = label_f, aes(label=label), fill = NA,
             y = 5, x = 2010, hjust=0.5,  label.size = NA,
             family = "Oswald Bold", size = 5, color = "white" ,
             inherit.aes = FALSE) +
  geom_label(data = label_m, aes(label=label), fill = NA,
             y = 95, x = 2010, hjust=0.5,  label.size = NA,
             family = "Oswald Bold", size = 5, color = "white" ,
             inherit.aes = FALSE) +
  scale_x_continuous(breaks = c(2000, 2020)) +
  scale_y_continuous(expand = c(0.01,0), breaks = c(0, 100)) +
  facet_wrap(.~ toupper(organisation_name), nrow = 1) +
  labs(title = "PERCENTAGE OF FEMALE AUTHORS", subtitle = "OVER TIME BY FACULTY: ALL AUTHORSHIPS COMBINED", x = "YEAR", y = "PERC.") +
  theme(legend.position = "none",
        axis.text.x = element_text(family = "Oswald", size = 9, colour = "grey60"),
        axis.text.y = element_text(family = "Oswald", size = 9, colour = "grey60"),
        panel.spacing = unit(1, "lines"),
        strip.text = element_text(family = "Oswald Bold", size = 11.5, colour = charcoal),
        axis.title.x = element_text(family = "Oswald Bold", size = 10, colour = "grey60"),
        axis.title.y = element_text(family = "Oswald Bold", size = 10, colour = "grey60", angle = 90),
        plot.title = element_text(face = "bold", family = "Oswald", hjust = 0, margin = margin(t = 15,b = 25), size = 20, color = charcoal),
        plot.subtitle = element_text(family = "Oswald", hjust = 0.008, margin = margin(b = 10), size = 17, color = charcoal),
        plot.caption =element_text(family = "Oswald ExtraLight", face = "italic", size = 6.5, hjust = 1, margin = margin(t = 5)))



# OVER TIME FEMALE 1ST and LAST AUTHOR BY FACULTY ----------------------------------------------------------------

# data
author_first <- pub_data %>% 
  filter(n_authors > 1) %>% 
  select(year, gender_first, `Org-Swiss Tropical and Public Health Institute`: `Org-Basel Institute on Governance`) %>%
  pivot_longer(!year:gender_first,names_to = "organisation_name", values_to = "applies") %>% 
  filter(applies == 1) %>% #only select those rows that could be classified
  mutate(organisation_name = case_when(organisation_name == "Org-Faculty of Theology" ~ "Faculty of\nTheology",
                                       organisation_name == "Org-Faculty of Science" ~ "Faculty of\nScience",
                                       organisation_name == "Org-Faculty of Law" ~ "Faculty of\nLaw",
                                       organisation_name == "Org-Faculty of Humanties and Social Sciences" ~ "Faculty of\nHumanties & Soc. Sci.",
                                       organisation_name == "Org-Faculty of Medicine"  ~ "Faculty of\nMedicine",
                                       organisation_name == "Org-Faculty of Business and Economics" ~ "Faculty of\nBusiness & Econ.",
                                       organisation_name == "Org-Faculty of Psychology" ~ "Faculty of\nPsychology")) %>% 
  filter(!is.na(organisation_name)) %>% # ignoring non-faculty organizations
  group_by(organisation_name, year) %>% 
  summarise(mean_f = 100*mean(gender_first, na.rm = TRUE))  %>% 
  mutate(author_type = "FIRST AUTHOR") 



author_last <- pub_data %>% 
  filter(n_authors > 1) %>% 
  select(year, gender_last, `Org-Swiss Tropical and Public Health Institute`: `Org-Basel Institute on Governance`) %>%
  pivot_longer(!year:gender_last,names_to = "organisation_name", values_to = "applies") %>% 
  filter(applies == 1) %>% #only select those rows that could be classified
  mutate(organisation_name = case_when(organisation_name == "Org-Faculty of Theology" ~ "Faculty of\nTheology",
                                       organisation_name == "Org-Faculty of Science" ~ "Faculty of\nScience",
                                       organisation_name == "Org-Faculty of Law" ~ "Faculty of\nLaw",
                                       organisation_name == "Org-Faculty of Humanties and Social Sciences" ~ "Faculty of\nHumanties & Soc. Sci.",
                                       organisation_name == "Org-Faculty of Medicine"  ~ "Faculty of\nMedicine",
                                       organisation_name == "Org-Faculty of Business and Economics" ~ "Faculty of\nBusiness & Econ.",
                                       organisation_name == "Org-Faculty of Psychology" ~ "Faculty of\nPsychology")) %>% 
  filter(!is.na(organisation_name)) %>% # ignoring non-faculty organizations
  group_by(organisation_name, year) %>% 
  summarise(mean_f = 100*mean(gender_last, na.rm = TRUE))  %>% 
  mutate(author_type = "LAST AUTHOR") 

author_dat <- bind_rows(author_first, author_last) 


label_num <- data.frame(organisation_name= unique(author_dat$organisation_name), label= c("50%", rep("", 6)))
label_f <- data.frame(organisation_name= unique(author_dat$organisation_name), label= c("FIRST AUTHOR", rep("", 6)))
label_m <- data.frame(organisation_name= unique(author_dat$organisation_name), label= c("LAST AUTHOR", rep("", 6)))




# plot



charcoal <-  "#31424B"
col_pal <- c("#4ADABF", "white")

pA2 <- author_dat %>% 
  ggplot(aes(x = year, y = mean_f, color = author_type ))+ 
  geom_line(size = 1.2 ) +
  geom_point(size = 2)+
  theme_void() +
  geom_label(data = label_num, aes(label=label), fill = NA,
             x = 2001, y = 51, hjust= 0, vjust = 0,  label.size = NA,
             family = "Oswald Bold", size = 5, color = "white" ,
             inherit.aes = FALSE) +
  geom_label(data = label_f, aes(label=label), fill = NA,
             y = 95, x = 2001, hjust= 0, vjust = 1,  label.size = NA,
             family = "Oswald Bold", size = 5, color = "#4ADABF" ,
             inherit.aes = FALSE) +
  geom_label(data = label_m, aes(label=label), fill = NA,
             y = 89, x = 2001, hjust= 0, vjust = 1,  label.size = NA,
             family = "Oswald Bold", size = 5, color = "white" ,
             inherit.aes = FALSE) +
  facet_wrap(toupper(organisation_name)~., nrow = 1)+
  scale_color_manual(values = col_pal) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "white") +
  scale_x_continuous(breaks = c(2000, 2020)) +
  scale_y_continuous(expand = c(0.01,0), breaks = c(0, 100)) +
  labs(title = "OVER TIME BY FACULTY: FIRST VERSUS LAST AUTHOR" , x = "YEAR", y = "PERC.",
       caption = "Data from the UNIBAS publication database.") +
  theme(legend.position = "none",
        axis.text.x = element_text(family = "Oswald", size = 10, colour = "grey60"),
        axis.text.y = element_text(family = "Oswald", size = 10, colour = "grey60",  margin = margin(l = 5, r = 5)),
        axis.title.x = element_text(family = "Oswald Bold", size = 10, colour = "grey60"),
        axis.title.y = element_text(family = "Oswald Bold", size = 10, colour = "grey60", angle = 90),
        panel.spacing = unit(2, "lines"),
        panel.background = element_rect(fill = "#4E4BD8"),
        plot.margin = unit(c(0,0.5,0,0), "cm"),
        strip.text.x = element_text(family = "Oswald Bold", size = 12, colour = charcoal, margin = margin(b = 3)),
        panel.border  = element_rect(colour = charcoal, fill = "NA", size = 0.75, linetype = "solid"),
        plot.title = element_text(family = "Oswald", hjust = 0, margin = margin(b = 10, t = 30),colour = charcoal, size = 17),
        plot.caption =element_text(family = "Oswald ExtraLight", face = "italic", size = 6.5, hjust = 1, margin = margin(t = 5)))



pA <- pA1/pA2





# OVER TIME NATION BY FACULTY ----------------------------------------------------------------

# data
nation_inst_time <- pub_data %>% 
  select(year, nation_proportion, `Org-Swiss Tropical and Public Health Institute`: `Org-Basel Institute on Governance`) %>%
  pivot_longer(!year:nation_proportion,names_to = "organisation_name", values_to = "applies") %>% 
  filter(applies == 1) %>% #only select those rows that could be classified
  mutate(organisation_name = case_when(organisation_name == "Org-Faculty of Theology" ~ "Faculty of\nTheology",
                                       organisation_name == "Org-Faculty of Science" ~ "Faculty of\nScience",
                                       organisation_name == "Org-Faculty of Law" ~ "Faculty of\nLaw",
                                       organisation_name == "Org-Faculty of Humanties and Social Sciences" ~ "Faculty of\nHumanties & Soc. Sci.",
                                       organisation_name == "Org-Faculty of Medicine"  ~ "Faculty of\nMedicine",
                                       organisation_name == "Org-Faculty of Business and Economics" ~ "Faculty of\nBusiness & Econ.",
                                       organisation_name == "Org-Faculty of Psychology" ~ "Faculty of\nPsychology")) %>% 
  filter(!is.na(organisation_name)) %>% # ignoring non-faculty organizations
  group_by(organisation_name, year) %>% 
  summarise(WEST = 100*mean(nation_proportion, na.rm = TRUE),
            NONWEST = 100 - WEST) %>%
  pivot_longer(WEST:NONWEST, names_to = "nation", values_to = "publication")

nation_inst_time$nation = factor(nation_inst_time$nation,levels = c("WEST", "NONWEST"))


# Plot

charcoal <-  "#31424B"

label_num <- data.frame(organisation_name= unique(nation_inst_time$organisation_name), label= c("50%", rep("", 6)))
label_f <- data.frame(organisation_name= unique(nation_inst_time$organisation_name), label= c("NON-WESTERN", rep("", 6)))
label_m <- data.frame(organisation_name= unique(nation_inst_time$organisation_name), label= c("WESTERN", rep("", 6)))

pB1 <- ggplot(nation_inst_time, aes(x= year, y=publication, fill=nation)) +
  geom_bar(width = 1,position = "stack", stat = "identity") +
  scale_fill_manual(values = c( "WEST" = "#0f4c5c", "NONWEST" = "#6ede8a"))+
  geom_hline(yintercept = 50, linetype = "dashed", color = "white") +
  theme_void() +
  geom_label(data = label_num, aes(label=label), fill = NA,
             x = 2001, y = 51, hjust= 0, vjust = 0,  label.size = NA,
             family = "Oswald Bold", size = 5, color = "white" ,
             inherit.aes = FALSE) +
  geom_label(data = label_f, aes(label=label), fill = NA,
             y = 5, x = 2010, hjust=0.5,  label.size = NA,
             family = "Oswald Bold", size = 5, color = "white" ,
             inherit.aes = FALSE) +
  geom_label(data = label_m, aes(label=label), fill = NA,
             y = 95, x = 2010, hjust=0.5,  label.size = NA,
             family = "Oswald Bold", size = 5, color = "white" ,
             inherit.aes = FALSE) +
  scale_x_continuous(breaks = c(2000, 2020)) +
  scale_y_continuous(expand = c(0.01,0), breaks = c(0, 100)) +
  facet_wrap(.~ toupper(organisation_name), nrow = 1) +
  labs(title = "PERCENTAGE OF NON-WESTERN AUTHORS", subtitle = "OVER TIME BY FACULTY: ALL AUTHORSHIPS COMBINED", x = "YEAR", y = "PERC.") +
  theme(legend.position = "none",
        axis.text.x = element_text(family = "Oswald", size = 9, colour = "grey60"),
        axis.text.y = element_text(family = "Oswald", size = 9, colour = "grey60"),
        panel.spacing = unit(1, "lines"),
        strip.text = element_text(family = "Oswald Bold", size = 11.5, colour = charcoal),
        axis.title.x = element_text(family = "Oswald Bold", size = 10, colour = "grey60"),
        axis.title.y = element_text(family = "Oswald Bold", size = 10, colour = "grey60", angle = 90),
        plot.title = element_text(face = "bold", family = "Oswald", hjust = 0, margin = margin(t = 15,b = 25), size = 20, color = charcoal),
        plot.subtitle = element_text(family = "Oswald", hjust = 0.008, margin = margin(b = 10), size = 17, color = charcoal),
        plot.caption =element_text(family = "Oswald ExtraLight", face = "italic", size = 6.5, hjust = 1, margin = margin(t = 5)))



# OVER TIME FEMALE 1ST and LAST AUTHOR BY FACULTY ----------------------------------------------------------------

# data
author_first <- pub_data %>% 
  filter(n_authors > 1) %>% 
  select(year, nation_first, `Org-Swiss Tropical and Public Health Institute`: `Org-Basel Institute on Governance`) %>%
  pivot_longer(!year:nation_first,names_to = "organisation_name", values_to = "applies") %>% 
  filter(applies == 1) %>% #only select those rows that could be classified
  mutate(organisation_name = case_when(organisation_name == "Org-Faculty of Theology" ~ "Faculty of\nTheology",
                                       organisation_name == "Org-Faculty of Science" ~ "Faculty of\nScience",
                                       organisation_name == "Org-Faculty of Law" ~ "Faculty of\nLaw",
                                       organisation_name == "Org-Faculty of Humanties and Social Sciences" ~ "Faculty of\nHumanties & Soc. Sci.",
                                       organisation_name == "Org-Faculty of Medicine"  ~ "Faculty of\nMedicine",
                                       organisation_name == "Org-Faculty of Business and Economics" ~ "Faculty of\nBusiness & Econ.",
                                       organisation_name == "Org-Faculty of Psychology" ~ "Faculty of\nPsychology")) %>% 
  filter(!is.na(organisation_name)) %>% # ignoring non-faculty organizations
  group_by(organisation_name, year) %>% 
  summarise(mean_f = 100-100*mean(nation_first, na.rm = TRUE))  %>% 
  mutate(author_type = "FIRST AUTHOR") 



author_last <- pub_data %>% 
  filter(n_authors > 1) %>% 
  select(year, nation_last, `Org-Swiss Tropical and Public Health Institute`: `Org-Basel Institute on Governance`) %>%
  pivot_longer(!year:nation_last,names_to = "organisation_name", values_to = "applies") %>% 
  filter(applies == 1) %>% #only select those rows that could be classified
  mutate(organisation_name = case_when(organisation_name == "Org-Faculty of Theology" ~ "Faculty of\nTheology",
                                       organisation_name == "Org-Faculty of Science" ~ "Faculty of\nScience",
                                       organisation_name == "Org-Faculty of Law" ~ "Faculty of\nLaw",
                                       organisation_name == "Org-Faculty of Humanties and Social Sciences" ~ "Faculty of\nHumanties & Soc. Sci.",
                                       organisation_name == "Org-Faculty of Medicine"  ~ "Faculty of\nMedicine",
                                       organisation_name == "Org-Faculty of Business and Economics" ~ "Faculty of\nBusiness & Econ.",
                                       organisation_name == "Org-Faculty of Psychology" ~ "Faculty of\nPsychology")) %>% 
  filter(!is.na(organisation_name)) %>% # ignoring non-faculty organizations
  group_by(organisation_name, year) %>% 
  summarise(mean_f = 100-100*mean(nation_last, na.rm = TRUE))  %>% 
  mutate(author_type = "LAST AUTHOR") 

author_dat <- bind_rows(author_first, author_last) 


label_num <- data.frame(organisation_name= unique(author_dat$organisation_name), label= c("50%", rep("", 6)))
label_f <- data.frame(organisation_name= unique(author_dat$organisation_name), label= c("FIRST AUTHOR", rep("", 6)))
label_m <- data.frame(organisation_name= unique(author_dat$organisation_name), label= c("LAST AUTHOR", rep("", 6)))




# plot



charcoal <-  "#31424B"
col_pal <- c("#6ede8a", "white")

pB2 <- author_dat %>% 
  ggplot(aes(x = year, y = mean_f, color = author_type ))+ 
  geom_line(size = 1.2 ) +
  geom_point(size = 2)+
  theme_void() +
  geom_label(data = label_num, aes(label=label), fill = NA,
             x = 2001, y = 51, hjust= 0, vjust = 0,  label.size = NA,
             family = "Oswald Bold", size = 5, color = "white" ,
             inherit.aes = FALSE) +
  geom_label(data = label_f, aes(label=label), fill = NA,
             y = 95, x = 2001, hjust= 0, vjust = 1,  label.size = NA,
             family = "Oswald Bold", size = 5, color = "#6ede8a" ,
             inherit.aes = FALSE) +
  geom_label(data = label_m, aes(label=label), fill = NA,
             y = 89, x = 2001, hjust= 0, vjust = 1,  label.size = NA,
             family = "Oswald Bold", size = 5, color = "white" ,
             inherit.aes = FALSE) +
  facet_wrap(toupper(organisation_name)~., nrow = 1)+
  scale_color_manual(values = col_pal) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "white") +
  scale_x_continuous(breaks = c(2000, 2020)) +
  scale_y_continuous(expand = c(0.01,0), breaks = seq(0, 100,100), limits = c(0,100)) +
  labs(title = "OVER TIME BY FACULTY: FIRST VERSUS LAST AUTHOR" , x = "YEAR", y = "PERC.",
       caption = "Data from the UNIBAS publication database.") +
  theme(legend.position = "none",
        axis.text.x = element_text(family = "Oswald", size = 10, colour = "grey60"),
        axis.text.y = element_text(family = "Oswald", size = 10, colour = "grey60",  margin = margin(l = 5, r = 5)),
        axis.title.x = element_text(family = "Oswald Bold", size = 10, colour = "grey60"),
        axis.title.y = element_text(family = "Oswald Bold", size = 10, colour = "grey60", angle = 90),
        panel.spacing = unit(2, "lines"),
        panel.background = element_rect(fill = "#0f4c5c"),
        plot.margin = unit(c(0,0.5,0,0), "cm"),
        strip.text.x = element_text(family = "Oswald Bold", size = 12, colour = charcoal, margin = margin(b = 3)),
        panel.border  = element_rect(colour = charcoal, fill = "NA", size = 0.75, linetype = "solid"),
        plot.title = element_text(family = "Oswald", hjust = 0, margin = margin(b = 10, t = 30),colour = charcoal, size = 17),
        plot.caption =element_text(family = "Oswald ExtraLight", face = "italic", size = 6.5, hjust = 1, margin = margin(t = 5)))



pB <- pB1/pB2


# DIVERSITY ---------------------------------------------------------------

# data: gender
gender_div <- pub_data %>% 
  filter(n_authors > 1) %>% 
  select(year, gender_diversity, `Org-Swiss Tropical and Public Health Institute`: `Org-Basel Institute on Governance`) %>%
  pivot_longer(!year:gender_diversity,names_to = "organisation_name", values_to = "applies") %>% 
  filter(applies == 1) %>% #only select those rows that could be classified
  mutate(organisation_name = case_when(organisation_name == "Org-Faculty of Theology" ~ "Faculty of\nTheology",
                                       organisation_name == "Org-Faculty of Science" ~ "Faculty of\nScience",
                                       organisation_name == "Org-Faculty of Law" ~ "Faculty of\nLaw",
                                       organisation_name == "Org-Faculty of Humanties and Social Sciences" ~ "Faculty of\nHumanties & Soc. Sci.",
                                       organisation_name == "Org-Faculty of Medicine"  ~ "Faculty of\nMedicine",
                                       organisation_name == "Org-Faculty of Business and Economics" ~ "Faculty of\nBusiness & Econ.",
                                       organisation_name == "Org-Faculty of Psychology" ~ "Faculty of\nPsychology")) %>% 
  filter(!is.na(organisation_name)) %>% # ignoring non-faculty organizations
  group_by(organisation_name, year) %>% 
  summarise(div = mean(gender_diversity, na.rm = TRUE)) 

# data: nation
nation_div <- pub_data %>% 
  filter(n_authors > 1) %>% 
  select(year, nation_diversity, `Org-Swiss Tropical and Public Health Institute`: `Org-Basel Institute on Governance`) %>%
  pivot_longer(!year:nation_diversity,names_to = "organisation_name", values_to = "applies") %>% 
  filter(applies == 1) %>% #only select those rows that could be classified
  mutate(organisation_name = case_when(organisation_name == "Org-Faculty of Theology" ~ "Faculty of\nTheology",
                                       organisation_name == "Org-Faculty of Science" ~ "Faculty of\nScience",
                                       organisation_name == "Org-Faculty of Law" ~ "Faculty of\nLaw",
                                       organisation_name == "Org-Faculty of Humanties and Social Sciences" ~ "Faculty of\nHumanties & Soc. Sci.",
                                       organisation_name == "Org-Faculty of Medicine"  ~ "Faculty of\nMedicine",
                                       organisation_name == "Org-Faculty of Business and Economics" ~ "Faculty of\nBusiness & Econ.",
                                       organisation_name == "Org-Faculty of Psychology" ~ "Faculty of\nPsychology")) %>% 
  filter(!is.na(organisation_name)) %>% # ignoring non-faculty organizations
  group_by(organisation_name, year) %>% 
  summarise(div = mean(nation_diversity, na.rm = TRUE)) 

# data: age
age_div <- pub_data %>% 
  filter(n_authors > 1) %>%  
  select(year, age_diversity, `Org-Swiss Tropical and Public Health Institute`: `Org-Basel Institute on Governance`) %>%
  pivot_longer(!year:age_diversity,names_to = "organisation_name", values_to = "applies") %>% 
  filter(applies == 1) %>% #only select those rows that could be classified
  mutate(organisation_name = case_when(organisation_name == "Org-Faculty of Theology" ~ "Faculty of\nTheology",
                                       organisation_name == "Org-Faculty of Science" ~ "Faculty of\nScience",
                                       organisation_name == "Org-Faculty of Law" ~ "Faculty of\nLaw",
                                       organisation_name == "Org-Faculty of Humanties and Social Sciences" ~ "Faculty of\nHumanties & Soc. Sci.",
                                       organisation_name == "Org-Faculty of Medicine"  ~ "Faculty of\nMedicine",
                                       organisation_name == "Org-Faculty of Business and Economics" ~ "Faculty of\nBusiness & Econ.",
                                       organisation_name == "Org-Faculty of Psychology" ~ "Faculty of\nPsychology")) %>% 
  filter(!is.na(organisation_name)) %>% # ignoring non-faculty organizations
  group_by(organisation_name, year) %>% 
  summarise(div = mean(age_diversity, na.rm = TRUE)) 






# plot: gender
pC1 <- ggplot(gender_div, aes(x= year, y=div)) +
  geom_bar(width = 1,position = "stack", stat = "identity", fill = "#4ADABF") +
  theme_void() +
  scale_x_continuous(breaks = c(2000, 2020), expand = c(0.01,0)) +
  scale_y_continuous(expand = c(0.01,0), breaks = seq(0, 0.25, 0.1), limits = c(0,0.25)) +
  facet_wrap(.~ toupper(organisation_name), nrow = 1) +
  labs(title = "AUTHOR DIVERSITY", subtitle = "OVER TIME BY FACULTY: GENDER", x = "YEAR", y = "DISPERTION") +
  theme(legend.position = "none",
        axis.text.x = element_text(family = "Oswald", size = 9, colour = "grey60"),
        axis.text.y = element_text(family = "Oswald", size = 9, colour = "grey60"),
        panel.spacing = unit(2, "lines"),
        # panel.border = element_rect(color = charcoal, fill = NA),
        strip.text = element_text(family = "Oswald Bold", size = 11.5, colour = charcoal),
        panel.grid.major.y = element_line(size = 0.2, colour = "grey60"),
        axis.title.x = element_text(family = "Oswald Bold", size = 10, colour = "grey60"),
        axis.title.y = element_text(family = "Oswald Bold", size = 10, colour = "grey60", angle = 90),
        plot.title = element_text(face = "bold", family = "Oswald", hjust = 0, margin = margin(t = 15,b = 25), size = 20, color = charcoal),
        plot.subtitle = element_text(family = "Oswald", hjust = 0.008, margin = margin(b = 10), size = 17, color = charcoal),
        plot.caption =element_text(family = "Oswald ExtraLight", face = "italic", size = 6.5, hjust = 1, margin = margin(t = 5)))





# plot: nation
pC2 <- ggplot(nation_div, aes(x= year, y=div)) +
  geom_bar(width = 1,position = "stack", stat = "identity", fill = "#0f4c5c") +
  theme_void() +
  scale_x_continuous(breaks = c(2000, 2020), expand = c(0.01,0)) +
  scale_y_continuous(expand = c(0.01,0), breaks = seq(0, 0.25, 0.1), limits = c(0,0.25)) +
  facet_wrap(.~ toupper(organisation_name), nrow = 1) +
  labs(subtitle = "OVER TIME BY FACULTY: NATIONALITY", x = "YEAR", y = "DISPERTION") +
  theme(legend.position = "none",
        axis.text.x = element_text(family = "Oswald", size = 9, colour = "grey60"),
        axis.text.y = element_text(family = "Oswald", size = 9, colour = "grey60"),
        panel.spacing = unit(2, "lines"),
        # panel.border = element_rect(color = charcoal, fill = NA),
        panel.grid.major.y = element_line(size = 0.2, colour = "grey60"),
        strip.text = element_text(family = "Oswald Bold", size = 11.5, colour = charcoal),
        axis.title.x = element_text(family = "Oswald Bold", size = 10, colour = "grey60"),
        axis.title.y = element_text(family = "Oswald Bold", size = 10, colour = "grey60", angle = 90),
        plot.title = element_text(face = "bold", family = "Oswald", hjust = 0, margin = margin(t = 15,b = 25), size = 20, color = charcoal),
        plot.subtitle = element_text(family = "Oswald", hjust = 0.008, margin = margin(b = 10, t = 30), size = 17, color = charcoal),
        plot.caption =element_text(family = "Oswald ExtraLight", face = "italic", size = 6.5, hjust = 1, margin = margin(t = 5)))





# plot: age
pC3 <- ggplot(age_div, aes(x= year, y=div)) +
  geom_bar(width = 1,position = "stack", stat = "identity", fill = "#ff5d8f") +
  theme_void() +
  scale_x_continuous(breaks = c(2000, 2020), expand = c(0.01,0)) +
  scale_y_continuous(expand = c(0.01,0), breaks = seq(0, 30, 10), limits = c(0,30)) +
  facet_wrap(.~ toupper(organisation_name), nrow = 1) +
  labs(subtitle = "OVER TIME BY FACULTY: AGE", x = "YEAR", y = "DISPERTION", 
       caption = "Data from the UNIBAS publication database. Excluding single author papers." ) +
  theme(legend.position = "none",
        axis.text.x = element_text(family = "Oswald", size = 9, colour = "grey60"),
        axis.text.y = element_text(family = "Oswald", size = 9, colour = "grey60"),
        panel.spacing = unit(2, "lines"),
        # panel.border = element_rect(color = charcoal, fill = NA),
        strip.text = element_text(family = "Oswald Bold", size = 11.5, colour = charcoal),
        axis.title.x = element_text(family = "Oswald Bold", size = 10, colour = "grey60"),
        panel.grid.major.y = element_line(size = 0.2, colour = "grey60"),
        axis.title.y = element_text(family = "Oswald Bold", size = 10, colour = "grey60", angle = 90),
        plot.title = element_text(face = "bold", family = "Oswald", hjust = 0, margin = margin(t = 15,b = 25), size = 20, color = charcoal),
        plot.subtitle = element_text(family = "Oswald", hjust = 0.008, margin = margin(b = 10, t = 30), size = 17, color = charcoal),
        plot.caption =element_text(family = "Oswald ExtraLight", face = "italic", size = 6.5, hjust = 1, margin = margin(t = 5)))



pC <- pC1/pC2/pC3


# SAVE PLOTS --------------------------------------------------------------

ggsave("3_figures/AB_pA.png", plot = pA ,dpi = 600, width = 45, height = 28, units = "cm")
ggsave("3_figures/AB_pB.png", plot = pB ,dpi = 600, width = 45, height = 28, units = "cm")
ggsave("3_figures/AB_pC.png", plot = pC ,dpi = 600, width = 45, height = 30, units = "cm")
