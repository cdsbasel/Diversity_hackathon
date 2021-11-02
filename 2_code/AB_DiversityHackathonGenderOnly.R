

# PACKAGES --------------------------------------------------------------------
library(tidyverse)
library(ggtext)
library(patchwork)
library(readxl)

pub_data <- read_csv("1_data/unibas_diversity.csv")
prof_data <- read_xlsx("1_data/akademisches_unibas.xlsx", sheet = "female_perc")
fund_data_a <- read_xlsx("1_data/forschungsfonds.xlsx", sheet = "forschungsfonds")
fund_data_b <- read_xlsx("1_data/forschungsfonds.xlsx", sheet = "forschungsfonds_clinical")
# PUBLICATION OVER TIME GENDER BY FACULTY ----------------------------------------------------------------

# data
gender_inst_time <- pub_data %>% 
  select(year, gender_proportion, `Org-Swiss Tropical and Public Health Institute`: `Org-Basel Institute on Governance`) %>%
  pivot_longer(!year:gender_proportion,names_to = "organisation_name", values_to = "applies") %>% 
  filter(applies == 1) %>% #only select those rows that could be classified
  mutate(organisation_name = case_when(organisation_name == "Org-Faculty of Theology" ~ "Theology",
                                       organisation_name == "Org-Faculty of Science" ~ "Natural Sciences",
                                       organisation_name == "Org-Faculty of Law" ~ "Law",
                                       organisation_name == "Org-Faculty of Humanties and Social Sciences" ~ "Humanities &\nSocial Sciences",
                                       organisation_name == "Org-Faculty of Medicine"  ~ "Medicine",
                                       organisation_name == "Org-Faculty of Business and Economics" ~ "Business &\nEconomics",
                                       organisation_name == "Org-Faculty of Psychology" ~ "Psychology")) %>% 
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
             family = "Oswald Bold", size = 7, color = "white" ,
             inherit.aes = FALSE) +
  geom_label(data = label_f, aes(label=label), fill = NA,
             y = 4, x = 2019.85, hjust= 1,  label.size = NA,
             family = "Oswald Bold", size = 9, color = "white" , # #4E4BD8
             inherit.aes = FALSE) +
  geom_label(data = label_m, aes(label=label), fill = NA,
             y = 96, x = 2019.85, hjust= 1,  label.size = NA,
             family = "Oswald Bold", size = 9, color ="white" , # #4ADABF
             inherit.aes = FALSE) +
  scale_x_continuous(breaks = c(2000, 2020)) +
  scale_y_continuous(expand = c(0.01,0), breaks = c(0, 100)) +
  facet_wrap(.~ toupper(organisation_name), nrow = 1) +
  labs(title = "PERCENTAGE OF FEMALE AUTHORS (2000 - 2020)", 
       x = "YEAR", y = "PERCENTAGE",
       caption = "Data from the University of Basel.") +
  theme(legend.position = "none",
        axis.text.x = element_text(family = "Oswald", size = 14, colour = "grey60"),
        axis.text.y = element_text(family = "Oswald", size = 14, colour = "grey60"),
        panel.spacing = unit(1, "lines"),
        strip.text = element_text(family = "Oswald Bold", size = 18, colour = charcoal),
        axis.title.x = element_text(family = "Oswald Bold", size = 18, colour = "grey60"),
        axis.title.y = element_text(family = "Oswald Bold", size = 18, colour = "grey60", angle = 90),
        plot.title = element_text(face = "bold", family = "Oswald", hjust = 0, margin = margin(t = 40,b = 25), size = 24, color = charcoal),
        plot.subtitle = element_text(family = "Oswald", hjust = 0.008, margin = margin(b = 10), size = 17, color = charcoal),
        plot.caption =element_text(family = "Oswald ExtraLight", size = 20, hjust = 1, margin = margin(t = 20, b = 3, r = 3)))


# EMPLOYMENT OVER TIME GENDER BY FACULTY ----------------------------------------------------------------

# data
gender_inst_time <- prof_data %>% 
  pivot_longer(!organisation_name:position,names_to = "year", values_to = "perc") %>% 
  mutate(position = toupper(position),
         year = as.numeric(year),
         organisation_name = case_when(organisation_name == "Phil.-Hist. Fakultät" ~ "Humanities &\nSocial Sciences",                 
                                       organisation_name =="Phil.-Nat. Fakultät"  ~"Natural Sciences",                
                                       organisation_name == "Medizinische Fakultät"  ~ "Medicine",              
                                       organisation_name == "Juristische Fakultät" ~ "Law",                
                                       organisation_name == "Fakultät für Psychologie"  ~"Psychology",           
                                       organisation_name == "Theologische Fakultät"  ~ "Theology",                
                                       organisation_name == "Wirtschaftswissenschaftliche Fakultät" ~ "Business &\nEconomics")) %>% 
  group_by(organisation_name, year) %>% 
  summarise(FEMALE = mean(perc),
            MALE = 100 - FEMALE) %>%
  pivot_longer(FEMALE:MALE, names_to = "gender", values_to = "employed")


missing_dat <- crossing(year = c(2000:2012),
                       gender = c("NA"),
                       organisation_name = unique(gender_inst_time$organisation_name),
                       employed = 100)

gender_inst_time <- bind_rows(gender_inst_time, missing_dat)
gender_inst_time$gender = factor(gender_inst_time$gender,levels = c("MALE","FEMALE", "NA"))

# Plot

charcoal <-  "#31424B"

label_num <- data.frame(organisation_name= unique(gender_inst_time$organisation_name), label= c("50%", rep("", 6)))
label_f <- data.frame(organisation_name= unique(gender_inst_time$organisation_name), label= c("FEMALE", rep("", 6)))
label_m <- data.frame(organisation_name= unique(gender_inst_time$organisation_name), label= c("MALE", rep("", 6)))

pB1 <- ggplot(gender_inst_time, aes(x= year, y=employed, fill=gender)) +
  geom_bar(width = 1,position = "stack", stat = "identity") +
  scale_fill_manual(values = c("FEMALE" = "#4ADABF", "MALE" = "#4E4BD8", "NA" = "grey"))+
  geom_hline(yintercept = 50, linetype = "dashed", color = "white") +
  theme_void() +
  geom_label(data = label_num, aes(label=label), fill = NA,
             x = 2013, y = 51, hjust= 0, vjust = 0,  label.size = NA,
             family = "Oswald Bold", size = 7, color = "white" ,
             inherit.aes = FALSE) +
  geom_label(data = label_f, aes(label=label), fill = NA,
             y = 4, x = 2019.85, hjust= 1,  label.size = NA,
             family = "Oswald Bold", size = 9, color = "white" , # #4E4BD8
             inherit.aes = FALSE) +
  geom_label(data = label_m, aes(label=label), fill = NA,
             y = 96, x = 2019.85, hjust= 1,  label.size = NA,
             family = "Oswald Bold", size = 9, color ="white" , # #4ADABF
             inherit.aes = FALSE) +
  scale_x_continuous(breaks = c(2000, 2013, 2020)) +
  scale_y_continuous(expand = c(0.01,0), breaks = c(0, 100)) +
  facet_wrap(.~ toupper(organisation_name), nrow = 1) +
  labs(title = "PERCENTAGE OF FEMALE FACULTY (2013 - 2020)", 
       x = "YEAR", y = "PERCENTAGE",
       caption = "Data from the University of Basel.") +
  theme(legend.position = "none",
        axis.text.x = element_text(family = "Oswald", size = 14, colour = "grey60"),
        axis.text.y = element_text(family = "Oswald", size = 14, colour = "grey60"),
        panel.spacing = unit(1, "lines"),
        strip.text = element_text(family = "Oswald Bold", size = 18, colour = charcoal),
        axis.title.x = element_text(family = "Oswald Bold", size = 18, colour = "grey60"),
        axis.title.y = element_text(family = "Oswald Bold", size = 18, colour = "grey60", angle = 90),
        plot.title = element_text(face = "bold", family = "Oswald", hjust = 0, margin = margin(t = 40,b = 25), size = 24, color = charcoal),
        plot.subtitle = element_text(family = "Oswald", hjust = 0.008, margin = margin(b = 10), size = 17, color = charcoal),
        plot.caption =element_text(family = "Oswald ExtraLight", size = 20, hjust = 1, margin = margin(t = 20, b = 3, r = 3)))


# PUBLICATION OVER TIME FEMALE 1ST and LAST AUTHOR BY FACULTY ----------------------------------------------------------------

author_first <- pub_data %>% 
  filter(n_authors > 1) %>% 
  select(year, gender_first, `Org-Swiss Tropical and Public Health Institute`: `Org-Basel Institute on Governance`) %>%
  pivot_longer(!year:gender_first,names_to = "organisation_name", values_to = "applies") %>% 
  filter(applies == 1) %>% #only select those rows that could be classified
  mutate(organisation_name = case_when(organisation_name == "Org-Faculty of Theology" ~ "Theology",
                                       organisation_name == "Org-Faculty of Science" ~ "Natural Sciences",
                                       organisation_name == "Org-Faculty of Law" ~ "Law",
                                       organisation_name == "Org-Faculty of Humanties and Social Sciences" ~ "Humanities &\nSocial Sciences",
                                       organisation_name == "Org-Faculty of Medicine"  ~ "Medicine",
                                       organisation_name == "Org-Faculty of Business and Economics" ~ "Business &\nEconomics",
                                       organisation_name == "Org-Faculty of Psychology" ~ "Psychology")) %>% 
  filter(!is.na(organisation_name)) %>% # ignoring non-faculty organizations
  group_by(organisation_name, year) %>% 
  summarise(mean_f = 100*mean(gender_first, na.rm = TRUE))  %>% 
  mutate(author_type = "FIRST AUTHOR") 



author_last <- pub_data %>% 
  filter(n_authors > 1) %>% 
  select(year, gender_last, `Org-Swiss Tropical and Public Health Institute`: `Org-Basel Institute on Governance`) %>%
  pivot_longer(!year:gender_last,names_to = "organisation_name", values_to = "applies") %>% 
  filter(applies == 1) %>% #only select those rows that could be classified
  mutate(organisation_name = case_when(organisation_name == "Org-Faculty of Theology" ~ "Theology",
                                       organisation_name == "Org-Faculty of Science" ~ "Natural Sciences",
                                       organisation_name == "Org-Faculty of Law" ~ "Law",
                                       organisation_name == "Org-Faculty of Humanties and Social Sciences" ~ "Humanities &\nSocial Sciences",
                                       organisation_name == "Org-Faculty of Medicine"  ~ "Medicine",
                                       organisation_name == "Org-Faculty of Business and Economics" ~ "Business &\nEconomics",
                                       organisation_name == "Org-Faculty of Psychology" ~ "Psychology")) %>% 
  filter(!is.na(organisation_name)) %>% # ignoring non-faculty organizations
  group_by(organisation_name, year) %>% 
  summarise(mean_f = 100*mean(gender_last, na.rm = TRUE))  %>% 
  mutate(author_type = "LAST AUTHOR") 

author_dat <- bind_rows(author_first, author_last) 


label_num <- data.frame(organisation_name= unique(author_dat$organisation_name), label= c("50%", rep("", 6)))
label_f <- data.frame(organisation_name= unique(author_dat$organisation_name), label= c("FIRST AUTHOR", rep("", 6)))
label_l <- data.frame(organisation_name= unique(author_dat$organisation_name), label= c("LAST AUTHOR", rep("", 6)))


# plot

charcoal <-  "#31424B"
col_pal <- c("#4ADABF", "white")

pA2 <- author_dat %>% 
  ggplot(aes(x = year, y = mean_f, color = author_type ))+ 
  geom_smooth(method = "lm",formula = y ~x, se = FALSE, fullrange=TRUE, size = 1.75) +
  geom_point(size = 3.5)+
  theme_void() +
  geom_label(data = label_num, aes(label=label), fill = NA,
             x = 2001, y = 51, hjust= 0, vjust = 0,  label.size = NA,
             family = "Oswald Bold", size = 7, color = "white" ,
             inherit.aes = FALSE) +
  geom_label(data = label_f, aes(label=label), fill = NA,
             y = 89, x = 2020, hjust= 1, vjust = 1,  label.size = NA,
             family = "Oswald Bold", size = 7, color = "#4ADABF" ,
             inherit.aes = FALSE) +
  geom_label(data = label_l, aes(label=label), fill = NA,
             y = 82, x = 2020, hjust= 1, vjust = 1,  label.size = NA,
             family = "Oswald Bold", size = 7, color = "white" ,
             inherit.aes = FALSE) +
  facet_wrap(toupper(organisation_name)~., nrow = 1)+
  scale_color_manual(values = col_pal) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "white") +
  scale_x_continuous(breaks = c(2000, 2020)) +
  scale_y_continuous(breaks = c(0, 100)) +
  labs(title = "PERCENTAGE OF FEMALE AUTHORS: FIRST VS. LAST (2000 - 2020)", 
       x = "YEAR", y = "PERCENTAGE") +
  theme(
    legend.position = "none",
    axis.text.x = element_text(family = "Oswald", size = 18, colour = "grey60"),
    axis.text.y = element_text(family = "Oswald", size = 18, colour = "grey60"),
    strip.text = element_text(family = "Oswald Bold", size = 18, colour = charcoal, margin = margin(b= 1)),
    axis.title.x = element_text(family = "Oswald Bold", size = 18, colour = "grey60"),
    axis.title.y = element_text(family = "Oswald Bold", size = 18, colour = "grey60", angle = 90),
    panel.spacing = unit(1.9, "lines"),
    panel.background = element_rect(fill = "#4E4BD8"),
    plot.margin = unit(c(0,0.5,0,0), "cm"),
    plot.title = element_text(face = "bold", family = "Oswald", hjust = 0, margin = margin(t = 40,b = 25), size = 24, color = charcoal),
    plot.subtitle = element_text(family = "Oswald", hjust = 0, margin = margin(b = 10, t = 30),colour = charcoal, size = 17),
    plot.caption =element_text(family = "Oswald ExtraLight", face = "italic", size = 6.5, hjust = 1, margin = margin(t = 5)))



# EMPLOYMENT OVER TIME PROFESSORS & NON-PROFESSORS BY FACULTY ----------------------------------------------------------------

# data
prof_data_l <- prof_data %>% 
  pivot_longer(!organisation_name:position,names_to = "year", values_to = "perc") %>% 
  mutate(position = toupper(position),
         year = as.numeric(year),
         organisation_name = case_when(organisation_name == "Phil.-Hist. Fakultät" ~ "Humanities &\nSocial Sciences",                 
                                       organisation_name =="Phil.-Nat. Fakultät"  ~"Natural Sciences",                
                                       organisation_name == "Medizinische Fakultät"  ~ "Medicine",              
                                       organisation_name == "Juristische Fakultät" ~ "Law",                
                                       organisation_name == "Fakultät für Psychologie"  ~"Psychology",           
                                       organisation_name == "Theologische Fakultät"  ~ "Theology",                
                                       organisation_name == "Wirtschaftswissenschaftliche Fakultät" ~ "Business &\nEconomics"))



prof_data_l$organisation_name <- factor(prof_data_l$organisation_name, levels = sort(unique(prof_data_l$organisation_name)))

label_num <- data.frame(organisation_name= unique(prof_data_l$organisation_name), label= c(rep("", 6), "50%"))
label_p <- data.frame(organisation_name= unique(prof_data_l$organisation_name), label= c(rep("", 6), "ACADEMIC STAFF"))
label_m <- data.frame(organisation_name= unique(prof_data_l$organisation_name), label= c(rep("", 6), "PROFESSORSHIPS"))



# plot
charcoal <-  "#31424B"
col_pal <- c("#4ADABF", "white")

pB2 <- prof_data_l %>% 
  ggplot(aes(x = year, y = perc, color = position))+ 
  geom_smooth(method = "lm",formula = y ~x, se = FALSE, fullrange=FALSE, size = 1.75) +
  geom_point(size = 3.5)+
  theme_void() +
  geom_label(data = label_num, aes(label=label), fill = NA,
             x = 2001, y = 51, hjust= 0, vjust = 0,  label.size = NA,
             family = "Oswald Bold", size = 6, color = "white" ,
             inherit.aes = FALSE) +
  geom_label(data = label_p, aes(label=label), fill = NA,
             y = 89, x = 2020, hjust= 1, vjust = 1,  label.size = NA,
             family = "Oswald Bold", size = 6, color = "#4ADABF" ,
             inherit.aes = FALSE) +
  geom_label(data = label_m, aes(label=label), fill = NA,
             y = 82, x = 2020, hjust= 1, vjust = 1,  label.size = NA,
             family = "Oswald Bold", size = 6, color = "white" ,
             inherit.aes = FALSE) +
  facet_wrap(toupper(organisation_name)~., nrow = 1)+
  scale_color_manual(values = col_pal) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "white") +
  scale_x_continuous(breaks = c(2000, 2020), limits = c(2000,2020)) +
  scale_y_continuous(breaks = c(0, 100), limits = c(0,100)) +
  labs(title = "PERCENTAGE OF FEMALES EMPLOYED: ACADEMIC STAFF VS. PROFESSORSHIPS (2013 - 2020)", 
       x = "YEAR", y = "PERCENTAGE",
       caption = "Data from the University of Basel. Lines represent the predictions from linear regressions") +
  theme(
    legend.position = "none",
    axis.text.x = element_text(family = "Oswald", size = 14, colour = "grey60"),
    axis.text.y = element_text(family = "Oswald", size = 14, colour = "grey60"),
    strip.text = element_text(family = "Oswald Bold", size = 18, colour = charcoal, margin = margin(b= 1)),
    axis.title.x = element_text(family = "Oswald Bold", size = 18, colour = "grey60"),
    axis.title.y = element_text(family = "Oswald Bold", size = 18, colour = "grey60", angle = 90),
    panel.spacing = unit(1.9, "lines"),
    panel.background = element_rect(fill = "#4E4BD8"),
    plot.margin = unit(c(0,0.5,0,0), "cm"),
    plot.title = element_text(face = "bold", family = "Oswald", hjust = 0, margin = margin(t = 40,b = 25), size = 24, color = charcoal),
    plot.subtitle = element_text(family = "Oswald", hjust = 0, margin = margin(b = 10, t = 30),colour = charcoal, size = 17),
    plot.caption =element_text(family = "Oswald ExtraLight", size = 20, hjust = 1, margin = margin(t = 20, b = 3, r = 3)))


# FUNDING OVER TIME : CLINICAL VS. THE OTHER FACULTIES (?)
# data
gender_inst_time <- pub_data %>% 
  select(year, gender_proportion, `Org-Swiss Tropical and Public Health Institute`: `Org-Basel Institute on Governance`) %>%
  pivot_longer(!year:gender_proportion,names_to = "organisation_name", values_to = "applies") %>% 
  filter(applies == 1) %>% #only select those rows that could be classified
  mutate(organisation_name = case_when(organisation_name == "Org-Faculty of Theology" ~ "Theology",
                                       organisation_name == "Org-Faculty of Science" ~ "Natural Sciences",
                                       organisation_name == "Org-Faculty of Law" ~ "Law",
                                       organisation_name == "Org-Faculty of Humanties and Social Sciences" ~ "Humanities &\nSocial Sciences",
                                       organisation_name == "Org-Faculty of Medicine"  ~ "Medicine",
                                       organisation_name == "Org-Faculty of Business and Economics" ~ "Business &\nEconomics",
                                       organisation_name == "Org-Faculty of Psychology" ~ "Psychology")) %>% 
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
             family = "Oswald Bold", size = 7, color = "white" ,
             inherit.aes = FALSE) +
  geom_label(data = label_f, aes(label=label), fill = NA,
             y = 4, x = 2019.85, hjust= 1,  label.size = NA,
             family = "Oswald Bold", size = 9, color = "white" , # #4E4BD8
             inherit.aes = FALSE) +
  geom_label(data = label_m, aes(label=label), fill = NA,
             y = 96, x = 2019.85, hjust= 1,  label.size = NA,
             family = "Oswald Bold", size = 9, color ="white" , # #4ADABF
             inherit.aes = FALSE) +
  scale_x_continuous(breaks = c(2000, 2020)) +
  scale_y_continuous(expand = c(0.01,0), breaks = c(0, 100)) +
  facet_wrap(.~ toupper(organisation_name), nrow = 1) +
  labs(title = "PERCENTAGE OF FEMALE AUTHORS (2000 - 2020)", 
       x = "YEAR", y = "PERCENTAGE",
       caption = "Data from the University of Basel.") +
  theme(legend.position = "none",
        axis.text.x = element_text(family = "Oswald", size = 14, colour = "grey60"),
        axis.text.y = element_text(family = "Oswald", size = 14, colour = "grey60"),
        panel.spacing = unit(1, "lines"),
        strip.text = element_text(family = "Oswald Bold", size = 18, colour = charcoal),
        axis.title.x = element_text(family = "Oswald Bold", size = 18, colour = "grey60"),
        axis.title.y = element_text(family = "Oswald Bold", size = 18, colour = "grey60", angle = 90),
        plot.title = element_text(face = "bold", family = "Oswald", hjust = 0, margin = margin(t = 40,b = 25), size = 24, color = charcoal),
        plot.subtitle = element_text(family = "Oswald", hjust = 0.008, margin = margin(b = 10), size = 17, color = charcoal),
        plot.caption =element_text(family = "Oswald ExtraLight", size = 20, hjust = 1, margin = margin(t = 20, b = 3, r = 3)))





# FUNDING OVER TIME: GENERAL ----------------------------------------------------------------

# data

colnames(fund_data_a)[1] <- "categ"
colnames(fund_data_b)[1] <- "categ"

fund_data_a <- fund_data_a %>%filter(categ == "Aufnahmen_Percentage") %>% mutate(faculty = "rest")
fund_data_b <- fund_data_b %>%filter(categ == "Aufnahmen_Percentage")%>% mutate(faculty = "clinical")


gender_inst_time <- fund_data_a %>% 
  pivot_longer(!c(categ, faculty),names_to = "year", values_to = "perc") %>% 
  mutate(year = as.numeric(year)) %>% 
  group_by(year) %>% 
  summarise(FEMALE = perc,
            MALE = 100 - FEMALE) %>%
  pivot_longer(FEMALE:MALE, names_to = "gender", values_to = "funding")

missing_dat <- crossing(year = c(2000:2007),
                        gender = c("NA"),
                        funding = 100)

gender_inst_time <- bind_rows(gender_inst_time, missing_dat)
gender_inst_time$gender = factor(gender_inst_time$gender,levels = c("MALE","FEMALE", "NA"))

# Plot

charcoal <-  "#31424B"

label_num <- data.frame(label= "50%")
label_f <- data.frame(label= "FEMALE")
label_m <- data.frame(label= "MALE")

pC1 <- ggplot(gender_inst_time, aes(x= year, y=funding, fill=gender)) +
  geom_bar(width = 1,position = "stack", stat = "identity") +
  scale_fill_manual(values = c("FEMALE" = "#4ADABF", "MALE" = "#4E4BD8", "NA" = "grey"))+
  geom_hline(yintercept = 50, linetype = "dashed", color = "white") +
  theme_void() +
  geom_label(data = label_num, aes(label=label), fill = NA,
             x = 2009, y = 51, hjust= 0, vjust = 0,  label.size = NA,
             family = "Oswald Bold", size = 7, color = "white" ,
             inherit.aes = FALSE) +
  geom_label(data = label_f, aes(label=label), fill = NA,
             y = 4, x = 2019.85, hjust= 1,  label.size = NA,
             family = "Oswald Bold", size = 9, color = "white" , # #4E4BD8
             inherit.aes = FALSE) +
  geom_label(data = label_m, aes(label=label), fill = NA,
             y = 96, x = 2019.85, hjust= 1,  label.size = NA,
             family = "Oswald Bold", size = 9, color ="white" , # #4ADABF
             inherit.aes = FALSE) +
  scale_x_continuous(breaks = c(2000, 2020)) +
  scale_y_continuous(expand = c(0.01,0), breaks = c(0, 100)) +
  labs(title = "PERCENTAGE OF FEMALE FUNDING: GENERAL (2008 - 2020)", 
       x = "YEAR", y = "PERCENTAGE",
       caption = "Data from the University of Basel.") +
  theme(legend.position = "none",
        axis.text.x = element_text(family = "Oswald", size = 14, colour = "grey60"),
        axis.text.y = element_text(family = "Oswald", size = 14, colour = "grey60"),
        panel.spacing = unit(1, "lines"),
        strip.text = element_text(family = "Oswald Bold", size = 18, colour = charcoal),
        axis.title.x = element_text(family = "Oswald Bold", size = 18, colour = "grey60"),
        axis.title.y = element_text(family = "Oswald Bold", size = 18, colour = "grey60", angle = 90),
        plot.title = element_text(face = "bold", family = "Oswald", hjust = 0, margin = margin(t = 40,b = 25), size = 24, color = charcoal),
        plot.subtitle = element_text(family = "Oswald", hjust = 0.008, margin = margin(b = 10), size = 17, color = charcoal),
        plot.caption =element_text(family = "Oswald ExtraLight", size = 20, hjust = 1, margin = margin(t = 20, b = 3, r = 3)))


# FUNDING OVER TIME: CLINICAL ----------------------------------------------------------------

# data

colnames(fund_data_b)[1] <- "categ"

fund_data_b <- fund_data_b %>%filter(categ == "Aufnahmen_Percentage")%>% mutate(faculty = "clinical")


gender_inst_time <- fund_data_b %>% 
  pivot_longer(!c(categ, faculty),names_to = "year", values_to = "perc") %>% 
  mutate(year = as.numeric(year)) %>% 
  group_by(year) %>% 
  summarise(FEMALE = perc,
            MALE = 100 - FEMALE) %>%
  pivot_longer(FEMALE:MALE, names_to = "gender", values_to = "funding")

missing_dat <- crossing(year = c(2000:2009),
                        gender = c("NA"),
                        funding = 100)

gender_inst_time <- bind_rows(gender_inst_time, missing_dat)
gender_inst_time$gender = factor(gender_inst_time$gender,levels = c("MALE","FEMALE", "NA"))

# Plot

charcoal <-  "#31424B"

label_num <- data.frame(label= "50%")
label_f <- data.frame(label= "FEMALE")
label_m <- data.frame(label= "MALE")

pC2 <- ggplot(gender_inst_time, aes(x= year, y=funding, fill=gender)) +
  geom_bar(width = 1,position = "stack", stat = "identity") +
  scale_fill_manual(values = c("FEMALE" = "#4ADABF", "MALE" = "#4E4BD8", "NA" = "grey"))+
  geom_hline(yintercept = 50, linetype = "dashed", color = "white") +
  theme_void() +
  geom_label(data = label_num, aes(label=label), fill = NA,
             x = 2011, y = 51, hjust= 0, vjust = 0,  label.size = NA,
             family = "Oswald Bold", size = 7, color = "white" ,
             inherit.aes = FALSE) +
  geom_label(data = label_f, aes(label=label), fill = NA,
             y = 4, x = 2019.85, hjust= 1,  label.size = NA,
             family = "Oswald Bold", size = 9, color = "white" , # #4E4BD8
             inherit.aes = FALSE) +
  geom_label(data = label_m, aes(label=label), fill = NA,
             y = 96, x = 2019.85, hjust= 1,  label.size = NA,
             family = "Oswald Bold", size = 9, color ="white" , # #4ADABF
             inherit.aes = FALSE) +
  scale_x_continuous(breaks = c(2000, 2020)) +
  scale_y_continuous(expand = c(0.01,0), breaks = c(0, 100)) +
  labs(title = "PERCENTAGE OF FEMALE FUNDING: CLINICAL (2010 - 2020)", 
       x = "YEAR", y = "PERCENTAGE",
       caption = "Data from the University of Basel.") +
  theme(legend.position = "none",
        axis.text.x = element_text(family = "Oswald", size = 14, colour = "grey60"),
        axis.text.y = element_text(family = "Oswald", size = 14, colour = "grey60"),
        panel.spacing = unit(1, "lines"),
        strip.text = element_text(family = "Oswald Bold", size = 18, colour = charcoal),
        axis.title.x = element_text(family = "Oswald Bold", size = 18, colour = "grey60"),
        axis.title.y = element_text(family = "Oswald Bold", size = 18, colour = "grey60", angle = 90),
        plot.title = element_text(face = "bold", family = "Oswald", hjust = 0, margin = margin(t = 40,b = 25), size = 24, color = charcoal),
        plot.subtitle = element_text(family = "Oswald", hjust = 0.008, margin = margin(b = 10), size = 17, color = charcoal),
        plot.caption =element_text(family = "Oswald ExtraLight", size = 20, hjust = 1, margin = margin(t = 20, b = 3, r = 3)))


# SAVE PLOTS --------------------------------------------------------------
pA <- pA1/pA2
pB <- pB1/pB2
pC <- pC1/pC2
ggsave("3_figures/AB_pA_new.png", plot = pA ,dpi = 600, width = 45, height = 37, units = "cm")
ggsave("3_figures/AB_pB_new.png", plot = pB ,dpi = 600, width = 45, height = 37, units = "cm")
ggsave("3_figures/AB_pC_new.png", plot = pC ,dpi = 600, width = 25, height = 37, units = "cm")
