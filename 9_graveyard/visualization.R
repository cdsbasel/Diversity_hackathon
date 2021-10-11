library(tidyverse)

unibas_cols = c(mint = "#A5D7D2",
         mint_hell = "#D2EBD9",
         red = "#D20537",
         anthra = "#2D373C",
         anthra_hell = "#46505A")
names(unibas_cols) = NULL


# read data
publications <- read_csv('0_data/publications_sdg.csv')
projects <- read_csv('0_data/projects_sdg.csv')

# count occurences
pub = publications %>% 
  select(starts_with("SDG") & ends_with("Elsevier")) %>% 
  summarize_all(sum) %>% 
  pivot_longer(everything()) %>% 
  mutate(item_type = "publication",
         name = substring(name,5,6),
         value = value/sum(value))

pro = projects %>% 
  select(type, starts_with("SDG") & ends_with("Elsevier")) %>% 
  group_by(type) %>% 
  summarize_all(sum) %>% 
  pivot_longer(-1) %>% 
  rename(item_type = type) %>% 
  group_by(item_type) %>% 
  mutate(name = substring(name,5,6),
         value = value/sum(value)) %>% 
  ungroup()


elsevier_counts = c(11323, 93390,3349291,25550,35625, 46074,383354,89498,
                    39114, 47227, 141331, 84127, 180102, 104532, 111202, 169330)

elsevier = tibble(item_type = "reference",name = pub$name, value = elsevier_counts / sum(elsevier_counts))

sdg_props = pub %>% bind_rows(pro) %>% 
  mutate(item_type = as_factor(item_type)) %>% 
  rename(`Research item` = item_type)
levels(sdg_props$`Research item`) = c("Publication", "Projects (external)", "Projects (internal)")

ggplot(sdg_props, aes(x = name, y = value)) + 
  geom_bar(stat = 'identity', position = "dodge", mapping = aes(fill = `Research item`)) + 
  geom_segment(data = elsevier %>% mutate(x = (1:16)-.45, xend = (1:16)+.45), 
               mapping = aes(x = x, xend=xend, y = value, yend=value), col = unibas_cols[3], size = .3) +
  theme_minimal() + scale_fill_manual(values = unibas_cols[c(5,1,2)]) +
  labs(x = "Sustainable development goal", y = "Relative frequency",
       title = "SDG Overview",
       subtitle = "SDG-matches in publications, self- and externally-funded research projects",
       caption = "Note: Red reference lines corresponds to numbers presented in a recent Elsevier report.")




cnts = publications %>% 
  select(starts_with("SDG") & ends_with("Elsevier")) %>% 
  rowSums()

org_cnts = sapply(publications %>% select(starts_with("Org-")), function(x) sum(cnts[x]))

orgs = names(org_cnts[org_cnts>50])

org_sdg_prop = publications %>% 
  pivot_longer(starts_with("Org-"), names_to = "Unit", values_to = "present") %>%
  filter(present, Unit %in% orgs) %>% 
  group_by(Unit) %>% 
  select(starts_with("SDG") & ends_with("Elsevier")) %>% 
  summarize_all(sum) %>% 
  ungroup() %>% 
  pivot_longer(-1, names_to = "sdg", values_to = "count") %>% 
  group_by(Unit) %>% 
  mutate(prop = count / sum(count),
         sdg = str_sub(sdg, 5, 6),
         Unit = paste0(str_replace(Unit, "Org-",""), "\n(n = ", org_cnts[Unit], ")"))

ggplot(org_sdg_prop, aes(x = sdg, y = Unit, fill = prop)) + geom_tile() + 
  scale_fill_gradient(low = unibas_cols[4], high = unibas_cols[1]) + 
  guides(fill = F) + 
  labs(x = "Sustainable development goal", 
       y = "",
       title = "Unibas organizational units",
       subtitle = "SDG-related research stratified by organizational unit",
       caption = "Note: Limited to organizational units with at least 50 matches.")



