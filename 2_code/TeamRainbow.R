### CDS Diversity Hackathon #####
### Script to visualize diversity gaps over time #####

getwd()
# load data
div_dat <- read.csv('1_data/unibas_diversity.csv')
head(div_dat)
View(div_dat)

### AGE GAP
plot(div_dat$age_first)

### GENDER GAP

### NATIONALITY GAP