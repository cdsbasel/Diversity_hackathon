# SDG hackathon
# Group: GROUPPSEUDONYM

library(tidyverse)

# read data
publications <- read_csv('0_data/publications_sdg.csv')
projects <- read_csv('0_data/projects_sdg.csv')

# non-quality visualization
pdf('2_figures/sdg_frequencies_GROUP_GROUPPSEUDONYM.pdf')
barplot(colMeans(publications[,paste0('SDG-',ifelse(1:17<10,paste0('0',1:17),1:17))]))
dev.off()