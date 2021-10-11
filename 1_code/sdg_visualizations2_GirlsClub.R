# SDG hackathon
# Group: GROUPPSEUDONYM

library(tidyverse)


# read data
publications <- read_csv('0_data/publications_sdg.csv')
projects <- read_csv('0_data/projects_sdg.csv')


# select columns of interest
publications <- publications %>% select(`SDG-01`:`SDG-17`) 

data <- publications


df <- matrix(nrow = 17, ncol = 17)
for (sdg_1 in 1:17) {
  data_sdg1 <- data[[sdg_1]]
  
  for (sdg_2 in 1:17) {
    
    data_sdg2 <- data[[sdg_2]]
    
     table_sdg <- table(data_sdg1,data_sdg2)
   phi_val <-  psych::phi(table_sdg, digits = 10)
   
   df[sdg_1, sdg_2]<- phi_val 
  }
}

# corrr::network_plot(df, min_cor = 0.0001)
# 
# mygraph <- igraph::graph_from_adjacency_matrix(df, mode = "undirected", weighted = TRUE, diag = FALSE)
# 
# 
# ggraph::ggraph(mygraph, layout="igraph", algorithm="randomly") + 
#   #geom_edge_density(edge_fill="#69b3a2") +
#   geom_edge_link(edge_colour="black") +
#   geom_node_point(aes(size= 1, alpha=0.5)) +
#   geom_node_text(aes(label = name, filter = degree > 150), color = 'white', 
#                  size = 3) +
#   theme_minimal() +
#   theme(
#     legend.position="none",
#     plot.margin=unit(rep(1,4), "cm")
#   )


p <- qgraph::qgraph(df, shape="circle", posCol="darkgreen", negCol="darkred", vsize=10)


ggsave(p, file="2_figures/sdg_co_occurences_GirlsClub.pdf", device="pdf",dpi = 600,width = 20, height = 20, units = "cm") 


