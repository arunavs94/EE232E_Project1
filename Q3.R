# clearing workspace
closeAllConnections()
rm(list=ls())

# Load libraries
library("igraph")

####################  QUESTION 3 ####################

# Read in the data from .txt file
graph_data <- read.table("facebook_combined.txt", sep = "", header = FALSE) # read text file
g1 <- graph.data.frame(graph_data,directed = TRUE) # covert table to directed graph

correct_nodes <- numeric()
deg_corr_nodes <- numeric()

# See which nodes have more than 201 nodes in their network
for (i in 1:vcount(g1)) {
  net_tmp <- neighborhood(g1, order = 1, nodes = i)
  pers_net_tmp <- induced_subgraph(g1, vids = unlist(net_tmp), impl = "auto")
  
  if (vcount(pers_net_tmp) > 201){
    correct_nodes <- c(correct_nodes,i)
    deg_corr_nodes <- c(deg_corr_nodes,degree(g1, v = i))
    
  }
  
}

# Calculate average degree of core nodes
core_deg_avg <- mean(deg_corr_nodes)