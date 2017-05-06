# clearing workspace
closeAllConnections()
rm(list=ls())

# Load libraries
library("igraph")

####################  QUESTION 6 ####################

# Read in the data from .txt file

# read text file
graph_data <- read.table("facebook_combined.txt", sep = "", header = FALSE) 

# convert table to undirected graph
g1 <- graph.data.frame(graph_data,directed = FALSE) 

#Since the core nodes are likely to belong to the most communities with size greater than 10
#we want to find all the core nodes
core_nodes <- numeric()


# See which nodes have more than 201 nodes in their network (200 plus core node)
for (i in 1:vcount(g1)) {
  net_tmp <- neighborhood(g1, order = 1, nodes = i)
  pers_net_tmp <- induced.subgraph(g1, vids = unlist(net_tmp), impl = "auto")
  
  if (vcount(pers_net_tmp) > 201){
    core_nodes <- c(core_nodes,i)
  }
  
}
cat("Number of core nodes =", length(core_nodes))


#analyze the networks for each of the core nodes
for(j in 1:length(core_nodes)){
  node = core_nodes[c(j)]
  net_tmp <- neighborhood(g1, order = 1, nodes = node)
  pers_net_tmp <- induced.subgraph(g1, vids = unlist(net_tmp), impl = "auto")  
  
  
}