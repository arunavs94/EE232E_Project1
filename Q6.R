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
#we want to find all of the core nodes and keep track of all their networks

correct_nodes <- numeric()
deg_corr_nodes <- numeric()

# See which nodes have more than 201 nodes in their network (200 plus core node)
core_nodes<-which(neighborhood.size(graph, 1 , nodes=V(g1) > 200)

