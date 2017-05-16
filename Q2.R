# This code is programmed in R v2.15.2.with iGraph v0.7.0 


# clearing workspace
closeAllConnections()
rm(list=ls())

# Load libraries
library("igraph")

####################  QUESTION 2 ####################
cat(' #################### QUESTION 2 #################### \n ')

# Read in the data from .txt file
graph_data <- read.table("facebook_combined.txt", sep = "", header = FALSE) # read text file
g1 <- graph.data.frame(graph_data,directed = FALSE) # covert table to directed graph

# Finding ID's of the neighbors of node 1
# n1_neigh <- neighbors(g1,1)
# n1_tot <- c(n1_neigh,1)

n1_tot = neighborhood(g1, order = 1, nodes = 1)


# Generate personal network of node 1
pers_net_1 <- induced.subgraph(g1, vids = unlist(n1_tot), impl = "auto")

pers_net_1_vertice_count <- vcount(pers_net_1)
pers_net_1_edge_count <- ecount(pers_net_1)

cat('Amount of vertices in personal network of node 1: ', pers_net_1_vertice_count , '\n')
cat('Amount of edges in personal network of node 1: ', pers_net_1_edge_count, '\n')
