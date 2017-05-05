# clearing workspace
closeAllConnections()
rm(list=ls())

# Load libraries
library("igraph")

####################  QUESTION 6 ####################

# Read in the data from .txt file
graph_data <- read.table("facebook_combined.txt", sep = "", header = FALSE) # read text file
g1 <- graph.data.frame(graph_data,directed = TRUE) # covert table to directed graph


