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
  
  if (vcount(pers_net_tmp) > 200){
    core_nodes <- c(core_nodes,i)
  }
  
}
cat("Number of core nodes =", length(core_nodes))
cat("\n\n")

#analyze the networks for each of the core nodes
#for(i in 1:length(core_nodes)){


#testing algorithm on just 1 network before running on all 40
for(i in 1:1){
  node = core_nodes[c(i)]
  
  net_tmp <- neighborhood(g1, order = 1, nodes = node)
  pers_net_tmp <- induced.subgraph(g1, vids = unlist(net_tmp), impl = "auto")  
  
  pers_net_vertice_count <- vcount(pers_net_tmp)
  
  cat('Analyzing communities of Core Node # ', i)  
  cat("\n")
  cat('Nodes in network: ', pers_net_vertice_count)
  cat("\n")
  
  comm_10plus = 0
  comm = fastgreedy.community(pers_net_tmp)
  for(j in 1:length(sizes(comm))){
    if(sizes(comm)[j] >= 10){
      comm_nodes = which(comm$membership==j)
      temp_comm = induced.subgraph(pers_net_tmp, comm_nodes)
      comms_comm = fastgreedy.community(temp_comm)
      cat('Analyzing Community # ', j)
      cat("\n")
      
      cat('Community size =' ,vcount(temp_comm))
      cat("\n")
      cat('Modularity =' , modularity(comms_comm))
      cat("\n")
      cat('Density =' , edge_density(temp_comm, loops = TRUE))
      cat("\n")
    } 
  }
  

  
  
  }