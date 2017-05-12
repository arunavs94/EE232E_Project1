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
cat("\n\n")

#analyze the networks for each of the core nodes
#for(i in 1:length(core_nodes)){


#testing algorithm on just 1 network before running on all 40

# Feature dictionary
features <- vector(mode = "list", length = 40)

for(i in 1:40){
  
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
  
  features[[i]] <- vector(mode = "list", length = length(which(sizes(comm) > 10)))
  count <- 1
  for(j in 1:length(sizes(comm))){
    if(sizes(comm)[j] > 10){
      comm_nodes = which(comm$membership==j)
      temp_comm = induced.subgraph(pers_net_tmp, comm_nodes)
      comms_comm = fastgreedy.community(temp_comm)
      
      # cat('Analyzing Community # ', count)
      # cat("\n")
      # 
      # cat('Community size =' ,vcount(temp_comm))
      # cat("\n")
      # cat('Modularity =' , modularity(comms_comm))
      # cat("\n")
      # cat('Density =' , graph.density(temp_comm, loops = TRUE))
      # cat("\n")
      # cat('Clustering Coeffifient =', transitivity(temp_comm))
      # cat("\n\n")
      
      # Load values into feature dictionary
      features[[i]][[count]] <- c(modularity(comms_comm),transitivity(temp_comm), graph.density(temp_comm, loops = TRUE),vcount(temp_comm)) # ModIndex, ClusertingCoeff, Density, CommSize
      count <- count + 1
    } 
  }
  
  
  
  
}

# Write values out to excel file

# library("xlsx")

totMat = numeric()
T1 = numeric() #familia 
T2 = numeric() #school
for (i in 1:40) { # For each core node
  
  # Create matrix of values
  
  # List of all features for each community concatenated
  list_core_i = unlist(features[[i]])
  num_idxs = length(list_core_i)/4
  temp_mat = t(matrix(list_core_i,4,num_idxs))
  

  # Find T1 (indicies for community of type 1)
    com_size_list = temp_mat[,4]
    density_list = temp_mat[,3]
    modularity_list = temp_mat[,1]
  
    # find 2 smallest size comms
    smallest_sizes = numeric() #store indices of 2 smallest community sizes
    min_idx = which.min(com_size_list)
    smallest_sizes = c(smallest_sizes, min_idx) #append
    com_size_list[min_idx] = Inf #set smallest idx to inf to find second smallest idx
    min_idx_2 = which.min(com_size_list)
    smallest_sizes = c(smallest_sizes, min_idx_2) #append
  
    # of the 2 smallest size comms, find highest density
    if (density_list[smallest_sizes[1]] >= density_list[smallest_sizes[2]] ){
      temp_t1 = smallest_sizes[1] # smallest_sizes[1] is greater 
    } else {
      temp_t1 = smallest_sizes[2] # smallest_sizes[2] is greater
    } 
    
      
    
  
  # Find T2 (indicies for community of type 2)
    com_size_list = temp_mat[,4]
    density_list = temp_mat[,3]
    modularity_list = temp_mat[,1]
    
    # find 2 largest size comms
    largest_sizes = numeric()  #store indices of 2 largest community sizses
    max_idx = which.max(com_size_list)
    largest_sizes = c(largest_sizes, max_idx) #append
    com_size_list[max_idx] = 0 # set largest idx to 0 to find second largest idx
    max_idx_2 = which.max(com_size_list)
    largest_sizes = c(largest_sizes,max_idx_2) #append
  
    # of the 2 largest size comms, find highest modularity
    if (modularity_list[largest_sizes[1]] >= modularity_list[largest_sizes[2]] ){
      temp_t2 = largest_sizes[1] # largest_sizes[1] is greater 
    } else {
      temp_t2 = largest_sizes[2] # largest_sizes[2] is greater
    } 
  
  long_t1 = rep(temp_t1,num_idxs) # repeat t1 for the number of indices 
  long_t2 = rep(temp_t2,num_idxs) # repeat t2 for the number of indices 
  
  T1 = c(T1,long_t1,NaN)
  T2 = c(T2,long_t2,NaN)
    
  totMat = rbind(totMat,temp_mat)
  totMat = rbind(totMat,c(NaN,NaN,NaN,NaN)) # add row of nans
  
}

  totMat = cbind(totMat,T1,T2) #append a col of T1 and T2 to totMat

# colNames = c('Modularity Index', 'Cluserting Coeff', 'Density', 'Community Size')
# write.xlsx(x=totMat, file = "test.writeout.xlsx", sheetName = "Test", row.names = FALSE, col.names = TRUE)



