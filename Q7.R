# clearing workspace
closeAllConnections()
rm(list=ls())

# Load libraries
library("igraph")

# Establish correct path to working directory (change for specific user)
setwd("~/Desktop/Project1/EE232E_Project1")

# Store file names into list
file_names = list.files("gplus/")
file_ids = sub("^([^.]*).*", "\\1", file_names)

unique_ids = unique(file_ids)
correct_ids = numeric()

# Find ids which include at least 2 circles -> correct_ids
for (i in 1:length(unique_ids)){ 
  
  # Define the path
  path_name_circ = paste("gplus/" , unique_ids[i], sep="", ".circles")
  
  # Open the file
  filename_circ = file(path_name_circ , open="r")
  
  # Read the file
  data_circ = readLines(filename_circ)

  if (length(data_circ) >= 2){
    correct_ids = c(correct_ids, unique_ids[i])
  }
  
  
  close(filename_circ)
  
}

  # Feature dictionary
  features <- vector(mode = "list", length = length(correct_ids))

  # Use edge data from correct files to create personal networks and determine feature vectors

  for (i in 1: length(correct_ids)){
    
    cat('Iteration: ', i)
    cat("\n")
    
    # Define the path
    path_name_edges = paste("gplus/" , correct_ids[i], sep="", ".edges")
    
    # Opens file and reads it
    data_edges = read.table(path_name_edges, sep = "", header = FALSE)
    
    # Create personal network from .edges file
    pers_net <- graph.data.frame(data_edges,directed = TRUE) 
    
    # Infomap to generate communities
    comm = infomap.community(pers_net)
    
    # Analyze ModIndex, ClusertingCoeff, Density, CommSize for each
    
    thresh = 10
    
    features[[i]] <- vector(mode = "list", length = length(which(sizes(comm) > thresh))) # How big should our thresh for comm be?
    count <- 1
    for(j in 1:length(sizes(comm))){
      if(sizes(comm)[j] > thresh){
        comm_nodes = which(comm$membership==j)
        temp_comm = induced.subgraph(pers_net, comm_nodes)
        comms_comm = infomap.community(temp_comm)
        cat('Analyzing Community # ', count)
        cat("\n")
        
        cat('Community size =' ,vcount(temp_comm))
        cat("\n")
        cat('Modularity =' , modularity(comms_comm))
        cat("\n")
        cat('Density =' , graph.density(temp_comm, loops = TRUE))
        cat("\n")
        cat('Clustering Coeffifient =', transitivity(temp_comm))
        cat("\n\n")
        
        # Load values into feature dictionary
        features[[i]][[count]] <- c(modularity(comms_comm),transitivity(temp_comm), graph.density(temp_comm, loops = TRUE),vcount(temp_comm)) # ModIndex, ClusertingCoeff, Density, CommSize
        count <- count + 1
      } 
    }
    
  }
  
  
  

