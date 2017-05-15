# clearing workspace
closeAllConnections()
rm(list=ls())

# Load libraries
library("igraph")
library("xlsx")
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

  if (length(data_circ) > 2){
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
    
    # Define paths
    path_name_edges = paste("gplus/" , correct_ids[i], sep="", ".edges")
    path_name_circ = paste("gplus/" , correct_ids[i], sep="", ".circles")
    
    # Opens file and read it
    data_edges = read.table(path_name_edges, sep = "", header = FALSE)
    
    filename_circ = file(path_name_circ , open="r")
    data_circ = readLines(filename_circ)
    
    # Create personal network from .edges file (but doesnt include core node)
    pers_net <- graph.data.frame(data_edges,directed = TRUE) 
    
    # Need to add core node to personal network
    pers_net = add.vertices(pers_net, nv = 1, name = correct_ids[i])
    
    # Need to add edges to core node in personal network
    
    core_node_idx = which(V(pers_net)$name==correct_ids[i])
    added_edge_pair = c()
    
    # Create vertex-vertex pair (core node to all other nodes)
    for (vert in 1:(vcount(pers_net)-1)){
      added_edge_pair = c(added_edge_pair, c(core_node_idx, vert))
    }
    
    add_edges(pers_net,added_edge_pair)
    
    
    # Allocate ID's in .circles into individual circles (circs) 
    circs = list()
    for (i in 1:length(data_circ)) {
      
      circ_nodes = strsplit(data_circ[i],"\t")
      circs = c(circs, list(circ_nodes[[1]][-1]))
      
    }
    
    close(filename_circ)
    
    # Generate communities
    im_comm = infomap.community(pers_net)
    wt_comm = walktrap.community(pers_net)
    
    # Compare walktrap communities with circles
    
    percentages_wt = c()
    
    for (i in 1: max(wt_comm$membership)){
      
      # Nodes of a specific community
      commNodes = V(pers_net)[which(wt_comm$membership==i)]
      
      # Compare to each circle
      for (j in 1:length(circs)){
        
        # Nodes in common
        intersectingNodes = intersect(commNodes,circs[[j]])
        
        if (length(intersectingNodes) != 0){
          cat('worked')
        }
        
        # What % of common nodes make up the circle
        percent_wt = length(intersectingNodes)/length(circs[[j]])
        percentages_wt = c(percentages_wt, percent_wt)
        
      }
      
    }
    
    # Compare infomap communities with circle
    
    percentages_im = vector()
    
    for (i in 1: max(im_comm$membership)){
      
      # Nodes of a specific community
      commNodes = V(pers_net)$name[which(im_comm$membership==i)]
      
      # Compare to each circle
      for (j in 1:length(circs)){
        
        # Nodes in common
        intersectingNodes = intersect(commNodes,circs[[j]])
        
        if (length(intersectingNodes) != 0){
          cat('worked')
        }
        
        # What % of common nodes make up the circle
        percent_im = length(intersectingNodes)/length(circs[[j]])
        percentages_im = c(percentages_im, percent_im)
        
      }
      
    }
    
    # Analyze ModIndex, ClusertingCoeff, Density, CommSize for each
    
  #   thresh = 10
  #   
  #   features[[i]] <- vector(mode = "list", length = length(which(sizes(im_comm) > thresh))) # How big should our thresh for comm be?
  #   count <- 1
  #   for(j in 1:length(sizes(im_comm))){
  #     if(sizes(im_comm)[j] > thresh){
  #       comm_nodes = which(im_comm$membership==j)
  #       temp_comm = induced.subgraph(pers_net, comm_nodes)
  #       comms_comm = infomap.community(temp_comm)
  #       cat('Analyzing Community # ', count)
  #       cat("\n")
  #       
  #       cat('Community size =' ,vcount(temp_comm))
  #       cat("\n")
  #       cat('Modularity =' , modularity(comms_comm))
  #       cat("\n")
  #       cat('Density =' , graph.density(temp_comm, loops = TRUE))
  #       cat("\n")
  #       cat('Clustering Coeffifient =', transitivity(temp_comm))
  #       cat("\n\n")
  #       
  #       # Load values into feature dictionary
  #       features[[i]][[count]] <- c(modularity(comms_comm),transitivity(temp_comm), graph.density(temp_comm, loops = TRUE),vcount(temp_comm)) # ModIndex, ClusertingCoeff, Density, CommSize
  #       count <- count + 1
  #     } 
  #   }
  #   

  
    
  }
  
