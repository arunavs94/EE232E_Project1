# clearing workspace
closeAllConnections()
rm(list=ls())

# Load libraries
library("igraph")
library("xlsx")
# Establish correct path to working directory (change for specific user)
setwd("~/Desktop/Project1/EE232E_Project1")
options(scipen=999)

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


  # Use edge data from correct files to create personal networks and determine feature vectors
  
# select index that you want to run (change everytime)
i = 1;

cat('Iteration: ', i)
cat("\n")

# Define paths
path_name_edges = paste("gplus/" , correct_ids[i], sep="", ".edges")
path_name_circ = paste("gplus/" , correct_ids[i], sep="", ".circles")

# Opens file and read it
filename_circ = file(path_name_circ , open="r")
data_circ = readLines(filename_circ)

# Create personal network from .edges file (but doesnt include core node)
pers_net = read.graph(path_name_edges , format = "ncol" , directed=TRUE)

# Nodes in network before adding core
nodes_wo_core = V(pers_net)

# Need to add core node to personal network
pers_net = add.vertices(pers_net, nv = 1, name = correct_ids[i])

# Need to add edges to core node in personal network
code_node_idx = which(V(pers_net)$name==correct_ids[i])

added_edge_pair = c()

# Create vertex-vertex pair (core node to all other nodes)
for (vert in 1:length(nodes_wo_core)){
  added_edge_pair = c(added_edge_pair, c(code_node_idx, vert))
}

# Add edges
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

# Compare infomap communities with circles

percentages_wt = c()

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
    percent_wt = length(intersectingNodes)/length(circs[[j]])
    percentages_wt = c(percentages_wt, percent_wt)
    
  }
  
}



# Compare walktrap communities with circles

percentages_wt = c()

for (i in 1: max(wt_comm$membership)){

  # Nodes of a specific community
  commNodes = V(pers_net)$name[which(wt_comm$membership==i)]

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
    
    
  
