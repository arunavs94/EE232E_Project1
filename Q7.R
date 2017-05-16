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

comm_circ_intersect <- function(core_node_num){
    # inputs: core_node_num-select index that you want to run (change everytime)
    # outputs: 1 big matrix with infomap and walktrap confussion matrices (circles vs communities) seperated by a row of NaNs
    #           will also output big matrices to excel file
  
  
      # Use edge data from correct files to create personal networks and determine feature vectors
  
  cat('Core Node # ', core_node_num)
  cat("\n")
  
  # Define paths
  path_name_edges = paste("gplus/" , correct_ids[core_node_num], sep="", ".edges")
  path_name_circ = paste("gplus/" , correct_ids[core_node_num], sep="", ".circles")
  
  # Opens file and read it
  filename_circ = file(path_name_circ , open="r")
  data_circ = readLines(filename_circ)
  
  # Create personal network from .edges file (but doesnt include core node)
  pers_net = read.graph(path_name_edges , format = "ncol" , directed=TRUE)
  
  # Nodes in network before adding core
  nodes_wo_core = V(pers_net)
  
  # Need to add core node to personal network
  pers_net = add.vertices(pers_net, nv = 1, name = correct_ids[core_node_num])
  
  # Need to add edges to core node in personal network
  code_node_idx = which(V(pers_net)$name==correct_ids[core_node_num])
  
  added_edge_pair = c()
  
  # Create vertex-vertex pair (core node to all other nodes)
  for (vert in 1:length(nodes_wo_core)){
    added_edge_pair = c(added_edge_pair, c(code_node_idx, vert))
  }
  
  # Add edges
  add.edges(pers_net,added_edge_pair)
  
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
  
  percentages_im = c()
  
  for (i in 1: max(im_comm$membership)){
    
    # Nodes of a specific community
    commNodes = V(pers_net)$name[which(im_comm$membership==i)]
    
    # Compare to each circle
    for (j in 1:length(circs)){
      
      # Nodes in common
      intersectingNodes = intersect(commNodes,circs[[j]])
      
      # if (length(intersectingNodes) != 0){
      #   cat('worked')
      # }
      
      # What % of common nodes make up the circle
      percent_im = length(intersectingNodes)/length(circs[[j]])
      percentages_im = c(percentages_im, percent_im)
      
    }
    
  }
  
  im_conf_mat = t(matrix(percentages_im, ncol = length(sizes(im_comm)), nrow = length(circs)))
  
  
  # Compare walktrap communities with circles
  
  percentages_wt = c()
  
  for (i in 1: max(wt_comm$membership)){
  
    # Nodes of a specific community
    commNodes = V(pers_net)$name[which(wt_comm$membership==i)]
  
    # Compare to each circle
    for (j in 1:length(circs)){
  
      # Nodes in common
      intersectingNodes = intersect(commNodes,circs[[j]])
  
      # if (length(intersectingNodes) != 0){
      #   cat('worked')
      # }
  
      # What % of common nodes make up the circle
      percent_wt = length(intersectingNodes)/length(circs[[j]])
      percentages_wt = c(percentages_wt, percent_wt)
  
    }
    
  }
  wt_conf_mat = t(matrix(percentages_wt, ncol = length(sizes(wt_comm)), nrow = length(circs)))
  
  entire_mat = rbind(im_conf_mat,rep(NaN,length(circs)),wt_conf_mat)
  write.xlsx(x=entire_mat, file = paste("circ_comm_compare_",core_node_num,sep="",".xlsx"),sheetName = 'Sheet 1', row.names = FALSE, col.names = TRUE)
  return(entire_mat)

}

matrix_1 = comm_circ_intersect(1) #core node 1
matrix_2 = comm_circ_intersect(2) #core node 2
matrix_3 = comm_circ_intersect(3) #core node 3
matrix_4 = comm_circ_intersect(4) #core node 4
matrix_5 = comm_circ_intersect(5) #core node 5


  
