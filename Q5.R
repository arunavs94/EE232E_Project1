# clearing workspace
closeAllConnections()
rm(list=ls())

# Load libraries
library("igraph")

####################  QUESTION 5  ####################
cat(' #################### QUESTION 5 #################### \n ')


# Read in the data from .txt file
graph_data <- read.table("facebook_combined.txt", sep = "", header = FALSE) # read text file
g1 <- graph.data.frame(graph_data,directed = FALSE) # covert table to directed graph

core_nodes <- numeric()
# See which nodes have more than 201 nodes in their network (200 plus core node)
for (i in 1:vcount(g1)) {
  net_tmp <- neighborhood(g1, order = 1, nodes = i)
  pers_net_tmp <- induced.subgraph(g1, vids = unlist(net_tmp), impl = "auto")
  
  if (vcount(pers_net_tmp) > 201){
    core_nodes <- c(core_nodes,i)
  }
}

CORE_NODE_COUNT <- 0
tot_dispersion <- numeric()
tot_embeddedness<- numeric()
# loop through each core node to calculate dispersion and embeddedness
for (i in core_nodes){
  
  CORE_NODE_COUNT <- CORE_NODE_COUNT + 1
  cat('Currently processing core node # ', CORE_NODE_COUNT, ' out of 40 \n')
  
  # Create induced_subgraph for i^th core node
  core_neighbors <- neighborhood(g1 , order = 1 , nodes = i)
  core_subgraph <- induced.subgraph(g1 , vids = unlist(core_neighbors) , impl = "auto")
  
  neighbors_i <- neighbors(g1 , i) # all nodes core node i is connected to
    # loop through all nodes in core_subgraph
  for (j in unlist(core_neighbors) ){
    temp_disp = numeric()
    if (i != j){
      
      # Calculate embeddedness
      neighbors_j <- neighbors( g1 , j ) # all nodes node j (in core_subgraph) is connected to
      mutual_friends <- intersect(neighbors_i , neighbors_j) 
      num_mutual_friends <- length(mutual_friends)
      tot_embeddedness <- c(tot_embeddedness , num_mutual_friends) 

      
      # Calculate dispersion 

      disp_subgraph <- delete.vertices(g1,c(i,j)) # delete said node (j) and core node
      if (num_mutual_friends >= 2){

        mutual_friends_all_pairs <- combn(mutual_friends,2) # find all pairs of mutual friends
        mutual_friends_all_pairs
        # loop through each pair of mutual friends
        for (k in 1:ncol(mutual_friends_all_pairs) ) {
          m_friend1 <- mutual_friends_all_pairs[,k][1]
          m_friend2 <- mutual_friends_all_pairs[,k][2]
          temp_disp <- c(temp_disp , shortest.paths(disp_subgraph , m_friend1 , m_friend2) )
          }
        }
       }
     tot_dispersion <- c(tot_dispersion , sum(temp_disp) )
    }
  
}

# plot distribution of embeddedness and dispersion over all personal networks
hist(x = tot_embeddedness, breaks = 50, 
     main = "Distribution of Embeddedness", 
     xlab = "Embeddedness",
     ylab = "Frequency" )

hist(x = tot_dispersion, breaks = 50,
     main = "Distribution of Dispersion",
     xlab = "Dispersion",
     ylab = "Frequency")







# # Plot 3 personal networks showing max dispersion, embeddedness, and dispersion/embeddedness
# 
# core_nodes_used <- c(1,419,954)
# 
# # Find nodes that maximize embed,disp, ratio
# 
# tot_max_embed <- numeric()
# tot_max_disp <- numeric()
# tot_max_disp_div_embed <- numeric()
# 
# old_disp_sum <- 0
# old_embed_val <- 0
# old_disp_embed_val <- 0
# 
# # loop through each core node to calculate dispersion and embeddedness
# for (i in core_nodes_used){
#   cat('\n core node used #',i,'\n')
# 
#   # Create induced_subgraph for i^th core node
#   core_neighbors <- neighborhood(g1 , order = 1 , nodes = i)
#   core_subgraph <- induced.subgraph(g1 , vids = unlist(core_neighbors) , impl = "auto")
# 
#   neighbors_i <- neighbors(g1 , i) # all nodes core node i is connected to
# 
#   # loop through all nodes in core_subgraph
#   for (j in unlist(core_neighbors) ){
#     cat(j,',')
# 
#         if (i != j){
# 
#       # Calculate embeddedness
#       neighbors_j <- neighbors( g1 , j ) # all nodes node j (in core_subgraph) is connected to
#       mutual_friends <- intersect(neighbors_i , neighbors_j)
#       num_mutual_friends <- length(mutual_friends)
# 
#       #save value for node j with largest embeddedness
#       if (num_mutual_friends > old_embed_val) {
#         # cat('\n embed stored::: num_mutual_friends=',num_mutual_friends,' old_embed_val = ',old_embed_val,'\n') #print statement to check update
#         old_embed_val <- num_mutual_friends #update comparison value
#         temp_max_embed <-j # update max node
#         }
# 
#       # Calculate dispersion
# 
#       disp_subgraph <- delete.vertices(g1,c(i,j)) # delete said node (j) and core node
# 
#       if (num_mutual_friends >= 2){
# 
#         mutual_friends_all_pairs <- combn(mutual_friends,2) # find all pairs of mutual friends
# 
#         temp_disp = numeric()
#           # loop through each pair of mutual friends
#           for (k in 1:ncol(mutual_friends_all_pairs) ) {
#             m_friend1 <- mutual_friends_all_pairs[,k][1]
#             m_friend2 <- mutual_friends_all_pairs[,k][2]
#             temp_disp <- c(temp_disp , shortest.paths(disp_subgraph , m_friend1 , m_friend2) )
#           }
# 
#         #save value for node j with largest dispersion
#         if (sum(temp_disp) > old_disp_sum & sum(temp_disp) < Inf) {
#           # cat('\n disp stored::: sum(temp_disp)=',sum(temp_disp),' old_disp_sum = ',old_disp_sum,'\n') #print statement to check update
#           old_disp_sum <- sum(temp_disp) #update comparison value
#           temp_max_disp <- j # update max node
#           }
# 
#         #save value for node j with largest dispersion/embeddedness
#         if (num_mutual_friends/sum(temp_disp) > old_disp_embed_val) {
#           # cat('\n frac stored::: num_mutual_friends/sum(temp_disp)=',num_mutual_friends/sum(temp_disp),' old_disp_embed_val = ',old_disp_embed_val,'\n') #print statement to check update
#           old_disp_embed_val <- num_mutual_friends/sum(temp_disp) #update comparison value
#           temp_max_disp_div_embed <- j # update max node
#           }
# 
#       }
#     }
#   }
#   tot_max_embed <- c(tot_max_embed , temp_max_embed)
#   tot_max_disp <- c(tot_max_disp , temp_max_disp)
#   tot_max_disp_div_embed <- c(tot_max_disp_div_embed , temp_max_disp_div_embed)
# }
# 
# 
# 
# #           TABLE SUMMARY FROM CALCULATIONS ABOVE FOR MAX EMBED, DISP, RATION NODES
# #
# #               |  embeddedness node   |   dispersion node   |   dispersion/embed node
# # ______________|______________________|_____________________|___________________________
# #core node 1    |        50            |         61          |           65
# #               |                      |                     |           
# #core node 419  |        100           |         100         |           553
# #               |                      |                     |           
# #core node 954  |        100           |         100         |           890
# 
# # Plot graphs
# MAX_NODE <- 890 #these values are found from previous section and hard coded (embed/disp/ration node)(see table above)
# CORE_NODE <- 954 #these values are found from previous section and hard coded (see table above)
# core_neighbors <- neighborhood(g1 , order = 1 , nodes = CORE_NODE)
# personal_network <- induced.subgraph(g1 , vids = unlist(core_neighbors) , impl = "auto")
# 
# #community structure
# community <- fastgreedy.community(personal_network)
# 
# #default plot colors (for all ndoes and edges)
# node_col <- community$membership+1
# node_size <- rep(3,length(node_col))
# edge_col <- rep("grey",length(E(personal_network)))
# edge_size = rep(0.5, length(E(personal_network)))
# 
# 
# # change color and size of max node(disp,embed, or disp/embed)
# actual_max_node <- which(unlist(core_neighbors)==MAX_NODE)
# actual_core_node <- which(unlist(core_neighbors)==CORE_NODE)
# node_col[actual_max_node] <- 81 #BLACK
# node_size[actual_max_node] <- 7
# node_col[actual_core_node] <- 0 #white
# node_size[actual_core_node] <- 7
# 
# edge_col[which(get.edgelist(personal_network,name=FALSE)[,1] == actual_max_node |
#                  get.edgelist(personal_network,name=FALSE)[,2]== actual_max_node) ] = "red"
# edge_size[which(get.edgelist(personal_network, name = FALSE)[,1] == actual_max_node |  
#                     get.edgelist(personal_network, name = FALSE)[,2] == actual_max_node)] = 3
# 
# plot.igraph(personal_network,
#             vertex.color = node_col,
#             vertex.size = node_size,
#             edge.color = edge_col,
#             edge.width = edge_size,
#             vertex.label = NA,
#             layout = layout.fruchterman.reingold(personal_network),
#             main = paste("Personal Network w/ Node ",CORE_NODE," Showing Node w/ Max Disp/Embed" ,collapse="" )
#             )
# 
