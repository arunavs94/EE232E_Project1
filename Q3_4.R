# This code is programmed in R v2.15.2.with iGraph v0.7.0 


# clearing workspace
closeAllConnections()
rm(list=ls())

# Load libraries
library("igraph")

####################  QUESTION 3  ####################
cat(' #################### QUESTION 3 #################### \n ')

# Read in the data from .txt file
graph_data <- read.table("facebook_combined.txt", sep = "", header = FALSE) # read text file
g1 <- graph.data.frame(graph_data,directed = FALSE) # covert table to undirected graph

correct_nodes <- numeric()
deg_corr_nodes <- numeric()

# See which nodes have more than 201 nodes in their network (200 plus core node)
for (i in 1:vcount(g1)) {
  net_tmp <- neighborhood(g1, order = 1, nodes = i)
  pers_net_tmp <- induced.subgraph(g1, vids = unlist(net_tmp), impl = "auto")
  
  if (vcount(pers_net_tmp) > 201){
    correct_nodes <- c(correct_nodes,i)
    deg_corr_nodes <- c(deg_corr_nodes,degree(g1, v = i))
    
  }
  
}

cat('Number of Core Nodes: ' ,length(correct_nodes),'\n')


# Calculate average degree of core nodes
core_deg_avg <- mean(deg_corr_nodes)

cat('Average Degree of Core Nodes: ', core_deg_avg, '\n')

# Chose a core node (picked 419 aribitrarily)
core_419 <- neighborhood(g1, order = 1, nodes = 419)
pers_net_419 <- induced.subgraph(g1, vids = unlist(core_419), impl = "auto")

# set size of core node to be larger in graph
node_size <- rep(4,length(unlist(core_419)))
node_size[which(unlist(core_419) == 419)] = 10

# Fast Greedy
# Generate community
fg_comm_419 <- fastgreedy.community(as.undirected(pers_net_419))

# plot the community (which to use?)
# plot(fg_comm_419, pers_net_419, vertex.size=4 , asp = 9/16,vertex.label=NA , edge.color = "grey", layout=layout.fruchterman.reingold)
# title("Fast Greedy with core 1")
plot(pers_net_419 , vertex.size=node_size , vertex.label=NA , vertex.color=fg_comm_419$membership, asp=9/16, layout=layout.fruchterman.reingold)
title("Fast Greedy with Core Node 419")

# Edge-Betweenness method
# Generate community
eb_comm_419 <- edge.betweenness.community(pers_net_419)

# plot the community (which to use?)
# plot(eb_comm_419, pers_net_419, vertex.size=4 , asp = 9/16,vertex.label=NA , edge.color = "grey", layout=layout.fruchterman.reingold)
# title("Edge-Betweenness with core 1")
plot(pers_net_419 , vertex.size=node_size , vertex.label=NA , vertex.color=eb_comm_419$membership, asp=9/16, layout=layout.fruchterman.reingold)
title("Edge-Betweenness with Core Node 419")

# Infomap method
# Generate community
im_comm_419 <- infomap.community(pers_net_419)

# plot the community (which to use?)
# plot(im_comm_419, pers_net_419, vertex.size=4 , asp = 9/16,vertex.label=NA , edge.color = "grey", layout=layout.fruchterman.reingold)
# title("Infomap with core 1")
plot(pers_net_419 , vertex.size=node_size , vertex.label=NA , vertex.color=im_comm_419$membership, asp=9/16, layout=layout.fruchterman.reingold)
title("Infomap with Core Node 419")

####################  QUESTION 4 ####################
cat(' #################### QUESTION 4 #################### \n ')


# Remove core node from community, then get induced subgraph
uncore_419 = unlist(core_419)[(unlist(core_419) != 419)]

pers_net_uncore_419 <- induced.subgraph(g1, vids = uncore_419, impl = "auto")

# Fast Greedy
# Generate community
fg_comm_uncore_419 <- fastgreedy.community(as.undirected(pers_net_uncore_419))

# plot the community (which to use?)
# plot(fg_comm_uncore_419, pers_net_uncore_419, vertex.size=4 , asp = 9/16,vertex.label=NA , edge.color = "grey", layout=layout.fruchterman.reingold)
# title("Fast Greedy w/o core 1")
plot(pers_net_uncore_419 , vertex.size=4 , vertex.label=NA , vertex.color=fg_comm_uncore_419$membership, asp=9/16, layout=layout.fruchterman.reingold)
title("Fast Greedy w/o Core Node")

# Edge-Betweenness method
# Generate community
eb_comm_uncore_419 <- edge.betweenness.community(pers_net_uncore_419)

# plot the community (which to use?)
# plot(eb_comm_uncore_419, pers_net_uncore_419, vertex.size=4 , asp = 9/16,vertex.label=NA , edge.color = "grey", layout=layout.fruchterman.reingold)
# title("Edge-Betweenness w/o core 1")
plot(pers_net_uncore_419 , vertex.size=4 , vertex.label=NA , vertex.color=eb_comm_uncore_419$membership, asp=9/16, layout=layout.fruchterman.reingold)
title("Edge-Betweenness w/o Core Node")

# Infomap method
# Generate community
im_comm_uncore_419 <- infomap.community(pers_net_uncore_419)

# plot the community (which to use?)
# plot(im_comm_uncore_419, pers_net_uncore_419, vertex.size=4 , asp = 9/16,vertex.label=NA , edge.color = "grey", layout=layout.fruchterman.reingold)
# title("Infomap w/o core 1")
plot(pers_net_uncore_419 , vertex.size=4 , vertex.label=NA , vertex.color=im_comm_uncore_419$membership, asp=9/16, layout=layout.fruchterman.reingold)
title("Infomap w/o Core Node")
