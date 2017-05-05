# clearing workspace
closeAllConnections()
rm(list=ls())

# Load libraries
library("igraph")

####################  QUESTION 1 ####################

# Read in the data from .txt file
graph_data <- read.table("facebook_combined.txt", sep = "", header = FALSE) # read text file
g1 <- graph.data.frame(graph_data,directed = FALSE) # covert table to directed garph

# Check for connectivity 
connectivity <- is.connected(g1, mode = "strong") # check if network is connected
# NOTE: do we need "strong here". If no, then that gives us a connected graph

if (connectivity == FALSE){
  cat('Graph is not connected.')
} else {
  cat('Graph is connected.')
}

# Compute the diameter
g1_diameter <- diameter(g1, directed = TRUE, unconnected = TRUE, weights = NULL)
cat('Diameter: ', g1_diameter ,'\n')

# Generate and display Degree Distribution
g1_deg <- degree(g1)
degree_dist <- hist(x = g1_deg, breaks = seq(from = min(g1_deg), to = max(g1_deg), by=1), main = "Degree Distribution", xlab = "Degrees")

# Fit curve to degree distribution
g1_df = data.frame(x=degree_dist$mids, y=degree_dist$density)
curve <- nls(y ~ (exp(1)^(a + b * x)), data = g1_df, start = list(a = 0, b = 0), trace=T)

Degrees <- degree_dist$mids
Frequency <- degree_dist$density

plot(Degrees,Frequency)
lines(Degrees,predict(curve),col="red")

# Curve Performance Metrics
mse <- function(sm){
  mean(sm$residuals^2)
}

sm <- summary(curve)
g1_fit_mse <- mse(sm)
cat('MSE: ', g1_fit_mse)

# Computing average degree
g1_deg_avg = mean(g1_deg)
cat('Average degree of network is: ', g1_deg_avg, '\n')


