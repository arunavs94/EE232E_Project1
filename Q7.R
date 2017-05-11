# clearing workspace
closeAllConnections()
rm(list=ls())

# Load libraries
library("igraph")

# Store file names into list
file_names = list.files("gplus/")
file_ids = sub("^([^.]*).*", "\\1", file_names)

for (i in 1:length(file_names)){
  
  # Define the path
  path = paste("gplus/" , file_names[i], sep="")
  
  # Open the file
  filename = file(path , open="r")
  
  # Read the file
  data = readLines(filename)

  
  break
}

# circles_file = paste("gplus/" , id , ".circles" , sep="")
# circles_connect = file(circles_file , open="r")
# circles_content = readLines(circles_connect)