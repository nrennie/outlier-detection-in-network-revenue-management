#visNet deustche bahn data
library(visNetwork)
library(igraph)
library(ggplot2)
library(dplyr)
library(RColorBrewer)

#Nodes
nodes <- data.frame(vertices.id=0:7, vertices.label=c("A", "B", "C", "D", "E", "F", "G", "H"))
colnames(nodes) <- c("id", "label")
nodes$id <- nodes$label
main_stations_data <- "B"
node_g <- data.frame(label=nodes$label, group=as.numeric(nodes$label %in% main_stations_data)+1)
nodes <- left_join(nodes, node_g, by = "label")
nodes$station_id <- sapply(nodes$group, function(x) c("Station", "Major Station")[x])
nodes$color <- sapply(nodes$group, function(x) c("lightgray", "deeppink3")[x])

#Edges
edges <- data.frame(from=c("A", "B", "C", "D", "F", "B", "C", "G"),
                    to=c("B", "C", "D", "E", "B", "C", "G", "H"),
                    width=rep(2,8))
colnames(edges) <- c("from", "to", "width")
edges$leg <- c("A-B_blue", "B-C_blue", "C-D_blue", "D-E_blue", "F-B_red", "B-C_red", "C-G_red", "G-H_red")
edges$color <- rep("grey", length=nrow(edges))


#Save
setwd("C:/Users/rennien/OneDrive - Lancaster University/GitHub/RShiny")
save(nodes, file="nodes.RData")
save(edges, file="edges.RData")




