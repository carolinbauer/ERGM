setwd("C:/Users/Carol/OneDrive/Documents/Uni/SoSe24/Bachelorarbeit_ERGM/ERGM")
library(statnet)
library(igraph)
# loading the workspace
load("workspaceProvided.RData")

# Save the network object to a file
saveRDS(net_it, file = "network.RData")

#clear workspace
remove(list=ls())
# Load the network object from the file
network_it <- readRDS("network.RData")
#convert to igraph object
igraph_it <- intergraph::asIgraph(network_it)


summary(igraph_it)
summary(network_it)

#Descriptive Analysis with igraph
class(igraph_it) #IGraph Object

vcount(igraph_it)#633 Nodes (Politicians)
V(igraph_it) #Vertex List
ecount(igraph_it)#25730 Edges
E(igraph_it) #Edgelist

igraph_it[] #adjacency matrix
as_adjacency_matrix(igraph_it)


#All Vertex Attributes
vertex_attr_names(igraph_it)
#All Edge Attributes
edge_attr_names(igraph_it)

#summary vertex attributes:
vertex_attr_names <- vertex_attr_names(igraph_it)
# Get the names of all vertex attributes
vertex_attr_names <- vertex_attr_names(igraph_it)
vertex_attr_names <- vertex_attr_names[! vertex_attr_names %in% c("vertex.names", "url", "photo")] #exclude var. for which we don't want a summary

# Creat summary for selected attr.
for (attr in vertex_attr_names) {
  cat("\nSummary for ", attr, "\n")
  
  # Extract the attribute values
  attr_values <- vertex_attr(igraph_it, attr)
  
  # print summary for numeric, table for cat. variables
  if (is.numeric(attr_values)) {
    print(summary(attr_values))
  } else {
    print(table(attr_values))
  }
}

#Summarize Edge attributes:
# Get the names of all edge attributes
edge_attr_names <- edge_attr_names(igraph_it)

# Creat summary for selected attr.
for (attr in edge_attr_names) {
  cat("\nSummary for ", attr, "\n")
  
  # Extract the attribute values
  attr_values <- edge_attr(igraph_it, attr)
  
  # print summary for numeric, table for cat. variables
  if (is.numeric(attr_values)) {
    print(summary(attr_values))
  } else {
    print(table(attr_values))
  }
}





table(V(igraph_it)$born, useNA = "ifany")
summary(V(igraph_it)$born)

print(igraph_it)

lapply(names(vertex.attributes(igraph_it)), table(V(igraph_it)$., useNA = "ifany"))
table(V(igraph_it)$sex, useNA = "ifany")
table(V(igraph_it)$sex, useNA = "ifany")
# igraph_net_it <- intergraph::asIgraph(net_it)
# write_graph(igraph_net_it,  file = "igraph_net_it.txt", format = "edgelist")
# all.equal(length(as.matrix(net_it, "edgelist")), length(as_edgelist(igraph_net_it)))


# install.packages(c("network", "sna", "igraph", "tidygraph", "ggraph", 
#                    "intergraph", "remotes"))
# remotes::install_bioc("graph")
# install.packages('statnet')



summary(net_it)
V(net_it)$names
#library(statnet)



class(net_it) #Network Object

summary.network(net_it, # the network we want to look at
                print.adj = FALSE # if TRUE then this will print out the whole adjacency matrix.
)
statnet::list.edge.attributes(net_it)
#V(net_it)
#vcount(net_it)



network.vertex.names(net_it)

df <- as.data.frame(net_it, unit = "vertices")


 g <- graph_from_literal(1-2, 1-3, 2-3, 2-4, 3-5, 4-5,
                            3 + 4-6, 4-7, 5-6, 6-7)
V(g)
is.weighted(net_it)
vcount(net_it)
as_adjacency_matrix(net_it)
get.node.attr(net_it, "party")
get.vertex.attribute(net_it)
pdf("Network_plot_1.pdf",
    width = 10,
    height = 10)
plot.network(net_it,
             )
