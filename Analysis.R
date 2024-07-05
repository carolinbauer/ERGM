install.packages(c("network", "sna", "igraph", "tidygraph", "ggraph", 
                   "intergraph", "remotes"))
remotes::install_bioc("graph")
install.packages('statnet')

#library(igraph)
library(statnet)


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
