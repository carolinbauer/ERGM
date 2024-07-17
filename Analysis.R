
setwd("C:/Users/Carol/OneDrive/Documents/Uni/SoSe24/Bachelorarbeit_ERGM/ERGM")
library(statnet)
library(igraph)
# loading the workspace
load("workspaceProvided.RData")
# Save the network object to a file
saveRDS(net_it, file = "network.RData")

#clear workspace
remove(list=ls())
#Load the network object from the file
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
is_weighted(igraph_it) #Not weigthed

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
edge_attr_names <- edge_attr_names[! edge_attr_names %in% c("source", "target")] #exclude var. for which we don't want a summary

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
rm(list=ls()[! ls() %in% c("igraph_it","network_it"
                           #,"m1", "m2", "m3"
                           )])



#plotting network
plot(igraph_it) #ugly plot
#set Node colors according to paryname
partynames <- levels(as.factor(vertex_attr(igraph_it, "party")))
colors <- c("blue1", #Forza Italia, FI-PDL
            "deeppink", #Futuro e Libertà per l'Italia, FLI-TP
            "yellow", #Italia dei Vlaori
            "grey", #mixed group, IND
            "darkgreen", #Lega Nord, LN
            "orange", #Partito Democratico, PD
            "red", #Popolo e Territorio, PT
            "cyan" #Unione di Centro, terzo Polo, UDC-TP
            )
colors <- adjustcolor(colors, alpha = 0.6)
color_mapping <- setNames(colors[1:length(partynames)], partynames)
V(igraph_it)$color <- color_mapping[V(igraph_it)$party]

#plot again (multiple steps)
# plot(igraph_it,
#      vertex.label = NA)
# plot(igraph_it,
#      vertex.label = NA,
#      vertex.size = 7)
par(mfrow = c(1,1))
set.seed(240)
plot(igraph_it,
     vertex.label = NA,
     vertex.size = 7,
     edge.arrow.size = 0.4)


#add legend:
legend(#x=1.2, y=0,   # Coordinates (x also accepts keywords)
       "bottomright",
       legend = levels(factor(V(igraph_it)$party)), # Vector with the name of each group
       fill = colors,   # Creates boxes in the legend with the specified colors
       col = par("col"), # Color of lines or symbols
       border = "black", # Fill box border color
#       lty, lwd,         # Line type and width
#       pch,              # Add pch symbols to legend lines or boxes
       bty = "o",        # Box type (bty = "n" removes the box)
       bg = par("bg"),    # Background color of the legend
       box.lwd = par("lwd"), # Legend box line width
       box.lty = par("lty"), # Legend box line type
       box.col = par("fg"),  # Legend box line color
       cex = 1,          # Legend size
       horiz = FALSE,     # Horizontal (TRUE) or vertical (FALSE) legend
       title = NULL      # Legend title
)


network_it2 <- network_it

network_it2$weights <- network.edgelabel(network_it, label = raw)
#### Models
## simple Model:
# https://www.r-bloggers.com/2016/05/ergm-tutorial/
# Estimate the simplest model, one with only a term for tie density 
# (akin to an intercept term in a glm):

m1 = ergm(network_it ~ edges) #logit model
summary(m1)
plogis(coef(m1)[['edges']])
# Because that is a dyadic-independent model 
# (the likelihood of a tie doesn’t depend on any other), 
# ergm solves the logistic regression instead of resorting to MCMC.

# Note that the edges term represents exactly the density of the network
# (in log-odds). That is, the probability of any tie 
# (aka the density of the network) is the inverse-logit of the 
# coefficient on edges:
all.equal(network.density(network_it), plogis(coef(m1)[[1]]))

## now estimate term for reciprocity of ties:
m2 = ergm(network_it ~ edges + mutual)
summary(m2)
plogis(coef(m2)[['edges']])
plogis(coef(m2)[['edges']] + coef(m2)[['mutual']])


#We need to check two things: 
# 1) that the MCMC routine behaved well 
# (that our estimates are likely good approximations of the MLEs), and 
# 2) that our model fits the data well. 

mcmc.diagnostics(m2)

# mbad = ergm(network_it ~ edges + mutual,
#             control = control.ergm(MCMC.interval = 2))
# mcmc.diagnostics(mbad)


# Now that we can trust our model estimates, 
# let’s see if they make a good fit to the data. 
# We use the gof (goodness-of-fit) function for this purpose
m2_gof = gof(m2, GOF = ~model)
m2_gof
m2_gof2 = gof(m2)
par(mfrow = c(2, 2))
plot(m2_gof2)

#To add a term for homophily within Sampson’s groups 
# we use the term nodematch, which takes at least one argument 
# (the nodal attribute), and provides the change in the likelihood 
# of a tie if the two nodes match on that attribute.

summary(network_it ~ edges + mutual + nodematch('party'))
# So of the 25730 ties in the network, 3681 of them are reciprocal, and 18816 
# of them are between politicians within a party. 
# So we should expect a strong positive coefficient for the 
# party-homophily term. Let’s see:
m3 <- ergm(network_it ~ edges + mutual + nodematch('party'))
summary(m3)
#Probability of a non-reciprocal, across-party tie:
  plogis(coef(m3)[1])
#Probability of a non-reciprocal, within-group tie:
  plogis(sum(coef(m3)[c(1, 3)]))  

exp(coef(m3)[3])


#Let’s take a look at the goodness of fit of that model:
par(mfrow = c(2, 2))
invisible(plot(gof(m3)))


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

network::get.vertex.attribute(
  network_it,
  "raw",
  na.omit = FALSE,
  null.na = TRUE,
  unlist = TRUE
)

weights <- network::get.edge.attribute(network_it, "raw")
network::set.edge.attribute(network_it, "weigths", weights)
