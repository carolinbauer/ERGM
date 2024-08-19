remotes::install_github("paleolimbot/rbbt")
remove(list=ls())
setwd("C:/Users/Carol/OneDrive/Documents/Uni/SoSe24/Bachelorarbeit_ERGM/ERGM")
library(statnet)
library(igraph)
library(dplyr)


# load package
library(sjPlot)
library(sjmisc)
library(sjlabelled)
#### load the network object from the file ---------------------------
network_it <- readRDS("network.RData")

# meta <- read.csv("parlnet.csv", sep = ",")
# meta <- meta %>%
#   filter(network == "net_it_ca2008" | network == "net_it_ca2013")
# View(meta)

#### Data transformations -----------

#####vertex attribute lr: set to 5 for all indepentend MPs----
current_values <- network::get.vertex.attribute(network_it, "lr")
modified_values <- ifelse(current_values == "Inf", 5, current_values)
network::set.vertex.attribute(network_it, "lr", modified_values)

#Relevel party (set reference level to IND (by naming it AAA))
party <- network::get.vertex.attribute(network_it, "party")
party[which(party == "IND")] <- "AAA"
network::set.vertex.attribute(network_it, "party_relevel", party)
#####set sex=M to 1, sex = F to 0----
# current_values <- network::get.vertex.attribute(network_it, "sex")
# modified_values <- ifelse(current_values == "M", 1, 0)
# network::set.vertex.attribute(network_it, "sex", modified_values)


####create Igraph Object ---------
igraph_it <- intergraph::asIgraph(network_it)

#clean enviroment
rm(list=ls()[! ls() %in% c("igraph_it","network_it")])

####create variables for models-------
#df <- igraph::as_data_frame(igraph_it, what = "both")
# ls <- list(edges = network::as.data.frame.network(network_it, unit = "edges"), 
#            vertices = network::as.data.frame.network(network_it, unit = "vertices"))

#####create source_sex ------------
# df$edges$source_sex <- df$vertices[df$edges$from, "sex"]
# df$edges[ ,c("source", "source_sex")] #check

# indx_source <- match(ls$edges$source, ls$vertices$vertex.names)
# ls$edges$source_sex <- ls$vertices$sex[indx_source]
# ls$edges[ ,c("source", "source_sex")] #check
# 
# 
# #####create target_sex ------------
# # df$edges$target_sex <- df$vertices[df$edges$to, "sex"]
# # df$edges[ ,c("target", "target_sex")] #check
# 
# indx_target <- match(ls$edges$target, ls$vertices$vertex.names)
# ls$edges$target_sex <- ls$vertices$sex[indx_target]
# ls$edges[ ,c("target", "target_sex")] #check
# 
# 
# #create network objects 
# network_it <- as.network(ls$edges, vertices = ls$vertices)
# igraph_it <- intergraph::asIgraph(network_it)

# rm(list=ls()[! ls() %in% c("igraph_it","network_it"
#                            #"igraph_it_new", "network_it_new"
#                            )])
# rm(list=ls()[! ls() %in% c("igraph_it","network_it",
#                            "igraph_it_new", "network_it_new")])

# delete_edge_attr(igraph_it_new, "source_sex")
# delete_edge_attr(igraph_it_new, "target_sex")
# identical_graphs(igraph_it_new, igraph_it, attrs = F)



#### models ----------------------------------------


load("workspacemodels.RData")

##### unweighted edges ====
set.seed(123)
m1 <- ergm(network_it ~ edges + mutual)
gof_m1 <- gof(m1)
gof_m1

set.seed(123)
m2 <- ergm(network_it ~ edges + mutual +
       nodematch('party') +
       nodematch('sex')+
       absdiff('lr')+
       absdiff('nyears')
       )
summary(m2)
gof_m2 <- gof(m2)

set.seed(123)
m3 <- ergm(network_it ~ edges + mutual +
             nodematch('party') +
             nodematch('sex')+
             absdiff('lr')+
             absdiff('nyears')+
             nodeofactor('party')+
             nodeifactor('party')+
             nodeofactor('sex')+
             nodeifactor('sex')
           )

summary(m3)
gof_m3 <- gof(m3)

set.seed(123)
m4 <- ergm(network_it ~ edges + mutual +
             nodematch('party') + #effect of same party ties
             nodematch('sex')+ #effect of same sex ties
             absdiff('lr')+ #effect of distance on political scale
             absdiff('nyears')+ #effect of seniority distance
             nodeofactor('party')+ #effect of party of co-sponsor (sender)
             nodeifactor('party')+ #effect of party of sponsor (receiver)
             nodeofactor('sex')+ #effect of sex of co-sponsor (sender)
             nodeifactor('sex')+
             nodeocov('nyears')+ #effect of seniority of co-sponsor (sender)
             nodeicov('nyears')
)
summary(m4)
gof_m4 <- gof(m4)


table(network_it %v% "party")
set.seed(123)
m5 <- ergm(network_it ~ edges + nodeofactor('party', levels = -4))
summary(m5)

gof_m5 <- gof(m5)
plot(gof_m5)

mcmc_diag_m5 <- mcmc.diagnostics(m5)

#Reference variable in party: IND
set.seed(123)
m6 <- ergm(network_it ~ edges + mutual +
             nodematch('party') + #effect of same party ties
             nodematch('sex')+ #effect of same sex ties
             absdiff('lr')+ #effect of distance on political scale
             absdiff('nyears')+ #effect of seniority distance
             nodeofactor('party', levels = -4)+ #effect of party of co-sponsor (sender)
             nodeifactor('party', levels = -4)+ #effect of party of sponsor (receiver)
             nodeofactor('sex')+ #effect of sex of co-sponsor (sender)
             nodeifactor('sex')+
             nodeocov('nyears')+ #effect of seniority of co-sponsor (sender)
             nodeicov('nyears')
)
summary(m5)

gof_m6 <- gof(m6)
gof_m6
par(mfrow = c(2, 3))
plot(gof_m6)
mcmc.diagnostics(m6)






##This doesn't work:
set.seed(123)
m7 <- ergm(network_it ~ edges + mutual +
             nodematch('party') + #effect of same party ties
             nodematch('sex')+ #effect of same sex ties
             absdiff('lr')+ #effect of distance on political scale
             absdiff('nyears')+ #effect of seniority distance
             nodeofactor('party')+ #effect of party of co-sponsor (sender)
             nodeifactor('party')+ #effect of party of sponsor (receiver)
             nodeofactor('sex')+ #effect of sex of co-sponsor (sender)
             nodeifactor('sex')+
             nodeocov('lr')+ #effect of position on political scale of co-sponsor (sender)
             nodeicov('lr')+
             nodeocov('nyears')+ #effect of seniority of co-sponsor (sender)
             nodeicov('nyears')
)



m
summary(m1)
m2 <- ergm(network_it ~ edges + mutual + 
             nodematch('party') +  #effect of same party ties
             nodematch('sex') + #effect of same sex ties
             absdiff('nyears') #effect of seniority distance
             ) 
summary(m2)
m3 <- ergm(network_it ~ edges + mutual + 
             nodematch('party') +  #effect of same party ties
             nodematch('sex') + #effect of same sex ties
             absdiff('nyears') + #effect of seniority distance
           absdiff('lr') #effect of distance on political sace
) 
summary(m3)



#### plotting ------------------------------------
#set Node colors according to paryname
#convert to igraph object
igraph_it <- intergraph::asIgraph(network_it)
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

#clean enviroment
rm(list=ls()[! ls() %in% c("igraph_it","network_it")])


