remove(list=ls())
setwd("C:/Users/Carol/OneDrive/Documents/Uni/SoSe24/Bachelorarbeit_ERGM/ERGM")
library(statnet)
library(igraph)
library(texreg) #Für Modell Summary Tabelle
library(dplyr)
library(ggplot2)


options(scipen=999) #wissensch. Notation ausschalten

#Laden der Daten:
network_it <- readRDS("network.RData")

#### Daten transformieren  -------------------------------------------------

#####vertex attribute lr: set to 5 for all indepentend MPs----
current_values <- network::get.vertex.attribute(network_it, "lr")
modified_values <- ifelse(current_values == "Inf", 5, current_values)
network::set.vertex.attribute(network_it, "lr", modified_values)

#####relevel party (set IND as reference level (by naming it AAIND))----
party <- network::get.vertex.attribute(network_it, "party")
party[which(party == "IND")] <- "AAIND"
network::set.vertex.attribute(network_it, "party_relevel", party)

#####create agegroup variable----
born <- network::get.vertex.attribute(network_it, "born")
ageGroup <- as.data.frame(born)%>%
  mutate(age = 2008 - born,
         ageGroup = ifelse(between(age, 25, 39), "<40",
                           ifelse(between(age, 40, 49), "40 - 49",
                                  ifelse(between(age, 50, 59), "50 - 59",
                                         ifelse(between(age, 60, 69), "60 - 69",
                                                ">= 70")))))%>%
  select(ageGroup)
network::set.vertex.attribute(network_it, "ageGroup", unlist(ageGroup))

#####create Igraph Object----
igraph_it <- intergraph::asIgraph(network_it)


#####clean enviroment----
rm(list=ls()[! ls() %in% c("igraph_it","network_it")])

#####create undirected networks----
undir_igraph_it <- as.undirected(
  igraph_it,
  mode = c("each"), #1 for at least 1 connection between nodes
  edge.attr.comb = igraph_opt("edge.attr.comb")
)

undir_network_it <- intergraph::asNetwork(undir_igraph_it)



###Modelle----
set.seed(123)
undir_m1 <- ergm(undir_network_it ~ 
                  edges +
                  nodefactor("party_relevel")+  #vergleicht mit unabh. Abgeordneten(IND)
                  nodefactor("sex")+
                  nodefactor("ageGroup")
                )
summary(undir_m1) #Model ohne Homophilieuntersuchung (nur Faktoren der Knotenatribute)

undir_m2 <-  ergm(undir_network_it ~ 
                    edges +
                    nodematch("party_relevel")+  #vergleicht mit unabh. Abgeordneten(IND)
                    nodematch("sex")+
                    nodematch("ageGroup")
)
summary(undir_m2) #Model mit Homophilie (ohne Aufschlüsselung in Kategorien der Knotenattribute)


undir_m3 <-  ergm(undir_network_it ~ 
                    edges +
                    nodematch("party_relevel", diff = T)+  #vergleicht mit unabh. Abgeordneten(IND)
                    nodematch("sex", diff = T)+
                    nodematch("ageGroup", diff = T)
)
summary(undir_m3)#Model mit Homphilie mit Aufschlüsselung in Kategorien der Knotenattribute

set.seed(123)
undir_m4 <- ergm(undir_network_it ~ 
                   edges +
                   nodefactor("party_relevel")+
                   nodefactor("sex")+
                   nodefactor("ageGroup")+
                   nodematch("party_relevel", diff = T)+  #vergleicht mit unabh. Abgeordneten(IND)
                   nodematch("sex", diff = T)+
                   nodematch("ageGroup", diff = T)
)
summary(undir_m4)#Model mit Knotenattributen und Homophilie aufgeschlüsselt in Kategorien

set.seed(12)
undir_m5 <- ergm(undir_network_it ~ 
                   edges +
                   gwesp(0.2, fixed=F, cutoff = 400)+
                   nodematch("party_relevel")+  #vergleicht mit unabh. Abgeordneten(IND)
                   nodematch("sex")+
                   nodematch("ageGroup")
)
summary(undir_m5) #Modell mit Homophilie + term für Netzwerkdichte und Transivität -> MCML kontrollieren

set.seed(123)
undir_m6 <- ergm(undir_network_it ~ 
                   edges +
                   gwesp(0.5, fixed=T)+
                   nodematch("party_relevel")+  #vergleicht mit unabh. Abgeordneten(IND)
                   nodematch("sex")+
                   nodematch("ageGroup")
)
summary(undir_m6)

set.seed(123)
undir_m7 <- ergm(undir_network_it ~ edges +
                   gwesp(0.2, fixed = T)+
                   nodematch("party_relevel")+
                   nodematch("sex")+
                   nodematch("ageGroup")
)
summary(undir_m7) #Anderer Decay Gewählt für GWESP



set.seed(123)
undir_m8 <- ergm(undir_network_it ~
                   edges +
                   gwesp(0.2, fixed=F, cutoff = 400)+
                   nodematch("party_relevel", diff = T)+  #vergleicht mit unabh. Abgeordneten(IND)
                   nodematch("sex")+
                   nodematch("ageGroup", diff = T)
)
 summary(undir_m8) #decay (fixed = False)


set.seed(123)
undir_m9 <- ergm(undir_network_it ~ edges +
                   gwesp(0.2, fixed = T)+
                   nodematch("party_relevel", diff = T)+
                   nodematch("sex")+
                   nodematch("ageGroup", diff = T)
)
summary(undir_m9) #differenzielle Homophilie + GWESP


#Modell mit GWDegree:
# set.seed(123)
# undir_m10 <- ergm(undir_network_it ~ edges +
#                     gwdegree(0.2, fixed = T)+
#                     nodematch("party_relevel", diff = T)+
#                     nodematch("sex")+
#                     nodematch("ageGroup", diff = T)
# )
# summary(undir_m10) 
#Lädt nicht!

set.seed(123)
undir_m11 <- ergm(undir_network_it ~ edges +
                   gwesp(0.2, fixed = T)+
                    nodefactor("party_relevel")+
                    nodefactor("sex")+
                    nodefactor("ageGroup")+
                   nodematch("party_relevel", diff = T)+
                   nodematch("sex")+
                   nodematch("ageGroup", diff = T)
)
summary(undir_m11) #Finales Modell, GWESP, Homophilie, Main Effects

#exp(Theta):
round(exp(coef(undir_m11)),3)

# Test Decay 0.5:
undir_m12 <- ergm(undir_network_it ~ edges +
                    gwesp(0.5, fixed = T)+
                    nodefactor("party_relevel")+
                    nodefactor("sex")+
                    nodefactor("ageGroup")+
                    nodematch("party_relevel", diff = T)+
                    nodematch("sex")+
                    nodematch("ageGroup", diff = T),
                  control = control.ergm(seed = 123)
                  
)
load("m12.RData")

#AIC prüfen
unlist(lapply(list(undir_m1, undir_m2, undir_m3, undir_m4, 
                   undir_m5, undir_m6, undir_m7, undir_m9, undir_m11, undir_m12), AIC))
#AIC m12 ist am kleinsten, danach m11


#GOF prüfen
gof_undir_m1 <- gof(undir_m1)
gof_undir_m2 <- gof(undir_m2)
gof_undir_m3 <- gof(undir_m3)
#gof_undir_m4 <- gof(undir_m4) #funktioniert nicht (weil nodematch sex)
gof_undir_m5 <- gof(undir_m5)
gof_undir_m6 <- gof(undir_m6)
gof_undir_m7 <- gof(undir_m7)
gof_undir_m8 <- gof(undir_m8)  
gof_undir_m9 <- gof(undir_m9)
#gof_undir_m10 <- gof(undir_m10) #Modell mit Term für GWDegree, Nicht gerechnet,lädt nicht
gof_undir_m11 <- gof(undir_m11)
gof_undir_m12 <- gof(undir_m12)


#### Enviroment Stand hier:
load("m12.RData")

plot(gof_undir_m1)
plot(gof_undir_m2)
plot(gof_undir_m3)
plot(gof_undir_m4)
plot(gof_undir_m5)
plot(gof_undir_m6)
plot(gof_undir_m7)
plot(gof_undir_m8)
plot(gof_undir_m9)
#plot(gof_undir_m10)
plot(gof_undir_m11)
plot(gof_undir_m12)

#MCMC prüfen 
mcmc.diagnostics(undir_m11)

#Save Model Coeff Plots
htmlreg(undir_m11, file ="undir_m11.html", single.row = TRUE, 
        custom.model.names = c("Koeffizienten und (SE)"),
        digits = 3,
        center = TRUE,
        caption = "Modell 11")


####Grafiken----
#Grafiken Netzwerke (Beispiele:)
g <- graph_from_literal(1-2, 1-3, 1-4, 
                        2-3,
                        4-5,
                        4-6,
                        1-6)
as_adjacency_matrix(g)
dg <- graph_from_literal(1-+2, 1-+3, 1+-4, 
                         2++3,
                         
                         4-+5,
                         4+-6,
                         1-+6)

as_adjacency_matrix(dg)
set.seed(123)
plot(dg,
     edge.arrow.size = 0,
     vertex.color = "cyan3"
)


set.seed(123)
plot(dg,
     edge.arrow.size = 0.9,
     vertex.color = "cyan3",
     
)

E(dg)$weights <- c(1, 1, 3, 1, 1, 1, 1, 1)
as_adjacency_matrix(dg, attr = "weights")
set.seed(123)
plot(dg,
     edge.arrow.size = 0.9,
     vertex.color = "cyan3",
     edge.label = E(dg)$weights
)

kstar <- make_star(6, mode="undirected")
plot(kstar,
     edge.arrow.size = 0.9,
     vertex.color = "cyan3"
)


#degree(undir_igraph_it) #ungewichtet, undirektet
#Degree Verteilung:
igraph::as_data_frame(undir_igraph_it, what = "vertices")%>%
  mutate(alldegrees = strength(igraph_it, weights = NULL, mode = "all"),
         outdegrees = strength(igraph_it, weights = NULL, mode = "out"),
         indegrees = strength(igraph_it, weights = NULL, mode = "in"),
         allstrength = strength(igraph_it, weights = E(igraph_it)$raw, mode = "all"),
         outstrength = strength(igraph_it, weights = E(igraph_it)$raw, mode = "out"),
         instrength = strength(igraph_it, weights = E(igraph_it)$raw, mode = "in"))%>%
  ggplot(aes(x = alldegrees))+
  geom_histogram(binwidth = 25, color = "black", fill = "white"
                 ,aes(y = ..density..)
  )+
  #  geom_density(alpha=.5, fill="#FF6666") +
  ylab("Prozent an Abgeordneten")+
  theme_minimal()+
  xlab("Knoten Grad")+
  scale_x_continuous(breaks=seq(0,500, 50))+
  #ggtitle("Histogram der Knotendichte")+
  geom_vline(aes(xintercept=mean(alldegrees)),
             color="blue", linetype="dashed", size=1)




#set Node colors according to paryname
#convert to igraph object
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
     vertex.size = 3,
     edge.arrow.size = 0.001,
     edge.width = 0.5,
     layout=layout_with_kk(igraph_it))
legend(x=1.1, y=-0.1,   # Coordinates 
       #"bottomright",
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







