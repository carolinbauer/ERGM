remove(list=ls())
setwd("C:/Users/Carol/OneDrive/Documents/Uni/SoSe24/Bachelorarbeit_ERGM/ERGM")
library(statnet)
library(igraph)
library(dplyr)
library(igraph)
library(ggplot2)


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




igraph::as_data_frame(undir_igraph_it, what = "both")

#deskr. statistik. ungerichtet:
df_undir$edges%>%
  group_by(from)%>%
  count(from)%>%
  ungroup()%>%
  summarize(mean_from = mean(n))

#gleich, weil ungerichtet:
df_undir$edges%>%
  group_by(to)%>%
  count(to)%>%
  ungroup()%>%
  summarize(mean_to = mean(n))

#deskr. statistik. ungerichtet:
df_undir$edges%>%
  group_by(from)%>%
  count(from)%>%
  ungroup()%>%
  summarize(min_from = min(n),
            max_from = max(n),
            median_from = median(n),
            mean_from = mean(n))

#gleich, weil ungerichtet:
df_undir$edges%>%
  group_by(to)%>%
  count(to)%>%
  ungroup()%>%
  summarize(mean_to = min(n))
