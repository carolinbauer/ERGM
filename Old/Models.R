remove(list=ls())
setwd("C:/Users/Carol/OneDrive/Documents/Uni/SoSe24/Bachelorarbeit_ERGM/ERGM")
library(statnet)
library(igraph)
library(dplyr)
library(ggplot2)
library(stargazer)

#load Data
network_it <- readRDS("network.RData")

#### Data transformations -------------------------------------------------

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

# #create edge variable for sex (both m, both f, not matching)
# source <- network::get.edge.attribute(network_it, "source")
# target <- network::get.edge.attribute(network_it, "target")
# name <- network::get.vertex.attribute(network_it, "vertex.names")
# sex <- network::get.vertex.attribute(network_it, "sex")
# 
# df <- data.frame(name, sex, row.names = NULL)
# source_indx <- match(source, df$name)
# source_sex <- df$sex[source_indx]
# target_indx <- match(target, df$name)
# target_sex <- df$sex[target_indx]
# 
# network::set.edge.attribute(network_it, "source_sex", source_sex)
# network::set.edge.attribute(network_it, "target_sex", target_sex)
# 
# edge_sex_match <- data.frame(source_sex, target_sex)%>%
#   mutate(edge_sex_match = ifelse(source_sex == "M" & target_sex == "M", "both M",
#                            ifelse(source_sex == "F" & target_sex == "F", "both F", "no match")))%>%
#   select(edge_sex_match)
# 
# network::set.edge.attribute(network_it, "edge_sex_match", edge_sex_match)

#####create Igraph Object----
igraph_it <- intergraph::asIgraph(network_it)


#####clean enviroment----
rm(list=ls()[! ls() %in% c("igraph_it","network_it", "m1", "m2", "m3")])


####Simple Model----
set.seed(123)
m1 <- ergm(network_it ~ edges +
             nodematch('party') +
             nodematch('sex')+
             nodematch('ageGroup')
)
summary(m1) #Modell mit Homophilie

set.seed(123)
m1_undir <- ergm(network_it ~ edges +
             nodematch('party') +
             nodematch('sex')+
             nodematch('ageGroup')
)
summary(m1) #Modell mit Homophilie undirected



set.seed(123)
m2 <- ergm(network_it ~ edges +
             nodematch('party') +
             nodematch('sex')+
             nodematch('ageGroup')+
             mutual
)
summary(m2) #Modell mit Homophilie und Mutualität

set.seed(123)
m2_reg <- ergm(network_it ~ edges +
             nodematch('party') +
             nodematch('sex')+
             nodematch('ageGroup')+
             nodematch('constituency')+
             mutual
)
summary(m2_reg) #Modell mit Homophilie und Mutualität (Auch RegionaleHomophilie)

set.seed(123)
m2_mix <- ergm(network_it ~ edges +
                 # nodemix('party') +
                 nodemix('sex')+
                 # nodemix('ageGroup')+
                 # nodemix('constituency')+
                 mutual
)
summary(m2_mix) #Modell MIX

# set.seed(123)
# m2_mix_diff <- ergm(network_it ~ edges +
#                  nodemix('party', diff = T) +
#                  nodemix('sex', diff = T)+
#                  nodemix('ageGroup', diff = T)+
#                  nodemix('constituency', diff = T)+
#                  mutual
# )
# summary(m2_mix_diff) #Ohne Homophilie und in Gruppen aufgeteilt


set.seed(123)
m3 <- ergm(network_it ~ edges +
             nodematch('party', diff = T, levels = c(1:8)) +
             nodematch('sex', diff = T, levels = c(1,2))+
             nodematch('ageGroup', diff = T, levels = c(1:5))
)
summary(m3) #Modell mit Homophilie aufgegliedert nach Kategorien

set.seed(123)
m4 <- ergm(network_it ~ edges +
             nodematch('party', diff = T, levels = c(1:8)) +
             nodematch('sex', diff = T, levels = c(1,2))+
             nodematch('ageGroup', diff = T, levels = c(1:5))+
             mutual
)
summary(m4) #Modell mit Homophilie aufgegliedert nach Kategorien + Mutualität

stargazer(m4, "html")

as.vector(unlist(lapply(list(m1,m2, m3, m4), AIC)))
AIC(m4)#ist minimum
as.vector(unlist(lapply(list(m1,m2, m3, m4), BIC)))
BIC(m4)#ist minimum


##Tabellen der Modelle für die Präsentation----
citation("texreg")
library(texreg)

#Speichert Tabellen als html Datei im Verzeichnis
htmlreg(m1, file ="m1.html", single.row = TRUE, 
        custom.model.names = c("Koeffizienten und (SE)"),
        digits = 3,
        center = TRUE,
        caption = "Modell mit Homophilie")
htmlreg(m2, file ="m2.html", single.row = TRUE, 
        custom.model.names = c("Koeffizienten und (SE)"),
        digits = 3,
        center = TRUE,
        caption = "Modell mit Homophilie und Mutualität")
htmlreg(m3, file ="m3.html", single.row = TRUE,
        custom.model.names = c("Koeffizienten und (SE)"),
        digits = 3,
        center = TRUE,
        caption = "Modell zur Homopholie über Kategorien ohne Mutualität",
        )
htmlreg(m4, file ="m4.html", single.row = TRUE, 
        custom.model.names = c("Koeffizienten und (SE)"),
        digits = 3,
        center = TRUE,
        caption = "Modell zur Homopholie über verschiedene Kategorien und Mutualität")





m0 <- ergm(network_it ~edges)
summary(m0)
network.density(network_it)

#gof_m2 <- gof(m2)
#gof_m2_2 <- gof(m2 ~ model + esp + distance)
gof_m2_2
gof_m2
par(mfrow = c(1,1))
plot(gof_m2_2, main = "")
par(mfrow = c(2, 3))
plot(gof_m2, main = '')








set.seed(123)
m_no <- ergm(network_it ~edges + mutual +
             nodefactor('party_relevel')+
             nodefactor('sex')+
             nodefactor('ageGroup'))
summary(m_no) #ohne homophilie, nur verschiedene faktorlevels


