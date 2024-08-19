remove(list=ls())
setwd("C:/Users/Carol/OneDrive/Documents/Uni/SoSe24/Bachelorarbeit_ERGM/ERGM")
library(statnet)
library(igraph)
# library(dplyr)
# library(ggplot2)
# library(stargazer)
load("ModelsNew.RData")
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

#####create Igraph Object----
igraph_it <- intergraph::asIgraph(network_it)


#####clean enviroment----
rm(list=ls()[! ls() %in% c("igraph_it","network_it")])

set.seed(123)
m1 <- ergm(network_it ~ edges +
             nodematch('party') +
             nodematch('sex')+
             nodematch('ageGroup')
)
summary(m1) #Modell mit Homophilie



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

#Speichert Tabellen als html Datei im Verzeichnis
htmlreg(m1, file ="m1.html", single.row = TRUE, 
        custom.model.names = c("Koeffizienten und (SE)"),
        digits = 3,
        center = TRUE,
        caption = "Modell 1 gerichtet: Modell mit Homophilie")
htmlreg(m2, file ="m2.html", single.row = TRUE, 
        custom.model.names = c("Koeffizienten und (SE)"),
        digits = 3,
        center = TRUE,
        caption = "Modell 2 gerichtet: Homophilie und Mutualität")
htmlreg(m3, file ="m3.html", single.row = TRUE,
        custom.model.names = c("Koeffizienten und (SE)"),
        digits = 3,
        center = TRUE,
        caption = "Modell 3 gerichtet: Differenzierte Homopholie ohne Mutualität",
)
htmlreg(m4, file ="m4.html", single.row = TRUE, 
        custom.model.names = c("Koeffizienten und (SE)"),
        digits = 3,
        center = TRUE,
        caption = "Modell 4 gerichtet: Differenzierte Homopholie und Mutualität")




plot(gof_m2)
plot(gof_m4)
