load("workspacemodels2.RData")

set.seed(123)
# m2 <- ergm(network_it ~ edges + mutual +
#              nodematch('party_relevel') +
#              nodematch('sex')+
#              absdiff('lr')+
#              absdiff('nyears')
# )
summary(m2) #Modell mit Homophilie und Mutualität

gof_m2
mcmc.diagnostics(m2)

set.seed(123)
# m3 <- ergm(network_it ~ edges + mutual +
#              nodematch('party_relevel') +
#              nodematch('sex')+
#              absdiff('lr')+
#              absdiff('nyears')+
#              nodeofactor('party_relevel')+
#              nodeifactor('party_relevel')+
#              nodeofactor('sex')+
#              nodeifactor('sex')
# )
summary(m3)

gof_m3
mcmc.diagnostics(m3)

set.seed(123)
# m4 <- ergm(network_it ~ edges + mutual +
#              nodematch('party_relevel') + #effect of same party ties
#              nodematch('sex')+ #effect of same sex ties
#              absdiff('lr')+ #effect of distance on political scale
#              absdiff('nyears')+ #effect of seniority distance
#              nodeofactor('party_relevel')+ #effect of party of co-sponsor (sender)
#              nodeifactor('party_relevel')+ #effect of party of sponsor (receiver)
#              nodeofactor('sex')+ #effect of sex of co-sponsor (sender)
#              nodeifactor('sex')+
#              nodeocov('nyears')+ #effect of seniority of co-sponsor (sender)
#              nodeicov('nyears')
# )
summary(m4) #Modell mit Homophilie, Mutualität, Vertexattr. jeweils für Sponsor und Co-Sponsor



gof_m4  #p-Value soll nah an 1 sein (hier nur bei: Goodness-of-fit for model statistics)
#plot(gof_m4)
mcmc.diagnostics(m4)#Sample statistics auto-correlation: GUT: nah to 0 nach lag 0


#Interpr. der Hompophilie Teile:
#exp(beta) Die ODDS zum  Co-Sponsoring ändern sich (steigen) multiplikativ, 
#c.t. wenn die Parlamentsmitgl. der gleichen Partei/Geschlecht angehören

exp(coef(m4)[3:4])
#exp(beta) Die ODDS zum Co-Sponsoring ändern sich (sinken) multiplikativ, 
#c.t. wenn sich der Abstand (auf pol. Skala/ in Seniority) um 1 Einheit vergrößert. 
#(1 jahr, 1 Skaleneinheit auf Politischer skala)
exp(coef(m4)[5:6])


coef(m4)
