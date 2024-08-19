
getwd()
setwd('downloads')
load("modelfull.RData")

install.packages("ergm")
library("ergm")

set.seed(123)
undir_m7 <- ergm(undir_network_it ~ edges +
                   gwesp(0.2, fixed = T)+
                   nodematch("party_relevel")+
                   nodematch("sex")+
                   nodematch("ageGroup")
                   )
summary(undir_m7)

set.seed(123)
undir_m9 <- ergm(undir_network_it ~ edges +
                    gwesp(0.2, fixed = T)+
                    nodematch("party_relevel", diff = T)+
                    nodematch("sex")+
                    nodematch("ageGroup", diff = T)
)
summary(undir_m9)


set.seed(123)
undir_m10 <- ergm(undir_network_it ~ edges +
                   gwdegree(0.2, fixed = T)+
                   nodematch("party_relevel", diff = T)+
                   nodematch("sex")+
                   nodematch("ageGroup", diff = T)
)




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
summary(undir_m11)

gof_undir_m7 <- gof(undir_m7)
gof_undir_m9 <- gof(undir_m9)
gof_undir_m10 <- gof(undir_m10)
gof_undir_m11 <- gof(undir_m11)
plot(gof_undir_m11)
AIC(undir_m4)
