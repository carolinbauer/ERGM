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

network::set.vertex.attribute(network_it, "ageGroup", ageGroup)



#####create Igraph Object----
igraph_it <- intergraph::asIgraph(network_it)




#create columns of target and source attr.
df <- igraph::as_data_frame(igraph_it, what = "both")
df$edges$source_sex <- df$vertices[df$edges$from, "sex"]
df$edges$target_sex <- df$vertices[df$edges$to, "sex"]
df$edges$source_party <- df$vertices[df$edges$from, "party"]
df$edges$target_party <- df$vertices[df$edges$to, "party"]
df$edges$source_born <- df$vertices[df$edges$from, "born"]
df$edges$target_born <- df$vertices[df$edges$to, "born"]

#####clean enviroment----
rm(list=ls()[! ls() %in% c("igraph_it","network_it", "df")])









#### Data Exploration -------------------------------------------------------
#####vertexes----

###### How many vertexes do we have? (Number of MP) ----
length(V(igraph_it))

###### Which vertex attributes do we have? (Attributes of MP)----
vertex_attr_names(igraph_it)

####### When were the MPs born/ How old were they in 2008?----
#The Age range in 2008 was 26 to 82; 
#The mean age was almost 52 years, the median 51 years;
#50% of the MP were between the age 45 and 57 in 2008.
summary(2008 - vertex.attributes(igraph_it)$born) 
summary(vertex.attributes(igraph_it)$born) 

write.csv(df$vertices%>%
  mutate(age = 2008 - born,
          Altersgruppe = ifelse(between(age, 25, 39), "<40",
                                ifelse(between(age, 40, 49), "40 - 49",
                                       ifelse(between(age, 50, 59), "50 - 59",
                                              ifelse(between(age, 60, 69), "60 - 69",
                                                     ">= 70")))))%>%
  group_by(Altersgruppe)%>%
  summarize(Anzahl = n(),
            Anteil = round(n()/nrow(df$vertices),2),
            Mean_n_au = round(mean(n_au),2),
            Mean_n_co = round(mean(n_co),2))%>%
  arrange(match(Altersgruppe, c("<40", "40 - 49", "50 - 59", "60 - 69", ">= 70"))),
  "Altersgruppen.csv", row.names = F
  )






####### Which constituency do they belong to?----
#The MPs in the dataset belong to 21 different constituencies ("Wahlkreise")
length(table(vertex.attributes(igraph_it)$constituency))
#The constituency with only one MP from 2008-2013 is Aosta, 
#the constituency with most MPs from 2008-2013 is Lombaria with 104 MPs
sort(table(vertex.attributes(igraph_it)$constituency))

####### Where do the MPs sit on a political scale?----
#The italian parliament from 2008-2013 was rather right leaning, with a mean 
#of 5.5 on the political scale and a median of 7.1.
summary(vertex.attributes(igraph_it)$lr)

#Table of political parties and lr score
table <- data.frame(lr = vertex.attributes(igraph_it)$lr,
                    party = vertex.attributes(igraph_it)$party,
                    partyname = vertex.attributes(igraph_it)$partyname,
                    n_co = vertex.attributes(igraph_it)$n_co,
                    n_au = vertex.attributes(igraph_it)$n_au)
table[table$party == "UDC-TP", c("party", "partyname")]

write.csv(table %>%
  mutate(Partei = party,
         Parteiname =  ifelse(party == "FI-PDL", "Forza Italia - Il Popolo della Liberta", 
                              ifelse(party == "FLI-TP", "Futuro e Libertà per l'Italia",
                                     ifelse(party == "IDV", "Italia dei Valori", 
                                            ifelse(party == "IND", "Freie Abgeordnete",
                                                   ifelse(party == "LN", "Lega Nord",
                                                          ifelse(party == "PD", "Partito Democratico",
                                                                 ifelse(party == "PT", "Popolo e Territorio", "Unione di Centro"))))))))%>%
  group_by(Partei, Parteiname)%>%
  summarize(Anzahl = n(),
            Anteil = round(n()/nrow(table),2),
            Politische_Skala = mean(lr),
            Mean_Sponsor = round(mean(n_au), 2),
            Mean_Co_Sponsor = round(mean(n_co), 2)),
  "Parteien.csv", row.names = F)
  
  
  count(party, partyname, lr)%>%
  select(party, partyname, Anzahl = n, Anteil = n/nrow(table), lr)%>%
  mutate(partyname = ifelse(party == "FI-PDL", "Forza Italia - Il Popolo della Liberta", partyname),
         partyname = ifelse(party == "FLI-TP", "Futuro e Libertà per l'Italia", partyname),
         partyname = ifelse(party == "UDC-TP", "Unione di Centro", partyname),
         lr = as.character(lr),
         lr = ifelse(party == "IND", "5 (imputed)", lr))

remove(table)
####### How many Bills do the MPs Sponsor?----
#The Average Number of Bills sponsored per MP is 6
summary(vertex.attributes(igraph_it)$n_au)
sum(vertex.attributes(igraph_it)$n_au)

####### How many Bills do the MPs Co-Sponsor?----
#The average number of Bills Co-Sponsered per MP is 83
summary(vertex.attributes(igraph_it)$n_co)

table <- data.frame(cosponsored = vertex.attributes(igraph_it)$n_co,
                   sponsored = vertex.attributes(igraph_it)$n_au) 
# table %>%
#    summarise(n_of_at_least_1_cosponsored = sum(cosponsored>1),
#              n_of_at_least_1_sponsored = sum(sponsored>1))

remove(table)
####### How many years is the MP in Parliament for?----
#The mean amount of years im parliament is 4 year, the Median 2.
#The max number of years in parliament is 36 years, the min 0.
summary(vertex.attributes(igraph_it)$nyears)

####### How many MPs are men, how many women?----
#21% of MPs are women, 89% men.
table(vertex.attributes(igraph_it)$sex)
prop.table(table(vertex.attributes(igraph_it)$sex))

df$vertices%>%
  mutate(Geschlecht = ifelse(sex == "M", "männlich", "weiblich"))%>%
  group_by(Geschlecht)%>%
  summarize(Anzahl = n(),
            Anteil = round(n()/nrow(df$vertices),2))

write.csv(
  df$vertices%>%
            mutate(Geschlecht = ifelse(sex == "M", "männlich", "weiblich"))%>%
            group_by(Geschlecht)%>%
            summarize(Anzahl = n(),
                      Anteil = round(n()/nrow(df$vertices),2),
                      Mittel_sponsor = mean(n_au),
                      Mittel_co_sponsor = mean(n_co)),
          "Geschlecht.csv", row.names = F)


table <- data.frame(sex = vertex.attributes(igraph_it)$sex,
                    party = vertex.attributes(igraph_it)$party,
                    lr = vertex.attributes(igraph_it)$lr,
                    cosponsored = vertex.attributes(igraph_it)$n_co,
                    sponsored = vertex.attributes(igraph_it)$n_au,
                    born = vertex.attributes(igraph_it)$born) 

#Women cosponsor on average 104 bills, men 77.
#Women sponsor on average 8 bills, men 6.
#Women are on avrg. on the political scale at 5, men at 6. Womens median is 6, mens is 7.
#
table%>%
  group_by(sex)%>%
  summarise(mean_cosponsored = mean(cosponsored),
            mean_sponsored = mean(sponsored),
            mean_lr = mean(lr),
            median_lr = median(lr),
            age_mean = mean(2008-born))




#####edges--------------------
#With summay table
meta <- read.csv("parlnet.csv", sep = ",")
meta <- meta %>%
  filter(network == "net_it_ca2008")
#How many bills were cosigned?
#4007 Bills were cosponsored
meta%>%
  select(n_of_bills = n_bills_co)
#How many MPs sponsored a proposed bill on average?
#On average 11.5 MP were sponsors per bill, in median 3
meta%>%
  select(s_mu, s_med)
#How many sponsorships between two MPs on average?
#On average, two MPs have 2 connections through co-signing between each other
mean(E(igraph_it)$raw)

meta%>%
  select(raw_mu)




#Amount of MPS who co-sponsored at least 1 bill    662
length(unique(df$edges$from))

#Amount of MPs who sponsored at least 1 bill     520
length(unique(df$edges$to))




#Homophilie
#same sex:
table_sex <- df$edges %>%
  mutate(both_f = (source_sex == "F" & target_sex == "F"),
         both_m = (source_sex == "M" & target_sex == "M"),
         not_matching_sex = (source_sex != target_sex))%>%
  summarize(nr_both_f = length(which(both_f)),
            nr_both_m = length(which(both_m)),
            nr_matching_sex = length(which(both_f)) + length(which(both_m)),
            nr_diff_sex = length(which(not_matching_sex)),
            perc_both_f = length(which(both_f))/nrow(df$edges),
            perc_both_m = length(which(both_m))/nrow(df$edges),
            perc_not_matching_sex = length(which(both_m == F & both_f == F))/nrow(df$edges),
            perc_matching_sex = length(which(both_m == T | both_f == T))/nrow(df$edges))



df$edges %>%
  mutate(both_f = (source_sex == "F" & target_sex == "F"),
         both_m = (source_sex == "M" & target_sex == "M"),
         same_sex = (source_sex == target_sex))%>%
  group_by(source_sex)%>%
  summarize(perc_same_sex = length(which(same_sex))/n(),
            perc_diff_sex = length(which(!same_sex))/n())%>%
  mutate(perc_same_sex = round(perc_same_sex, 2),
         perc_diff_sex = round(perc_diff_sex, 2))




#same party:
table_party <- df$edges %>%
  mutate(same_party = ifelse(source_party == target_party, T, F))%>%
  summarize(nr_same_party = length(which(same_party)),
            nr_diff_party= length(which(!same_party)),
            perc_same_party = length(which(same_party))/nrow(df$edges),
            perc_diff_party = length(which(!same_party))/nrow(df$edges))

df$edges %>%
  mutate(same_party = ifelse(source_party == target_party, T, F))%>%
  group_by(source_party)%>%
  summarize(perc_same_party = length(which(same_party))/n(),
            perc_diff_party = length(which(!same_party))/n())%>%
  mutate(perc_same_party = round(perc_same_party, 2),
         perc_diff_party = round(perc_diff_party, 2))

#same agegroup:
table_age <- df$edges%>%
  mutate(source_age = 2008 - source_born,
         target_age = 2008 - target_born)%>%
  mutate(source_agegroup = ifelse(between(source_age, 25, 39), "<40",
                                 ifelse(between(source_age, 40, 49), "40 - 49",
                                        ifelse(between(source_age, 50, 59), "50 - 59",
                                               ifelse(between(source_age, 60, 69), "60 - 69",
                                                       ">= 70")))))%>%
  mutate(target_agegroup = ifelse(between(target_age, 25, 39), "<40",
                                  ifelse(between(target_age, 40, 49), "40 - 49",
                                         ifelse(between(target_age, 50, 59), "50 - 59",
                                                ifelse(between(target_age, 60, 69), "60 - 69",
                                                       ">= 70")))))%>%
  mutate(same_agegroup = ifelse(source_agegroup == target_agegroup, T, F))%>%
  summarize(nr_same_agegroup = length(which(same_agegroup)),
            nr_diff_agegroup = length(which(!same_agegroup)),
    perc_same_agegroup= length(which(same_agegroup))/n(),
            perc_diff_agegroup = length(which(!same_agegroup))/n())


df$edges%>%
  mutate(source_age = 2008 - source_born,
         target_age = 2008 - target_born)%>%
  mutate(source_agegroup = ifelse(between(source_age, 25, 39), "<40",
                                  ifelse(between(source_age, 40, 49), "40 - 49",
                                         ifelse(between(source_age, 50, 59), "50 - 59",
                                                ifelse(between(source_age, 60, 69), "60 - 69",
                                                       ">= 70")))))%>%
  mutate(target_agegroup = ifelse(between(target_age, 25, 39), "<40",
                                  ifelse(between(target_age, 40, 49), "40 - 49",
                                         ifelse(between(target_age, 50, 59), "50 - 59",
                                                ifelse(between(target_age, 60, 69), "60 - 69",
                                                       ">= 70")))))%>%
  mutate(same_agegroup = ifelse(source_agegroup == target_agegroup, T, F))%>%
  group_by(source_agegroup)%>%
  summarize(perc_same_agegroup= length(which(same_agegroup))/n(),
            perc_diff_agegroup = length(which(!same_agegroup))/n())%>%
  arrange(source_agegroup)


# 25-40
# 40-50
# 50-60
# 60-70
# 70-85

table_match <- data.frame(Kategorie = c("Partei", "Geschlecht", "Altersgruppe"),
           Anzahl_Match = c(table_party$nr_same_party,
                            table_sex$nr_matching_sex,
                            table_age$nr_same_agegroup),
           Anzahl_Diff = c(table_party$nr_diff_party,
                            table_sex$nr_diff_sex,
                            table_age$nr_diff_agegroup))

write.csv(table_match %>%
  group_by(Kategorie)%>%
  mutate(Anteil_Match = round(Anzahl_Match/sum(Anzahl_Match, Anzahl_Diff),2),
         Anteil_Diff = round(Anzahl_Diff/sum(Anzahl_Match, Anzahl_Diff),2))%>%
  select(Kategorie, Anteil_Match, Anteil_Diff),
  "Match_Diff.csv", row.names = F)



#### Plots ------
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
     edge.arrow.size = 0.01,
     layout=layout_in_circle(igraph_it))
legend(x=-1.75, y=0.1,   # Coordinates 
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


set.seed(240)
plot(igraph_it,
     vertex.label = NA,
     vertex.size = 3,
     edge.arrow.size = 0.001,
     edge.width = 0.5,
     layout=layout_with_kk(igraph_it))

legend(#x=-1.75, y=0.1,   # Coordinates 
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



set.seed(240)
plot(igraph_it,
     vertex.label = NA,
     vertex.size = 3,
     edge.arrow.size = 0.01,
     edge.width = 0.1,
     layout=layout.fruchterman.reingold)

legend(#x=-1.75, y=0.1,   # Coordinates 
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


set.seed(240)
plot(igraph_it,
     vertex.label = NA,
     vertex.size = 3,
     edge.arrow.size = 0.01,
     edge.width = 0.1,
     layout=layout_nicely(igraph_it))




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





### In and Out Degree----
#network::set.edge.value(network_it, "weight", network::get.edge.attribute(network_it, "raw"))

hist(degree(igraph_it, mode = "all"),
     col="grey", 
     main = "Grad der Knoten",
     xlim = c(0, 350),
     xlim=c(0,50),
     4 + xlab="Vertex Degree", ylab="Frequency", main="")


#Die meisten Knotenpunkte haben einen in-strength von 0-50 (gewichtet, Eingang von co-signing)
hist(strength(igraph_it, weights = E(igraph_it)$raw, mode = "in"),
     col = "grey",
     breaks = 20,
     xlab="Grad der Knoten",
     ylab="Anzahl an Knoten",
     main="")
#UNGEWICHTET IN DEGREE:
#Die meisten Knotenpunkte haben einen in-degree von 0-ca25 (ungewichtet, Anzahl von Edges als Sponsor)
hist(strength(igraph_it, weights = NULL, mode = "in"),
     col = "grey",
     breaks = 15,
     xlab="Grad der Knoten",
     ylab="Anzahl an Knoten",
     main="")

#Gewichtet Out-Degree
#Die meisten Knotenpunkte haben einen out-strength von 0-150 (gewichtet, Vergabe von co-signing)
hist(strength(igraph_it, weights = E(igraph_it)$raw, mode = "out"),
     col = "grey",
     breaks = 20,
     xlab="Grad der Knoten",
     ylab="Anzahl an Knoten",
     main="")

#UNGEWICHTET OUT-DEGREE
#Hier haben die meisten Knotenpunkte einen out-degree von 25-50. etwas weniger 0-25 (ungewichtet, Anzahl von Edges als Co-Sponsor)
hist(strength(igraph_it, weights = NULL, mode = "out"),
     col = "grey",
     breaks = 20,
     xlab="Grad der Knoten",
     ylab="Anzahl an Knoten",
     main="")





hist(strength(igraph_it, weights = E(igraph_it)$raw, mode = "all"),
     col = "grey",
     breaks = 50,
     xlab="Grad der Knoten",
     ylab="Anzahl an Knoten",
     main="Histogram der Knotengrade")

hist(strength(igraph_it, weights = E(igraph_it)$raw, mode = "all"),
     freq = F,
     col = "grey",
     breaks = 50,
     xlab="Grad der Knoten",
     ylab="Dichte",
     main="")

#Number of Edges
sum(strength(igraph_it,
  weights = NULL))/2

#Number of Bills
sum(strength(igraph_it,
             weights = E(igraph_it)$raw))/2











#### plots in- and out-degrees/strength

#Histogramm Knotenstärke In und Out
df$vertices%>%
  mutate(alldegrees = strength(igraph_it, weights = NULL, mode = "all"),
         outdegrees = strength(igraph_it, weights = NULL, mode = "out"),
         indegrees = strength(igraph_it, weights = NULL, mode = "in"),
         allstrength = strength(igraph_it, weights = E(igraph_it)$raw, mode = "all"),
         outstrength = strength(igraph_it, weights = E(igraph_it)$raw, mode = "out"),
         instrength = strength(igraph_it, weights = E(igraph_it)$raw, mode = "in"))%>%
  ggplot(aes(x = allstrength))+
  geom_histogram(binwidth = 50, color = "black", fill = "white"
                 ,aes(y = ..density..)
                 )+
#  geom_density(alpha=.5, fill="#FF6666") +
  ylab("Prozent an Abgeordneten")+
  theme_minimal()+
  xlab("Stärke der Knoten")+
  scale_x_continuous(breaks=seq(0,2000, 100))+
  ggtitle("Histogram der Knotenstärke (gewichtet) - in und out")+
  geom_vline(aes(xintercept=mean(allstrength)),
             color="blue", linetype="dashed", size=1)

#Histogramm der Knotenstärke Out
df$vertices%>%
  mutate(alldegrees = strength(igraph_it, weights = NULL, mode = "all"),
         outdegrees = strength(igraph_it, weights = NULL, mode = "out"),
         indegrees = strength(igraph_it, weights = NULL, mode = "in"),
         allstrength = strength(igraph_it, weights = E(igraph_it)$raw, mode = "all"),
         outstrength = strength(igraph_it, weights = E(igraph_it)$raw, mode = "out"),
         instrength = strength(igraph_it, weights = E(igraph_it)$raw, mode = "in"))%>%
  ggplot(aes(x = outstrength))+
  geom_histogram(binwidth = 50, color = "black", fill = "white"
                 ,aes(y = ..density..)
  )+
#  geom_density(alpha=.5, fill="#FF6666") +
  ylab("Prozent an Abgeordneten")+
  theme_minimal()+
  xlab("Stärke der Knoten")+
  scale_x_continuous(breaks=seq(0,2000, 100))+
  ggtitle("Histogram der Knotenstärke (gewichtet) - out")+
  geom_vline(aes(xintercept=mean(outstrength)),
             color="blue", linetype="dashed", size=1)

#Histogramm der Knotenstärke in
df$vertices%>%
  mutate(alldegrees = strength(igraph_it, weights = NULL, mode = "all"),
         outdegrees = strength(igraph_it, weights = NULL, mode = "out"),
         indegrees = strength(igraph_it, weights = NULL, mode = "in"),
         allstrength = strength(igraph_it, weights = E(igraph_it)$raw, mode = "all"),
         outstrength = strength(igraph_it, weights = E(igraph_it)$raw, mode = "out"),
         instrength = strength(igraph_it, weights = E(igraph_it)$raw, mode = "in"))%>%
  ggplot(aes(x = instrength))+
  geom_histogram(binwidth = 50, color = "black", fill = "white"
                 ,aes(y = ..density..)
  )+
#  geom_density(alpha=.5, fill="#FF6666") +
  ylab("Prozent an Abgeordneten")+
  theme_minimal()+
  xlab("Stärke der Knoten")+
  scale_x_continuous(breaks=seq(0,2000, 100))+
  ggtitle("Histogram der Knotenstärke (gewichtet) - in")+
  geom_vline(aes(xintercept=mean(instrength)),
             color="blue", linetype="dashed", size=1)



#Knotendichte------
#Histogramm Knotendichte In und Out
df$vertices%>%
  mutate(alldegrees = strength(igraph_it, weights = NULL, mode = "all"),
         outdegrees = strength(igraph_it, weights = NULL, mode = "out"),
         indegrees = strength(igraph_it, weights = NULL, mode = "in"),
         allstrength = strength(igraph_it, weights = E(igraph_it)$raw, mode = "all"),
         outstrength = strength(igraph_it, weights = E(igraph_it)$raw, mode = "out"),
         instrength = strength(igraph_it, weights = E(igraph_it)$raw, mode = "in"))%>%
  ggplot(aes(x = alldegrees))+
  geom_histogram(binwidth = 50, color = "black", fill = "white"
                 ,aes(y = ..density..)
  )+
#  geom_density(alpha=.5, fill="#FF6666") +
  ylab("Prozent an Abgeordneten")+
  theme_minimal()+
  xlab("Dichte der Knoten")+
  scale_x_continuous(breaks=seq(0,2000, 100))+
  ggtitle("Histogram der Knotendichte (ungewichtet) - in und out")+
  geom_vline(aes(xintercept=mean(alldegrees)),
             color="blue", linetype="dashed", size=1)


#Histogramm der knotendichte Out
df$vertices%>%
  mutate(alldegrees = strength(igraph_it, weights = NULL, mode = "all"),
         outdegrees = strength(igraph_it, weights = NULL, mode = "out"),
         indegrees = strength(igraph_it, weights = NULL, mode = "in"),
         allstrength = strength(igraph_it, weights = E(igraph_it)$raw, mode = "all"),
         outstrength = strength(igraph_it, weights = E(igraph_it)$raw, mode = "out"),
         instrength = strength(igraph_it, weights = E(igraph_it)$raw, mode = "in"))%>%
  ggplot(aes(x = outdegrees))+
  geom_histogram(binwidth = 50, color = "black", fill = "white"
                 ,aes(y = ..density..)
  )+
#  geom_density(alpha=.5, fill="#FF6666") +
  ylab("Prozent an Abgeordneten")+
  theme_minimal()+
  xlab("Dichte der Knoten")+

  scale_x_continuous(breaks=seq(0,2000, 100))+
  ggtitle("Histogram der Knotendichte (ungewichtet) - out")+
  geom_vline(aes(xintercept=mean(outdegrees)),
             color="blue", linetype="dashed", size=1)

#Histogramm der knotendichte in
df$vertices%>%
  mutate(alldegrees = strength(igraph_it, weights = NULL, mode = "all"),
         outdegrees = strength(igraph_it, weights = NULL, mode = "out"),
         indegrees = strength(igraph_it, weights = NULL, mode = "in"),
         allstrength = strength(igraph_it, weights = E(igraph_it)$raw, mode = "all"),
         outstrength = strength(igraph_it, weights = E(igraph_it)$raw, mode = "out"),
         instrength = strength(igraph_it, weights = E(igraph_it)$raw, mode = "in"))%>%
  ggplot(aes(x = instrength))+
  geom_histogram(binwidth = 50, color = "black", fill = "white"
                 ,aes(y = ..density..)
  )+
#  geom_density(alpha=.5, fill="#FF6666") +
  ylab("Prozent an Abgeordneten")+
  theme_minimal()+
  xlab("Dichte der Knoten")+
  scale_x_continuous(breaks=seq(0,2000, 100))+
  ggtitle("Histogram der Knotendichte (ungewichtet) - in")+
  geom_vline(aes(xintercept=mean(indegrees)),
             color="blue", linetype="dashed", size=1)





plot(degree.distribution(igraph_it), pch= 19)


df$edges%>%
#  group_by(source)%>%
  select(source, target, raw)%>%
#  summarize(n_out = )
  filter(source == "ANDREA RONCHI")

df$vertices$vertex.names

  summarise(n_out = raw)%>%
  ggplot(aes(x = n_out))+
  geom_histogram(binwidth = 10, color = "black", fill = "white")+
  theme_minimal()+
  xlab("Grad der Out-Degree")+
  ylab("Anzahl an Abgeordneter")


df$edges%>%
  group_by(source)%>%
  summarise(n_out = n())%>%
  ggplot(aes(x = n_out))+
  geom_histogram(binwidth = 10, color = "black", fill = "white")+
  theme_minimal()+
  xlab("Grad der Out-Degree")+
  ylab("Anzahl an Abgeordneter")






