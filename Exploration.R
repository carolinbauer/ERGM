remove(list=ls())
setwd("C:/Users/Carol/OneDrive/Documents/Uni/SoSe24/Bachelorarbeit_ERGM/ERGM")
library(statnet)
library(igraph)
library(dplyr)

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

#####create Igraph Object----
igraph_it <- intergraph::asIgraph(network_it)

#####clean enviroment----
rm(list=ls()[! ls() %in% c("igraph_it","network_it")])

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
                    partyname = vertex.attributes(igraph_it)$partyname)
table %>%
  group_by(party, partyname)%>%
  count(party, partyname, lr)%>%
  select(party, partyname, n, lr)%>%
  mutate(partyname = ifelse(party == "FI-PDL", "Forza Italia - Il Popolo della Liberta", partyname),
         partyname = ifelse(party == "FLI-TP", "Futuro e Libertà per l'Italia", partyname),
         partyname = ifelse(party == "UDC-TP", "Unione di Centr", partyname),
         lr = as.character(lr),
         lr = ifelse(party == "IND", "5 (imputed)", lr))

remove(table)
####### How many Bills do the MPs Sponsor?----
#The Average Number of Bills sponsored per MP is 6
summary(vertex.attributes(igraph_it)$n_au)


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
meta%>%
  select(raw_mu)




#create columns of target and source attr.
df <- igraph::as_data_frame(igraph_it, what = "both")
df$edges$source_sex <- df$vertices[df$edges$from, "sex"]
df$edges$target_sex <- df$vertices[df$edges$to, "sex"]
df$edges$source_party <- df$vertices[df$edges$from, "party"]
df$edges$target_party <- df$vertices[df$edges$to, "party"]
df$edges$source_born <- df$vertices[df$edges$from, "born"]
df$edges$target_born <- df$vertices[df$edges$to, "born"]

#Amount of MPS who co-sponsored at least 1 bill    662
length(unique(df$edges$from))
#Amount of MPs who sponsored at least 1 bill     520
length(unique(df$edges$to))

#Homophilie
#same sex:
df$edges %>%
  mutate(both_f = (source_sex == "F" & target_sex == "F"),
         both_m = (source_sex == "M" & target_sex == "M"))%>%
  summarize(nr_both_f = length(which(both_f)),
            nr_both_m = length(which(both_m)),
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
df$edges %>%
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
  summarize(perc_same_agegroup= length(which(same_agegroup))/n(),
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
     vertex.size = 7,
     edge.arrow.size = 0.01,
     layout=layout_in_circle(igraph_it))

set.seed(240)
plot(igraph_it,
     vertex.label = NA,
     vertex.size = 7,
     edge.arrow.size = 0.01,
     layout=layout_nicely(igraph_it))

set.seed(240)
plot(igraph_it,
     vertex.label = NA,
     vertex.size = 7,
     edge.arrow.size = 0.01,
     layout=layout.fruchterman.reingold)

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
network::set.edge.value(network_it, "Weight") <- network::get.edge.attribute(network_it, "raw")
hist(degree(igraph_it, mode = "all"), col="grey", 
     main = "Grad der Knoten",
     xlim = c(0, 350), )
     , xlim=c(0,50),
     4 + xlab="Vertex Degree", ylab="Frequency", main="")



sum(degree(igraph_it, mode = "all"))
set.ed
set.edge.value()







