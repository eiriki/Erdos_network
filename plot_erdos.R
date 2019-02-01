library(igraph)
library(qgraph)

#project data.xlsx was produced by another student in a different script. 
#should not be hard to reproduce: comes from the Erdos1.txt
data <- read_xlsx('./data/project data.xlsx',col_names = F)
colnames(data) <- c("Name","Year","Count")

#replace the 300 NAs with value of 1, which is what the NA's represent (1 paper with Erdos)
data$Count[is.na(data$Count)] <- 1

# Use our model to calculate influence
data$Influence <- (2018-data$Year)/median(data$Year) + data$Count/median(data$Count)

#read in coauthor matrix
coauth <- readRDS('./data/coauthor.RDS')

g <- graph.adjacency(coauth)
x <- get.edgelist(g,names = F)
V(g)$size = data$Influence
V(g)$type = cut(data$Influence,breaks = 4)
l <- qgraph.layout.fruchtermanreingold(x,vcount=vcount(g),
                                       area=8*(vcount(g)^2),repulse.rad=(vcount(g)^3.1))
plot(g,layout=l,vertex.size= 2*log(data$Influence),vertex.label= NA,edge.arrow.size =.1)

# run2 <- read.csv('./InfluenceModel2.csv')
# run2 <- run2[-512,]

#use a different Influence model: we add a third term using coauthor matrix 
#we use sum of all the authors worked with, then add 1 to account for Erdos
new_model_term <- colSums(coauth) + 1  
V(g)$size = data$Influence + (new_model_term/ median(new_model_term))
#setting the influence value to the new model
data$Influence <- data$Influence + (new_model_term/ median(new_model_term))

plot(g,layout=l,vertex.size = 2*log(data$Influence),vertex.label= NA,edge.arrow.size =.1)
#adding this term seemed to increase the influence of central nodes with less amount of 
#papers with Erdos himself

#degree calulations
out <- degree(g,v=V(g))
data$outdegree <- out

#adding the new model term term to the master table
data$Coauthors <- new_model_term
