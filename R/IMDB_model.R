###NOTE###
# This script was an attempt to extend our Erdos Influence network approach to IMDB movie database
# The plan was to use Leonardo DiCaprio as our "Erdos": that is, among all of Leonardo DiCaprio's
# costars in his movies, who is the most influential? Our models did not extend well to the film
# industry and this script is not very useful in looking at anything. 
###END NOTE###
library(stringr)
library(igraph)
library(qgraph)
actors <- read.csv('./data/finalaComb.csv',header = F)
raw <- read.csv('./data/raw.csv',header=F)
actors$V3 = as.numeric(actors$V3)

#make the coauthor matrix
frame <- matrix(data= 0, nrow = 455, ncol = 455)
act_vect <- as.character(actors$V1)
#real <- str_remove_all(act_vect, 'skip')
real <- act_vect 
real[real == "skip"] <- NA
real <- real[complete.cases(real)]
real <- str_trim(real)
colnames(frame) <- (real)
rownames(frame) <- real

i = 1
while(i < length(act_vect)){
  j = i
  #get the author and clean it up
  rowIDX = act_vect[i]
  rowIDX <- str_trim(rowIDX)
  while(j < 479 && act_vect[j]!="skip"){
    j = j+1
  }
  l = i+1
  while(l < j){
    author = act_vect[l]
    author = str_trim(author)
    frame[rowIDX,author] = 1
    frame[author,rowIDX] = 1
    print(author)
    l = l +1
  }
  i = j+1
}


#get degree
real <- actors 
real[real == "skip"] <- NA
real <- real[complete.cases(real),]
real$V1 <- str_trim(real$V1)
#infl
real$V2 <- as.numeric(as.character(real$V2))
real$influence <- real$V3/median(real$V3) + (2018-real$V2)/median(real$V2)

#plot the resulting network
g <- graph.adjacency(frame)
x <- get.edgelist(g,names = F)
V(g)$size = real$V3
V(g)$label = real$V1
V(g)$degree = real$outdegree
l <- qgraph.layout.fruchtermanreingold(x,vcount=vcount(g),
                                       area=8*(vcount(g)^2),repulse.rad=(vcount(g)^3.1))
plot(g,layout=l,vertex.size= 3*(real$V3),vertex.label=ifelse(V(g)$size >= 2,V(g)$label,NA), 
     edge.arrow.size =.1)

ttt <- degree(g,v=V(g))
real$outdegree <- ttt