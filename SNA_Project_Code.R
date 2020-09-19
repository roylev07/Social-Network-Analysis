library(sna)
library(igraph)
library(intergraph)
library(centiserve)
library(ergm)
library(arm)


#---------Q1---------------------------------------

# ---Q1_A-------------
Q1<-read.csv(file.choose(),header=TRUE)

decade_1950 <- Q1[Q1$styear>=1950 & Q1$styear<1960, 1:2]
decade_1950$Counter <- 1
decade_1960 <- Q1[Q1$styear>=1960 & Q1$styear<1970, 1:2]
decade_1960$Counter <- 1
decade_1970 <- Q1[Q1$styear>=1970 & Q1$styear<1980, 1:2]
decade_1970$Counter <- 1
decade_1980 <- Q1[Q1$styear>=1980 & Q1$styear<1990, 1:2]
decade_1980$Counter <- 1
decade_1990 <- Q1[Q1$styear>=1990 & Q1$styear<2000, 1:2]
decade_1990$Counter <- 1
decade_2000 <- Q1[Q1$styear>=2000 & Q1$styear<=2010, 1:2]
decade_2000$Counter <- 1

graph_1950 <- graph_from_data_frame(decade_1950, directed = F, vertices = NULL)
graph_1960 <- graph_from_data_frame(decade_1960, directed = F, vertices = NULL)
graph_1970 <- graph_from_data_frame(decade_1970, directed = F, vertices = NULL)
graph_1980 <- graph_from_data_frame(decade_1980, directed = F, vertices = NULL)
graph_1990 <- graph_from_data_frame(decade_1990, directed = F, vertices = NULL)
graph_2000 <- graph_from_data_frame(decade_2000, directed = F, vertices = NULL)

graph_1950<-igraph::simplify(graph_1950,remove.multiple = T,edge.attr.comb = "min")
graph_1960<-igraph::simplify(graph_1960,remove.multiple = T,edge.attr.comb = "min")
graph_1970<-igraph::simplify(graph_1970,remove.multiple = T,edge.attr.comb = "min")
graph_1980<-igraph::simplify(graph_1980,remove.multiple = T,edge.attr.comb = "min")
graph_1990<-igraph::simplify(graph_1990,remove.multiple = T,edge.attr.comb = "min")
graph_2000<-igraph::simplify(graph_2000,remove.multiple = T,edge.attr.comb = "min")

V(graph_1950)
V(graph_1960)
V(graph_1970)
V(graph_1980)
V(graph_1990)
V(graph_2000)

# ---Q1_B-------------
groups_1950 <- edge.betweenness.community(graph_1950, modularity = T)
groups_1950$membership
igraph::modularity(graph_1950,groups_1950$membership)
l<-layout_with_fr(graph_1950)
plot(graph_1950, edge.lty=1, edge.arrow.size=0.1, layout=l*0.2-1.4,rescale=F,vertex.label.cex = 0.7,vertex.size=12, vertex.color=groups_1950$membership,vertex.label.color= "black")
ISR_membership <- groups_1950$membership[which(V(graph_1950)$name == "ISR")]
V(graph_1950)$name[groups_1950$membership== ISR_membership]


groups_1960 <- edge.betweenness.community(graph_1960, modularity = T)
groups_1960$membership
igraph::modularity(graph_1960,groups_1960$membership)
l<-layout_with_fr(graph_1960)
plot(graph_1960, edge.lty=1, edge.arrow.size=0.1, layout=l*0.16-0.7,rescale=F,vertex.label.cex = 0.7,vertex.size=12, vertex.color=groups_1960$membership,vertex.label.color= "black")
ISR_membership <- groups_1960$membership[which(V(graph_1960)$name == "ISR")]
V(graph_1960)$name[groups_1960$membership== ISR_membership]

groups_1970 <- edge.betweenness.community(graph_1970, modularity = T)
groups_1970$membership
igraph::modularity(graph_1970,groups_1970$membership)
l<-layout_with_fr(graph_1970)
plot(graph_1970, edge.lty=1, edge.arrow.size=0.1, layout=l*0.16-0.7,rescale=F,vertex.label.cex = 0.7,vertex.size=12, vertex.color=groups_1970$membership,vertex.label.color= "black")
ISR_membership <- groups_1970$membership[which(V(graph_1970)$name == "ISR")]
V(graph_1970)$name[groups_1970$membership== ISR_membership]

groups_1980 <- edge.betweenness.community(graph_1980, modularity = T)
groups_1980$membership
igraph::modularity(graph_1980,groups_1980$membership)
l<-layout_with_fr(graph_1980)
plot(graph_1980, edge.lty=1, edge.arrow.size=0.1, layout=l*0.16-0.7,rescale=F,vertex.label.cex = 0.7,vertex.size=12, vertex.color=groups_1980$membership,vertex.label.color= "black")
ISR_membership <- groups_1980$membership[which(V(graph_1980)$name == "ISR")]
V(graph_1980)$name[groups_1980$membership== ISR_membership]

groups_1990 <- edge.betweenness.community(graph_1990, modularity = T)
groups_1990$membership
igraph::modularity(graph_1990,groups_1990$membership)
l<-layout_with_fr(graph_1990)
plot(graph_1990, edge.lty=1, edge.arrow.size=0.1, layout=l*0.16-0.7,rescale=F,vertex.label.cex = 0.7,vertex.size=12, vertex.color=groups_1990$membership,vertex.label.color= "black")
ISR_membership <- groups_1990$membership[which(V(graph_1990)$name == "ISR")]
V(graph_1990)$name[groups_1990$membership== ISR_membership]

groups_2000 <- edge.betweenness.community(graph_2000, modularity = T)
groups_2000$membership
igraph::modularity(graph_2000,groups_2000$membership)
l<-layout_with_fr(graph_2000)
plot(graph_2000, edge.lty=1, edge.arrow.size=0.1, layout=l*0.16-0.7,rescale=F,vertex.label.cex = 0.7,vertex.size=12, vertex.color=groups_2000$membership,vertex.label.color= "black")
ISR_membership <- groups_2000$membership[which(V(graph_2000)$name == "ISR")]
V(graph_2000)$name[groups_2000$membership== ISR_membership]

# ---Q1_C-------------
weighted_graph_1950 <- graph_from_data_frame(decade_1950, directed = F, vertices = NULL)
mat <- get.adjacency(weighted_graph_1950)
mat <- as.matrix(mat)
weighted_graph_1950 <- graph_from_adjacency_matrix(mat, mode = "undirected", weighted = TRUE)
weighted_graph_1950 <- igraph::simplify(weighted_graph_1950,remove.multiple = T,edge.attr.comb = "sum") 
groups_weighted1950 <- edge.betweenness.community(weighted_graph_1950, modularity = T)
nodesDegrees <-igraph::degree(weighted_graph_1950, mode = "out")
nodesDegrees <- as.double(nodesDegrees)
nodesDegrees <- (scale(nodesDegrees))*3+12
nodesDegrees <- as.double(nodesDegrees)
weights <- edge_attr(weighted_graph_1950)$weight
l <- layout_with_fr(weighted_graph_1950)
plot(weighted_graph_1950,edge.width=weights, edge.lty=1, edge.arrow.size=0.1, layout=l*0.2-0.25,rescale=F,vertex.label.cex = 0.7,vertex.size=nodesDegrees, vertex.color=groups_weighted1950$membership,vertex.label.color= "black")

weighted_graph_2000 <- graph_from_data_frame(decade_2000, directed = F, vertices = NULL)
mat <- get.adjacency(weighted_graph_2000)
mat <- as.matrix(mat)
weighted_graph_2000 <- graph_from_adjacency_matrix(mat, mode = "undirected", weighted = TRUE)
weighted_graph_2000 <- igraph::simplify(weighted_graph_2000,remove.multiple = T,edge.attr.comb = "sum") 
groups_weighted2000 <- edge.betweenness.community(weighted_graph_2000, modularity = T)
nodesDegrees <-igraph::degree(weighted_graph_2000, mode = "out")
nodesDegrees <- as.double(nodesDegrees)
nodesDegrees <- (scale(nodesDegrees))*2+12
nodesDegrees <- as.double(nodesDegrees)
weights <- edge_attr(weighted_graph_2000)$weight
l <- layout_with_fr(weighted_graph_2000)
plot(weighted_graph_2000,edge.width=weights, edge.lty=1, edge.arrow.size=0.1, layout=l*0.15-0.7,rescale=F,vertex.label.cex = 0.7,vertex.size=nodesDegrees, vertex.color=groups_weighted2000$membership,vertex.label.color= "black")

# ---Q1_D-------------
Countries_from_1950And2000 <- c(V(graph_1950)$name, V(graph_2000)$name)
Countries_from_1950And2000 <- unique(Countries_from_1950And2000)
graph_1950<-add_vertices(graph_1950,nv = length(setdiff(Countries_from_1950And2000,as_ids(V(graph_1950)))),name=setdiff(Countries_from_1950And2000,as_ids(V(graph_1950))))
graph_2000<-add_vertices(graph_2000,nv = length(setdiff(Countries_from_1950And2000,as_ids(V(graph_2000)))),name=setdiff(Countries_from_1950And2000,as_ids(V(graph_2000))))

Net_1950<-intergraph::asNetwork(graph_1950)
Netg_2000<-intergraph::asNetwork(graph_2000)

qap1<-qaptest(list(Net_1950,Netg_2000),FUN = gcor,g1=1,g2=2)
summary(qap1)
plot(qap1)

# ---Q1_E-------------
Q1_allRowsWithoudYears <- Q1[,1:2]
Q1_allRowsWithoudYears$Counter <- 1
completedGraph <- graph_from_data_frame(Q1_allRowsWithoudYears, directed = F, vertices = NULL)

mat <- get.adjacency(completedGraph)
mat <- as.matrix(mat)

triad <- sna::triad.census(mat, mode= "graph")
dyad <- sna::dyad.census(mat)
nodesNum <- length(V(completedGraph))
randomNets <- rguman(100,nodesNum,mut=dyad[1],asym=dyad[2],null=dyad[3])
traidExample <- sna::triad.census(randomNets, mode="graph")
triadNum2 <- traidExample[,3]
mu_h0 <-  as.integer(triad[1,3])
t <- t.test(triadNum2, mu= mu_h0, alternative = "less")
t


#---------Q2---------------------------------------

Q2<-read.csv(file.choose(),header=TRUE)
Q2 <- Q2[,2:3]
UserNetworkGraph <- graph_from_data_frame(Q2, directed = F, vertices = NULL)

# ---Q2_A-------------
nodesDegree <- igraph:: degree(UserNetworkGraph)
powerLaw<-fit_power_law(nodesDegree, xmin = 1)
powerLaw
fitted_Y_Line<-c(powerLaw$xmin:max(nodesDegree))^-powerLaw$alpha
plot (tabulate(nodesDegree)/vcount(UserNetworkGraph),  ylab = "log Frequency", xlab = " log Degree", 
      main = "Log-log of degree", log="xy")
lines(x=c(powerLaw$xmin:max(nodesDegree)),fitted_Y_Line,col='red')

SSE <- 0
SST <- 0
samples <- tabulate(nodesDegree)/vcount(UserNetworkGraph)
for (i in 1:length(samples)){
  SSE <- SSE+ (fitted_Y_Line[i]-samples[i])^2
  SST <- SST+ (fitted_Y_Line[i]-mean(fitted_Y_Line))^2
}
R2 <- 1-(SSE/SST)

# ---Q2_B-------------

# find R2 for each dataset without the ith sample
# and than we can remove the 316 sample with the highest R2 which was achieved without them
nodesCount <- vcount(UserNetworkGraph)-1
R2WihtoutEachSample <- c()
for (samp in 1:vcount(UserNetworkGraph)){
  temp_nodesDegree <- nodesDegree[-samp]
  powerLaw<-fit_power_law(temp_nodesDegree, xmin = 1)
  fitted_Y_Line<-c(powerLaw$xmin:max(temp_nodesDegree))^-powerLaw$alpha
  plot (tabulate(temp_nodesDegree)/nodesCount,  ylab = "log Frequency", xlab = " log Degree", 
        main = "Option A Log-log", log="xy")
  lines(x=c(powerLaw$xmin:max(temp_nodesDegree)),fitted_Y_Line,col='red')
  
  SSE <- 0
  SST <- 0
  samples <- tabulate(temp_nodesDegree)/nodesCount
  for (i in 1:length(samples)){
    SSE <- SSE+ (fitted_Y_Line[i]-samples[i])^2
    SST <- SST+ (fitted_Y_Line[i]-mean(fitted_Y_Line))^2
  }
  R2 <- 1-(SSE/SST)
  R2WihtoutEachSample <- c(R2WihtoutEachSample, R2)
}
indicesToremove <- c()
quantity <- 0

tempMax <- max(R2WihtoutEachSample)
tempIndices <-which(R2WihtoutEachSample==tempMax) # 226 indices  

substractVec <- R2WihtoutEachSample[R2WihtoutEachSample<tempMax]
tempMax2 <- max(substractVec)
tempIndices2 <-which(R2WihtoutEachSample==tempMax2)
tempIndices2 <- tempIndices2[1:90] # the remaining amount for removing 316 nodes

indicesToremove <- c(tempIndices,tempIndices2)

optionB_nodesDegree <- nodesDegree[-indicesToremove]
nodesCount <- vcount(UserNetworkGraph)-316

powerLaw<-fit_power_law(optionB_nodesDegree, xmin = 1)
fitted_Y_Line<-c(powerLaw$xmin:max(optionB_nodesDegree))^-powerLaw$alpha
plot (tabulate(optionB_nodesDegree)/nodesCount,  ylab = "log Frequency", xlab = " log Degree", 
      main = "Option A Log-log", log="xy")
lines(x=c(powerLaw$xmin:max(optionB_nodesDegree)),fitted_Y_Line,col='red')

SSE <- 0
SST <- 0
samples <- tabulate(optionB_nodesDegree)/nodesCount
for (i in 1:length(samples)){
  SSE <- SSE+ (fitted_Y_Line[i]-samples[i])^2
  SST <- SST+ (fitted_Y_Line[i]-mean(fitted_Y_Line))^2
}
R2 <- 1- (SSE/SST)


# ---Q2_C-------------
Cnet <- transitivity(UserNetworkGraph)
Lnet <- mean_distance(UserNetworkGraph, directed = FALSE, unconnected = TRUE)
E(UserNetworkGraph)
LrandVec <- c()
CrandVec <- c()
for (i in 1:100){
  random_graph <- erdos.renyi.game(6333, 42558, type =  "gnm", directed = FALSE)
  CrandVec <- c(CrandVec,transitivity(random_graph))
  LrandVec <- c(LrandVec,mean_distance(random_graph, directed = FALSE, unconnected = TRUE))
}
Crand <- mean(CrandVec)
Lrand <- mean(LrandVec)


#---------Q3---------------------------------------

# ---Q3_A-------------
numOfNodes <- 1000
numOfEdges <- 0.25*((numOfNodes*(numOfNodes-1))/2)
rand_scaleFree_graph <- static.power.law.game(numOfNodes, numOfEdges, exponent.out= 2.5, exponent.in = -1)

# ---Q3_B-------------
rand_scaleFree_graphList <- vector("list",10)
for (i in 1:10){
  rand_scaleFree_graph <- static.power.law.game(numOfNodes, numOfEdges, exponent.out= 2.5, exponent.in = -1)
  rand_scaleFree_graphList[[i]] <- rand_scaleFree_graph
}

numOfEdgesToDeleteEachTime <- round(0.05*numOfEdges)
centrality_per_quantity <- c()
for (i in 1:20){
  centrality_per_graph <- c()
  for (j in 1:length(rand_scaleFree_graphList)){
    centrality_for_same_graph <- c()
    for (k in 1:10){
      current_graph <- rand_scaleFree_graphList[[j]]
      quantityEdgesToDelete <- i*numOfEdgesToDeleteEachTime
      if (i==20){
        quantityEdgesToDelete <- numOfEdges # because the round, we are not able to use the sample function in the last step
      }
      edgesToDelete <- sample(1:numOfEdges, quantityEdgesToDelete, replace=F)
      graphAfterDeleting <- igraph::delete.edges(current_graph,edgesToDelete)
      centralization <- centralization.degree(graphAfterDeleting)$centralization
      centrality_for_same_graph <- c(centrality_for_same_graph,centralization)
    }
    meanGraphCloseness <- mean(centrality_for_same_graph)
    centrality_per_graph <- c(centrality_per_graph,meanGraphCloseness)
  }
  meanClosenessPerQuantity <- mean(centrality_per_graph)
  centrality_per_quantity <- c(centrality_per_quantity,meanClosenessPerQuantity)
}

# ---Q3_C-------------
x <- c()
for (i in 1:20){
  x <- c(x,paste(i/20,"%"))
}
x <- as.data.frame(x)
centrality_per_quantity <- as.data.frame(centrality_per_quantity)
df <- as.data.frame(cbind(x$x,centrality_per_quantity$centrality_per_quantity))
colnames(df) <-  c("X","Y")
for (i in 1:20){
  df$X[i] <- df$X[i]/20
}
plot(df,xlab = "Precentage of removing edges", ylab = "degree centralization")
lines(df, type="l")


#---------Q4---------------------------------------

fall_norm <-read.csv(file.choose(),header=TRUE)
spring_norm <- read.csv(file.choose(),header=TRUE)

fall_ques <- read.csv(file.choose(),header=TRUE)
spring_ques <- read.csv(file.choose(),header=TRUE)

fall_norm[is.na(fall_norm)] <- 0
spring_norm[is.na(spring_norm)] <- 0

fall_norm <- as.matrix(fall_norm)
spring_norm <- as.matrix(spring_norm)

colnames(fall_norm) <- fall_ques$ID
colnames(spring_norm) <- spring_ques$ID 


fall_net <- as.network(fall_norm, directed = TRUE)
spring_net <- as.network(spring_norm, directed = TRUE)

# set attributes
fall_net %v% "Gender" <- as.character(fall_ques$gender)
spring_net %v% "Gender" <- as.character(spring_ques$gender)

fall_net %v% "Grade" <- fall_ques$Total.Curved.grade
spring_net %v% "Grade" <- spring_ques$Total.Curved.grade

CategorialCommunity_fall <- rep("NA",nrow(fall_norm))
CategorialCommunity_fall[fall_ques$en.community==1] <- "1"
CategorialCommunity_fall[fall_ques$en.community==2] <- "2"
CategorialCommunity_fall[fall_ques$en.community==3] <- "3"
CategorialCommunity_fall[fall_ques$en.community==4] <- "4"
CategorialCommunity_fall[fall_ques$en.community==5] <- "5"
CategorialCommunity_fall[is.na(fall_ques$en.community)] <- "NA"
fall_net %v% "Community" <- CategorialCommunity_fall

CategorialCommunity_spring <- rep("NA",nrow(spring_norm))
CategorialCommunity_spring[spring_ques$en.community==1] <- "1"
CategorialCommunity_spring[spring_ques$en.community==2] <- "2"
CategorialCommunity_spring[spring_ques$en.community==3] <- "3"
CategorialCommunity_spring[spring_ques$en.community==4] <- "4"
CategorialCommunity_spring[is.na(spring_ques$en.community)] <- "NA"
spring_net %v% "Community" <- CategorialCommunity_spring


# ---Q4_A-------------
#1
model1_fall <- ergm(fall_net~edges+mutual)
model1_spring <- ergm(spring_net~edges+mutual)
#2
model2_fall <- ergm(fall_net~edges+mutual+nodematch('Gender'))
model2_spring <- ergm(spring_net~edges+mutual+nodematch('Gender'))
#3
model3_fall <- ergm(fall_net~edges+mutual+nodematch('Gender')+absdiff('Grade'))
model3_spring <- ergm(spring_net~edges+mutual+nodematch('Gender')+absdiff('Grade'))
#4
model4_fall <- ergm(fall_net~edges+mutual+nodematch('Gender')+absdiff('Grade')+nodefactor('Community'))
model4_spring <- ergm(spring_net~edges+mutual+nodematch('Gender')+absdiff('Grade')+nodefactor('Community'))

# ---Q4_B-------------

#1
summary(model1_fall)
summary(model1_spring)
mcmc.diagnostics(model1_fall)
mcmc.diagnostics(model1_spring)
#2
summary(model2_fall)
summary(model2_spring)
par(mar = rep(2, 4))
mcmc.diagnostics(model2_fall)
mcmc.diagnostics(model2_spring)
#3
summary(model3_fall)
summary(model3_spring)
mcmc.diagnostics(model3_fall)
mcmc.diagnostics(model3_spring)
#4
summary(model4_fall)
summary(model4_spring)
mcmc.diagnostics(model4_fall)
mcmc.diagnostics(model4_spring)

# ---Q4_C-------------

p_fall <- invlogit(model3_fall$coef[1]+model3_fall$coef[2]*0+model3_fall$coef[3]*1+model3_fall$coef[4]*0)
p_spring <- invlogit(model3_spring$coef[1]+model3_spring$coef[2]*0+model3_spring$coef[3]*1+model3_spring$coef[4]*0)

# ---Q4_D-------------

gof_model4_fall <- gof(model4_fall, GOF = ~ idegree + odegree + espartners + distance+ triadcensus + model)
gof_model4_fall
plot(gof_model4_fall)
mcmc.diagnostics(model4_fall)

# ---Q4_E-------------

CategorialMajor_fall <- rep("Other",nrow(fall_norm))
CategorialMajor_fall[fall_ques$major=='me'] <- "me"
fall_net %v% "Major" <- CategorialMajor_fall

improve_model1 <- ergm(fall_net~edges+mutual+nodematch('Gender')+absdiff('Grade')+nodefactor('Community')+nodefactor('Major'))
summary(improve_model1)


#---------Q5---------------------------------------

load(file.choose())

# ---Q5_A-------------

my_igraph <- as.undirected(gotNetwork, mode = "collapse")
l<-layout_with_fr(my_igraph)
plot(my_igraph, edge.lty=1, edge.arrow.size=0.1, layout=l*0.15-0.8,rescale=F,vertex.label.cex = 0.7,vertex.size=12, vertex.color="burlywood1",vertex.label.color= "black")

nodesDegrees <- as.data.frame(degree(my_igraph))
colnames(nodesDegrees) <- c("degree")
nodesBetweenness <- as.data.frame(betweenness(my_igraph, directed = FALSE, weights = NA))
colnames(nodesBetweenness) <- c("betweeness")
nodesCloseness <-as.data.frame(closeness(my_igraph, weights = NA))
colnames(nodesCloseness) <- c("closenesss")
graph_components <- igraph::components(my_igraph)
my_igraph_2 <- delete_vertices(my_igraph, which(graph_components$membership!=1))

# ---Q5_B-------------

graph_diameter <- diameter(my_igraph, directed = FALSE, unconnected = TRUE,weights = NA)
d <- get.diameter(my_igraph,directed = TRUE, unconnected = TRUE, weights = NA)
d
E(my_igraph)$color= "gray"
E(my_igraph)$width= 2
E(my_igraph,path = d)$color= "red"
E(my_igraph,path = d)$width= 4
color_diameter <- rep("#FFD87D", vcount(my_igraph))
color_diameter[d] <- "red"
plot(my_igraph, edge.lty=1, edge.arrow.size=0.1, layout=l*0.11-0.7,rescale=F,vertex.label.cex = "NA",vertex.size=8, vertex.color=color_diameter,vertex.label.color= "black")
plot(my_igraph, edge.lty=1, edge.arrow.size=0.1, layout=l*0.11-0.7,rescale=F,vertex.label.cex = 0.6,vertex.size=8, vertex.color=color_diameter,vertex.label.color= "black")

# ---Q5_C-------------

dyad <- dyad.census(my_igraph)
adjacency_matrix <- get.adjacency(my_igraph)
adjacency_matrix <- as.matrix(adjacency_matrix)
triad <- sna::triad.census(adjacency_matrix, mode= "graph")
exampleNet <- rguman(100,208,mut=dyad$mut,asym=dyad$asym,null=dyad$null)
traidExample <- sna::triad.census(exampleNet, mode= "graph")

triadNum0 <- traidExample[,1]
mu_h0 <-  as.integer(triad[1,1])
t <- t.test(triadNum0, mu= mu_h0)
t

triadNum1 <- traidExample[,2]
mu_h0 <-  as.integer(triad[1,2])
t <- t.test(triadNum1, mu= mu_h0)
t

triadNum2 <- traidExample[,3]
mu_h0 <-  as.integer(triad[1,3])
t <- t.test(triadNum2, mu= mu_h0)
t

triadNum3 <- traidExample[,4]
mu_h0 <-  as.integer(triad[1,4])
t <- t.test(triadNum3, mu= mu_h0)
t

# ---Q5_D-------------

AryaIndex <-  which(V(my_igraph)$name == "Arya Stark")
igraph::max_cliques(my_igraph, min = 3)

# ---Q5_E-------------

clus <- cluster_edge_betweenness(my_igraph)
modularity(clus)

l<-layout_with_fr(my_igraph)
plot(my_igraph, edge.lty=1, edge.arrow.size=0.1, layout=l*0.11-0.5,rescale=F,vertex.label.cex = 0.6,vertex.size=(V(my_igraph)$popularity + 1) * 8,vertex.shape = V(my_igraph)$shape, vertex.color=clus$membership,vertex.label.color= "black")

popularityMale <- V(my_igraph)$popularity[V(my_igraph)$shape=="square"]
popularityFemale <- V(my_igraph)$popularity[V(my_igraph)$shape=="circle"]
t<- t.test(popularityMale,popularityFemale)
t
