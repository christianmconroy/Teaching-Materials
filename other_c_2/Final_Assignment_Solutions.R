####################################################################
# 410.733 Communication Network Analysis in Public Health Programs #
# Final Assignment Solutions                                       #
####################################################################

setwd("~/Onboarding")
library(statnet)

# Load in the Kaduna edgelist and convert to a network
kad<-read.csv("kaduna.csv", header=FALSE, sep = ",")
kad.net<-network(kad,matrix.type="edgelist",directed=TRUE)

# Load in the Ibadan edgelist and convert to a network
ibd<-read.csv("ibadan.csv", header=FALSE, sep = ",")
ibd.net<-network(ibd,matrix.type="edgelist",directed=TRUE)

# Load in the "coct" variable data for Kaduna and Ibadan for later use
kad.coct<-read.csv("Kaduna_attr.csv", header=TRUE, sep = ',')
ibd.coct<-read.csv("Ibadan_attr.csv", header=TRUE, sep = ',')


# Take a moment to become familiar with the data.  You can look at the data in Excel or use the head command in R.

head(kad)
head(ibd)

# First, symmetrize the networks and create a sociogram for each network. For each city, a)
# show a plot/visualization of the sociogram, and b) describe the structure of the network in terms of # of components, # of isolates, and main component size.

kad.sym<-as.network(symmetrize(kad.net))  #symmetrize network (Goes from directed to undirected)
ibd.sym<-as.network(symmetrize(ibd.net))  

# component size
plot(kad.sym)
plot(ibd.sym)

isolates(kad.sym)
isolates(ibd.sym)

# For each network there is one main component. Kaduna has 2 isolates, while Ibadan has 3
# The main component for Kaduna has 67-2=65 nodes and Ibadan has 69-3=66 nodes.
# (We're just taking out the isolates)
# Is a component anything connected with at least 2 nodes? 

# 2. For each city, calculate
# a.	The average distance among the nodes in the main component of each of the symmetrized networks (2 pts)
kad.isol<-isolates(kad.sym)           #identify isolates
kad.ni<-kad.sym                       #create a clone network, so we can remove isolates
delete.vertices(kad.ni, kad.isol)     #remove isolates (network, nodes to remove)
sum(geodist(kad.ni)$gdist)/(65*64)    #calc. mean geodesic distance. Note, will incl diag

ibd.isol<-isolates(ibd.sym)               
ibd.ni<-ibd.sym                           
delete.vertices(ibd.ni, ibd.isol)        
sum(geodist(ibd.ni)$gdist)/(66*65)        

# Ibadan: 2.249# Kaduna: 2.476 (Though I got 2.48 and 2.25)

# the diameter of the main component of each of the symmetrized networks (2 pts)

## b. the diameter of the main component of each of the symmetrized networks 
dia<-function(x){     # Include diameter function
  g<-geodist(x)
  g1<-g$gdist
  g1[g1==(Inf)]=0
  d<-max(g1) # Because diameter is the longest shortest
  return(d)
}

dia(kad.ni)
dia(ibd.ni)

# Ibadan: 4
# Kaduna: 6

# . the density of in the symmetrized and unsymmetrized data (Note: report two densities for each city - one for the symmetrized network and one for the unsymmetrized network). Explain why the symmetrized and unsymmetrized densities are or are not different in each city. (2 points)

# the density of the whole network in the symmetrized and unsymmetrized data
# (Note: report two densities for each city - one for the symmetrized network and one for the unsymmetrized network). Explain why the symmetrized and unsymmetrized densities are or are not different in each city.
gden(kad.net)
gden(kad.sym)
gden(ibd.net)
gden(ibd.sym)

# Kaduna: 0.140 vs 0.087
# Ibadan: 0.140 vs 0.091

# The symmetrized and unsymmetrized densities differ because not all of the relationships in Ibadan and Kaduna are reciprocal. In the unsymmetrized data, we look at the density of the main component only considering one-directional ties. In the symmetrized data, we look at the density and assume a bidirectional relationship between every pair of individuals where at least one directional tie exists. For example, in the symmetrized main component of Kaduna there are 626 ties or 313 one- directional ties (one in each direction between each pair), however, in the non- symmetrized main component, there are 391 single-direction ties.


## a. the percent of nodes in the main component of each of the symmetrized networks
65/67   #Kaduna: 97.0%
66/69   #Ibadan: 95.6%

## b. the average distance among the nodes in the main component of each network
kad.sym<-as.network(symmetrize(kad.net))  #symmetrize network to avoid problems with directed ties
kad.isol<-isolates(kad.sym)               #identify isolates
kad.ni<-kad.sym                           #create a clone network, so we can remove isolates
delete.vertices(kad.ni, kad.isol)         #remove isolates
sum(geodist(kad.ni)$gdist)/(65*64)        #calculate mean geodesic distance. Note, mean will include diagonal

ibd.sym<-as.network(symmetrize(ibd.net))  #repeat for Ibadan
ibd.isol<-isolates(ibd.sym)               
ibd.ni<-ibd.sym                           
delete.vertices(ibd.ni, ibd.isol)        
sum(geodist(ibd.ni)$gdist)/(65*64)        

# Ibadan: 2.320
# Kaduna: 2.476

## c. the diameter of the main component of each of the symmetrized networks 
dia<-function(x){     # Include diameter function
  g<-geodist(x)
  g1<-g$gdist
  g1[g1==(Inf)]=0
  d<-max(g1)
  return(d)
}

dia(kad.ni)
dia(ibd.ni)

# Ibadan: 4
# Kaduna: 6

# d. the density of the whole network in the symmetrized and unsymmetrized data
# (Note: report two densities for each city – one for the symmetrized network and
#  one for the unsymmetrized network). Explain why the symmetrized and
# unsymmetrized densities are or are not different in each city.
gden(kad.net)
gden(kad.sym)
gden(ibd.net)
gden(ibd.sym)

# Kaduna: 0.140 vs 0.087
# Ibadan: 0.140 vs 0.091

# The symmetrized and unsymmetrized densities differ because not all
# of the relationships in Ibadan and Kaduna are reciprocal. In the
# unsymmetrized data, we look at the density of the main component
# only considering one-directional ties. In the symmetrized data, we look
# at the density and assume a bidirectional relationship between every
# pair of individuals where at least one directional tie exists. For
# example, in the symmetrized main component of Kaduna there are
# 626 ties or 313 one- directional ties (one in each direction between
#     each pair), however, in the non- symmetrized main component, there
# are 391 single-direction ties.


# 4. (2 points) Based on the sociograms you presented in Q2 and results from Q3, how would you characterize the cohesiveness of the two networks? Is one more cohesive than the other or would you consider them to be comparable? Justify your conclusion. 

# Comparable 

# 4. Using each city'ss network, conduct centrality analyses and select 2 opinion leaders and 2 key bridges for each city and justify your decision. 
b.kad<-betweenness(kad.net)
d.kad<-degree(kad.net)
e.kad<-evcent(kad.net)

kad.net %v% "name" <- 1:67
names(b.kad)<-kad.net %v% "name" # %v% is for adding vertices to the network 
names(d.kad)<-kad.net %v% "name"
names(e.kad)<-kad.net %v% "name"

sort(b.kad, decreasing = TRUE)    #scaling not necessary for identification of top nodes
sort(d.kad, decreasing = TRUE)     #scaling not necessary for identification of top nodes
sort(e.kad, decreasing = TRUE)

b.ibd<-betweenness(ibd.net)
d.ibd<-degree(ibd.net)
e.ibd<-evcent(ibd.net)

ibd.net %v% "name" <- 1:69
names(b.ibd)<-ibd.net %v% "name"
names(d.ibd)<-ibd.net %v% "name"
names(e.ibd)<-ibd.net %v% "name"

sort(b.ibd, decreasing = TRUE)    #scaling not necessary for identification of top nodes
sort(d.ibd, decreasing = TRUE)     #scaling not necessary for identification of top nodes
sort(e.ibd, decreasing = TRUE)

# 5. Some providers in Nigeria have indicated a reluctance to prescribe contraceptives to women for 
# certain reasons. Our data contains a variable (coct) indicating whether each provider in the network 
# has indicated that they would not prescribe oral contraceptives to a woman who does not have her 
# partner’s approval to use contraception. Apply exponential random graph modeling (ERGM) to network 
# data from each city to investigate whether there exists a correlation between attitudes toward 
# prescribing contraception and network ties.  Describe your findings.  What does this tell you about 
# potential intervention strategies in each city?

#Add attributes to the networks
kad.net %v% "coct" <- as.list(kaduna[,2])
ibd.net %v% "coct" <- as.list(Ibadan[,2])

#Develop an ergm restricted model
km1<-ergm(kad.net~edges + mutual)
summary(km1)

km2<- ergm(kad.net~edges + mutual + gwesp(0.2))
summary(km2)    #triadic term appears insignificant

#Develop an unrestricted ergm model
km3<-ergm(kad.net~edges + mutual + nodematch("coct"))
summary(km3)

#Repeat for Ibadan
im1<-ergm(ibd.net~edges + mutual)
summary(im1)
im2<- ergm(ibd.net~edges + mutual + gwesp(0.2))
summary(im2)
im3<- ergm(ibd.net~edges + mutual + nodematch("coct"))
summary(im3)

#Color nodes based on coct attitude
plot(kad.net, 
     vertex.col=c("red","green")[1+(get.vertex.attribute(kad.net, "coct")=="Yes")],
     edge.col=8,
     label=ifelse(b.kad>100.0,network.vertex.names(kad.net),""), 
     label.cex=.7,vertex.cex=(d.kad/10))

plot(ibd.net, 
     vertex.col=c("red","green")[1+(get.vertex.attribute(ibd.net, "coct")=="Yes")],
     edge.col=8,
     label=ifelse(b.ibd>100.0,network.vertex.names(ibd.net),""), 
     label.cex=.7,vertex.cex=(d.ibd/10))

