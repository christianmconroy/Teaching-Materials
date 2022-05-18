#######################################
# DataWorksMD Social Network Analysis #
# Ian McCulloh, Ph.D.  15 May 2018    #
#######################################

# Please feel free to use this code and share with friends

# If you have not installed igraph, use the following command
#install.packages("igraph")

# Each time you start a new evnironment, you need to load the package
library(igraph)

# For R specific applications, I recommend the following packages
library(data.table)  #this is a package for more efficient data management
library(cluster)     #this is a package for subgroup analysis

# For loading data
# dt<-read.table("mydata.csv", header=TRUE, sep = ",", quote = "\"")

# For generating networks
g1 <- graph( edges=c(1,2, 2,3, 3, 1), n=3, directed=F )
plot(g1)
class(g1)
g1

# For networks with more nodes
g2 <- graph( edges=c(1,2, 2,3, 3, 1), n=7 )
plot(g2)

# For naming nodes
g3 <- graph( edges = c("Alex", "Bill", "Bill", "Carl", "Carl", 
                       "Alex"))
plot(g3)

g4 <- graph( edges = c("Alex", "Bill", "Bill", "Carl", "Carl", 
                       "Alex"), isolates = c("Dave", "Eric", "Frank", "Greg"))
plot(g4)
plot(g4, arrow.size=.2, vertex.color="red", vertex.size=10,
     vertex.frame.color="gray", vertex.label.color="black",
     vertex.label.cex=1.2, vertex.label.dist=2, edge.curved=0.2)

g5 <- graph_from_literal(a-b-c-d-e, a-h-g-i-b, a-g, h-i, d-j:k:l, e-l:k)
plot(g5)

# To inspect the graph
E(g5)    #edges
V(g5)    #vertices
g5[]     #adjacency matrix
V(g5)$name

V(g5)$gender <- c("m","f","m","f","m","m","f","f","m","m","m")
plot(g5, edge.arrow.size=.5, vertex.label.color="black", vertex.label.dist=1.5,
     vertex.color=c( "pink", "blue")[1+(V(g5)$gender=="m")] )

#Density
edge_density(g5, loops = FALSE)
# 15/55 = 0.27 is what is happening here

#Diameter
diameter(g5, directed = FALSE, weights=NA)

d<-get_diameter(g5, directed = FALSE, weights=NA)
d

#Color nodes along the diameter
vcol <- rep("gray40", vcount(g5))
vcol[d] <- "red"
ecol <- rep("gray80", ecount(g5))
ecol[E(g5, path=d)] <- "orange"
plot(g5, vertex.color=vcol, edge.color=ecol, edge.arrow.mode=0)

#Centrality
deg <- igraph::degree(g5, mode="all")  #Degree Centrality
plot(g5, vertex.size=deg*5) # Here is how to size based on the degree!
hist(deg, breaks=1:vcount(g5)-1, main="Histogram of Degree")

clo1<-closeness(g5, mode="all", weights=NA)   #closeness based on distance
clo2<-centr_clo(g5, mode="all", normalized=T) #inverse geodesic distance
plot(g5, vertex.size=clo1*300)
plot(g5, vertex.size=clo2$res*30)
clo2

e<-eigen_centrality(g5, directed=T, weights=NA) #Eigenvector Centrality
e$vector

b<-igraph::betweenness(g5, directed=FALSE, weights=NA)  #Betweenness Centrality
plot(g5, vertex.size=b)
b

#Clustering

#Cliques
cliq<-cliques(g5)
cliq # ahg for example 
lg.cliq<-largest_cliques(g5)
lg.cliq #delk not a clique because k is not connected directly to l
vcol <- rep("grey80", vcount(g5))
vcol[unlist(largest_cliques(g5))] <- "gold"
plot(as.undirected(g5), vertex.label=V(g5)$name, vertex.color=vcol)

#Newman Grouping
g5.new<-cluster_edge_betweenness(g5)
plot(g5.new, g5)
length(g5.new)
membership(g5.new)
modularity(g5.new)

#Louvain Grouping
cl<-cluster_louvain(g5, weights = NULL)
plot(cl, g5)
length(cl)
membership(cl)
modularity(cl)

############# Newman Network- 12/10/19
# Create basic network with a
new.net <- as.network(matrix(0, 8,8), directed = F)
add.edges(new.net, c(1,1, 2,2, 3, 4,4,4, 5, 6, 7),
          c(3,4, 3,4, 5, 5,6,8, 6, 7, 8))
new.net %v% "names" <- c("A","B","C","D","E","F","G","H")


# Can highlight nodes that you want to bring to people's attention with the interactive 
coords <- plot.network(new.net, label="names", interactive=T)
#coords <- plot.network(new.net, label="names", interactive=T, coord=coords)
plot.network(new.net, label="names")
plot.network(new.net, label="names", coord=coords, jitter=F, label.pos = 1, label.pad = 0)
# Nodes will be slightly moved around if jitter isn't false
new.amt <- as.matrix.network.adjacency(new.net)
new.immat <- as.matrix.network.incidence(new.net)
# Incidence matrix says two nodes are incident on one another if they share an edge (The rows and columns are the edges not the nodes this time)
# 8 nodes in the rows and 11 edges in the clumns 
next.mat <- t(new.immat) %*% new.immat
next.mat
plot.network(as.network(next.mat, directed = F))
# Instead of going back through and writing out all the edges, take the incidence matrix to create the second network 
# We've essentially flipped nodes and edges
  # Original we had 8 nodes and 11 edges
  # New one we have 11 nodes and 8 edges

# Why would you mutliply by a transpose? 
  # Depends on the form of data you are getting 
  # If the data shows connections between entities, you can do this to show the entire network (you don't have nolan to christian, christian to dylan)

# Remove iteratively based on edge betweeness 
next.net<-as.network(next.mat, directed = F)
plot.network(next.net, label=next.net%v%"vertex.names")
betweenness(next.net)
# Just count up 1 is 2, 2 is 9, 3 is 2, 4 is 9, etc.

# 8 had highest 
delete.vertices(next.net, 8)

betweenness(next.net)

# 9 is highest
delete.vertices(next.net, 9)

betweenness(next.net)

# 5 is the highest 
delete.vertices(next.net, 5)

betweenness(next.net)

# 2 is the highest 
delete.vertices(next.net, 2)

plot.network(next.net, label=next.net%v%"vertex.names")

betweenness(next.net)

# 3 is the highest 
delete.vertices(next.net, 3)

betweenness(next.net)

# We've hit 0 for betweeness 
plot.network(next.net, label=next.net%v%"vertex.names")
# 6,9,7 end up being a cluster here that we care about


# Why would we want to reduce all of this betweeness? 
  # Bridging ties, which have high betweeneess, have weaker ties 
  # We want those that are strongly linked to each other 
  # We reduce down to the strongest ties to see cohesive groups 

############ Twitter API
install.packages('rtweet')
library(rtweet)


# whatever name you assigned to your created app
library(rtweet)

#App name
an <- "New_RUseCase"
#API key
ak <- "j68U5uq6eojJBVu4RJWGBZvel"
#API secret key
ask <- "AvXJaeDoKvAP0gymWZXo2pgxXWMbhvXECi5ygNl79KxMr7NUif"
#Access token
at <- "216776122-308i2Ifv4Z2ZSMdoP4et4gjm9JZuAzlMzsmVCQl3"
#Access token secret
ats <- "Gg74jYXUrAQNijiCxAlcdWKZD13NEXi1eKNlGb1KgUeom"

t <- create_token(an, ak, ask, at, ats)
data <- search_tweets("", n = 100, include_rts = FALSE, token = t)

# Quote: Someone doesn't retweet, but instead copy and pastes it (pseudo-retweet so it is no longer original)
# Some people have geocordinates on but it will always be small 
file.edit("~/.Renviron")
# This only queries from 1% of the tweets 

ts_plot(tweets_USDA, "1 hour") + ggplot2::theme_minimal()
  
  # Streaming API: From this moment forward, I want to capture any tweet that has a term 
stream <- stream_tweets("trump", timeout = 30, parse = TRUE)

ts_plot(stream, "1 seconds")

# Get account timelines - what else have they talked about 
accounts <- as.character(unique(tweets_USDA$screen_name))
accounts <- lookup_users(accounts)
accounts <- rtweet::flatten(accounts)
network <- sum(accounts$followers_count)
accounts_sample <- accounts[sample(nrow(accounts), 10),]

account_timelines <- get_timeline(accounts_sample$screen_name, n =50)