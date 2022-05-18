#code for ERGM slides:
  #using statnet tutorial: https://statnet.github.io/Workshops/ergm_tutorial.html

#load statnet library
  #install.packages(statnet)  #install if haven't already
  #detach igraph if have it loaded
library(statnet)

#useful function for getting probability of an edge from log-odds:
expit <- function(c, n){
  exp(c)/(n + exp(c))
}

# Nolan likes this better than log odds


#load Florentine marriage data set
data(florentine)
  #look at network:
flomarriage

# Get the density
network.density(flomarriage)

#lets see if wealth appears relevant:
par(mfrow=c(1,2)) # Setup a 2 panel plot
plot(flomarriage, 
     main="Florentine Marriage", 
     cex.main=0.8, 
     label = network.vertex.names(flomarriage), label.pos=5) # Plot the network
  #make wealth a vertex attribute
wealth <- flomarriage %v% 'wealth' # %v% references vertex attributes
wealth

  #plot network with node size scaled by wealth
plot(flomarriage, 
     vertex.cex=wealth/25, 
     main="Florentine marriage by wealth", cex.main=0.8,
     label = network.vertex.names(flomarriage), label.pos=5) # Plot the network with vertex size proportional to wealth

  #looking at a simple model:
summary(flomarriage ~ edges) # Look at the $g(y)$ statistic for this model
flomodel.01 <- ergm(flomarriage ~ edges) # Estimate the model 
    #above is a good way to get different network statistics:

summary(flomarriage ~ triangle)

summary(flomodel.01) # Look at the fitted model object

  #lets try another model:
summary(flomarriage~edges+triangle) # Look at the g(y) stats for this model

flomodel.02 <- ergm(flomarriage~edges+triangle) 
summary(flomodel.02)

class(flomodel.02) # this has the class ergm

names(flomodel.02) # the ERGM object contains lots of components.

flomodel.02$coef # you can extract/inspect individual components
flomodel.02$coef[2] # Triangle 

summary(wealth) # summarize the distribution of wealth

# For every edge that ends on a node, what is that wealth 
  # We have 20 edges, or 40 different individual relationships
  # We take thw wealth of Lamb times 1 but Salviati will be it's wealth times two, and Medici will be it's wealth times 6 -> If you add up all of these, you have a sense of how much wealth is driving the degree distribution 

# MCMC is random draws (is that a tie, is that a tie, OK let's sum that up)
  # Think of it like hill climbing 
    # We're getting closer and closer moving through the parameter spaces to get there 
  # The Markhov graph focuses on the fact that the future state is totally independent but controlling for the past 
  # You can either add an edge of delete an edge one at a time 

# So with gerrymandering, we were adding and deleting edges iteratively moving closer and closer to "convergence" -> Makes sense!

# Hill climber keeps getting stuck if you don't have enough ties 
  # Why goodness of fit can tell you how well your model is performing 


# Adding edges without a triangle is now less likely
# Still random at this point 

plot(flomarriage, 
      vertex.cex=wealth/25, 
      main="Florentine marriage by wealth", 
      cex.main=0.8) # network plot with vertex size proportional to wealth
summary(flomarriage~edges+nodecov('wealth')) # observed statistics for the model

# Nodecov is asking what is the average degree for someone who has this much wealth

flomodel.03 <- ergm(flomarriage~edges+nodecov('wealth'))

summary(flomodel.03)

# Can bring in number of steps as variables too when you think that there might be a differential impact (Can also do interaction terms too)

# https://statnet.org/trac/raw-attachment/wiki/Sunbelt2016/ergm_tutorial.html
# ^ Look back here for real clear way on how to explain all of this 

#nodal covariates: homophilly
  #school friendship network
data(faux.mesa.high) 
mesa <- faux.mesa.high

# Pretty sparse

#plot the network:
par(mfrow=c(1,1)) # Back to 1-panel plots
plot(mesa, vertex.col='Grade')
legend('bottomleft',fill=7:12,
       legend=paste('Grade',7:12),cex=0.75)

#fit a model with homophilly terms
fauxmodel.01 <- ergm(mesa ~edges + 
                       nodefactor('Grade') + nodematch('Grade',diff=T) +
                       nodefactor('Race') + nodematch('Race',diff=T))

#nodefactor used for categorical and nodecovar used for any continuous 
# nodematch is about whether people in same grade are likley to have ties together 

#lets look at model
summary(fauxmodel.01)

# inf for black is related to how there is no ties between black students # Same with other, so checks out

#why is one of the terms "-Inf"?
table(mesa %v% 'Race') # Frequencies of race
mixingmatrix(mesa, "Race")

#direct ties: see website
#missing data: see website

# dyad dependent: One tie existing increases the probability of another recriprocal tie existing 
# Conditional log odds gets at this to some degree
# More network that have transitive triangles have a greater conditional probability given the state of the network 

# Dyad dependent takes longer because it has to go through MCMC
  # homopholy = TRUE: How many diads/traids do we have where they also match on node attribute

# Convergence 
  # You can make a longer chain 
  # You likely want to increase sample size if presenting to a client 


#assessing model convergence:
summary(flobusiness~edges+degree(1))

fit <- ergm(flobusiness~edges+degree(1),
            control=control.ergm(MCMC.interval=1))

summary(fit)

mcmc.diagnostics(fit)


#network simulation: (We've got edges and wealth here)
flomodel.03.sim <- simulate(flomodel.03,nsim=10)
class(flomodel.03.sim) # what does this produce?
summary(flomodel.03.sim) # quick summary

# are the simulated stats centered on the observed stats?
rbind("obs"=summary(flomarriage~edges+nodecov("wealth")),
      "sim mean"=colMeans(attr(flomodel.03.sim, "stats"))) 

# we can also plot individual simulations
flomodel.03.sim[[1]]

plot(flomodel.03.sim[[1]], 
     label= flomodel.03.sim[[1]] %v% "vertex.names",
     vertex.cex = (flomodel.03.sim[[1]] %v% "wealth")/25)

#how does it look compared to original network?
par(mfrow=c(1,2))
flo.coords <- plot(flomarriage, 
     label= flomarriage %v% "vertex.names",
     vertex.cex = (flomarriage %v% "wealth")/25, label.pos=5)

plot(flomodel.03.sim[[1]], 
     label= flomodel.03.sim[[1]] %v% "vertex.names",
     vertex.cex = (flomodel.03.sim[[1]] %v% "wealth")/25, coord = flo.coords, label.pos=5)

# Output isn't great here of course because we only included two variables (wealth and edges)

# If we changed a way ties were being formed, what would that do to the network 
  # Do a bunch of simulations 

# There are weighted ERGMS too 
# You can look at ranked order with elections 
# You can look at temporal as well to have cross-sectional 
# Seperable temporal (expontential family on tie formation and disolution)

#how different are the two?
sum(abs(as.matrix.network.adjacency(flomarriage) - 
          as.matrix.network.adjacency(flomodel.03.sim[[1]])))

#how else can we compare the networks?



#examining the quality of fit: Goodness-of-Fit (GoF)
flomodel.03.gof <- gof(flomodel.03)
flomodel.03.gof

plot(flomodel.03.gof)

# There isn't really a great metric for evaluating a model 
  # There is goodness of fit but it's more descriptive 
  # Goodness of fit is model assessment 

# What's missing is the labeled model assessment
  # But if you don't have data, then this is pretty fricken good 
  # Temporal ERGM of who is likely to be radicalized next 

