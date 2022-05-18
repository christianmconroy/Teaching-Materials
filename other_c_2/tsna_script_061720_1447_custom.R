############################################## Temporal Social Network Analysis (TSNA) Script #######################################################
# Last Updated: 06/15/20

###########Description########

# This module will enable you to prepare data for conversion into a networkDynamic object for Temporal Social Network Analysis (TSNA) and quanity and visualize the temporal network using the NDTV package in R. It will also enable you to quantify and visualize network-level and node-level metrics describing the temporal network using the TSNA package in R.

# Short for Tools for Temporal Social Network Analysis, tsna extends the tools of the sna  package for for modeling and analyzing longitudinal (temporal) networks.
# Short for Network Dynamic Temporal Visualizations, ndtv was renders temporal network data as movies, interactive animations, or other representations of changing relational structures and attributes.
# Both of these packages extend and depend on the networkDynamic package, which provides a robust data structure for storing and manipulating temporal network data

# All of the R libraries in this tutorial assume that your network is unimodal – that is, that all of the nodes are the same type of thing, and all of the edges are too 

# Input
# Brandwatch Data

# Output

# Overview 
# The sample data is a list of accounts and the accounts which they mentioned in posts (mention, retweet, or reply)
# Data first models as a unimodal network consisting of the posting accounts (head) and the accounts mentioned (tail)
# The node list for this data is the list of accounts in the Brandwatch input data
# Each edge represents that an account was mentioned by another account (we use an unidirected network but this can be adapted to be directed)
# An undirected edge list must contain three columns of data: a unique identifier for the edge, a source node (posting account), and a target         node (mentioned account) for each edge.
# A temporal edge list must also  contain at the very minimum two additional pieces of information: when a link comes into existence, also           known as the onset of the edge, and when the edge disappears, or the terminus (Terminus = Onset in our case)
# The NDTV and TSNA libraries that we are using throughout this tutorial will expect your data to include an onset, terminus, tail, head, and        edge id
# While these packages can be used to visualize and analyze relationships than span time (i.e. the relationship started at onset t and ended at      terminus t+1), given the nature of our data, we model it only with an onset (the mention happend at onset t).
# Ther terminus therefore is the same as the onset and the duration is 0
# In order to convert this static network into a temporal one, you need to add temporal information to these two lists. 
# Basically, we need to supply a span of time that represents the period in which each edge and each node exists.

## For further reference: 
# https://programminghistorian.org/en/lessons/temporal-network-analysis-with-r

##########Legend############## 

# Start at Line 492 for Converting BW Data (Kremlin-friendly outlets) into networkDynamic object 
# Line 716 for filtering the network object by degree 
# Start at Line 718 specifically to get the correct data output to start the build # 7145 for conversion 
# Start at 768 for the actual networkDynamic output 
# 797 for interactive timeline plot in HTML in browser
# 834 for forward and backward reachable paths 
# 1004 for creating the network viz movie 
# 1308 for snapshots 

######### Questions ############
# Is there a reason to make this directional? 
    # David's Score is the directional built in now (integrate better with the image) 
    # Arrows don't look good when the graph is dense, particularly with bidirectional arrows
    # Have David's score across time too (make that the fourth plot perhaps)
# Is there any situation where we would want a terminus?
    # Not really
    # Duration of followership (so follower/followership networks)
        # If we pull for accounts prior to being suspended (Survival analysis)
            # An application here? Key factors leading to suspension. (Paper/Conference)
        # Evan will keep pulling data from CCP accounts over time
        # Evan is collecting this now 
# Is there any situation where we'd want dynamic nodes too? 
  # Have tails only appear when they are first mentioned (Do. Easy.)
  # Accounts created during the period 
  # Hell threads and how long someone is in one of these groups 
  # Take off if they are not mentioned during a period. Does it do that automatically by chunking? (Leave when not mentioned)
# Is there any situation were we'd want dynamic attributes
  # Nodes 
    # Issue is that any attribute stuff we can collect is point in time, right? Would we be able to collect anything over time? 
    # Would there be any incentive to use Twint to periodically pull from notable accounts and develop longitudinal data that way?
  # Edges
    # At the least we could do edge weights per time slice - then can do edge width and/or vertex proximity based on that
      # We're already thresholding by an edgeweight, so they are really high (and might be similar too)
     # Big thing is get David's Score back into this 
      # Worth reporting the edge list along with centralities 
        # Do for the main script
# Is there any world where we want to visualize times on the edges? Probably not IMO. 

# Can we get this integrated with TWINT to get stuff for TERGMS real fast? 
# Should we do the forward and backward reachable sets in chunks of time? Or just get for the entire network? 
  # I did for the entire network in V1
# Can not claim that A mentioned B and then B mentioned C without that time connection (can we control for this?)
      # Look for paths that are temporally possible - then it'd be a more realistic chain of influence

# Is it useful to have edge counts for each period or do we capture that basically with network degree?
  # Just add this if we want it: tErgmStats(short.stergm.sim,'edges',start = 0,end=25,time.interval=1)

## Notes from 6/8/20
# For hashtag networks, we can use the fwd and bwd reachable sets
# How do we put the movie in a product - static graphs at different shots in time is the low hanging fruit that is useful to people 
    # Static graphs where nodes are fixed in place 

# Big need to make each time slice consistent in layout
# Probably best to drop the isolates for all of this. Structure of giant component. 
    # Might be better to see the big graph grow instead of having the nodes just sitting there

# Elements are presumed inactive until the onset of their first activity spell, and are likewise presumed inactive after the termination of their last active spell.

############################### Module 2: Temporal Network Analysis in R #############################
######################## 1. Install and import packages #############
####### Run Evan's SNA Script first (Ask later whether it makes sense to filter like he does )
################### Testing out functions ########################
library(tidyverse)
library(statnet)
library(readxl)
#library(intergraph)
#library(igraph)
library(openxlsx)
library(data.table)
library(qdapRegex)
library(directlabels)
library(ggnetwork)
library(ggrepel)
library(ggthemes)
library(ggfortify)
library(reshape)
library(sna)
library(gridExtra)
library(tsna)
library(ndtv)
library(anytime)
library(stringi)
library(zoo)
options(scipen=999)

############################### 2. Set up objects and paths #################
# Set up input directory and bring in file
input <- "/Users/christian.conroy/OneDrive - Accenture Federal Services/Documents/StateGec/Brandwatch/PRC_accounts_ts/1447_network/data/ch_data"
setwd(input) #set working directory

# Amount of lines to skip 
skip_lines = 5

## finds all files ending in .xlsx in working directory (brandwatch)
files <- dir(pattern = "*.xlsx")
files <- paste(input, list.files(path = input, pattern = '*.xlsx'), sep = "/")

# set output directory
output_folder <- "/Users/christian.conroy/OneDrive - Accenture Federal Services/Documents/StateGec/Brandwatch/PRC_accounts_ts/1447_network/custom_results_new/"

## Interaction-network size is assessed by looking at the top n*100 interactions so an interaction level of 5 will look at the interactions of the top~500 people in the network.If the network is too busy or computations take too long, reduce the interaction level:
## For full network (not recommended) change interaction_level to NA
interaction_level = 1

# Interaction_level: Threshold based on edge weight (how many times two people interact)
# We do the number times 100 - We get the top based on edge weight (so 1 means top 100)

## If you pulled data by twitter handle, those accounts will have massive influence in your network. Those accounts can be dropped prior to running the network. Include caps.
## if you don't want to drop any authors, change to NA.
remove_authors = NA

# If we are looking at official accounts, we might not care about that account and only about the mentions around it, hence what we'd need here

# only return top betweenness nodes (right now they are displayed as top degree, but can segment based on betweeness instead if we want)
# was added for one report but hasn't been used for a while
filter_betweenness = FALSE

# If you pulled the network with a list of accounts and you want those accounts to be included. If this is the case, you'll also need to upload a dataframe with those accounts where the account names are in the first column (colname irrelevant) it should be csv format.
pulled_list_of_accs = TRUE
path_to_acc_list = "/Users/christian.conroy/OneDrive - Accenture Federal Services/Documents/StateGec/Brandwatch/PRC_accounts_ts/1447_network/data/ch_data/Suspicious Cofollower Accounts - 1447 count.csv"

getwd()
# This just helps with the viz of others and original (Can see where the accounts we pulled are and the ones not pulled originally)

## What do you want these original accounts to be labeled as in your graph?
orig_accounts = "Original"

## What do you want the other accounts to be called in your graph?
other_accounts = "Other"

# Do you want to label based on centrality measures or some other narrative? (if false, make sure to inpust labels)
centrality_label = TRUE
#custom_labels = c()

################ Visual Filtering 
custom_graph = TRUE

reachable_paths_on = TRUE

reachable_labels <- c('@whale90196377', '@gftdduzyhxjzids')
# Above example is from Temporal CCP Network product

## Custom color/ legend guide:
# to create custom labels, you'll need to do 2 things:
# 1: create a csv with 2 columns where the first column contains account names and the second column contains labels. Point to that file here:

path_to_nodes_categorized = "~/OneDrive - Accenture Federal Services/Documents/StateGec/Brandwatch/PRC_accounts_ts/1447_network/data/ch_data/1447_pull_suspended.csv"

# 2: each label will need a custom color hexcode: red, dark blue, green, light blue, yellow
gec_pal <- gec_pal <- c("Eventually Suspended" = "#C00000", "Active" = "#E7E6E6")  

################ Time Filtering 

week = TRUE
day = FALSE

# 604800: Number of seconds in a week 
# 86400: Number of seconds in day

# if both week and day are false, set a custom interval (using epoch unix time) below
custom_interval = 604800

#################################################### Run helper functions ##########################################################################
# If data are pulled by authors, we should drop the authors. Otherwise they dominate the newtork.
# If remove_authors is NA, this function will not drop any authors.  
drop_authors <- function(x){
  if(!is.na(remove_authors)){
    x <- x %>% filter(!authors %in% remove_authors)
    x <- x %>% filter(!authors %in% tolower(remove_authors))
    x <- x %>% filter(!authors %in% paste0(remove_authors, "'s"))
    x <- x %>% filter(!authors %in% tolower(paste0(remove_authors, "'s")))
    x <- x %>% filter(!target %in% remove_authors)
    x <- x %>% filter(!target %in% tolower(remove_authors))
    x <- x %>% filter(!target %in% paste0(remove_authors, "'s"))
    x <- x %>% filter(!target %in% tolower(paste0(remove_authors, "'s")))
  }
  return(x)
}

## Generates a number of interactions based on the top
## 100 * interaction_level authors
recommended_interactions <- function(x){
  if(!is.na(interaction_level)){
    a1 <- x %>% 
      group_by(`head`, `tail`) %>% 
      count(sort = TRUE)
    recommended <- a1[(interaction_level*100),][[3]]
  }else{
    recommended = 1
  }
  return(recommended)
}

## here we group authors and targets and sort them by count.
## We then ONLY take authors who meet the threshhold found in recommended_interactions
# Cut down to managable network size
agent_filter <- function(x, limit = 2){
  a1 <- x %>% 
    dplyr::group_by(`head`, `tail`) %>% 
    dplyr::count()
  a2 <- a1 %>% dplyr::filter(n >= limit)
  a3 <- a2 %>% dplyr::left_join(x) %>% select(-n)
  return(a3)
  }

get_centrality <- function(x, label_names = "Degree"){
  # Get centrality stats and set labels. Defaults to top 10 degree
  # can be changed to top 10 "Betweenness" by changing label_names = "Betweenness")
  centrality <- tibble(
    names = as.character(x%v%'label'),
    degree = sna::degree(x),
    betweenness = sna::betweenness(x)
  )
  if(label_names == "Degree"){
    centrality_labels <- centrality %>%
      arrange(desc(degree)) %>% 
      slice(1:10) %>% .$names
  } 
  else if(label_names == "Betweenness"){
    centrality_labels <- centrality %>% 
      arrange(desc(betweenness)) %>% 
      slice(1:10) %>% .$names
  }
  return(list(centrality, centrality_labels)) 
}

## This normalizes degree between 0-10. Any values below
## 1 are changed to 1. This is for sizing nodes in the graph.
normalize_label_size <- function(x){
  y <- (x - min(x))/(max(x)-min(x))*10
  y <- ifelse(y<=1, 1, y)
  return(y)
}

# min max normalization function for
# get_informal_leaders
normalized <- function(x) {
  x<-(x - min(x)) / (max(x) - min(x))
  return(x)
}

# Calculate David's Score
getDS <- function(X){
  if (static == TRUE) {
    q <- as.data.frame(as.edgelist(X))
    colnames(q) <- c("tail", "head")
    b <- as.data.frame(cbind(X %v% 'label', X %v% 'vertex.names'))
    b$V2 <- as.numeric(b$V2)
    }else{
      q <- as.data.frame(X)
      b <- as.data.frame(cbind(get.vertex.attribute(X, "label"), get.vertex.attribute(X, "vertex.names")))
      b$V2 <- as.numeric(b$V2)
      }
  author_edge <- left_join(q, b, by = c("head" = "V2"))
  author_edge <- left_join(author_edge, b, by = c("tail" = "V2"))
  author_edge <- author_edge[,c("V1.x", "V1.y")]
  colnames(author_edge) <- c("head", "tail")
  adj <- as.data.frame(as.matrix(igraph::get.adjacency(igraph::graph.data.frame(author_edge, directed = TRUE))))
  if (dim(adj)[1] != dim(adj)[2]) stop("not a square matrix")
  
  # The paper this is based on doesn't add 1 here and neither do other
  # open source implementations, but adding 1 allows us to not divide by 0 
  # and makes the rest of the function actually work.
  adj <- as.matrix(adj) + 1
  dyadc <- adj + t(adj)
  ## calculate on the basis of a uniform distribution.
  # see http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.490.7556&rep=rep1&type=pdf
  Dij <- adj/dyadc-(((adj/dyadc)-0.5)/(dyadc+1))
  diag(Dij) <- 0
  w1 <- rowSums(Dij)
  w2 <- Dij%*%w1
  l1 <-colSums(Dij)
  l2 <- t(l1)%*%Dij
  DS <- w1 + w2 - l1 - t(l2)
  #maxDS <- nrow(X)*(nrow(X)-1)/2
  #NormDS <- (DS + maxDS) / nrow(X)
  DS_1 <- as.data.frame(DS) %>% rownames_to_column("names") %>% 
    select(names, David_Score = V1) %>% arrange(desc(David_Score))
  
  centrality <- get_centrality(X, label_names = "Degree")
  
  centrality[[1]] <- centrality[[1]] %>% left_join(DS_1)
  
  #NormDS <- array(NormDS,dim=c(nrow(X),1),dimnames=c(list(names),"NormDS"))
  return(centrality)
}

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

major_component <- function(x){
  ig <- intergraph::asIgraph(x)
  gclust<-igraph::clusters(ig, mode='weak')
  lcc<-igraph::induced.subgraph(ig, igraph::V(ig)[which(gclust$membership == which.max(gclust$csize))])
  G <- intergraph::asNetwork(lcc)
}

# Create function to Import data #####################################################################
get_dynamic_edgelist <- function(x){
  # This function generates an edgelist for the data. Authors are individuals who wrote tweets.
  # targets are individuals within the tweets. T
  print('importing brandwatch data...')
  raw <- x %>%
    map_df(~read_excel(., skip = skip_lines)) %>% 
    mutate(authors = paste0("@", Author))
  pre_agent2 <- raw %>% 
    select(Date, authors, `Mentioned Authors`) %>% 
    separate(`Mentioned Authors`, c("A1", "A2", "A3", "A5", "A6", "A7",
                                    "A8", "A9", "A10", "A11", "A12", "A13",
                                    "A14", "A15", "A16", "A17", "A18", "A19", 
                                    "A20", "A21", "A22", "A23", "A24", "A25", "A26",
                                    "A27", "A28", "A29", "A30", "A31", "A32", "A33",
                                    "A34", "A35", "A36", "A37", "A38", "A39", "A40",
                                    "A41", "A42", "A43", "A44", "A45", "A46", "A47",
                                    "A48", "A49", "A50", "A51", "A52", "A53", "A54",
                                    "A55"), sep = ",") %>% 
    gather(junk, target, A1:A55, na.rm = TRUE) %>% 
    select(-junk) %>% 
    mutate(authors = tolower(authors),
           target = tolower(target))
  
  pre_agent2$authors <- str_trim(pre_agent2$authors)
  pre_agent2$target <- str_trim(pre_agent2$target)
  
  # create final edgelist
  pre_agent3 <- pre_agent2[!(pre_agent2$target == ""),]
  
  # drop ego network authors (optional)
  pre_agent3 <- drop_authors(pre_agent3)
  
  pre_agent3_index <- pre_agent3
  colnames(pre_agent3_index) <- c("onset", "head", "tail")
  
  time_edgelist <- pre_agent3_index
  
  time_edgelist$onset <- as.numeric(as.POSIXct(time_edgelist$onset, "%Y-%m-%d %H:%M:%OS"))
  time_edgelist$terminus <- time_edgelist$onset
  time_edgelist <- as.data.frame(time_edgelist)

  return(time_edgelist)
}


#################################### Load in Data and Convert to Edgelist for CH if needed ##########################
get_dynamic_edgelist_crimson <- function(x){
  # This function generates an edgelist for the data. Authors are individuals who wrote tweets.
  # targets are individuals within the tweets. T
  print('importing ch data...')
  raw <- x %>%
    map_df(~read_excel(., skip = 1)) %>%
    mutate_all(as.character) %>% 
    mutate(authors = Author)
  
  raw$target_auths = str_extract_all(raw$Contents, "(?<=^|\\s)@[^\\s]+")
  
  # clean up the new column
  raw$target_auths <- sub("\\)$", "", raw$target_auths)
  raw$target_auths <- sub("c\\(", "", raw$target_auths)
  raw$target_auths <- str_remove_all(raw$target_auths, '\\"')
  raw <- raw[raw$target_auths != "character(0",]
  colnames(raw)[2] <- "Date"
  
  pre_agent2 <- raw %>% 
    select(Date, authors, target_auths) %>% 
    separate(target_auths, c("A1", "A2", "A3", "A5", "A6", "A7",
                             "A8", "A9", "A10", "A11", "A12", "A13",
                             "A14", "A15", "A16", "A17", "A18", "A19", 
                             "A20", "A21", "A22", "A23", "A24", "A25", "A26",
                             "A27", "A28", "A29", "A30", "A31", "A32", "A33",
                             "A34", "A35", "A36", "A37", "A38", "A39", "A40",
                             "A41", "A42", "A43", "A44", "A45", "A46", "A47",
                             "A48", "A49", "A50", "A51", "A52", "A53", "A54",
                             "A55"), sep = ",") %>% 
    gather(junk, target, A1:A55, na.rm = TRUE) %>% 
    select(-junk) %>% 
    mutate(authors = tolower(authors),
           target = tolower(target))
  
  pre_agent2$authors <- str_trim(pre_agent2$authors)
  pre_agent2$target <- str_trim(pre_agent2$target)
  
  # create final edgelist
  pre_agent3 <- pre_agent2[!(pre_agent2$target == ""),]
  
  # drop ego network authors (optional)
  pre_agent3 <- drop_authors(pre_agent3)
  
  pre_agent3_index <- pre_agent3
  colnames(pre_agent3_index) <- c("onset", "head", "tail")
  
  time_edgelist <- pre_agent3_index

  time_edgelist$onset <- as.numeric(as.POSIXct(time_edgelist$onset, "%Y-%m-%d %H:%M:%OS"))
  time_edgelist$terminus <- time_edgelist$onset
  time_edgelist <- as.data.frame(time_edgelist)
  
  return(time_edgelist)
}
#######################################################

############## Pruning the network
# Will want to prune before doing this each time as creating the network and visualizing it can be intensive
reduce_network <- function(x){
  
  interactions = recommended_interactions(x)
  time_edgelist_new <- as.data.frame(agent_filter(x, interactions))
  
  return(list(time_edgelist_new, interactions))
}

########################################## Create function to build the dynamic network #############################################################
# Required for a dynamic network 
    # 1. Edge Spells - A matrix or data.frame of spells specifying edge timing. Assumed to be [onset,terminus,tail vertex.id, head vertex.id]. 
# Optional for as dynamic network 
    # 2. create.TEAs - If TRUE, Dynamic TEA attributes will be created corresponding to the static attributes appear on the network elements of          network.list
    # 3. edge.TEA.names - An optional vector of names for the dynamic (TEA) edge attributes to be imported from the extra columns of edge.spells (if     create.TEAs=TRUE)
    # 4. vertex.TEA.names - an optional vector of names for the dynamic (TEA) vertex attributes to be imported from the extra columns of vertex          .spells (if create.TEAs=TRUE)


if(week == TRUE){
  interval_val = 604800
} else if(day == TRUE){
  interval_val = 86400
} else {
  interval_val = custom_interval
}

get_dynamic_network <- function(x){
  # Cut down the dataset to a manageable size based on the interaction_level set
  # Change back to normal report when done
  time_edgelist_both <- reduce_network(get_dynamic_edgelist_crimson(x))
  time_edgelist_new <- time_edgelist_both[[1]]
  interactions <- time_edgelist_both[[2]]

  # Create nodes to pass as vertex attributes to network object
  head <- time_edgelist_new %>%
    distinct(head) %>%
    dplyr::rename(label = head)
  
  tail <- time_edgelist_new %>%
    distinct(tail) %>%
    dplyr::rename(label = tail)
  
  nodes <- full_join(head, tail, by = "label")
  
  nodes <- nodes %>% rowid_to_column("id")
  
  mentions <- time_edgelist_new %>%  
    group_by(head, tail) %>%
    summarise(weight = n()) %>% 
    ungroup()
  
  # Create edgelist for normal network object
  
  edges <- mentions %>% 
    left_join(nodes, by = c("head" = "label")) %>% 
    dplyr::rename(from = id)
  
  edges <- edges %>% 
    left_join(nodes, by = c("tail" = "label")) %>% 
    dplyr::rename(to = id)
  
  edges <- select(edges, from, to, weight)
  
  # Create edgespells for networkdynamic object
  
  time_edgelist_new$terminus <- time_edgelist_new$onset
  time_edgelist_new$onset.censored <- FALSE
  time_edgelist_new$terminus.censored <- FALSE
  time_edgelist_new$duration <- time_edgelist_new$terminus - time_edgelist_new$onset
  time_edgelist_new$edge.id <- as.numeric(factor(paste0(time_edgelist_new$head, time_edgelist_new$tail)))
  time_edgelist_new <- time_edgelist_new %>% 
    left_join(nodes, by = c("tail" = "label")) 
  time_edgelist_new <- time_edgelist_new %>% 
    left_join(nodes, by = c("head" = "label")) 
  time_edgelist_new  <- time_edgelist_new[,c(3,4,9,10)]
  colnames(time_edgelist_new)[3] <- "tail"
  colnames(time_edgelist_new)[4] <- "head"
  
  nodes_spells_tail <- time_edgelist_new[,c("onset", "terminus", "tail")]
  colnames(nodes_spells_tail)[3] <- "vertex.id"
  
  nodes_spells_head <- time_edgelist_new[,c("onset", "terminus", "head")]
  colnames(nodes_spells_head)[3] <- "vertex.id"
  
  node_spells <- rbind (nodes_spells_tail, nodes_spells_head)

  # Add dynamic node object based on time slicing for node coloring over time
  # First create time groupings
  q <- 1:(floor(((max(node_spells$onset)-min(node_spells$onset))/interval_val))+1)
  for (i in 1:length(q)) {
    if (i == 1){
      node_spells$time_group <- ifelse(node_spells$onset >= min(node_spells$onset) & node_spells$onset < (min(node_spells$onset) + interval_val), 1, "NA")}else if(i > 1 & i < length(q)){
      node_spells$time_group <- ifelse(node_spells$onset >= (min(node_spells$onset) + ((i-1)* interval_val))  & node_spells$onset < (min(node_spells$onset) + ((i)* interval_val)), i, node_spells$time_group)
    }else {
      node_spells$time_group <- ifelse(node_spells$onset >= (min(node_spells$onset) + ((i-1)* interval_val)), i,node_spells$time_group)
    }
  }
  
  # Then color based on whether the account was in the previous group
  for (i in 1:length(q)) {
    if (i == 1){
      node_spells$new.active.color <- "red"
    }else{
  node_spells$new.active.color <- ifelse(node_spells$time_group == i & node_spells$vertex.id %in% node_spells[node_spells$time_group == i-1, "vertex.id"], "grey", node_spells$new.active.color)}
  }

  # We add the temporal data for the vertices to the static network we created to form a dynamic network
  mentions_network <- network::network(edges, vertex.attr = nodes, matrix.type = "edgelist", directed = F, hyper = F, loops = F, multiple = F, bipartite = F)
  
  mentions_network_dynamic_nodes <- networkDynamic(mentions_network, edge.spells = time_edgelist_new, vertex.spells = node_spells[1:3])
  mentions_network_dynamic <- networkDynamic(mentions_network, edge.spells = time_edgelist_new)
  
  for(i in 1:length(q)){
    if(i == 1){
      activate.vertex.attribute(mentions_network_dynamic_nodes, "active.color", "grey", onset = min(node_spells$onset), terminus = (min(node_spells$onset) + interval_val))
      }else if(i > 1 & i < length(q)){
        activate.vertex.attribute(mentions_network_dynamic_nodes, "active.color", "grey", onset = (min(node_spells$onset) + ((i-1)* interval_val)), terminus = (min(node_spells$onset) + ((i)* interval_val)), v = node_spells[node_spells$time_group == i & node_spells$new.active.color == "grey", "vertex.id"])
        activate.vertex.attribute(mentions_network_dynamic_nodes, "active.color", "red", onset = (min(node_spells$onset) + ((i-1)* interval_val)), terminus = (min(node_spells$onset) + ((i)* interval_val)), v = node_spells[node_spells$time_group == i & node_spells$new.active.color == "red", "vertex.id"])
        }else {
      activate.vertex.attribute(mentions_network_dynamic_nodes, "active.color", "grey", onset = (min(node_spells$onset) + ((i-1)* interval_val)), terminus = max(node_spells$onset), v = node_spells[node_spells$time_group == i & node_spells$new.active.color == "grey", "vertex.id"])
      activate.vertex.attribute(mentions_network_dynamic_nodes, "active.color", "red", onset = (min(node_spells$onset) + ((i-1)* interval_val)), terminus = Inf, v = node_spells[node_spells$time_group == i & node_spells$new.active.color == "red", "vertex.id"])
    }
  }
  
  mentions_network_dynamic_directed_nodes <- networkDynamic(edge.spells = time_edgelist_new, vertex.spells = node_spells[1:3])
  set.vertex.attribute(mentions_network_dynamic_directed_nodes,"id",as.vector(nodes$id))
  set.vertex.attribute(mentions_network_dynamic_directed_nodes,"label",as.vector(nodes$label))
  
  mentions_network_dynamic_directed <- networkDynamic(edge.spells = time_edgelist_new)
  set.vertex.attribute(mentions_network_dynamic_directed,"id",as.vector(nodes$id))
  set.vertex.attribute(mentions_network_dynamic_directed,"label",as.vector(nodes$label))
  
  for(i in 1:length(q)) {
    if (i == 1) {
      activate.vertex.attribute(mentions_network_dynamic_directed_nodes, "active.color", "grey", onset = min(node_spells$onset), terminus = (min(node_spells$onset) + interval_val))
    }else if(i > 1 & i < length(q)){
      activate.vertex.attribute(mentions_network_dynamic_directed_nodes, "active.color", "grey", onset = (min(node_spells$onset) + ((i-1)* interval_val)), terminus = (min(node_spells$onset) + ((i)* interval_val)), v = node_spells[node_spells$time_group == i & node_spells$new.active.color == "grey", "vertex.id"])
      activate.vertex.attribute(mentions_network_dynamic_directed_nodes, "active.color", "red", onset = (min(node_spells$onset) + ((i-1)* interval_val)), terminus = (min(node_spells$onset) + ((i)* interval_val)), v = node_spells[node_spells$time_group == i & node_spells$new.active.color == "red", "vertex.id"])
    }else {
      activate.vertex.attribute(mentions_network_dynamic_directed_nodes, "active.color", "grey", onset = (min(node_spells$onset) + ((i-1)* interval_val)), terminus = max(node_spells$onset), v = node_spells[node_spells$time_group == i & node_spells$new.active.color == "grey", "vertex.id"])
      activate.vertex.attribute(mentions_network_dynamic_directed_nodes, "active.color", "red", onset = (min(node_spells$onset) + ((i-1)* interval_val)), terminus = Inf, v = node_spells[node_spells$time_group == i & node_spells$new.active.color == "red", "vertex.id"])
    }
  }
  return(list(mentions_network_dynamic, mentions_network_dynamic_nodes, mentions_network_dynamic_directed, mentions_network_dynamic_directed_nodes, node_spells, interactions, time_edgelist_new))
}

dynamic_objects <- get_dynamic_network(files)
mentions_network_dynamic <- dynamic_objects[[1]]
mentions_network_dynamic_nodes <- dynamic_objects[[2]]
mentions_network_dynamic_directed <- dynamic_objects[[3]]
mentions_network_dynamic_directed_nodes <- dynamic_objects[[4]]
node_spells <- dynamic_objects[[5]]
# Come back and make sure this is appended on the end of the saved files and not the interaction term
interactions <- dynamic_objects[[6]]
time_edgelist_new <- dynamic_objects[[7]]

setwd(output_folder)
dir.create("node_movement_stats")
setwd(input)


node_over_time <- function(x,y){
  # new: Grey means they were in the previous time period and red means they were not (1 all grey as no previous)
  # fut: Grey means they were in the next time period and red means they were not (5 all grey as no future)
  # return: Grey means they did return after not being in the next time period and red means they did not (5 all grey as no future)
  x$new.active.color[x$time_group == 1] <- c("grey")
  x$fut.active.color <- "red"
  q <- 1:(floor(((max(x$onset)-min(x$onset))/interval_val))+1)
  
  for (i in 1:length(q)) {
    if (i != length(q)){
      x$fut.active.color <- ifelse(x$time_group == i & x$vertex.id %in% x[x$time_group == i+1, "vertex.id"], "grey", x$fut.active.color)
    }else{
      x$fut.active.color <- ifelse(x$time_group == i, "grey", x$fut.active.color)
    }
  }
  x$return.active.color <- "red"
  for (i in 1:length(q)) {
    if (i != length(q)){
      x$return.active.color <- ifelse(x$time_group == i & x$fut.active.color == "red" & x$vertex.id %in% x[x$time_group > i & x$time_group <= length(q), "vertex.id"], "grey", x$return.active.color)
    }else{
      x$return.active.color <- ifelse(x$time_group == i, "grey", x$return.active.color)
    }
  }
  
  x_new <- x %>%
    group_by(time_group, new.active.color) %>%
    summarise(count = n() ) %>%
    mutate(new_prop = count / sum(count) )
  
  x_fut <- x %>%
    group_by(time_group, fut.active.color) %>%
    summarise(count = n() ) %>%
    mutate(fut_prop = count / sum(count) )
  
  x_ret <- x %>%
    group_by(time_group, return.active.color) %>%
    summarise(count = n() ) %>%
    mutate(return_prop = count / sum(count) )
  
  x_com <- x_new[,c(1:2,4)] %>% 
    full_join(x_fut[,c(1:2,4)], by = c("time_group", "new.active.color" = "fut.active.color")) 
  
  x_com <- x_com %>% 
    full_join(x_ret[,c(1:2,4)], by = c("time_group", "new.active.color" = "return.active.color")) 
  
  write.csv(x_com, file=paste0(output_folder, 'node_movement_stats/', sprintf('node_movement_stats_%s.csv', as.character(interaction_level))))
  
  # Interactions between nodes 
  q <- 1:(floor(((max(y$onset)-min(y$onset))/interval_val))+1)
  for (i in 1:length(q)) {
    if (i == 1){
      y$time_group <- ifelse(y$onset >= min(y$onset) & y$onset < (min(y$onset) + interval_val), 1, "NA")}else if(i > 1 & i < length(q)){
        y$time_group <- ifelse(y$onset >= (min(y$onset) + ((i-1)* interval_val))  & y$onset < (min(y$onset) + ((i)* interval_val)), i, y$time_group)
      }else {
        y$time_group <- ifelse(y$onset >= (min(y$onset) + ((i-1)* interval_val)), i,y$time_group)
      }
  }
  
  y$concat_men <- paste(as.character(y$tail), as.character(y$head))
  y$concat_men_rec <- paste(as.character(y$head), as.character(y$tail))
  
  mutual <- y %>%
    group_by(time_group) %>%
    mutate(mutual = ifelse((concat_men %in% concat_men_rec), "mutual", "not mutual"))
  
  head <- mutual %>%
    group_by(time_group) %>%
    distinct(head, mutual) %>%
    dplyr::rename(label = head)
  
  tail <- mutual %>%
    group_by(time_group) %>%
    distinct(tail, mutual) %>%
    dplyr::rename(label = tail)
  
  accounts_by_week <- rbind(head, tail)
  accounts_by_week <- accounts_by_week[!duplicated(accounts_by_week[c("label","time_group", "mutual")]),]
  
  mutual <- accounts_by_week  %>%
    group_by(time_group, mutual) %>%
    summarise(count = n() ) %>%
    mutate(mutual_prop = count / sum(count) )
  
  write.csv(mutual, file=paste0(output_folder, 'node_movement_stats/', sprintf('reciprocal_ties_stats_%s.csv', as.character(interaction_level))))
}

node_move <- node_over_time(node_spells, time_edgelist_new)


################################################# Visualize Dynamic Networks ########################################################################
# To really see these changes, we’ll use an animation that shows a sliding interval, and aggregates all of the collaborations within that interval.
# We convert the dynamic network into a series of static networks (slices) which represents the accumulated state over the time span
# The NDTV package actually breaks up the calculations behind the animation from the rendering of the animation itself.

# First, it computes the animation given a set of parameters that tell it when to start, stop, how much to incrementally advance between frames, and how much time we want each interval to aggregate. 

#Once NDTV has computed the animation, it can generate a webpage with a rendering of this animation using the render.d3movie() function. As with the compute.animation() function above, this step can take a long time to finish processing depending on the size of the network.

#The default labels are simply the identification number for each vertex. The vertex.tooltip parameter of this function might look a little scary, but basically it supplies each frame or “slice” of the animation with the correct tooltip information so we can see the name and region of each vertex if we click on it.

# Come back and fix the labels and thenodes to reflect the same as ggplot.

########################### Lay out time intervals 

# Seems boring at the week intervals. Is that the best? Or maybe it's just how I've subset it.

# Set out labeling 
# If else to plot labels by either centrality values or a provided list of custom labels
if(centrality_label == TRUE) {
  centrality <- get_centrality(mentions_network_dynamic, label_names = "Degree")
  label_names <- centrality[[2]]
}else{
  label_names <- custom_labels  
}

########################### Interactive Visualizations 
##### At its most basic, rendering a movie consists of four key steps:
# 1. Determining appropriate parameters (time range, aggregation rule, etc)
# 2. Computing layout coordinates for each time slice
# 3. Rendering a series of plots for each time slice
# 4. Replaying the cached sequence of plots (or writing to a file on disk)

# The ndtv package aims for the following sometimes conflicting animation goals:
  # Similar layout goals as static layouts (minimize edge crossing, vertex overlap, etc)
  # Changes in network structure should be reflected in changes in the vertex positions in the layout.
  # Layouts should remain as visually stable as possible over time.
  # Small changes in the network structure should lead to small changes in the layouts.
# They all accept the coordinates of the previous layout as an argument so that they can try to construct a suitably smooth sequence of node positions. Using the previous coordinates allows us to “chain” the layouts together. 
    # This means that each visualization step can often avoid some computational work by using a previous solution as its starting point, and      it     is likely to find a solution that is spatially similar to the previous step.

# The Kamada-Kawai network layout algorithm is often described as a “force-directed” or “spring embedded” simulation, but it is mathematica     lly equivalent to some forms of MDS (Kamada-Kawai uses Newton-Raphson optimization instead of SMACOF stress-majorization).

time_start <- min(as.numeric(as.POSIXct(round.POSIXt(anytime(as.data.frame(mentions_network_dynamic_nodes)$onset, tz = "America/New_York"), unit = "days"), tz = "America/New_York")))

time_end <- max(as.data.frame(mentions_network_dynamic_nodes)$onset)

setwd(output_folder)
dir.create("network_movies")
setwd(input)

compute.animation(
  mentions_network_dynamic_nodes,
  animation.mode = "kamadakawai",
  default.dist=6,
  slice.par = list(
    start = time_start,
    end = time_end,
    interval = interval_val,
    aggregate.dur = interval_val,
    rule = "any"
  )
)

mentions_network_dynamic_nodes%v%'label_fil' <- ifelse(mentions_network_dynamic_nodes%v%"label" %in% label_names, mentions_network_dynamic_nodes%v%"label", NA)
mentions_network_dynamic_nodes%v%'hold' <- mentions_network_dynamic_nodes%v%'vertex.names'
mentions_network_dynamic_nodes%v%'vertex.names' <- mentions_network_dynamic_nodes%v%'label_fil'

movie <- render.d3movie( #label.cex
  mentions_network_dynamic_nodes, filename= paste0(output_folder,  'network_movies/', sprintf("network_movie_%s.html", as.character(interaction_level))), launchBrowser=FALSE, displaylabels = TRUE, vertex.col= 'active.color', vertex.cex =0.6, main= paste( "Weekly Time Slices: ", paste(anytime(time_start, tz = "America/New_York"), round.POSIXt(anytime(time_end, tz = "America/New_York"), unit = "day"), sep = " to "), sep = " "),
  # This slice function makes the labels work
  vertex.tooltip = function(slice) {
    paste(
      (slice %v% "label")
    )
  }
)

compute.animation(
  mentions_network_dynamic,
  animation.mode = "kamadakawai",
  default.dist=6,
  slice.par = list(
    start = time_start,
    end = time_end,
    interval = interval_val,
    aggregate.dur = interval_val,
    rule = "any"
  )
)

mentions_network_dynamic%v%'label_fil' <- ifelse(mentions_network_dynamic%v%"label" %in% label_names, mentions_network_dynamic%v%"label", NA)

if(custom_graph==TRUE){
  df <- read_csv(path_to_nodes_categorized)
  df$suspended <- ifelse(df$suspended == 1, "Eventually Suspended", "Active")
  colnames(df)[1:2] <- c('names', 'ref')
  df$names <- tolower(df$names)
  df$names <- ifelse(grepl('^@', df$names), df$names, paste0('@', df$names))
  # run the following code as is:
  newdf <- tibble(names = mentions_network_dynamic%v%'label')
  labels <- label_names
  ndf2 <- newdf %>% left_join(df)
  ndf2$newor <- ifelse(is.na(ndf2$ref), "Excluded", ndf2$ref)
  mentions_network_dynamic%v%'Orientation' <- ifelse(mentions_network_dynamic%v%'label' %in% ndf2$names, ndf2$newor, NA)
  mentions_network_dynamic%v%'color' <- ifelse(mentions_network_dynamic%v%"Orientation" == "Eventually Suspended", "#C00000", "#E7E6E6")
  
  movie <- render.d3movie( # gec_pal  #label.cex #ifelse(harry_potter_support%v%'gender'==1,'blue','green')
    mentions_network_dynamic, filename= paste0(output_folder, 'network_movies/', sprintf("network_movie_custom_%s.html", as.character(interaction_level))), launchBrowser=FALSE, displaylabels = TRUE, vertex.col=mentions_network_dynamic %v% 'color', label=function(slice, gmode = "graph"){ifelse(degree(slice, gmode = "graph") >= 1, mentions_network_dynamic%v%'label_fil', NA)}, vertex.cex =function(slice){ifelse(degree(slice, gmode = "graph") >= 1, 0.6, NA)}, main= paste( "Weekly Time Slices: ", paste(anytime(time_start, tz = "America/New_York"), round.POSIXt(anytime(time_end, tz = "America/New_York"), unit = "day"), sep = " to "), sep = " "), 
    # This slice function makes the labels work
    vertex.tooltip = function(slice) {
      paste(
        (slice %v% "label")
      )
    }
  )  
}

# Noteably, curved edges, edge labels, and label positioning are not yet implemented and will be ignored.

# As noted earlier, the MDSJ library is released under Creative Commons License “by-nc-sa” 3.0. This means using the algorithm for commercial purposes would be a violation of the license. More information about the MDSJ library and its licensing can be found at http://www.inf.uni-konstanz.de/algo/software/mdsj/.
      # So MDSJ is slightly better, but stuck with kamadakawai to not have to deal with the potential licensing issue of the external install like with graphviz

# Great stuff for how to further format here: https://kateto.net/network-visualization

########### Creating the animated visual with a directed network 
compute.animation(
  mentions_network_dynamic_directed_nodes,
  animation.mode = "kamadakawai",
  default.dist=6,
  slice.par = list(
    start = time_start,
    end = time_end,
    interval = interval_val,
    aggregate.dur = interval_val,
    rule = "any"
  )
)

mentions_network_dynamic_directed_nodes%v%'label_fil' <- ifelse(mentions_network_dynamic_directed_nodes%v%"label" %in% label_names, mentions_network_dynamic_directed_nodes%v%"label", NA)
mentions_network_dynamic_directed_nodes%v%'hold' <- mentions_network_dynamic_directed_nodes%v%'vertex.names'
mentions_network_dynamic_directed_nodes%v%'vertex.names' <- mentions_network_dynamic_directed_nodes%v%'label_fil'

movie <- render.d3movie( #label.cex
  mentions_network_dynamic_directed_nodes, filename= paste0(output_folder,  'network_movies/', sprintf("directed_network_movie_%s.html", as.character(interaction_level))), launchBrowser=FALSE, displaylabels = TRUE,  vertex.col= 'active.color', vertex.cex =0.6, main= paste( "Weekly Time Slices: ", paste(anytime(time_start, tz = "America/New_York"), round.POSIXt(anytime(time_end, tz = "America/New_York"), unit = "day"), sep = " to "), sep = " "),
  # vertex.cex =function(slice){ifelse(degree(slice) > 1, 0.6, NA)}, 
  # This slice function makes the labels work
  vertex.tooltip = function(slice) {
    paste(
      (slice %v% "label")
    )
  }
)

compute.animation(
  mentions_network_dynamic_directed,
  animation.mode = "kamadakawai",
  default.dist=6,
  slice.par = list(
    start = time_start,
    end = time_end,
    interval = interval_val,
    aggregate.dur = interval_val,
    rule = "any"
  )
)

mentions_network_dynamic_directed%v%'label_fil' <- ifelse(mentions_network_dynamic_directed%v%"label" %in% label_names, mentions_network_dynamic_directed%v%"label", NA)

if(custom_graph==TRUE){
  df <- read_csv(path_to_nodes_categorized)
  df$suspended <- ifelse(df$suspended == 1, "Eventually Suspended", "Active")
  colnames(df)[1:2] <- c('names', 'ref')
  df$names <- tolower(df$names)
  df$names <- ifelse(grepl('^@', df$names), df$names, paste0('@', df$names))
  # run the following code as is:
  newdf <- tibble(names = mentions_network_dynamic_directed%v%'label')
  labels <- label_names
  ndf2 <- newdf %>% left_join(df)
  ndf2$newor <- ifelse(is.na(ndf2$ref), "Excluded", ndf2$ref)
  mentions_network_dynamic_directed%v%'Orientation' <- ifelse(mentions_network_dynamic_directed%v%'label' %in% ndf2$names, ndf2$newor, NA)
  mentions_network_dynamic_directed%v%'color' <- ifelse(mentions_network_dynamic_directed%v%"Orientation" == "Eventually Suspended", "#C00000", "#E7E6E6")
  
  movie <- render.d3movie( # gec_pal  #label.cex #ifelse(harry_potter_support%v%'gender'==1,'blue','green')
    mentions_network_dynamic_directed, filename= paste0(output_folder,  'network_movies/', sprintf("directed_network_movie_custom_%s.html", as.character(interaction_level))), launchBrowser=FALSE, displaylabels = TRUE, vertex.col=mentions_network_dynamic_directed %v% 'color', label=function(slice){ifelse(degree(slice) >= 1, mentions_network_dynamic_directed%v%'label_fil', NA)}, vertex.cex =function(slice){ifelse(degree(slice) >= 1, 0.6, NA)}, main= paste( "Weekly Time Slices: ", paste(anytime(time_start, tz = "America/New_York"), round.POSIXt(anytime(time_end, tz = "America/New_York"), unit = "day"), sep = " to "), sep = " "), 
    # This slice function makes the labels work
    # vertex.cex =function(slice){ifelse(degree(slice) > 1, 0.6, NA)},
    vertex.tooltip = function(slice) {
      paste(
        (slice %v% "label")
      )
    }
  )  
}

mentions_network_dynamic%v%'vertex.names' <- mentions_network_dynamic_directed%v%'hold'
mentions_network_dynamic%v%'vertex.names' <- mentions_network_dynamic_directed%v%'hold'

################################################ Network metrics over time ##########################################################################
#To compare different moments, however, quantifiable metrics for the network or for individual nodes may be more useful than animated visualizations.

# Plot formation of edges over time 

# Just like you can analyze centrality at the node level or network level in static network analysis, you can analyze how centrality changes over time with temporal network analysis

# Calculate and graph the rolling betweenness centralization of the network

setwd(output_folder)
dir.create("network_level_stats")
dir.create("node_level_stats")
dir.create("node_level_stats/high_betweenness_accounts")
dir.create("node_level_stats/high_degree_accounts")
dir.create("node_level_stats/high_eigenvector_accounts")
dir.create("node_level_stats/high_david_accounts")
setwd(input)

network_metrics_graph <- function(x) {
#x <- mentions_network_dynamic
  # Network Level Stats
  dynamicBetweenness <- tSnaStats(
    x,
    snafun = "centralization",
    start = min(as.numeric(as.POSIXct(round.POSIXt(anytime(as.data.frame(x)$onset, tz = "America/New_York"), unit = "days"), tz = "America/New_York"))),
    end = max(as.data.frame(x)$onset),
    time.interval = interval_val,
    aggregate.dur = interval_val,
    FUN = "betweenness")
  
  dynamicDegree <- tSnaStats(
    x,
    snafun = "centralization",
    start = min(as.numeric(as.POSIXct(round.POSIXt(anytime(as.data.frame(x)$onset, tz = "America/New_York"), unit = "days"), tz = "America/New_York"))),
    end = max(as.data.frame(x)$onset),
    time.interval = interval_val,
    aggregate.dur = interval_val,
    FUN = "degree")

  dynamicEvCent <- tSnaStats(
    x,
    snafun = "centralization",
    start = min(as.numeric(as.POSIXct(round.POSIXt(anytime(as.data.frame(x)$onset, tz = "America/New_York"), unit = "days"), tz = "America/New_York"))),
    end = max(as.data.frame(x)$onset),
    time.interval = interval_val,
    aggregate.dur = interval_val,
    FUN = "evcent")
  
  centrality_measures <- as.ts(ts.union(dynamicBetweenness, dynamicDegree, dynamicEvCent))
  centrality_measures <- data.frame(Y=as.matrix(centrality_measures), date=time(centrality_measures))
  centrality_measures$date <- as.POSIXct(format(round.POSIXt(anytime(centrality_measures$date, tz = "America/New_York"), unit = "days"), "%Y-%m-%d"))
  colnames(centrality_measures) <- c("Betweenness", "Degree", "Eigenvector", "Date")

  centrality_measures <- centrality_measures %>%
    gather(key = "Metric", value = "Value", -Date)
  
  centrality_measures$Date <- as.Date(centrality_measures$Date)
  
  # Get percent change from previous periods 
  centrality_measures <- centrality_measures%>%
    group_by(Metric) %>%
    arrange(Date) %>%
    mutate(Pctchg = 100 * ((Value - lag(Value))/lag(Value)))
 
  # Label based on intensity of change 
  centrality_measures$Pctchg_cat <- ifelse(abs(centrality_measures$Pctchg) >= 50, "High Change", ifelse(abs(centrality_measures$Pctchg) < 50 & abs(centrality_measures$Pctchg) >= 25, "Medium Change", ifelse(abs(centrality_measures$Pctchg) < 25, "Low Change", NA)))
  
  bet <- ggplot(centrality_measures[centrality_measures$Metric == "Betweenness",c(1,3)], aes(x = Date, y = Value)) +   
    geom_line() +
    scale_x_date(breaks = centrality_measures$Date) +
    theme_classic() + theme(axis.text.x=element_text(angle=60, hjust=1)) + ggtitle("Network Betweeness Centrality Over Time") + xlab("Time Slice Start Date")
  
  deg <- ggplot(centrality_measures[centrality_measures$Metric == "Degree",c(1,3)], aes(x = Date, y = Value)) +   
    geom_line() +
    scale_x_date(breaks = centrality_measures$Date) +
    theme_classic() + theme(axis.text.x=element_text(angle=60, hjust=1)) + ggtitle("Network Degree Centrality Over Time") + xlab("Time Slice Start Date")
  
  eig <- ggplot(centrality_measures[centrality_measures$Metric == "Eigenvector",c(1,3)], aes(x = Date, y = Value)) +   
    geom_line() +
    scale_x_date(breaks = centrality_measures$Date) +
    theme_classic() + theme(axis.text.x=element_text(angle=60, hjust=1)) + ggtitle("Network Eigenvector Centrality Over Time") + xlab("Time Slice Start Date")
  
  full_net <- grid.arrange(bet, deg, eig, ncol = 2, nrow = 2, layout_matrix = rbind(c(2,2), c(1,3)))
  
  ggsave(full_net, file=paste0(output_folder, 'network_level_stats/', sprintf('network_stats_time_%s.png', as.character(interaction_level))), height = 9, width = 11, units = "in")
  write.csv(centrality_measures, file=paste0(output_folder, 'network_level_stats/', sprintf('network_stats_time_%s.csv', as.character(interaction_level))))
}

  ####### Degree Level Stats
# 2020-05-13 - 2020-05-20
# 2020-05-20 - 2020-05-27
# 2020-05-27 - 2020-06-03
# 2020-06-10 - 2020-06-15

degree_level_stats <- function(x) {
  q <- 1:(floor(((time_end-time_start)/interval_val))+1)
  listofdfs <- list()
  for (i in 1:length(q)) {
    if(i == 1){
      network_slice <- network.collapse(x, onset = time_start, terminus = (time_start + (interval_val)), rule = "any")
      network_slice_title <- paste(anytime(time_start, tz = "America/New_York"), anytime((time_start + (interval_val)), tz = "America/New_York"), sep = " - ") }else if(i > 1 & i < length(q)){
        network_slice <- network.collapse(x, onset = (time_start + ((i-1)* interval_val)), terminus = (time_start + ((i)* interval_val)), rule = "any")
        network_slice_title <- paste(anytime((time_start + ((i-1)* interval_val)), tz = "America/New_York"), anytime((time_start + ((i)* interval_val)), tz = "America/New_York"), sep = " - ")}else {
        network_slice <- network.collapse(x, onset = (time_start + ((i-1)* interval_val)), terminus = time_end, rule = "any")
        network_slice_title <- paste(anytime((time_start + ((i-1)* interval_val)), tz = "America/New_York"), round.POSIXt(anytime(time_end, tz = "America/New_York"), unit = "day"), sep = " - ") 
      }
  centrality_nodes <- tibble(
      Author = as.character(network_slice%v%'label'),
      Degree = sna::degree(network_slice),
      Betweenness = sna::betweenness(network_slice),
      Eigenvector = sna::evcent(network_slice),
      Davids = pull(getDS(network_slice)[[1]], David_Score))
  centrality_nodes <- as.data.frame(centrality_nodes)
  centrality_nodes$time_slice <- network_slice_title
  listofdfs[[i]] <- centrality_nodes
  }

  df <- do.call("rbind", listofdfs)
  df$Eigenvector <- round(df$Eigenvector, 4)
  unique(df$time_slice)
  df_wide <- df %>%
    gather(Metric, Value, Degree:Davids)
  df_wide <- spread(df_wide, time_slice, Value)
  
  df_long <- df_wide %>% 
    gather(Date, Value, 3:ncol(df_wide))
  
  # Get percent change from previous periods 
  df_long  <- df_long %>%
    group_by(Author, Metric) %>%
    arrange(Date) %>%
    mutate(Pctchg = 100 * ((Value - lag(Value))/lag(Value)))
  
  get.quantile <- function(x)names(q)[which(abs(q-x)==min(abs(q-x)))]
  
  # Betweenness: NaN - Not on graph now or previously (0-0/0), Inf - Not on graph previously (218-0/0), -100: Not on graph now but on previously
  # Degree: NaN - Not on graph now or previously (0-0/0), Inf - Not on graph previously (218-0/0), -100: Not on graph now but on previously
  # Eigenvector: NaN - Not on graph now or previously (0-0/0), Inf - Not on graph previously (218-0/0), -100: Not on graph now but on previously
  # David's Score: NA - Not on graph now or not on graph previously 
  
  # Label based on intensity of change using quartiles 
  
  df_long_quants <- df_long[is.finite(df_long$Pctchg) & !is.na(df_long$Pctchg) & df_long$Pctchg != -100, c("Metric", "Pctchg")] %>%
    group_by(Metric) %>% 
    group_modify(~ {
      quantile(abs(.x$Pctchg), probs = c(0.25, 0.5, 0.75)) %>%
        tibble::enframe(name = "prob", value = "quantile")
    })
  
  # Create Labeling Scheme 
  
df_long$Pctchg_cat <- ifelse(df_long$Metric == "Degree" & is.finite(df_long$Pctchg) & !is.na(df_long$Pctchg) & df_long$Pctchg != -100 & abs(df_long$Pctchg) < as.double(df_long_quants[df_long_quants$Metric == "Degree" & df_long_quants$prob == "25%", "quantile"]), "Low Change", ifelse(df_long$Metric == "Betweenness" & is.finite(df_long$Pctchg) & !is.na(df_long$Pctchg) & df_long$Pctchg != -100 & abs(df_long$Pctchg) < as.double(df_long_quants[df_long_quants$Metric == "Betweenness" & df_long_quants$prob == "25%", "quantile"]), "Low Change", ifelse(df_long$Metric == "Eigenvector" & is.finite(df_long$Pctchg) & !is.na(df_long$Pctchg) & df_long$Pctchg != -100 & abs(df_long$Pctchg) < as.double(df_long_quants[df_long_quants$Metric == "Eigenvector" & df_long_quants$prob == "25%", "quantile"]), "Low Change", ifelse(df_long$Metric == "Davids" & is.finite(df_long$Pctchg) & !is.na(df_long$Pctchg) & df_long$Pctchg != -100 & abs(df_long$Pctchg) < as.double(df_long_quants[df_long_quants$Metric == "Davids" & df_long_quants$prob == "25%", "quantile"]), "Low Change", ifelse(df_long$Metric == "Degree" & is.finite(df_long$Pctchg) & !is.na(df_long$Pctchg) & df_long$Pctchg != -100 & abs(df_long$Pctchg) >=as.double(df_long_quants[df_long_quants$Metric == "Degree" & df_long_quants$prob == "25%", "quantile"]) & abs(df_long$Pctchg) <= as.double(df_long_quants[df_long_quants$Metric == "Degree" & df_long_quants$prob == "75%", "quantile"]), "Medium Change", ifelse(df_long$Metric == "Betweenness" & is.finite(df_long$Pctchg) & !is.na(df_long$Pctchg) & df_long$Pctchg != -100 & abs(df_long$Pctchg) >=as.double(df_long_quants[df_long_quants$Metric == "Betweenness" & df_long_quants$prob == "25%", "quantile"]) & abs(df_long$Pctchg) <= as.double(df_long_quants[df_long_quants$Metric == "Betweenness" & df_long_quants$prob == "75%", "quantile"]), "Medium Change", ifelse(df_long$Metric == "Eigenvector" & is.finite(df_long$Pctchg) & !is.na(df_long$Pctchg) & df_long$Pctchg != -100 & abs(df_long$Pctchg) >=as.double(df_long_quants[df_long_quants$Metric == "Eigenvector" & df_long_quants$prob == "25%", "quantile"]) & abs(df_long$Pctchg) <= as.double(df_long_quants[df_long_quants$Metric == "Eigenvector" & df_long_quants$prob == "75%", "quantile"]), "Medium Change", ifelse(df_long$Metric == "Davids" & is.finite(df_long$Pctchg) & !is.na(df_long$Pctchg) & df_long$Pctchg != -100 & abs(df_long$Pctchg) >=as.double(df_long_quants[df_long_quants$Metric == "Davids" & df_long_quants$prob == "25%", "quantile"]) & abs(df_long$Pctchg) <= as.double(df_long_quants[df_long_quants$Metric == "Davids" & df_long_quants$prob == "75%", "quantile"]), "Medium Change", ifelse(df_long$Metric == "Degree" & is.finite(df_long$Pctchg) & !is.na(df_long$Pctchg) & df_long$Pctchg != -100 & abs(df_long$Pctchg) > as.double(df_long_quants[df_long_quants$Metric == "Degree" & df_long_quants$prob == "75%", "quantile"]), "High Change", ifelse(df_long$Metric == "Betweenness" & is.finite(df_long$Pctchg) & !is.na(df_long$Pctchg) & df_long$Pctchg != -100 & abs(df_long$Pctchg) >as.double(df_long_quants[df_long_quants$Metric == "Betweenness" & df_long_quants$prob == "75%", "quantile"]), "High Change", ifelse(df_long$Metric == "Eigenvector" & is.finite(df_long$Pctchg) & !is.na(df_long$Pctchg) & df_long$Pctchg != -100 & abs(df_long$Pctchg) >as.double(df_long_quants[df_long_quants$Metric == "Eigenvector" & df_long_quants$prob == "75%", "quantile"]), "High Change", ifelse(df_long$Metric == "Davids" & is.finite(df_long$Pctchg) & !is.na(df_long$Pctchg) & df_long$Pctchg != -100 & abs(df_long$Pctchg) > as.double(df_long_quants[df_long_quants$Metric == "Davids" & df_long_quants$prob == "75%", "quantile"]), "High Change", NA)))))))))))) 
                                                                                                                                                    df_long$Pctchg_cat <- ifelse(is.infinite(df_long$Pctchg), "Not in network in previous time slice", ifelse((df_long$Metric == "Degree" | df_long$Metric == "Betweenness" | df_long$Metric == "Eigenvector") & is.nan(df_long$Pctchg), "Not in network in current or previous time slice", ifelse(df_long$Date == min(df_long$Date), "No previous time slice observed", ifelse(df_long$Pctchg == -100, "Not in network currently but in network in previous time slice", ifelse(df_long$Metric == "Davids" & df_long$Date > min(df_long$Date) & is.na(df_long$Pctchg), "Not in network currently or not in network in previous time slice", ifelse(df_long$Metric == "Davids" & df_long$Date > min(df_long$Date) & is.na(df_long$Pctchg), "Not in network currently or not in network in previous time slice", df_long$Pctchg_cat))))))    
                  
  # Convert to Wide Format to show values, percent change, and intensity per period                                                                                                                             
 df_wide_full_vals <- df_long[,1:4] %>% spread(Date, Value)
 colnames(df_wide_full_vals)[3:ncol(df_wide_full_vals)] <- paste("Vals", colnames(df_wide_full_vals)[3:ncol(df_wide_full_vals)], sep = "_")
 df_wide_full_pct <- df_long[,c(1:3, 5)] %>% spread(Date, Pctchg)  
 colnames( df_wide_full_pct)[3:ncol(df_wide_full_vals)] <- paste("PctChg", colnames( df_wide_full_pct)[3:ncol(df_wide_full_pct)], sep = "_")
 df_wide_full_cats <- df_long[,c(1:3,6)] %>% spread(Date, Pctchg_cat)   
 colnames( df_wide_full_cats)[3:ncol(df_wide_full_cats)] <- paste("PctChgCat", colnames(df_wide_full_cats)[3:ncol(df_wide_full_cats)], sep = "_")
 
 for_merge <- df_long[!duplicated(df_long[c("Author", "Metric")]),1:2]
 
 df_wide_full <- for_merge %>% 
   left_join( df_wide_full_vals, by = c("Author", "Metric")) 
 
 df_wide_full <-  df_wide_full %>%
   left_join( df_wide_full_pct, by = c("Author", "Metric")) 
 
 df_wide_full <-  df_wide_full %>%
   left_join( df_wide_full_cats, by = c("Author", "Metric")) 
                                                                                                                                                    
  write.csv(df_wide_full, file=paste0(output_folder, 'node_level_stats/', sprintf('node_stats_time_byauthor_%s.csv', as.character(interaction_level))))
  
  full_df <- df %>%
    group_by(time_slice) %>%
    summarise(Betweenness = mean(Betweenness),
              Degree = mean(Degree),
              Eigenvector = mean(Eigenvector),
              Davids = mean(Davids, na.rm = TRUE))
  
  write.csv(full_df, file=paste0(output_folder, 'node_level_stats/', sprintf('node_stats_time_avg_%s.csv', as.character(interaction_level))))
  
  full_df <- full_df %>%
    separate(time_slice, c("start", "end"), " - ")
  
  full_df$start <- as.Date(full_df$start)
  full_df$end <- as.Date(full_df$end)
  
  full_df_long <- full_df[,c(1,3:6)] %>%
    gather(key = "Metric", value = "Value", -start)

  bet <- ggplot(full_df_long[full_df_long$Metric == "Betweenness", c(1,3)], aes(x = start, y = Value)) +   
    geom_line() +
    scale_x_date(breaks = full_df_long$start) +
    theme_classic() + theme(axis.text.x=element_text(angle=60, hjust=1)) + ggtitle("Average Node Betweenness Centrality Over Time") + xlab("Time Slice Start Date")
  
 deg <-  ggplot(full_df_long[full_df_long$Metric == "Degree", c(1,3)], aes(x = start, y = Value)) +   
    geom_line() +
    scale_x_date(breaks = full_df_long$start) +
    theme_classic() + theme(axis.text.x=element_text(angle=60, hjust=1)) + ggtitle("Average Node Degree Centrality Over Time") + xlab("Time Slice Start Date")
  
  eig <- ggplot(full_df_long[full_df_long$Metric == "Eigenvector", c(1,3)], aes(x = start, y = Value)) +   
    geom_line() +
    scale_x_date(breaks = full_df_long$start) +
    theme_classic() + theme(axis.text.x=element_text(angle=60, hjust=1)) + ggtitle("Average Node Eigenvector Centrality Over Time") + xlab("Time Slice Start Date")
  
  full_net <- grid.arrange(bet, deg, eig, ncol = 2, nrow=2, layout_matrix = rbind(c(2,2), c(1,3)))
  
  ggsave(full_net, file = paste0(output_folder, 'node_level_stats/', sprintf('node_stats_time_%s.png', as.character(interaction_level))), height = 9, width = 11, units = "in")
  
  # Convert to Wide Form with Metrics to create filtering for individual account node statistics generation
  df_wide_metrics_vals <- df_long[,c(1:3,5)] %>% spread(Metric, Pctchg)
  
  bet_list <- df_wide_metrics_vals[is.finite(df_wide_metrics_vals$Betweenness),c("Author", "Betweenness")] %>% 
    arrange(desc(abs(Betweenness))) 
  bet_list <- unique(bet_list[1:10,1])[[1]]
  
  deg_list <- df_wide_metrics_vals[is.finite(df_wide_metrics_vals$Degree),c("Author", "Degree")] %>% 
    arrange(desc(abs(Degree))) 
  deg_list <- unique(deg_list[1:10,1])[[1]]
  
  eig_list <- df_wide_metrics_vals[is.finite(df_wide_metrics_vals$Eigenvector),c("Author", "Eigenvector")] %>% 
    arrange(desc(abs(Eigenvector))) 
  eig_list <- unique(eig_list[1:10,1])[[1]]
  
  david_list <- df_wide_metrics_vals[is.finite(df_wide_metrics_vals$Eigenvector),c("Author", "Davids")] %>% 
    arrange(desc(abs(Davids))) 
  david_list <- unique(david_list[1:10,1])[[1]]
  
  # Fix Dates for Plotting 
  df_long <- df_long %>%
    separate(Date, c("start", "end"), " - ")
  
  df_long$start <- as.Date(df_long$start)
  df_long$end <- as.Date(df_long$end)
  
  # Plot individual accounts 
  
  deg_list <- c("@sagaclan1", "@turmpd", "@yang32024699")
  
  for (i in bet_list) {
    # Betweeness 
    df_long_a <- df_long[df_long$Author == i & df_long$Metric == "Betweenness", c("start", "Value")]
    
    bet <- ggplot(df_long_a, aes(x = start, y = Value)) +   
      geom_line() +
      scale_x_date(breaks = full_df_long$start) +
      theme_classic() + theme(axis.text.x=element_text(angle=60, hjust=1)) + ggtitle(paste0("Node Betweenness Centrality for ", i, " Over Time")) + xlab("Time Slice Start Date")
    
    ggsave(paste0(output_folder, 'node_level_stats/high_betweenness_accounts/', gsub("@", "", i), sprintf('_betweeness_over_time_%s.png', as.character(interaction_level))))
    
  }
  
  # Degree
  for (i in deg_list) {
    # Betweeness 
    df_long_a <- df_long[df_long$Author == i & df_long$Metric == "Degree", c("start", "Value")]
    
    bet <- ggplot(df_long_a, aes(x = start, y = Value)) +   
      geom_line() +
      scale_x_date(breaks = df_long_a$start) +
      theme_classic() + theme(axis.text.x=element_text(angle=60, hjust=1)) + ggtitle(paste0("Number of Mentions or Mentioned for ", i, " Over Time")) + xlab("Time Slice Start Date")
    
    ggsave(paste0(output_folder, 'node_level_stats/high_degree_accounts/', gsub("@", "", i), sprintf('_degree_over_time_%s.png', as.character(interaction_level))))
    
  }
  
  # Eigenvector 
  for (i in eig_list) {
    # Betweeness 
    df_long_a <- df_long[df_long$Author == i & df_long$Metric == "Eigenvector", c("start", "Value")]
    
    bet <- ggplot(df_long_a, aes(x = start, y = Value)) +   
      geom_line() +
      scale_x_date(breaks = df_long_a$start) +
      theme_classic() + theme(axis.text.x=element_text(angle=60, hjust=1)) + ggtitle(paste0("Node Eigenvector Centrality for ", i, " Over Time")) + xlab("Time Slice Start Date")
    
    ggsave(paste0(output_folder, 'node_level_stats/high_eigenvector_accounts/', gsub("@", "", i), sprintf('_eignenvector_over_time_%s.png', as.character(interaction_level))))
    
  }
  
  # Davids 
  for (i in david_list) {
    # Betweeness 
    df_long_a <- df_long[df_long$Author == i & df_long$Metric == "Davids", c("start", "Value")]
    
    bet <- ggplot(df_long_a, aes(x = start, y = Value)) +   
      geom_line() +
      scale_x_date(breaks = df_long_a$start) +
      theme_classic() + theme(axis.text.x=element_text(angle=60, hjust=1)) + ggtitle(paste0("Davids's List for ", i, " Over Time")) + xlab("Time Slice Start Date")
    
    ggsave(paste0(output_folder, 'node_level_stats/high_david_accounts/', gsub("@", "", i), sprintf('_david_over_time_%s.png', as.character(interaction_level))))
  }
    }

static = TRUE
network_metrics_graph(mentions_network_dynamic)
degree_level_stats(mentions_network_dynamic)
static = FALSE


#plot(tErgmStats(x, start = time_start, end = time_end, time.interval = 10000, aggregate.dur =  interval_val, 'meandeg'), main='Mean degree of cls33, 1 minute aggregation')

# Makes a big difference! Figure this time.interval vs aggregate.dur out!

# Note: Really useful when we get to TERGMS, TWINT, or modeling account info. 

# This will produce a graph of the rolling aggregated centralization of the network

########### 8. Dynamic Network Metrics - Only Temporal Metrics

# In a temporal network, because nodes and edges are popping in and out of existence all the time, it can be useful to know not only how many nodes can be reached from a given node at any specific time, but also how many nodes were or will be connected to a given node over the course of the network’s existence

# These past and future groups are known as backward reachable sets and forward reachable sets, respectively.

# The size of these sets adds important nuance to measurements of centrality – depending on whether an account came to occupy a central position in      the network near the beginning or end of the period we’re observing, the actual impact it could have had on the larger community is completely           different.

# To calculate the size of the forward reachable sets of each node, we can use the tReach() function on our entire network.
# This function defaults to calculating the size of the forward reachable set of a given node, but to calculate the backward reachable set we            simply specify direction = "bkwd" instead.

###################################################### Static Graph Outputs #########################################################################

# Calculate and store the sizes of forward and backward reachable sets for each node

# A temporal path in a dynamic network is a sequence of vertices and edges such that the onset times of successive elements are greater than or equal than those of the previous. In other words, the path is a directed traversal of the network that respects the constraints of edge activity spells and permits 'waiting' at intermediate vertices for 'future' edges to form.

# When set to use direction='fwd' , type='earliest.arrive' tPath performs a time-minimizing Dijkstra's style Depth First Search to find the set of vertices reachable on a forward temporal path from the initial seed vertex v while respecting the constraints of edge timing

# When set to direction='bkwd' and type='latest.depart' the path will be found by searching backwards in time from the end point. In other words, it returns the set of vertices that can reach v, along with latest possible departure times from those vertices

# Calculate and store the sizes of forward and backward reachable sets for each node
#fwd_reach <- tReach(mentions_network_dynamic)
#bkwd_reach <- tReach(mentions_network_dynamic, direction = "bkwd")
#plot(fwd_reach, bkwd_reach)

# This produces a graph of the sizes of the forward and backward reachable sets for each account. 

# We can also visualize these sets using the tPath() function to find the path that connects a given node to its forward or backward reachable set, and the plotPaths() function to graph it over a representation of the entire network. 

# In the example below, we’ll choose a single workshop – that of the Hospitaller Master, selected by his vertex id number 3 – and visualize its forward reachable set.

# Calculate and plot the forward reachable paths
# of node number 3 (the Hospitaller Master)

# This produces a graph of the sizes of the forward and backward reachable sets for each workshop or illuminator. 

# Calculate and plot the forward reachable paths
setwd(output_folder)
if(reachable_paths_on == TRUE){
  dir.create("reachable_paths_undirected")
  dir.create("reachable_paths_directed")
}
setwd(input)

reachable_paths <- function(undirected, directed) {
  # Break apart network object into collapsed networks, label with week label, and reconstruct (So that edges can be labeled with week interval instead of Unix Epoch)
  
  q <- 1:(floor(((time_end-time_start)/interval_val))+1)
  listofnets_un <- list()
  listofnets_di <- list()
  listofdates <- list()
  for (i in 1:length(q)) {
    if(i == 1){
      network_slice <- network.collapse(undirected, onset = time_start, terminus = (time_start + (interval_val)), rule = "any")
      network_slice_title <- paste(anytime(time_start, tz = "America/New_York"), anytime((time_start + (interval_val)), tz = "America/New_York"), sep = " - ") }else if(i > 1 & i < length(q)){
        network_slice <- network.collapse(undirected, onset = (time_start + ((i-1)* interval_val)), terminus = (time_start + ((i)* interval_val)), rule = "any")
        network_slice_title <- paste(anytime((time_start + ((i-1)* interval_val)), tz = "America/New_York"), anytime((time_start + ((i)* interval_val)), tz = "America/New_York"), sep = " - ")}else {
          network_slice <- network.collapse(undirected, onset = (time_start + ((i-1)* interval_val)), terminus = time_end, rule = "any")
          network_slice_title <- paste(anytime((time_start + ((i-1)* interval_val)), tz = "America/New_York"), round.POSIXt(anytime(time_end, tz = "America/New_York"), unit = "day"), sep = " - ") 
        }
    listofnets_un[[i]] <- network_slice
    listofdates[[i]] <- cbind(i, network_slice_title)
  }
  # Directed 
  for (i in 1:length(q)) {
    if(i == 1){
      network_slice <- network.collapse(directed, onset = time_start, terminus = (time_start + (interval_val)), rule = "any")
       }else if(i > 1 & i < length(q)){
        network_slice <- network.collapse(directed, onset = (time_start + ((i-1)* interval_val)), terminus = (time_start + ((i)* interval_val)), rule = "any")
        }else {
          network_slice <- network.collapse(directed, onset = (time_start + ((i-1)* interval_val)), terminus = time_end, rule = "any")
        }
    listofnets_di[[i]] <- network_slice
  }
  
  undirected <- harry_potter_support<-networkDynamic(network.list=listofnets_un)
  directed <- harry_potter_support<-networkDynamic(network.list=listofnets_di)
  listofdates_df <- sapply(listofdates, paste, collapse = ": ")

  # Setup coords 
  q = gplot.layout.kamadakawai(directed, NULL)
  directed %v% "lon" = q[, 1]
  directed %v% "lat" = q[, 2]
  coords <- matrix(cbind(directed %v% "lon", directed %v% "lat"), ncol=2) 
  # Set up labels 
  bf <- as.data.frame(cbind(directed %v% c("label"), undirected %v% c("id")))
  bf$V2 <- as.numeric(as.character(bf$V2))
  bf <- bf[bf$V1 %in% reachable_labels,]
  # Manually add in accounts to use from labeling (Have to come back to)
  gft_bw <- c('@bbcchinese', '@sbsnews', '@dw_chinese', '@voachinese', '@joshuawongcf', '@whale90196377', '@freeeveryprov', '@iataw224iancicu', '@gftdduzyhxjzids','@zhangeric16','@fuym4lpkgfm1t34', '@vcmf_china', '@mang047', '@wongtafuk', '@steve10247087', '@josefli2', '@realxavierbin', '@nick_komo')
  gft_fw <- c('@whale90196377', '@iataw224iancicu', '@gftdduzyhxjzids','@zhangeric16','@fuym4lpkgfm1t34', '@mang047', '@wongtafuk', '@steve10247087', '@josefli2', '@nick_komo', '@supernan123', '@zjhzxs')
  whl_bw <- c('@bbcchinese', '@sbsnews', '@dw_chinese', '@voachinese', '@joshuawongcf', '@whale90196377', '@freeeveryprov', '@iataw224iancicu', '@gftdduzyhxjzids','@zhangeric16','@fuym4lpkgfm1t34', '@vcmf_china', '@mang047', '@wongtafuk', '@josefli2', '@realxavierbin', '@nick_komo')
  whl_fw <- c('@whale90196377', '@iataw224iancicu', '@gftdduzyhxjzids','@zhangeric16','@fuym4lpkgfm1t34', '@mang047', '@steve10247087', '@josefli2', '@supernan123')
  bf$bkw <- c(I(list(gft_bw)), I(list(whl_bw)))
  bf$fwd <- c(I(list(gft_fw)), I(list(whl_fw)))

  for (i in bf[,"V2"]) {
    account_name <- as.character(bf[bf$V2 == i,][,1])
    
    # Forward Reach in Directed
    fwd_reach_dir <- tPath(
      directed,
      v = i,
      direction = "fwd", 
      type='earliest.arrive')
    
    # Cannot plot individual path if it does not exist, so we use if else statement here. 
    
    jpeg(filename = paste0(output_folder, 'reachable_paths_directed/', account_name, sprintf("_all_reachable_dir_%s.png", as.character(interaction_level))), width = 1000, height = 500)
    
    layout.matrix <- rbind(c(1,2),c(3,3))
    layout(layout.matrix, heights=c(8,2))  # put legend on bottom 1/8th of the chart
    layout.show(3)
    
    if((length(fwd_reach_dir$tdist[between(fwd_reach_dir$tdist, 0, Inf, incbounds = FALSE) == TRUE]) >0) == TRUE) {
      
      directed %v% 'fwd_label' <- ifelse(directed %v% 'label' %in% unlist(bf[bf$V2 == i,][,4]), directed %v% 'label', NA)
      
      par(mar=c(0, 0, 1, 1))
      plot(fwd_reach_dir, main=paste0("Chains from accounts that mentioned ", account_name, " over time"), label = directed %v% 'fwd_label', edge.lwd = 1, edge.label.cex = .8, jitter = TRUE, edge.label.col = "black", label.cex = 1, mode = 'fruchtermanreingold')

    }else{
      par(par(mar = c(0, 0, 0, 0)))
      
      plotPaths(
        directed,
        coord = coords,
        fwd_reach_dir,
        displaylabels = FALSE,
        vertex.col = "white",
        jitter = TRUE,
        edge.label.cex = 0.7,
        label.cex = 1,
        main = paste0(account_name, " did not mention other accounts")
      )
    }
    
    # Backward Reach in Directed
    bkw_reach_dir <- tPath(
      directed,
      v = i,
      direction = "bkwd",
      type = 'latest.depart')
    
    if((length(fwd_reach_dir$tdist[between(bkw_reach_dir$tdist, 0, Inf, incbounds = FALSE) == TRUE]) >0) == TRUE) {
      
      directed %v% 'bwd_label' <- ifelse(directed %v% 'label' %in% unlist(bf[bf$V2 == i,][,3]), directed %v% 'label', NA)
      
      par(mar=c(0, 0, 1, 1))
      
    plot(bkw_reach_dir, main=paste0("Chains from accounts that were mentioned by ", account_name, " over time"), edge.col = rgb(0, 97, 255, max=255, alpha=166), label = directed %v% 'bwd_label', edge.lwd = 1, edge.label.cex = 0.8, jitter = TRUE, edge.label.col = "black", label.cex = 1, mode= "fruchtermanreingold")

    }else{
      
      par(par(mar = c(0, 0, 0, 0)))
      
      plotPaths(
        directed,
        coord = coords,
        bkw_reach_dir,
        path.col = rgb(0, 97, 255, max=255, alpha=166),
        displaylabels = FALSE,
        edge.label.col = rgb(0,0,0,0),
        vertex.col = "white",
        jitter = TRUE,
        edge.label.cex = 0.7,
        label.cex = 1,
        main = paste0(account_name, " was not mentioned by other accunts")
      )
    }
    
    # setup for no margins on the legend
    par(mar=c(0, 0, 0, 0))
    plot.new()
    legend("center",title="Time Groupings", legend=listofdates_df, bty ="n", xpd = TRUE, inset = c(0,0), pt.cex = 1, horiz = TRUE)
    #, text.font = 2,
    
    dev.off()
  }  
}

if(reachable_paths_on == TRUE){
  reachable_paths(mentions_network_dynamic, mentions_network_dynamic_directed)
}

  ######## Time Slicing Visual 

#fwd_reach_undir$tdist <- format(round.POSIXt(anytime(fwd_reach_undir$tdist, tz = "America/New_York"), unit = "days"), "%Y-%m-%d")
# Come back and make for sure that network.collapse is the one to use here and not network.extract

setwd(output_folder)
dir.create("full_unlabled")
dir.create("orig_pulled")
dir.create("other_pulled")
dir.create("new_entrants")
if(custom_graph==TRUE){
  dir.create("custom_graph")
}
setwd(input)

########## Network Split Viz

network_splits_viz <- function(x) {
  # Set up coordinates to make repeatable across visuals in order to better detect changes
  q = gplot.layout.kamadakawai(x, NULL)
  x %v% "lon" = q[, 1]
  x %v% "lat" = q[, 2]
  
  coords <- matrix(cbind(x %v% "lon", x %v% "lat"), ncol=2) 
  
  q <- q <- 1:(floor(((time_end-time_start)/interval_val))+1)
  for (i in 1:length(q)) {
    if(i == 1){
      network_slice <- network.collapse(x, onset = time_start, terminus = (time_start + (interval_val)), rule = "any")
      network_slice_title <- paste(anytime(time_start, tz = "America/New_York"), anytime((time_start + (interval_val)), tz = "America/New_York"), sep = " - ") 
      }else if(i > 1 & i < length(q)){
        network_slice <- network.collapse(x, onset = (time_start + ((i-1)* interval_val)), terminus = (time_start + ((i)* interval_val)), rule = "any")
        network_slice_title <- paste(anytime((time_start + ((i-1)* interval_val)), tz = "America/New_York"), anytime((time_start + ((i)* interval_val)), tz = "America/New_York"), sep = " - ") 
        
        }else {
          network_slice <- network.collapse(x, onset = (time_start + ((i-1)* interval_val)), terminus = time_end, rule = "any")
          network_slice_title <- paste(anytime((time_start + ((i-1)* interval_val)), tz = "America/New_York"), round.POSIXt(anytime(time_end, tz = "America/New_York"), unit = "day"), sep = " - ") 
        }
  # plot with top 10 degree labeled
   
    # Eliminate Isolates 
    delete.vertices(network_slice, which(degree(network_slice) < 2))
    
    # Update coordinates for each slice  (subset out vertices not in a slice)
    coords_df <- as.data.frame(coords)
    network_cols <- as.data.frame(cbind(as.vector(network_slice%v%'lon'), as.vector(network_slice%v%'lat')))
    #coords_df <- coords_df[(coords_df$V1 %in% as.vector(network_slice%v%'lon')) & (coords_df$V2 %in% as.vector(network_slice%v%'lat')),]
    coords_df <- right_join(coords_df, network_cols, by = c("V1", "V2"))
    coords <- as.matrix(coords_df)
    
  ggplot(ggnetwork(network_slice, layout = coords), aes(x = x, y = y, xend = xend, yend = yend)) + 
    geom_edges(color = "grey50", alpha = 0.2) +
    geom_nodes(color = "#12ABB6", size = 7, alpha = 0.8) +
    scale_size_area(max_size = 8, guide = F)+
    #scale_color_manual(values = gec_pal)+
    geom_nodelabel_repel(aes(label = label),
                         box.padding = unit(1, "lines"),
                         size = 6,
                         data = function(x) { x[ x$label%in% label_names, ]})+
    theme_blank()+
    theme(legend.text=element_text(size=12)) + ggtitle(network_slice_title)
  ggsave(paste0(output_folder, 'full_unlabled/', 'time_slice_', i, sprintf('_full_unlabeled_%s.png', as.character(interaction_level))), height = 9, width = 11, units = "in")
  #legend.position = "top",
  #legend.title = element_blank()) +
  #ggtitle(sprintf("Most central Agents in a %s-interaction network", as.character(interactions)))
  #theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5))

  # custom labels -----------------------------------------------------------
  if (pulled_list_of_accs > 0 ){
    ## if pulled_list_of_accs is true, this will create a labeled graph of
    ## the authors originally pulled (that are in the network at your
    ## chosen threshhold). df needs to be a csv with account names pulled in the first
    ## column (@ signs included)!
    
    ## Accounts names you pulled need to be in the first column
    df <- read_csv(path_to_acc_list, col_names = FALSE)
    colnames(df)[1] <- "Account"
    df$Account <- tolower(df$Account)
    df$Account <- ifelse(grepl('^@', df$Account), df$Account, paste0('@', df$Account))
    df <- df %>%  mutate(names = tolower(Account),
                         category = orig_accounts) %>%
      select(names, Account, category)
    
    # vertex names
    newdf <- tibble(
      names = tolower(network_slice%v%'label'))
    ndf2 <- newdf %>% left_join(df) %>% na.omit()
    ndf2$newor <- ifelse(is.na(ndf2$Account), other_accounts, orig_accounts)
    network_slice%v%'Orientation' <- ifelse(network_slice%v%'label' %in% ndf2$names, ndf2$newor, other_accounts)
    
    ggplot(ggnetwork(network_slice, layout = "kamadakawai"), aes(x = x, y = y, xend = xend, yend = yend)) +
      geom_edges(color = "black", alpha = 0.6) +
      geom_nodes(aes(color = Orientation), size = 7, alpha = 1) +
      scale_size_area(max_size = 8, guide = F)+
      scale_color_manual(values = c("#981B1E","#969696"))+
      geom_nodelabel_repel(aes(label = label),
                           box.padding = unit(1, "lines"),
                           size = 6,
                           data = function(x) { x[ x$label %in% label_names, ]})+
      theme_blank()+
      theme(legend.text=element_text(size=12))  + ggtitle(network_slice_title)
    
    # output folder is # of interactions
    ggsave(paste0(output_folder, 'orig_pulled/',  'time_slice_', i, sprintf('orig_pulled_%s.png', as.character(interaction_level))), height = 9, width = 11, units = "in", dpi = 100)
  }
  ## Visualizes the top 10 accounts that were not originally pulled
  
  if (pulled_list_of_accs > 0 ){
    centrality <- getDS(x)
    cent <- centrality[[1]]
    # Top 10 degree labels- not originally pulled
    labels <- newdf %>% left_join(cent) %>% left_join(df) %>% filter(is.na(Account)) %>% 
      arrange(desc(degree)) %>% slice(1:10,) %>% .$names
    ndf_2 <- newdf %>% left_join(df)
    ndf_2$newor <- ifelse(is.na(ndf_2$Account), other_accounts, orig_accounts)
    baddies <- ndf_2 %>% filter(newor == other_accounts) %>% .$names
    
    ndf_3 <- ndf_2 %>% filter(ndf_2$newor == other_accounts)
    network_slice%v%'Orientation' <- ifelse(network_slice%v%'label' %in% ndf_2$names, ndf_2$newor, other_accounts)
    
    ggplot(ggnetwork(network_slice, layout = coords), aes(x = x, y = y, xend = xend, yend = yend)) +
      geom_edges(color = "black", alpha = 0.6) +
      geom_nodes(aes(color = Orientation), size = 7, alpha = 1) +
      scale_size_area(max_size = 8, guide = F)+
      scale_color_manual(values = c("#981B1E","#969696"))+
      geom_nodelabel_repel(aes(label = label),
                           box.padding = unit(1, "lines"),
                           size = 6,
                           data = function(x) { x[ x$label %in% labels, ]})+
      theme_blank()+
      theme(legend.text=element_text(size=12))  + ggtitle(network_slice_title)
    ggsave(paste0(output_folder, 'other_pulled/',  'time_slice_', i, sprintf('other_pulled_%s.png', as.character(interaction_level))), height = 9, width = 11, units = "in",
           dpi = 100)
  }

  if (custom_graph == TRUE){
    df <- read_csv(path_to_nodes_categorized)
    df$suspended <- ifelse(df$suspended == 1, "Eventually Suspended", "Active")
    colnames(df)[1:2] <- c('names', 'ref')
    df$names <- tolower(df$names)
    df$names <- ifelse(grepl('^@', df$names), df$names, paste0('@', df$names))
    # run the following code as is:
    newdf <- tibble(names = x%v%'label')
    labels <- label_names
    ndf2 <- newdf %>% left_join(df)
    ndf2$newor <- ifelse(is.na(ndf2$ref), "Active", ndf2$ref)
    network_slice%v%'Orientation' <- ifelse(network_slice%v%'label' %in% ndf2$names, ndf2$newor, NA)
    
    ggplot(ggnetwork(network_slice, layout = coords), aes(x = x, y = y, xend = xend, yend = yend)) +
      geom_edges(color = "black", alpha = 0.6) +
      geom_nodes(aes(color = Orientation), size = 7, alpha = 1) +
      scale_size_area(max_size = 8, guide = F)+
      scale_color_manual(values = gec_pal)+
      geom_nodelabel_repel(aes(label = label),
                           box.padding = unit(1, "lines"),
                           size = 6,
                           data = function(x) { x[ x$label %in% labels, ]})+
      theme_blank()+
      theme(legend.text=element_text(size=12))  + ggtitle(network_slice_title)
    ggsave(paste0(output_folder, "custom_graph/",  'time_slice_', i, sprintf('final_ntwk_%s.png', as.character(interaction_level))), height = 9, width = 11, units = "in", dpi = 100)
  }
  }
}

network_splits_viz(mentions_network_dynamic)


############

label_names[[11]] <- "@turmpd"

new_entrants_network_splits_viz <- function(x) {
  # Set up coordinates to make repeatable across visuals in order to better detect changes
  q = gplot.layout.kamadakawai(x, NULL)
  x %v% "lon" = q[, 1]
  x %v% "lat" = q[, 2]
  
  coords <- matrix(cbind(x %v% "lon", x %v% "lat"), ncol=2) 
  
  q <- q <- 1:(floor(((time_end-time_start)/interval_val))+1)
  listofgraphs <- list()
  for (i in 1:length(q)) {
    if(i == 1){
      network_slice <- network.collapse(x, onset = time_start, terminus = (time_start + (interval_val)), rule = "any")
      network_slice_title <- paste(anytime(time_start, tz = "America/New_York"), anytime((time_start + (interval_val)), tz = "America/New_York"), sep = " - ") 
    }else if(i > 1 & i < length(q)){
      network_slice <- network.collapse(x, onset = (time_start + ((i-1)* interval_val)), terminus = (time_start + ((i)* interval_val)), rule = "any")
      network_slice_title <- paste(anytime((time_start + ((i-1)* interval_val)), tz = "America/New_York"), anytime((time_start + ((i)* interval_val)), tz = "America/New_York"), sep = " - ") 
      
    }else {
      network_slice <- network.collapse(x, onset = (time_start + ((i-1)* interval_val)), terminus = time_end, rule = "any")
      network_slice_title <- paste(anytime((time_start + ((i-1)* interval_val)), tz = "America/New_York"), round.POSIXt(anytime(time_end, tz = "America/New_York"), unit = "day"), sep = " - ") 
    }
    # plot with top 10 degree labeled
    
    # Eliminate Isolates 
    #delete.vertices(network_slice, which(degree(network_slice) < 2))
    network_slice <- major_component(network_slice) 
    # Update coordinates for each slice  (subset out vertices not in a slice)
    coords_df <- as.data.frame(coords)
    network_cols <- as.data.frame(cbind(as.vector(network_slice%v%'lon'), as.vector(network_slice%v%'lat')))
    #coords_df <- coords_df[(coords_df$V1 %in% as.vector(network_slice%v%'lon')) & (coords_df$V2 %in% as.vector(network_slice%v%'lat')),]
    coords_df <- right_join(coords_df, network_cols, by = c("V1", "V2"))
    coords <- as.matrix(coords_df)
    
    network_slice %v% 'network.status' <- ifelse(network_slice %v% 'active.color' == "grey", "In network previous time slice", ifelse(network_slice %v% 'active.color' == "red", "Not in network previous time slice", NA))
    
    if(i == 1){
      ggplot(ggnetwork(network_slice, layout = coords), aes(x = x, y = y, xend = xend, yend = yend)) + 
        geom_edges(color = "grey50", alpha = 0.6) +
        geom_nodes(color = "#969696", size = 7, alpha = 1) +
        scale_size_area(max_size = 8, guide = F)+
        #scale_color_manual(values = gec_pal)+
        geom_nodelabel_repel(aes(label = label),
                             box.padding = unit(1, "lines"),
                             size = 6,
                             data = function(x) { x[ x$label%in% label_names, ]})+
        theme_blank()+
        theme(legend.text=element_text(size=12), legend.title=element_text(size=12), legend.position = "right")
      
      ggsave(paste0(output_folder, 'new_entrants/', 'time_slice_', i, sprintf('_newentrants_labeled_%s.png', as.character(interaction_level))), height = 9, width = 11, units = "in")
      
      p <- ggplot(ggnetwork(network_slice, layout = coords), aes(x = x, y = y, xend = xend, yend = yend)) + 
        geom_edges(color = "black", alpha = 0.7) +
        geom_nodes(color = "#969696", size = 9, alpha = 1) +
        scale_size_area(max_size = 8, guide = F)+
        #scale_color_manual(values = gec_pal)+
        geom_nodelabel_repel(aes(label = label),
                             box.padding = unit(1, "lines"),
                             size = 17,
                             data = function(x) { x[ x$label%in% label_names, ]})+
        theme_blank()+
        theme(legend.text=element_text(size=50), legend.title=element_text(size=50), plot.title = element_text(size = 50), legend.position = "top") + ggtitle(network_slice_title) + labs(colour="Network Status") +theme(panel.border=element_rect(fill=NA))
      
    }else{
      ggplot(ggnetwork(network_slice, layout = coords), aes(x = x, y = y, xend = xend, yend = yend)) +
        geom_edges(color = "black", alpha = 0.6) +
        geom_nodes(aes(color = network.status), size = 7, alpha = 1) +
        scale_size_area(max_size = 8, guide = F)+
        scale_color_manual(values = c("#969696", "#981B1E"))+
        geom_nodelabel_repel(aes(label = label),
                             box.padding = unit(1, "lines"),
                             size = 6,
                             data = function(x) { x[ x$label %in% label_names, ]})+
        theme_blank()+
        theme(legend.text=element_text(size=12), legend.title=element_text(size=12), legend.position = "right")  + labs(colour="Network Status")
      
      ggsave(paste0(output_folder, 'new_entrants/', 'time_slice_', i, sprintf('_newentrants_labeled_%s.png', as.character(interaction_level))), height = 9, width = 11, units = "in")
      
      p <- ggplot(ggnetwork(network_slice, layout = coords), aes(x = x, y = y, xend = xend, yend = yend)) +
        geom_edges(color = "black", alpha = 0.7) +
        geom_nodes(aes(color = network.status), size = 9, alpha = 1) +
        scale_color_manual(values = c("#969696", "#981B1E"))+
        geom_nodelabel_repel(aes(label = label),
                             box.padding = unit(1, "lines"),
                             size = 17,
                             data = function(x) { x[ x$label %in% label_names, ]})+
        theme_blank() +
        theme(legend.text=element_text(size=50), legend.title=element_text(size=50), plot.title = element_text(size = 50), legend.position = "top") + ggtitle(network_slice_title) + labs(colour="Network Status") +theme(panel.border=element_rect(fill=NA))
      
      legend <- get_legend(p)
      p <- p + theme(legend.position="none")
      
    }
    listofgraphs[[i]] <- p
  }
  listofgraphs[[(length(listofgraphs)+1)]] <- legend
  
  #full_net <- grid.arrange(grobs=listofgraphs, ncol=12, nrow=3, layout_matrix = rbind(c(0,0,1,1,1,1,2,2,2,2,0,0), c(3,3,3,3,4,4,4,4,5,5,5,5), c(6,6,6,6,6,6,6,6,6,6,6,6)), heights = c(3,3,.02))
  
  full_net <- ggarrange(ggarrange(listofgraphs[[1]], listofgraphs[[2]], ncol = 2), legend, heights = c(4,0.5), nrow=2)
  
  ggsave(plot = full_net, paste0(output_folder, 'new_entrants/', 'time_slice_', i, sprintf('two_grid_newentrants_labeled_%s.png', as.character(interaction_level))), height = 20, width = 45, units = "in")
}

new_entrants_network_splits_viz(mentions_network_dynamic_nodes)




######## Zoom in on reply threads code ###########

new_zoom_viz <- function(x) {
  # Set up coordinates to make repeatable across visuals in order to better detect changes
  q = gplot.layout.kamadakawai(x, NULL)
  x %v% "lon" = q[, 1]
  x %v% "lat" = q[, 2]

  i = 4
  reply_nodes = c("@whale90196377", "@iataw224iancicu", "@fuym4lpkgfm1t34", "@us93492643", "@vcmf_china", "@josefli2", "@theo44757406", "@mang047", "@h36g3z2h", "@wsl691", "@sixozombah8om2o", "@supernan123")
  label_names = c("@whale90196377", "@iataw224iancicu", "@fuym4lpkgfm1t34", "@josefli2", "@mang047", "@h36g3z2h", "@wsl691", "@supernan123")
  
  coords <- matrix(cbind(x %v% "lon", x %v% "lat"), ncol=2) 
  network_slice <- network.collapse(x, onset = (time_start + ((i-1)* interval_val)), terminus = (time_start + ((i)* interval_val)), rule = "any")
  network_slice_title <- paste(anytime((time_start + ((i-1)* interval_val)), tz = "America/New_York"), anytime((time_start + ((i)* interval_val)), tz = "America/New_York"), sep = " - ") 
  
  # Nodes to retain 
  bf <- as.data.frame(cbind(network_slice %v% c("label"), network_slice %v% c("id")))
  bf$V2 <- as.numeric(as.character(bf$V2))
  bf <- bf[bf$V1 %in% reply_nodes ,]

  network_slice <- get.inducedSubgraph(network_slice, v=which(network_slice %v% 'id' %in% bf$V2))
   
  # Update coordinates for each slice  (subset out vertices not in a slice)
  coords_df <- as.data.frame(coords)
  network_cols <- as.data.frame(cbind(as.vector(network_slice%v%'lon'), as.vector(network_slice%v%'lat')))
  #coords_df <- coords_df[(coords_df$V1 %in% as.vector(network_slice%v%'lon')) & (coords_df$V2 %in% as.vector(network_slice%v%'lat')),]
  coords_df <- right_join(coords_df, network_cols, by = c("V1", "V2"))
  coords <- as.matrix(coords_df)
    
    network_slice %v% 'network.status' <- ifelse(network_slice %v% 'active.color' == "grey", "In network previous time slice", ifelse(network_slice %v% 'active.color' == "red", "Not in network previous time slice", NA))
    ggplot(ggnetwork(network_slice, layout = coords), aes(x = x, y = y, xend = xend, yend = yend)) +
        geom_edges(color = "black", alpha = 0.6) +
        geom_nodes(aes(color = network.status), size = 7, alpha = 1) +
        scale_size_area(max_size = 8, guide = F)+
        scale_color_manual(values = c("#969696", "#981B1E"))+
        geom_nodelabel_repel(aes(label = label),
                             box.padding = unit(1, "lines"),
                             size = 6,
                             data = function(x) { x[ x$label %in% label_names, ]})+
        theme_blank()+
        theme(legend.text=element_text(size=12))  + ggtitle(network_slice_title) + labs(colour="Network Status")
    
    ggsave(paste0(output_folder, 'new_entrants/', 'time_slice_', i, sprintf('_zoomed_replythread_%s.png', as.character(interaction_level))), height = 9, width = 11, units = "in")
}

new_zoom_viz(mentions_network_dynamic_nodes)


