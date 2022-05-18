  ########################################################## MisDis TSNA and TERGM Projection ################################################################
  # Last Updated: 09/28/21
  
  #################################################################Description###############################################################################
  # Prepare data for conversion into a networkDynamic object for Temporal Social Network Analysis (TSNA) and quantify and visualize the temporal network using the NDTV package in R. It will also enable you to quantify and visualize network-level and node-level metrics describing the temporal network using the TSNA package in R.
  
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
  
  # Interactive visuals use Kamada-Kawai network layout algorithm - “force-directed” or “spring embedded” simulation. Nice because it isolates the major component or densest part of the network and pushes smaller ones to the outside 
  # Static sliced visuals use Fruchterman-Reingold algorithm – Can run fairly quickly but not as good as Kamada-Kwai on dynamic visuals that are used more for investigation vs. report viz. 
  
  ################# Outputs 
  
  # 1. Node Movement Stats: CSV output of proportion of accounts that:
  # are in time period t but not in time period t -1, 
  # are in time period t but not in time period t + 1, 
  # are in time period t but not in time period t + 1 and do not return,
  # engage in mutual interaction during time period t (i.e. they mention accounts and are mentioned) 
  
  # 2. Interactive HTML Network Slice Video: 
  # Directed and undirected with nodes colored in each slice based on whether the account was in the network in the previous slice 
  # Option for custom directed and undirected graphs with node coloring based on custom scheme as well 
  
  # 3. Network Metrics over time:
  # CSV output with degree, eigenvector, and betweeenness centrality for network over time
  # Gridded line graphs of degree, eigenvector, and betweeenness centrality for network over time
  
  # 4. Node Metrics over time:
  # CSV output with degree, eigenvector, and betweeenness centrality, as well as David's Score, for each account over time
  # CSV output with degree, eigenvector, and betweeenness centrality averages for accounts over time
  # Gridded line graphs of top 10 degree, eigenvector, and betweeenness centrality, as well as David's Score for each account over time
  
  # 5. Reachable Sets: Visuals showing:
  # Temporal path: Sequence of vertices and edges such that the onset times of successive elements are greater than or equal than those of the previous; In other words, the path is a directed traversal of the network that respects the constraints of edge activity spells and permits 'waiting' at intermediate vertices for 'future' edges to form.
  # Chain from accounts that mentioned a pre-specified account over time 
  # Chain from accounts that were mentioned by an account over time
  # For backward sets, the path will be found by searching backwards in time from the end point. In other words, it returns the set of vertices that can reach v, along with latest possible departure times from those vertices
  # For forward sets, we are talking about the set of nodes that can be reached from an initial seed via a path of temporally ordered edges; a natural extension of connected component measures to dynamic networks
  
  
  # 6. Network Visuals (individual):
  # Outputs static network for each time period of the dynamic network split by full, original, and other accounts 
  # Option for custom graphs for output as well 
  
  # 7. Comparison network visuals:
  # Outputs major component of static network for two time periods gridded with nodes colored based on whether nodes are new to network or not
  # Outputs major component of each static network for dynamic network with nodes colored based on whether nodes are new or not in that time period
  
  # 8. Network zoom visualization:
  # This outputs a zoom in of a specific pre-specified reply thread
  
  #################################################3##### Module: Temporal Network Analysis in R ##############################################################
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
  library(feather)
  library(tergm)
  library(dplyr)
  options(scipen=999)
  
####################################################### 2. Set up objects and paths ##########################################################################
# Set up input directory and bring in file
input <- "/Users/christian.conroy/OneDrive - Accenture Federal Services/Documents/Behavioral Analytics/misdispro/data_vax_for_python"
setwd(input) #set working directory

# Amount of lines to skip (if excel)
skip_lines = 0

# set output directory
output_folder <- "/Users/christian.conroy/OneDrive - Accenture Federal Services/Documents/Behavioral Analytics/misdispro/updated_sim_output/"
  
## finds all files ending in .csv in working directory
files <- paste(input, list.files(path = input, pattern = '*.xlsx'), sep = "/")

## Interaction-network size is assessed by looking at the top n*100 interactions so an interaction level of 5 will look at the interactions of the top~500 people in the network.If the network is too busy or computations take too long, reduce the interaction level:
## For full network (not recommended) change interaction_level to NA
interaction_level = 3
  
# Interaction_level: Threshold based on edge weight (how many times two people interact)
# We do the number times 100 - We get the top based on edge weight (so 1 means top 100)

## If you pulled data by twitter handle, those accounts will have massive influence in your network. Those accounts can be dropped prior to running the network. Include caps.
## if you don't want to drop any authors, change to NA.
remove_authors = NA

# If we are looking at official accounts, we might not care about that account and only about the mentions around it, hence what we'd need here

# Only return top betweenness nodes (right now they are displayed as top degree, but can segment based on betweeness instead if we want)
filter_betweenness = FALSE
  
# If you pulled the network with a list of accounts and you want those accounts to be included. If this is the case, you'll also need to upload a dataframe with those accounts where the account names are in the first column (colname irrelevant) it should be csv format.
pulled_list_of_accs = FALSE
path_to_acc_list = "/Users/christian.conroy/OneDrive - Accenture Federal Services/Documents/Behavioral Analytics/misdispro/data_4_vax/dprk_net_accounts.csv"

# This just helps with the viz of others and original (Can see where the accounts we pulled are and the ones not pulled originally)
  
## What do you want these original accounts to be labeled as in your graph?
orig_accounts = "Original"

## What do you want the other accounts to be called in your graph?
other_accounts = "Other"

# Do you want to label based on centrality measures or some other narrative? (if false, make sure to inpust labels)
centrality_label = TRUE
#custom_labels = c()
  
################ Visual Filtering 
custom_graph = FALSE

reachable_paths_on = TRUE
custom_reachable_output = FALSE
custom_reachable_labels = FALSE

reachable_labels <- c('@whale90196377', '@gftdduzyhxjzids')
# NOTE: THE ABOVE IS FOR WHICH ACCOUNTS TO OUTPUT THE FWD AND BACKWD REACHABLE SETS FOR 
# NEED TO MANUALLY ADJUST WHICH LABELS APPEAR ON EACH OUTPUT IN THE FUNCTION (COMMENTED OUT NOW)
  
## Custom color/ legend guide:
# to create custom labels, you'll need to do 2 things:
# 1: create a csv with 2 columns where the first column contains account names and the second column contains labels. Point to that file here:

path_to_nodes_categorized = "/Users/christian.conroy/OneDrive - Accenture Federal Services/Documents/Behavioral Analytics/misdispro/data_4_vax/accounts.csv"

# 2: each label will need a custom color hexcode: red, dark blue, green, light blue, yellow
gec_pal <- c("Cat 1" = "#C00000", "Cat 2" = "#E7E6E6", "Cat 3" = "#439c40")  
  
# MUST GO IN AND MANUALLY CHANGE IN FUNCTION BELOW: mentions_network_dynamic%v%'color' <- ifelse(mentions_network_dynamic%v%"Orientation" == "Eventually Suspended", "#C00000", "#E7E6E6")
  
## For creating Custom Zoomed in on specific accounts, links, and time slice (Likely need to run everything, explore and come back to)
custom_zoom = FALSE
i_zoom = 17
reply_nodes_zoom = c("@pedrocastillote", "@vladimir_cerron", "@keikofujimori", "@carhuanca", "@cecuray")
label_names_zoom = c("@pedrocastillote", "@vladimir_cerron", "@keikofujimori", "@carhuanca", "@cecuray")

################ Time Filtering 

week = FALSE
day = TRUE

# 604800: Number of seconds in a week 
# 86400: Number of seconds in day

# if both week and day are false, set a custom interval (using epoch unix time) below
custom_interval = 2629743
  
  ######################################################## A NOTE ON WHICH PARAMETERS NEED TO BE ADJUSTED MANUALLY BELOW 
  # 1. For Interactive Custom Visualizations Custom Graph (If TRUE), you need to adjust the ifelse to reflect your categories and colors for directed and undirected
  # mentions_network_dynamic%v%'color' <- ifelse(mentions_network_dynamic%v%"Orientation" == "Eventually Suspended", "#C00000", "#E7E6E6")
  
  # 2. If you'd like to output node centrality or David Score measure over time for accounts OTHER THAN TOP 10 FOR EACH, need to adjust the below:
  # deg_list <- c("@sagaclan1", "@turmpd", "@yang32024699")
  # bet_list <-
  # eig_list <- 
  # david_list <- 
  
  # 3. If you want forward or backward reachable sets for specific accounts and only specific accounts highlighted on the reachable sets of those acounts, need to do manually below  (custom_reachable_output = TRUE and custom_reachable_labels = TRUE), need to adjust:
  
  # Manually add in accounts to use from labeling 
  #gft_bw <- c('@bbcchinese', '@sbsnews', '@dw_chinese', '@voachinese', '@joshuawongcf', '@whale90196377', '@freeeveryprov', '@iataw224iancicu', '@gftdduzyhxjzids','@zhangeric16','@fuym4lpkgfm1t34', '@vcmf_china', '@mang047', '@wongtafuk', '@steve10247087', '@josefli2', '@realxavierbin', '@nick_komo')
  #gft_fw <- c('@whale90196377', '@iataw224iancicu', '@gftdduzyhxjzids','@zhangeric16','@fuym4lpkgfm1t34', '@mang047', '@wongtafuk', '@steve10247087', '@josefli2', '@nick_komo', '@supernan123', '@zjhzxs')
  #whl_bw <- c('@bbcchinese', '@sbsnews', '@dw_chinese', '@voachinese', '@joshuawongcf', '@whale90196377', '@freeeveryprov', '@iataw224iancicu', '@gftdduzyhxjzids','@zhangeric16','@fuym4lpkgfm1t34', '@vcmf_china', '@mang047', '@wongtafuk', '@josefli2', '@realxavierbin', '@nick_komo')
  #whl_fw <- c('@whale90196377', '@iataw224iancicu', '@gftdduzyhxjzids','@zhangeric16','@fuym4lpkgfm1t34', '@mang047', '@steve10247087', '@josefli2', '@supernan123')
  #bf$bkw <- c(I(list(gft_bw)), I(list(whl_bw)))
  #bf$fwd <- c(I(list(gft_fw)), I(list(whl_fw)))
  
  # 4. For Network Slice Visualizations Custom Graph (If TRUE), you need to adjust the ifelse to reflect your categories and colors
  # df$suspended <- ifelse(df$suspended == 1, "Eventually Suspended", "Active")
  
  # 5. For Network Slice Visualizations, you need to adjust to get whatever time slice comparisons you want (below compares t1 and t2)
  # df$suspended <- ifelse(df$suspended == 1, "Eventually Suspended", "Active")
  # full_net <- ggarrange(ggarrange(listofgraphs[[1]], listofgraphs[[2]], ncol = 2), legend, heights = c(4,0.5), nrow=2)
  
  
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
    a3 <- a2 %>% dplyr::left_join(x) %>% dplyr::select(-n)
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

x <- files
# Create function to Import data #####################################################################
  get_dynamic_edgelist_zignal <- function(x){
    # This function generates an edgelist for the data. Authors are individuals who wrote tweets
    # targets are individuals within the tweets. 
    print('importing zignal data...')

    raw <- x %>% map_df(function(x) read_xlsx(x)) %>% 
      mutate(authors = author_username)
    
    raw$target_auths = str_extract_all(raw$body, "(?<=^|\\s)@[^\\s]+")
    
    # clean up the new column
    raw$target_auths <- sub("\\)$", "", raw$target_auths)
    raw$target_auths <- sub("c\\(", "", raw$target_auths)
    raw$target_auths <- sub(":", "", raw$target_auths)
    raw$target_auths <- str_remove_all(raw$target_auths, '\\"')
    raw <- raw[raw$target_auths != "character(0",]
    
    raw$date <- gsub('T', ' ', raw$date)
    raw$date <- gsub('Z', '', raw$date)
    raw$date <- as.POSIXct(raw$date, format = "%Y-%m-%d %H:%M:%OS")

    raw_full <- raw  
    raw_full$target <- raw_full$target_auths
    
    pre_agent2 <- raw_full
    
    colnames(pre_agent2)
    pre_agent2$authors <- str_trim(pre_agent2$authors)
    pre_agent2$target <- str_trim(pre_agent2$target)
    
    # create final edgelist
    pre_agent3 <- pre_agent2[!(pre_agent2$target == ""),]
    
    # drop ego network authors (optional)
    pre_agent3 <- drop_authors(pre_agent3)
    
    pre_agent3 <- pre_agent3[,c("date", "authors", "target", "tweet_id")]
    
    # Give IDs
    head <- pre_agent3 %>%
      distinct(authors) %>%
      dplyr::rename(label = authors)
    
    tail <- pre_agent3  %>%
      distinct(target) %>%
      dplyr::rename(label = target)
    
    write.csv(tail, paste0(output_folder,"node_list_unpruned_onlySource_nomen_NoOnlyRTs_JustSource.csv"), row.names = FALSE)
    
    nodes <- full_join(head, tail, by = "label")
    nodes  <- nodes %>%
      distinct(label)
    
    nodes <- nodes %>% rowid_to_column("id")
    write.csv(nodes, paste0(output_folder,"node_list_unpruned_onlySource_nomen_NoOnlyRTs.csv"), row.names = FALSE)
    
    pre_agent3 <- left_join(pre_agent3, nodes, by = c("authors" = "label"))
    names(pre_agent3)[5] <- "head_id"
    pre_agent3 <- left_join(pre_agent3, nodes, by = c("target" = "label"))
    names(pre_agent3)[6] <- "tail_id"
    
    colnames(pre_agent3) <- c("onset",  "head", "tail", "tweet_id", "head_id", "tail_id")
    
    # Output edge list with urls 
    write.csv(pre_agent3, paste0(output_folder,"edge_list_with_urls_unpruned_onlySource_nomen_NoOnlyRTs.csv"))
    
    pre_agent3_index <- pre_agent3
    # onset, head, tail
    pre_agent3_index <- pre_agent3_index[,c(1:3)]
    
    time_edgelist <- pre_agent3_index

    time_edgelist$onset <- as.numeric(as.POSIXct(time_edgelist$onset, "%Y-%m-%d %H:%M:%OS"))
    time_edgelist$terminus <- time_edgelist$onset
    time_edgelist <- as.data.frame(time_edgelist)
    
    return(time_edgelist)
  }
  
  # Pruning Function
  reduce_network <- function(x){
    
    interactions = recommended_interactions(x)
    time_edgelist_new <- as.data.frame(agent_filter(x, interactions))
    
    return(list(time_edgelist_new, interactions))
  }
  
  
  
  #######################################################
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
    time_edgelist_both <- reduce_network(get_dynamic_edgelist_zignal(x))
   
    time_edgelist_new <- time_edgelist_both[[1]]
    interactions <- time_edgelist_both[[2]]
    
    time_edgelist_new <- time_edgelist_new[!(time_edgelist_new$head == time_edgelist_new$tail),]
    
    # Create nodes to pass as vertex attributes to network object
    head <- time_edgelist_new %>%
      distinct(head) %>%
      dplyr::rename(label = head)
    
    tail <- time_edgelist_new %>%
      distinct(tail) %>%
      dplyr::rename(label = tail)
    
    nodes <- full_join(head, tail, by = "label")
    nodes  <- nodes %>%
      distinct(label)
    
    nodes <- nodes %>% rowid_to_column("id")
   # write.csv(nodes, paste0(output_folder, "nodes_list_pruned.csv", row.names = FALSE))
    
    mentions <- time_edgelist_new %>%  
      group_by(head, tail) %>%
      summarise(weight = n()) %>% 
      ungroup()
    
    time_edgelist_full <- left_join(time_edgelist_new, nodes, by = c("head" = "label"))
    names(time_edgelist_full)[5] <- "head_id"
    time_edgelist_full <- left_join(time_edgelist_full, nodes, by = c("tail" = "label"))
    names(time_edgelist_full)[6] <- "tail_id"
    #write.csv(time_edgelist_full, paste0(output_folder,"edge_list_pruned.csv.csv", row.names = FALSE))
    
    # Create edgelist for normal network object
    
    edges <- mentions %>% 
      left_join(nodes, by = c("head" = "label")) %>% 
      dplyr::rename(from = id)
    
    edges <- edges %>% 
      left_join(nodes, by = c("tail" = "label")) %>% 
      dplyr::rename(to = id)
    
    edges <- dplyr::select(edges, from, to, weight)
    
    edges_undir <- edges %>% 
      rowwise() %>%
      mutate(key = paste(sort(c(from, to)), collapse="")) %>%
      distinct(key, .keep_all=T) %>%
      dplyr::select(-key)
    
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
    mentions_network <- network::network(edges_undir[, 1:2], vertex.attr = nodes, matrix.type = "edgelist", directed = F, hyper = F, loops = F, multiple = F, bipartite = F)
    
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
  
  # Create TSNA Objects

  dynamic_objects <- get_dynamic_network(files)
  mentions_network_dynamic <- dynamic_objects[[1]]
  mentions_network_dynamic_nodes <- dynamic_objects[[2]]
  mentions_network_dynamic_directed <- dynamic_objects[[3]]
  mentions_network_dynamic_directed_nodes <- dynamic_objects[[4]]
  node_spells <- dynamic_objects[[5]]
  # Come back and make sure this is appended on the end of the saved files and not the interaction term
  interactions <- dynamic_objects[[6]]
  time_edgelist_new <- dynamic_objects[[7]]
  
  #################################### Create Node Over Time Metrics #################################
  # Node Movement Stats: CSV output of proportion of accounts that:
  # are in time period t but not in time period t -1, 
  # are in time period t but not in time period t + 1, 
  # are in time period t but not in time period t + 1 and do not return,
  # engage in mutual interaction during time period t (i.e. they mention accounts and are mentioned) 
  
  # new: Grey means they were in the previous time period and red means they were not (1 all grey as no previous)
  # fut: Grey means they were in the next time period and red means they were not (5 all grey as no future)
  # return: Grey means they did return after not being in the next time period and red means they did not (5 all grey as no future)
  
  # Example in output 
  #time_group	new.active.color	new_prop	fut_prop	return_prop
  #8	grey	0.722222222	0.944444444	NA
  #8	red	0.277777778	0.055555556	1
  
  # In week 8, 72.22% of accounts were there the previous week, 94.44% were also in the next time period, none returned in future weeks after leaving 
  # In week 8, 27.78% of accounts were not there the previous week, 5.56% were not there in the next time period, and none returned in future weeks after leaving
  
  # time_group	mutual	count	mutual_prop
  # 8	not mutual	5	1
  
  # In week 8, there were no mutual/reciprocal exchanges between accounts 
  
  # Example insight 
  # e.	What can this tell us? 
  # i.	Several accounts were active over the entire duration and shifted replies to whichever posts were most critical of China at that time. 
  # ii.	 Others activated from a dormant state to address issues of concern for the CCP, like the Hong Kong National Security Law or inauguration of Taiwan President Tsai Ing-wen, and then subsequently were suspended or returned to dormancy. 
  # iii.	We often observed accounts activating in order to refute or amplify stances on high-profile events publicized by media outlets and government officials
  # iv.	This pattern of activating and returning to dormancy may mean that either accounts are being rotated after periods of time by their operators, or that accounts are assigned to respond to specific issue areas.  


setwd(output_folder)
dir.create("node_movement_stats")
setwd(input)

node_over_time <- function(x,y){
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

# The Kamada-Kawai network layout algorithm is often described as a “force-directed” or “spring embedded” simulation, but it is mathematically equivalent to some forms of MDS (Kamada-Kawai uses Newton-Raphson optimization instead of SMACOF stress-majorization).

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
  df$credible <- ifelse(df$credible == 1, "Credible", ifelse(df$credible == 0, "Not Credible", ifelse(df$credible == 2, "Neutral", NA)))
  colnames(df)[1:2] <- c('names', 'ref')
  df$names <- tolower(df$names)
  df$names <- ifelse(grepl('^@', df$names), df$names, paste0('@', df$names))
  # run the following code as is:
  newdf <- tibble(names = mentions_network_dynamic%v%'label')
  labels <- label_names
  ndf2 <- newdf %>% left_join(df)
  ndf2$newor <- ifelse(is.na(ndf2$ref), "Excluded", ndf2$ref)
  mentions_network_dynamic%v%'Orientation' <- ifelse(mentions_network_dynamic%v%'label' %in% ndf2$names, ndf2$newor, NA)
  mentions_network_dynamic%v%'color' <- ifelse(mentions_network_dynamic%v%"Orientation" == "Credible", "#439c40", ifelse(mentions_network_dynamic%v%"Orientation" == "Not Credible", "#C00000", ifelse(mentions_network_dynamic%v%"Orientation" == "Neutral", "#E7E6E6", NA)))
  
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
  df$credible <- ifelse(df$credible == 1, "Credible", ifelse(df$credible == 0, "Not Credible", ifelse(df$credible == 2, "Neutral", NA)))
  colnames(df)[1:2] <- c('names', 'ref')
  df$names <- tolower(df$names)
  df$names <- ifelse(grepl('^@', df$names), df$names, paste0('@', df$names))
  # run the following code as is:
  newdf <- tibble(names = mentions_network_dynamic_directed%v%'label')
  labels <- label_names
  ndf2 <- newdf %>% left_join(df)
  ndf2$newor <- ifelse(is.na(ndf2$ref), "Excluded", ndf2$ref)
  mentions_network_dynamic_directed%v%'Orientation' <- ifelse(mentions_network_dynamic_directed%v%'label' %in% ndf2$names, ndf2$newor, NA)
  mentions_network_dynamic%v%'color' <- ifelse(mentions_network_dynamic%v%"Orientation" == "Credible", "#439c40", ifelse(mentions_network_dynamic%v%"Orientation" == "Not Credible", "#C00000", ifelse(mentions_network_dynamic%v%"Orientation" == "Neutral", "#E7E6E6", NA)))
  
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
  
  # deg_list <- c("@sagaclan1", "@turmpd", "@yang32024699")
  # bet_list <-
  # eig_list <- 
  # david_list <- 
  
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







############################## Simulating the Network Outwards ###################
################# https://rstudio-pubs-static.s3.amazonaws.com/786777_b196f403338842199fa58b14cac5155b.html#Form()_+_Persist()
## TERGMS

# There are three general modeling frameworks for temporal network data analysis in the social networks field.
# Stochastic Actor Oriented Models (SAOM)--implemented in the `RSiena` package written by Tom A.B. Snijders and colleagues.  SAOMs model the              # coevolution of nodal attributes and tie dynamics.  They are formulated as continuous time models, but typically are estimated on discrete time data.

# Relational Event Models--implemented in the `statnet` package `relevent` written by Carter Butts and colleagues.  These are continuous time models      # for node and tie dynamics, so they can assume dyad independence,  and exploit the methodology of logistic regression.

# Temporal Exponential-family Random Graph Models (TERGMS)--This is a large family of models that extend ERGMs to the analysis of dynamic networks.       # TERGMs represent tie dynamics only, and are typically formulated in discrete time.

# The `tErgmStats` function calculates cross-sectional ergm term statistics at multiple time points, as a time-series object.

# From the descriptive statistics we saw that the number of triangles increased over time. Given directedness, it would be natural to examine the role of cyclical (non-hierarchical) vs. transitive (hierarchical) triangles in the network. The robust way to model this in ERGMs is with the terms transitiveties and cyclicalties (the number of ties i→j that have at least one two-path from i→j and from j→i, respectively).

# In this case, we will specify the same terms in both the formation and persistence model. That is not necessary in stergms, we could instead specify a simple homogenous Bernoulli model for persistence (all ties are equally likely to persist at each time step) if we thought that was appropriate. Having the same terms also does not mean we have the “same model” for formation and persistence: we might find that the coefficient estimates differ in sign or magnitude. The reason it’s useful to use the Persist() rather than the Diss() operator here is that the sign of the coefficient indicates the same impact on whether you expect a tie to be present.

################## https://rpubs.com/runnytone/tergm
## STERGMS
# Separable Temporal ERGMs (STERGMs) are an extension of ERGMs for modeling dynamic networks in discrete time, introduced in Krivitsky and Handcock (2010)
# STERGMs, in contrast, posit two models: one ERGM underlying relational formation, and a second one underlying relational dissolution
# Specifying a STERGM thus entails writing two ERGM formulas instead of one.
# To translate this to dynamic networks: first think of a Bernoulli model where all ties are equally likely. This expression indicates that the number of ties       present in the cross-section (prevalence) is a function of the rate at which they form (incidence) and the rate at which they dissolve (1/duration). The faster      ties form, the more ties that are present in the cross-section. And the slower ties dissolve, the more ties that are present in the cross-section.

# Using the notation from the ERGM review above: the two equations governing a STERGM are commonly called the formation equation and the dissolution (or persistence) equation
# These largely parallel the one ERGM equation. We have simply:
# added a time index to the tie values
# added in a new conditional. In the formation equation, the expression is conditional on the tie not existing at the prior time step, and in the dissolution         equation it is conditional on the tie existing; and

# Both the formation and dissolution (persistence) equations would contain the ergm term “concurrent”, which counts the number of nodes in the network of degree 2 or more
# In both models, the coefficient for the concurrent term would be negative; new ties are relatively unlikely to form if they will generate a new node of degree 2 or more. And they are relatively unlikely to persist if they maintain the current number of nodes of degree 2 or more, when they could bring it down by dissolving.
#nFinally, remember that the s in stergm stands for separable; this refers to the fact that the two models assume that relational formation and dissolution occur independently of one another within a time step.
# In modeling the transition from a network Yt at time t to a network Yt+1 at time t+1, the separable temporal ERGM assumes that the formation and dissolution of     ties occur independently from each other within each time step, with each half of the process modeled as an ERGM. 

# For a call to stergm, there is still one network (or a list of networks), but two formulas. These are now passed as three separate arguments: the network(s) (argument nw), the formation formula (argument formation), and the dissolution formula (argument dissolution). The latter two both take the form of a one-sided formula. E.g.:

######### 2. file:///Users/christian.conroy/Downloads/v83i06%20(3).pdf
# Temporal Exponential Random Graph Models with btergm: Estimation and Bootstrap Confidence Intervals
# The feature of network data that is both scientifically and methodologically distinct is     that the probability of a tie forming between any two nodes in the network – often the           dependent variable of interest – depends upon the structure of the rest of the network        (Cranmer and Desmarais 2011).
# Additionally, the likelihood of two nodes tying can also depend upon attributes of nodes,     which makes tie prediction in part analogous to regression modeling with covariates.

# The temporal exponential random graph model, usually called TERGM, is an extension of the ERGM designed to accommodate inter-temporal dependence in longitudinally observed networks.
# The extension is accomplished by incorporating parameters into an ERGM specification that reflect the ways in which previous realizations of the network determine current features of the network.
# So-called “memory terms” play a special role in the specification of TERGMs. “Memory terms” refer to a class of intertemporal dependencies designed to capture temporal processes without capturing additional network structure.

library("statnet")
#install.packages('texreg')
library("texreg")
#install.packages("xergm.common")
library("xergm.common")
#install.packages('btergm')
library(btergm)
set.seed(10)

#Network data for use with package btergm should be stored as lists of network objects in
#chronological order, with one network per time period.

# In a first simple model, a TERGM without any temporal dependencies is estimated. This corresponds to a pooled ERGM – pooling across T networks that are assumed to be independent of each other – where the estimates reflect the average effects across the 20 time points. The (somewhat unrealistic) assumption here is that the consecutive networks are independent from each other.

# The first argument of the btergm function is a formula like in the ergm function, but accepting lists of networks or matrices instead of a single network or matrix as the dependent variable. In all other regards, the syntax of btergm and the syntax of ergm are identical.

# The model is estimated with 50 bootstrap replications (argument R = 50). More replications means less simulation error in the confidence intervals calculated, but also longer runtime

# Here, we use a smaller sample for purposes of illustration. Options for parallel processing on multicore CPUs or HPC servers are available (see ?btergm).

# The model contains network dependencies within time steps, such as gwesp(0, fixed = TRUE), which controls for triadic closure by at least one shared partner.
# We can assess the endogenous goodness-of-fit of the model using the gof function in the btergm package. gof is a generic function that has methods for several kinds of network models, including ‘btergm’, ‘mtergm’, ‘ergm’, and RSiena models, both for within-sample and out-of-sample fit assessment, following the procedures suggested by Hunter, Goodreau, and Handcock (2008a). 

#The nsim = 50 argument causes the gof function to simulate a total of 1,000 networks (50 from each of the 20 time steps). Naturally, when estimating models for publication, more simulations are preferable. Here, however, exposition is accomplished with fewer simulations and less computing time.

#  Interpretation of these plots is straightforward; the model is said to fit better the closer the medians of the box plots (based on the simulated networks) come to the line that plots the actual value of these statistics in the observed network. Obviously, the endogenous model fit of Model 1a is poor. Therefore we estimate a model with some simple temporal  dependencies as follows:

# The new model is identical to the previous model, except for three additional model terms that represent temporal dependence:

# memory(type = "stability") is a dyadic stability memory term (see Section 2.3), which controls whether ties and non-ties at one time point carry over to ties and nonties, respectively, at the next time point. As indicated by the positive and significant coefficient, there is a great amount of stability in alliance ties over time.

# timecov(transform = function(t) t) is a time covariate (see Section 2.4) that simply checks whether there is a (linear) time trend in the number of alliance edges over time.

# timecov(warNet, transform = function(t) t) is an interaction between a linear time effect and the warNet exogenous covariate.

# Besides memory and timecov, the package contains a third hard-coded temporal network statistic, delrecip, which tests for delayed reciprocity (i.e., whether an edge from node j to node i in one time period leads to an edge from i to j later). More temporal dependencies may be included in the future.

# While some of the temporal model terms are significant, some of the other model terms that were previously significant have lost their explanatory power, such as the warNet covariate and the nodal effects for polity and cinc. Simply controlling for previous states of the system provides a better-fitting account of who is allied with whom than these variables. However, the majority of model terms are still significant after controlling for the temporal effects.

# If temporal dynamics are modeled, at least one time step is lost. Usually, the first time step is dropped from the list of dependent networks. The reason is that the estimation is conditioned on the network at the previous time step(s) each time, and for the first observed network, there is simply no previous time step that could be used.

# If a TERGM is applied to six independent networks, one could estimate the model based on four or five of them and try to predict the networks that were left out using the model.

# In many settings, however, only one time series is available. For example, there is just one realization of international conflict per year. In such a situation, the best one can do is leave out some time steps when estimating the model, and predict the time steps that were left out using the model. If this works reasonably well, the model seems to fit well out-of-sample.
# The btergm package offers both within-sample and out-of-sample prediction and goodness-offit assessment through the generic gof function and its methods.

# Next, we can employ the gof function to simulate 100 networks from the model and compare them to the observed network at t = 4.

# . To simulate new networks, one needs to supply the covariates for the coefficients, including the temporal statistics.

# The main differences between the btergm and tergm implementations of TERGMs are the estimation procedures and the specifications supported. First, estimation in package tergm is done by either MCMC-MLE or a simulated method of moments procedure. These are effective estimation procedures when the networks are modestly sized and/or T is small, but will be noticeably more computationally demanding than btergm when dealing with voluminous data. 

# The following command simulates ten new networks based on the coefficients stored in Model 2e and for time step 3. The index argument refers to the third item in the list of dependent networks used in the model; if the model included time steps 2 to 4, index = 3 would refer to time step 4. The resulting ten networks are stored in a list. The simulate methods use the ‘formula’ method from the ergm package internally to create new networks via MCMC (Hunter et al. 2008b; Handcock et al. 2017).

#network <- mentions_network_dynamic_directed
#start=1609488511
#end= 1622037674
#increment = 2629743
#num_networks_total = 10

get_dynamic_network_simulation <- function(network, start, end, increment, num_networks_total){
  # Turn networkdynamic object into a list 
  mentions_network_dynamic_list <- get.networks(network, start= start , end= end, time.increment = increment)
  
  # run TERGM model and simulate x numberof networks forward
  while (length(mentions_network_dynamic_list) < num_networks_total){
    model.2d <- btergm(mentions_network_dynamic_list ~ edges + gwesp(0, fixed = TRUE) + memory(type = "stability"), R = 50)
    
    nw <- simulate(model.2d, nsim = 1, index = (length(mentions_network_dynamic_list) + 1))
    
    mentions_network_dynamic_list <- append(mentions_network_dynamic_list, list(nw))
    length(mentions_network_dynamic_list)
  }
  
  mentions_network_dynamic_projected <- networkDynamic(network.list = mentions_network_dynamic_list)
  id_label_map <- do.call(rbind, Map(data.frame, id=mentions_network_dynamic_projected %v% "id", label=mentions_network_dynamic_projected %v% "label"))
  
  # Convert and add node spells to demonstrate new entrants 
  
  time_edgelist_new <- as.data.frame(mentions_network_dynamic_projected)
  
  time_edgelist_new_labels <- left_join(time_edgelist_new,   id_label_map, by = c("tail" = "id"))
  names(time_edgelist_new_labels)[9] <- "tail_id"
  time_edgelist_new_labels <- left_join(time_edgelist_new_labels,   id_label_map, by = c("head" = "id"))
  names(time_edgelist_new_labels)[10] <- "head_id"
  write.csv(time_edgelist_new_labels, paste0(output_folder, "edgelist_full_sim.csv"))
  # Create nodes to pass as vertex attributes to network object
  head <- time_edgelist_new %>%
    distinct(head) %>%
    dplyr::rename(label = head)
  
  tail <- time_edgelist_new %>%
    distinct(tail) %>%
    dplyr::rename(label = tail)
  
  nodes <- full_join(head, tail, by = "label")
  
  nodes <- nodes %>% rowid_to_column("id")
  write.csv(nodes, paste0(output_folder, "nodes_sim.csv"))
  
  mentions <- time_edgelist_new %>%  
    group_by(head, tail) %>%
    summarise(weight = n()) %>% 
    ungroup()
  
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
  
  edges <- dplyr::select(edges, from, to, weight)
  
  # Create edgespells for networkdynamic object
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
  mentions_network_directed <- network.collapse(network,onset=1,terminus=num_networks_total)
  
  mentions_network_dynamic_directed_nodes <- networkDynamic(edge.spells = time_edgelist_new, vertex.spells = node_spells[1:3])
  set.vertex.attribute(mentions_network_dynamic_directed_nodes,"id",as.vector(nodes$id))
  set.vertex.attribute(mentions_network_dynamic_directed_nodes,"label",  mentions_network_dynamic_projected %v% "label")
  
  mentions_network_dynamic_directed <- networkDynamic(mentions_network_directed, edge.spells = time_edgelist_new)
  set.vertex.attribute(mentions_network_dynamic_directed,"id",as.vector(nodes$id))
  
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
  return(list(mentions_network_directed, mentions_network_dynamic_directed, mentions_network_dynamic_directed_nodes, node_spells, time_edgelist_new, mentions_network_dynamic_list))
}

dynamic_simulation_objects <- get_dynamic_network_simulation(mentions_network_dynamic_directed, start=time_start , end= time_end, increment = interval_val, num_networks_total = 44)
mentions_network_directed_sim <- dynamic_simulation_objects[[1]]
mentions_network_dynamic_directed_sim <- dynamic_simulation_objects [[2]]
mentions_network_dynamic_directed_nodes_sim <- dynamic_simulation_objects[[3]]
node_spells_sim <- dynamic_simulation_objects[[4]]
time_edgelist_new_sim <- dynamic_simulation_objects[[5]]
mentions_network_dynamic_list_sim <- dynamic_simulation_objects [[6]]

# If else to plot labels by either centrality values or a provided list of custom labels
if(centrality_label == TRUE) {
  centrality <- get_centrality(mentions_network_directed_sim, label_names = "Degree")
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

# The Kamada-Kawai network layout algorithm is often described as a “force-directed” or “spring embedded” simulation, but it is mathematically equivalent to some forms of MDS (Kamada-Kawai uses Newton-Raphson optimization instead of SMACOF stress-majorization).

time_start <- min(as.data.frame(mentions_network_dynamic_directed_nodes_sim)$onset)

time_end <- max(as.data.frame(mentions_network_dynamic_directed_nodes_sim)$terminus)

interval_val <- 1

setwd(output_folder)
dir.create("network_movies_projected")
setwd(input)

# Noteably, curved edges, edge labels, and label positioning are not yet implemented and will be ignored.

# As noted earlier, the MDSJ library is released under Creative Commons License “by-nc-sa” 3.0. This means using the algorithm for commercial purposes would be a violation of the license. More information about the MDSJ library and its licensing can be found at http://www.inf.uni-konstanz.de/algo/software/mdsj/.
# So MDSJ is slightly better, but stuck with kamadakawai to not have to deal with the potential licensing issue of the external install like with graphviz

# Great stuff for how to further format here: https://kateto.net/network-visualization

if(centrality_label == TRUE) {
  centrality <- get_centrality(mentions_network_directed_sim, label_names = "Degree")
  label_names <- centrality[[2]]
}else{
  label_names <- custom_labels  
}

########### Creating the animated visual with a directed network 
compute.animation(
  mentions_network_dynamic_directed_nodes_sim,
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

mentions_network_dynamic_directed_nodes_sim%v%'label_fil' <- ifelse(mentions_network_dynamic_directed_nodes_sim%v%"label" %in% label_names, mentions_network_dynamic_directed_nodes_sim%v%"label", NA)

mentions_network_dynamic_directed_nodes_sim%v%'hold' <- mentions_network_dynamic_directed_nodes_sim%v%'vertex.names'
mentions_network_dynamic_directed_nodes_sim%v%'vertex.names' <- mentions_network_dynamic_directed_nodes_sim%v%'label_fil'



movie <- render.d3movie( #label.cex
  mentions_network_dynamic_directed_nodes_sim, filename= paste0(output_folder,  'network_movies_projected/', sprintf("directed_network_movie_%s.html", as.character(interaction_level))), launchBrowser=FALSE, displaylabels = TRUE,  vertex.col= 'active.color', vertex.cex =0.6, main= paste( "Weekly Time Slices: ", paste(anytime(time_start, tz = "America/New_York"), round.POSIXt(anytime(time_end, tz = "America/New_York"), unit = "day"), sep = " to "), sep = " "),
  # vertex.cex =function(slice){ifelse(degree(slice) > 1, 0.6, NA)}, 
  # This slice function makes the labels work
  vertex.tooltip = function(slice) {
    paste(
      (slice %v% "label")
    )
  }
)

compute.animation(
  mentions_network_dynamic_directed_sim,
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

mentions_network_dynamic_directed_sim%v%'label_fil' <- ifelse(mentions_network_dynamic_directed_sim%v%"label" %in% label_names, mentions_network_dynamic_directed_sim%v%"label", NA)
