# Visualize Brandwatch Data -----------------------------------------------
library(tidyverse)
library(statnet)
library(readxl)
#library(intergraph)
#library(igraph)
library(openxlsx)
library(data.table)
library(qdapRegex)
library(ggnetwork)
library(ggrepel)
library(ggthemes)
library(data.table)
library(feather)
options(scipen=999)

install.packages("devtools")
library(devtools)
install_dev("shiny")
## Set your working directory to the folder containing your data.
## Keep in mind, smaller is better for network visualization.

setwd("/Users/christian.conroy/OneDrive - Accenture Federal Services/Documents/StateGec/Brandwatch/Afghanistan/Data/")

# choose: brandwatch, rtweet, or crimson
datasource = 'rtweet'

# number of lines to skip if you import BW data. This can change based on the import
brandwatch_skip = 9

# set output directory
output_folder <- "/Users/christian.conroy/OneDrive - Accenture Federal Services/Documents/StateGec/Brandwatch/Afghanistan/Results/" 

## interaction-network size is assessed by looking at the top n*100 interactions
## so an interaction level of 5 will look at the interactions of the top
## ~500 people in the network.
## if the network is too busy, reduce the interaction level:
## For full network (not recommended) change interaction_level to NA
interaction_level = NA

## if you pulled data by twitter handle, those accounts will have massive influence in your
## network. those accounts should be dropped prior to running the network. Include caps.
## if you don't want to drop any authors, change to NA.
remove_authors = NA
#remove_authors = NA

# Only run network for the giant component
giant_component = FALSE

# only return top betweenness nodes
filter_betweenness = FALSE

# if you pulled the network with a list of accounts and you want
# those accounts to be included. IF this is the case, you'll
# also need to upload a dataframe with those accounts where
# the account names are in the first column (colname irrelevant)
# it should be csv format.
pulled_list_of_accs = TRUE
path_to_acc_list = "/Users/christian.conroy/OneDrive - Accenture Federal Services/Documents/StateGec/Brandwatch/Afghanistan/Data/PRC_SM_Accounts.csv"

## What do you want these original accounts to be labeled as in your graph?
orig_accounts = "Original"

## What do you want the other accounts to be called in your graph?
other_accounts = "Other"

# Run helper functions --------------------------------------

if(datasource == 'brandwatch' | datasource == 'brandwatch and rtweet' | datasource == 'rtweet'){
  ## finds all files ending in .xlsx in working directory (brandwatch)
  files <- dir(pattern = "*.xlsx")
  }else if (datasource == 'crimson'){
  ## if data are from CH, they need to be converted to xlsx and then read in.
  file.list <- list.files(pattern = '*.xls$')
  file.list <- setNames(file.list, file.list)
  #
  sapply(file.list,FUN=function(eachPath){
    file.rename(from=eachPath,to=sub(pattern=".xls$",replacement=".xlsx",eachPath))
  })
  files <- dir(pattern = "*.xlsx")
  } 

drop_authors <- function(x){
  # If data are pulled by authors, we should drop the authors. Otherwise they dominate the newtork.
  # If remove_authors is NA, this function will not drop any authors.
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

crimson_clean <- function(raw){
  # clean up the new column
  raw$target_auths <- sub("\\)$", "", raw$target_auths)
  raw$target_auths <- sub("c\\(", "", raw$target_auths)
  raw$target_auths <- str_remove_all(raw$target_auths, '\\"')
  raw <- raw %>% select(authors, target_auths)
  raw <- raw[!(raw$target_auths == ""),]
  raw <- raw[!(raw$target_auths == "character(0"),]
  raw <- raw[!(raw$authors == "character(0"),]
  
  dummy_vec = c()
  pre_agent4 = c()
  
  if (nrow(raw) > 5000000){
    # if there are over 1M edges, it'll be faster if we chop up the data
    # and operate piece-wise
    num_groups = 100
    pre_agent2 <- raw %>% group_by((row_number()-1) %/% (n()/num_groups)) %>%
      nest %>% pull(data)
    
    for (i in seq_along(pre_agent2)){
      dummy_vec[[i]] <- raw[[i]] %>%
        select(authors, target_auths) %>%
        separate(target_auths, c("A1", "A2", "A3", "A5", "A6", "A7",
                                 "A8", "A9", "A10", "A11", "A12", "A13",
                                 "A14", "A15", "A16", "A17", "A18", "A19",
                                 "A20", "A21", "A22", "A23", "A24", "A25", "A26",
                                 "A27", "A28", "A29", "A30", "A31", "A32", "A33",
                                 "A34", "A35", "A36", "A37", "A38", "A39", "A40",
                                 "A41", "A42", "A43", "A44", "A45", "A46", "A47",
                                 "A48", "A49", "A50", "A51", "A52", "A53", "A54",
                                 "A55"), sep = ",")  %>%
        gather(key = "junk", value = "target", -authors, na.rm = TRUE) %>%
        select(-junk) %>%
        mutate(authors = tolower(authors),
               target = tolower(target))
    }
    for (i in seq_along(dummy_vec)){
      pre_agent2 <- dummy_vec[[i]]
      pre_agent2$authors <- str_trim(pre_agent2$authors)
      Encoding(pre_agent2$target) <- "UTF-8"
      pre_agent2$target <- iconv(pre_agent2$target, "UTF-8", "UTF-8",sub='') ## replace any non UTF-8 by ''
      pre_agent2$target <- str_trim(pre_agent2$target)
      
      # create final edgelist
      pre_agent3 <- pre_agent2[!(pre_agent2$target == ""),]
      pre_agent3 <- pre_agent3[!(pre_agent3$target == "character(0"),]
      pre_agent3 <- pre_agent3[!(pre_agent3$authors == "character(0"),]
      
      # drop ego network authors (optional)
      pre_agent3 <- drop_authors(pre_agent3)
      
      pre_agent4[[i]] <- pre_agent3
    }} else {
    pre_agent2 <- raw %>%
      select(authors, target_auths) %>%
      separate(target_auths, c("A1", "A2", "A3", "A5", "A6", "A7",
                               "A8", "A9", "A10", "A11", "A12", "A13",
                               "A14", "A15", "A16", "A17", "A18", "A19",
                               "A20", "A21", "A22", "A23", "A24", "A25", "A26",
                               "A27", "A28", "A29", "A30", "A31", "A32", "A33",
                               "A34", "A35", "A36", "A37", "A38", "A39", "A40",
                               "A41", "A42", "A43", "A44", "A45", "A46", "A47",
                               "A48", "A49", "A50", "A51", "A52", "A53", "A54",
                               "A55"), sep = ",") %>%
      gather(key = "junk", value = "target", -authors, na.rm = TRUE) %>%
      select(-junk) %>%
      mutate(authors = tolower(authors),
             target = tolower(target))
    pre_agent2$authors <- str_trim(pre_agent2$authors)
    Encoding(pre_agent2$target) <- "UTF-8"
    pre_agent2$target <- iconv(pre_agent2$target, "UTF-8", "UTF-8",sub='') ## replace any non UTF-8 by ''
    pre_agent2$target <- str_trim(pre_agent2$target)
    
    # create final edgelist
    pre_agent3 <- pre_agent2[!(pre_agent2$target == ""),]
    pre_agent3 <- pre_agent3[!(pre_agent3$target == "character(0"),]
    pre_agent3 <- pre_agent3[!(pre_agent3$authors == "character(0"),]
    
    # drop ego network authors (optional)
    pre_agent4 <- drop_authors(pre_agent3)
    return(pre_agent4)
  }
}

get_agent_network <- function(x){
  # This function generates an edgelist for the data. Authors are individuals who wrote tweets.
  # targets are individuals within the tweets. The column names are different for BW and CH, so
  # the function will act accordingly.
  if(datasource == 'brandwatch'){
  print('importing brandwatch data...')
  raw <- x %>%
    map_df(~read_excel(., skip = brandwatch_skip)) %>% 
    mutate(authors = paste0("@", Author))
  pre_agent2 <- raw %>% 
    select(authors, `Mentioned Authors`) %>% 
    separate(`Mentioned Authors`, c("A1", "A2", "A3", "A5", "A6", "A7",
                                    "A8", "A9", "A10", "A11", "A12", "A13",
                                    "A14", "A15", "A16", "A17", "A18", "A19", 
                                    "A20", "A21", "A22", "A23", "A24", "A25", "A26",
                                    "A27", "A28", "A29", "A30", "A31", "A32", "A33",
                                    "A34", "A35", "A36", "A37", "A38", "A39", "A40",
                                    "A41", "A42", "A43", "A44", "A45", "A46", "A47",
                                    "A48", "A49", "A50", "A51", "A52", "A53", "A54",
                                    "A55"), sep = ",") %>% 
    gather(key = "junk", value = "target", -authors, na.rm = TRUE) %>% 
    select(-junk) %>% 
    mutate(authors = tolower(authors),
           target = tolower(target))
    pre_agent2$authors <- str_trim(pre_agent2$authors)
    Encoding(pre_agent2$target) <- "UTF-8"
    pre_agent2$target <- iconv(pre_agent2$target, "UTF-8", "UTF-8",sub='') ## replace any non UTF-8 by ''
    pre_agent2$target <- str_trim(pre_agent2$target)
    
    # create final edgelist
    pre_agent3 <- pre_agent2[!(pre_agent2$target == ""),]
    pre_agent3 <- pre_agent3[!(pre_agent3$target == "character(0"),]
    pre_agent4 <- pre_agent3[!(pre_agent3$authors == "character(0"),]
    
    # drop ego network authors (optional)
    pre_agent4 <- drop_authors(pre_agent4)
    
  } else if(datasource == 'crimson') {
    print('importing CH data...')
    raw <- x %>%
      map_df(~read_excel(enc2utf8(.), skip =1))%>%
        mutate_all(as.character) %>%
        mutate(authors = Author)
    
    raw$target_auths = str_extract_all(raw$Contents, "(?<=^|\\s)@[^\\s]+")
    pre_agent4 <- crimson_clean(raw)
  } else if (datasource == 'rtweet'){
    raw <- list.files(path = './',
                      pattern = "*.feather",
                      full.names = T) %>% 
      map_df(function(x) read_feather(x) %>% mutate(filename=gsub(".feather","",basename(x))))
    raw <- raw %>% select(authors = screen_name, target_auths = mentions_screen_name)
    
    pre_agent4 <- crimson_clean(raw)
  } else if (datasource == 'rtweet'){
    print('importing rtweet data...')
    raw <- str_subset(x, "mentions", negate = TRUE) %>%
      map_df(~read_excel(., skip = 0)) %>%
      mutate(authors = paste0("@", screen_name))
    
    raw$target_auths = str_extract_all(raw$text, "(?<=^|\\s)@[^\\s]+")
    
    # clean up the new column
    raw$target_auths <- sub("\\)$", "", raw$target_auths)
    raw$target_auths <- sub("c\\(", "", raw$target_auths)
    raw$target_auths <- str_remove_all(raw$target_auths, '\\"')
    raw <- raw[raw$target_auths != "character(0",]
    
    pre_agent2 <- raw %>% 
      select(authors, target_auths) %>% 
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
    pre_agent4 <- drop_authors(pre_agent3)
    
  } else if (datasource == 'brandwatch and rtweet'){
    print('importing brandwatch data...')
     
    raw <- str_subset(x, "mentions") %>%
      map_df(~read_excel(., skip = brandwatch_skip)) %>% 
      mutate(authors = paste0("@", Author))
    pre_agent2_bw <- raw %>% 
      select(authors, `Mentioned Authors`) %>% 
      separate(`Mentioned Authors`, c("A1", "A2", "A3", "A5", "A6", "A7",
                                      "A8", "A9", "A10", "A11", "A12", "A13",
                                      "A14", "A15", "A16", "A17", "A18", "A19", 
                                      "A20", "A21", "A22", "A23", "A24", "A25", "A26",
                                      "A27", "A28", "A29", "A30", "A31", "A32", "A33",
                                      "A34", "A35", "A36", "A37", "A38", "A39", "A40",
                                      "A41", "A42", "A43", "A44", "A45", "A46", "A47",
                                      "A48", "A49", "A50", "A51", "A52", "A53", "A54",
                                      "A55"), sep = ",") %>% 
      gather(key = "junk", value = "target", -authors, na.rm = TRUE) %>% 
      select(-junk) %>% 
      mutate(authors = tolower(authors),
             target = tolower(target))
    pre_agent2_bw$authors <- str_trim(pre_agent2_bw$authors)
    Encoding(pre_agent2_bw$target) <- "UTF-8"
    pre_agent2_bw$target <- iconv(pre_agent2_bw$target, "UTF-8", "UTF-8",sub='') ## replace any non UTF-8 by ''
    pre_agent2_bw$target <- str_trim(pre_agent2_bw$target)
    
    # create final edgelist
    pre_agent3_bw <- pre_agent2_bw[!(pre_agent2_bw$target == ""),]
    pre_agent3_bw <- pre_agent3_bw[!(pre_agent3_bw$target == "character(0"),]
    pre_agent4_bw <- pre_agent3_bw[!(pre_agent3_bw$authors == "character(0"),]
    
    # drop ego network authors (optional)
    pre_agent4_bw <- drop_authors(pre_agent4_bw)
    
    print('importing rtweet data...')
    
    raw <- str_subset(x, "mentions", negate = TRUE) %>%
      map_df(~read_excel(., skip = 0)) %>%
      mutate(authors = paste0("@", screen_name))
    
    raw$target_auths = str_extract_all(raw$text, "(?<=^|\\s)@[^\\s]+")
    
    # clean up the new column
    raw$target_auths <- sub("\\)$", "", raw$target_auths)
    raw$target_auths <- sub("c\\(", "", raw$target_auths)
    raw$target_auths <- str_remove_all(raw$target_auths, '\\"')
    raw <- raw[raw$target_auths != "character(0",]
    
    pre_agent2_fth <- raw %>% 
      select(authors, target_auths) %>% 
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
    
    pre_agent2_fth$authors <- str_trim(pre_agent2_fth$authors)
    pre_agent2_fth$target <- str_trim(pre_agent2_fth$target)
    
    # create final edgelist
    pre_agent3_fth <- pre_agent2_fth[!(pre_agent2_fth$target == ""),]
    
    # drop ego network authors (optional)
    pre_agent3_fth <- drop_authors(pre_agent3_fth)
    
    pre_agent4 <- rbind(pre_agent4_bw, pre_agent3_fth)
    
  }
    return(pre_agent4)
  }

recommended_interactions <- function(x){
  if(!is.na(interaction_level)){
    a1 <- x %>% 
      group_by(authors, target) %>% 
      count(sort = TRUE)
    recommended <- a1[(interaction_level*100),][[3]]
  }else{
    recommended = 1
  }
  return(recommended)
}

## here we group authors and targets and sort them by count.
## We then ONLY take authors who meet the threshhold found in recommended_interactions
agent_filter <- function(x, limit = 2){
  a1 <- x %>% 
    group_by(authors, target) %>% 
    count()
  a2 <- a1 %>% filter(n >= limit)
  a3 <- a2 %>% left_join(x) %>% select(-n)
  return(a3)
}

# create a network. filter by interactions
create_network <- function(x, components = 2, largest_component = FALSE, betweenness_filter = FALSE){
  ## Create network creates a network object from the edgelist it has 3 arguments
  ## Components are determined by interaction_level.
  ## largest_component = If true, will only take data for the largest component in the graph
  ## betweenness_filter = if true, will generate a graph, and then return ONLY the top 75 nodes by betweenness
  
  edge <- agent_filter(x, components)
  adj <- edge
  
  adj <- as.data.frame(as.matrix(igraph::get.adjacency(igraph::graph.data.frame(adj))))
  # create network
  G <- network(adj, directed = F, hyper = F, loops = F, multiple = F, bipartite = F)
  G%v%'vertex_names' <- colnames(adj)
  
  if (betweenness_filter == TRUE){
    ## get the top 50 vertices by betweenness
    G%v%'bet_filter' <- sna::betweenness(G)
    bthreshhold = sort(G%v%'bet_filter', decreasing = TRUE)[75]
    G <- G %s% which(G %v% "bet_filter" >= bthreshhold)
  }
  
  if(largest_component == TRUE){
    ig <- intergraph::asIgraph(G)
    gclust<-igraph::clusters(ig, mode='weak')
    lcc<-igraph::induced.subgraph(ig, igraph::V(ig)[which(gclust$membership == which.max(gclust$csize))])
    G <- intergraph::asNetwork(lcc)
  }
  return(G)
}


# create a network. filter by interactions
create_weighted_d_network <- function(x, components = 2, largest_component = FALSE, betweenness_filter = FALSE){
  ## Create network creates a network object from the edgelist it has 3 arguments
  ## Components are determined by interaction_level.
  ## largest_component = If true, will only take data for the largest component in the graph
  ## betweenness_filter = if true, will generate a graph, and then return ONLY the top 75 nodes by betweenness
  
  edge <- agent_filter(x, components)
  adj <- edge
  
  adj <- as.data.frame(as.matrix(igraph::get.adjacency(igraph::graph.data.frame(adj))))
  # create network
  G <- network(adj, directed = F, hyper = F, loops = F, multiple = F, bipartite = F,
               ignore.eval=FALSE, names.eval='weight')
  G%v%'vertex_names' <- colnames(adj)
  
  if (betweenness_filter == TRUE){
    ## get the top 50 vertices by betweenness
    G%v%'bet_filter' <- sna::betweenness(G)
    bthreshhold = sort(G%v%'bet_filter', decreasing = TRUE)[75]
    G <- G %s% which(G %v% "bet_filter" >= bthreshhold)
  }
  
  if(largest_component == TRUE){
    ig <- intergraph::asIgraph(G)
    gclust<-igraph::clusters(ig, mode='weak')
    lcc<-igraph::induced.subgraph(ig, igraph::V(ig)[which(gclust$membership == which.max(gclust$csize))])
    G <- intergraph::asNetwork(lcc)
  }
  return(G)
}

get_centrality <- function(x, label_names = "Degree"){
  # Get centrality stats and set labels. Defaults to top 10 degree
  # can be changed to top 10 "Betweenness" by changing label_names = "Betweenness")
    centrality <- tibble(
    names = as.character(x%v%'vertex_names'),
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
  if (dim(adj)[1] != dim(adj)[2]) stop("not a square matrix")
  
  # The paper this is based on doesn't add 1 here and neither do other
  # open source implementations, but adding 1 allows us to not divide by 0 
  # and makes the rest of the function actually work.
  X <- as.matrix(X) + 1
  dyadc <- X + t(X)
  ## calculate on the basis of a uniform distribution.
  # see http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.490.7556&rep=rep1&type=pdf
  Dij <- X/dyadc-(((X/dyadc)-0.5)/(dyadc+1))
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
  
  #NormDS <- array(NormDS,dim=c(nrow(X),1),dimnames=c(list(names),"NormDS"))
  return(DS_1)
}


## generates a graph of degree betweenness
## residuals. Off-diagnal nodes can be worth talking
## about.
get_informal_leaders <- function(x){
  ## get centrality
  df <- x
  
  df$degree <- normalized(df$degree)
  df$betweenness <- normalized(df$betweenness)
  #df$hashtag_count <- normalized(df$hashtag_count)
  df %>% 
    filter(!degree < .05 & !betweenness < .05) %>% 
    ggplot(aes(degree, betweenness, label = names))+
    geom_point()+
    geom_text_repel()+
    ggtitle('Normalized Degree vs. Betweenness')+
    theme_tufte() +
    geom_line(aes(y=degree), linetype = "dashed")
  ggsave(paste0(output_folder, "centrality_residuals.png"), units= "in", width = 11, height = 9)
  
}


# Create Palettes ---------------------------------------------------------

# craete palettes- change as needed
#gec_pal <- gec_pal <- c("Anti-Referendum" = "#981B1E", "Pro-Referendum" = "#12ABB6",
#                        "Excluded" = "#969696")

# Create Network ----------------------------------------------------------

## generate an edgelist
## if this is an ego network, delete the ego.
edgelist <- get_agent_network(files)

full_list <- edgelist %>% 
  group_by(authors, target) %>% 
  count(sort = TRUE)

write.xlsx(full_list, file = paste0(output_folder, "full_connections.xlsx"))


## it's not recommended to use 0 for networks of over 500 vertices
interactions = recommended_interactions(edgelist)


## create a network
#betweenness takes a long time to calculate. if it's taking a long time,
# you can either comment out betweenness or increase the number of interactions
# that are required. it's better to start with a low interaction_level: (eg 0.5, 1, 2) 
G <- create_network(edgelist, interactions, largest_component = giant_component,
                    betweenness_filter = filter_betweenness)

## get centrality statistics centrality[[1]] is the full tabel
## centrality[[2]] contains the names of the top 10 authors by degree
centrality <- get_centrality(G, label_names = "Degree")

# f<- centrality[[1]] %>% arrange(desc(degree))
get_informal_leaders(centrality[[1]])

# Visualize ---------------------------------------------------------------

# specify minimum number of interactions and create network.
# increasing interactions reduces the size of the network and surfaces accounts
# with high degree


# plot with top 10 degree labeled
ggplot(G, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(color = "grey50", alpha = 0.2) +
  geom_nodes(color = "#12ABB6", size = 7, alpha = 0.8) +
  scale_size_area(max_size = 8, guide = F)+
  #scale_color_manual(values = gec_pal)+
  geom_nodelabel_repel(aes(label = vertex.names),
                       box.padding = unit(1, "lines"),
                       size = 6,
                       data = function(x) { x[ x$vertex.names %in% centrality[[2]], ]})+
  theme_blank()+
  theme(legend.text=element_text(size=12)) 
        #legend.position = "top",
        #legend.title = element_blank()) +
  #ggtitle(sprintf("Most central Agents in a %s-interaction network", as.character(interactions)))
  #theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5))
#ggtitle("Most central Agents in full network")
ggsave(paste0(output_folder, sprintf('full_unlabeled_%s.png', as.character(interactions))), height = 9, width = 11, units = "in")


# Get David Scores --------------------------------------------------------
# higher david scores mean accounts send a high volume of tweets to many different individuals
# without being often tagged themselves. Low david scores mean accounts are tagged or rt'd frequently, by
# many differenct accs but do not tag or retweet back 
trimmed_edgelist <- agent_filter(edgelist, limit=interactions)
adj <- as.data.frame(as.matrix(igraph::get.adjacency(igraph::graph.data.frame(trimmed_edgelist, directed = TRUE))))
DS1 <- getDS(adj)

# Centrality table --------------------------------------------------------

# get centrality stats for a network of x interactions
centrality[[1]] <- centrality[[1]] %>% left_join(DS1)
list_of_datasets <- list("bw_centrality" = centrality[[1]])
write.xlsx(list_of_datasets, file = paste0(output_folder, sprintf("bw_centrality_%s.xlsx", as.character(interactions))))



# custom labels -----------------------------------------------------------
if (pulled_list_of_accs > 0 ){
  ## if pulled_list_of_accs is true, this will create a labeled graph of
  ## the authors originally pulled (that are in the network at your
  ## chosen threshhold). df needs to be a csv with account names pulled in the first
  ## column (@ signs included)!
  
  ## Accounts names you pulled need to be in the first column
  df <- read_csv(path_to_acc_list)
  colnames(df)[1] <- "Account"
  df$Account <- tolower(df$Account)
  #df$Account <- ifelse(grepl('^@', df$Account), df$Account, paste0('@', df$Account))
  df <- df %>%  mutate(names = tolower(Account),
                       category = orig_accounts) %>%
    select(names, Account, category)
  # vertex names
  newdf <- tibble(
    names = G%v%'vertex.names')
  ndf2 <- newdf %>% left_join(df) %>% na.omit()
  ndf2$newor <- ifelse(is.na(ndf2$Account), other_accounts, orig_accounts)
  G%v%'Orientation' <- ifelse(G%v%'vertex.names' %in% ndf2$names, ndf2$newor, other_accounts)
  
  ggplot(G, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_edges(color = "black", alpha = 0.6) +
    geom_nodes(aes(color = Orientation), size = 7, alpha = 1) +
    scale_size_area(max_size = 8, guide = F)+
    scale_color_manual(values = c("#981B1E","#969696"))+
    geom_nodelabel_repel(aes(label = vertex.names),
                         box.padding = unit(1, "lines"),
                         size = 6,
                         data = function(x) { x[ x$vertex.names %in% ndf2$names, ]})+
    theme_blank()+
    theme(legend.text=element_text(size=12)) 
  
  # output folder is # of interactions
  ggsave(paste0(output_folder, sprintf('orig_pulled_%s.png', as.character(interactions))), height = 9, width = 11, units = "in",
         dpi = 100)
}


## Visualizes the top 10 accounts that were not originally pulled
if (pulled_list_of_accs > 0 ){
  cent <- centrality[[1]]
  # Top 10 degree labels- not originally pulled
  labels <- newdf %>% left_join(cent) %>% left_join(df) %>% filter(is.na(Account)) %>% 
    arrange(desc(degree)) %>% slice(1:10,) %>% .$names
  ndf_2 <- newdf %>% left_join(df)
  ndf_2$newor <- ifelse(is.na(ndf_2$Account), other_accounts, orig_accounts)
  baddies <- ndf_2 %>% filter(newor == other_accounts) %>% .$names

  ndf_3 <- ndf_2 %>% filter(ndf_2$newor == other_accounts)
  G%v%'Orientation' <- ifelse(G%v%'vertex.names' %in% ndf_2$names, ndf_2$newor, other_accounts)

  ggplot(G, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_edges(color = "black", alpha = 0.6) +
    geom_nodes(aes(color = Orientation), size = 7, alpha = 1) +
    scale_size_area(max_size = 8, guide = F)+
    scale_color_manual(values = c("#981B1E","#969696"))+
    geom_nodelabel_repel(aes(label = vertex.names),
                        box.padding = unit(1, "lines"),
                        size = 6,
                        data = function(x) { x[ x$vertex.names %in% labels, ]})+
    theme_blank()+
    theme(legend.text=element_text(size=12)) 
  ggsave(paste0(output_folder, sprintf('other_pulled_%s.png', as.character(interactions))), height = 9, width = 11, units = "in",
         dpi = 100)
}


# custom colors and labels -----------------------------------------------------------

custom_graph = FALSE

## Custom color/ legend guide:
# to create custom labels, you'll need to do 2 things:
# 1: create a csv with 2 columns where the first column contains account names and the second column
# contains labels. Point to that file here:

path_to_nodes_categorized = "/Users/christian.conroy/OneDrive - Accenture Federal Services/Documents/StateGec/Brandwatch/eswatini/Data/eswatini_cat.csv"

# 2: each label will need a custom color hexcode:
# NB: any node that does not recevie a will need its own category in the palette above.
# e.g. if you call unlabeled_node names "Other", you'd need ("Other" = "#E4E5E6")
gec_pal <- gec_pal <- c("Official Government" = "#F9C642", "Credible" = "#205493", "Not Credible" = "#981B1E",
                        "Other" = "#969696")

unlabeled_node_names = "Other"

if (custom_graph > 0){
  df <- read_csv(path_to_nodes_categorized)
  colnames(df)[1:2] <- c('names', 'ref')
  df <- df %>% distinct() #drop duplicates - these cause problems
  df$names <- tolower(df$names)
  df$names <- ifelse(grepl('^@', df$names), df$names, paste0('@', df$names))
  # run the following code as is:
  newdf <- tibble(names = G%v%'vertex.names')
  ndf2 <- newdf %>% left_join(df)
  ndf2$newor <- ifelse(is.na(ndf2$ref), unlabeled_node_names, ndf2$ref)
  G%v%'Orientation' <- ifelse(G%v%'vertex.names' %in% ndf2$names, ndf2$newor, NA)
  ggplot(G, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_edges(color = "black", alpha = 0.6) +
    geom_nodes(aes(color = Orientation), size = 7, alpha = 1) +
    scale_size_area(max_size = 8, guide = F)+
    scale_color_manual(values = gec_pal)+
    geom_nodelabel_repel(aes(label = vertex.names),
                       box.padding = unit(1, "lines"),
                       size = 6,
                       data = function(x) { x[ x$vertex.names %in% df$names, ]})+
    theme_blank()+
    theme(legend.text=element_text(size=12))
  ggsave(paste0(output_folder, sprintf('final_ntwk_%s.png', as.character(interactions))), height = 9, width = 11, units = "in",
       dpi = 100)
}

########################## Supplemntary Analysis #########################
bw_df = read_excel(paste0(output_folder, "/Static_3/bw_centrality_6.xlsx"))

bw_df_inauthentic = bw_df[bw_df$names %in% grep("[0-9]{8}", bw_df$names, value=T),]

raw <- list.files(path = './',
                  pattern = "*.feather",
                  full.names = T) %>% 
  map_df(function(x) read_feather(x) %>% mutate(filename=gsub(".feather","",basename(x))))

write.csv(raw, paste0(output_folder, "/full_afghanistan_tweets.csv"))

colnames(raw)
bw_accounts <- raw[,c(1,4,43:66)]
colnames(bw_accounts)
bw_accounts = bw_accounts[!duplicated(bw_accounts$user_id), ]
bw_accounts$account_created_at

bw_accounts$account_created_at <- as.Date(bw_accounts$account_created_at)

ggplot(bw_accounts, aes(x=account_created_at)) + geom_histogram(binwidth=30, colour="white") +
  ylab("Frequency") + xlab("Year and Month")
ggsave(paste0(output_folder, "creation_dates.png"), height = 9, width = 11, units = "in",
       dpi = 100)

account_creation_table <- bw_accounts %>% group_by(bw_accounts$account_created_at) %>% count(sort = TRUE)

account_friends_table <- bw_accounts %>% group_by(bw_accounts$friends_count) %>% count(sort = TRUE)

account_followers_table <- bw_accounts %>% group_by(bw_accounts$followers_count) %>% count(sort = TRUE)

