import import_data
import pandas as pd
import networkx as nx
from scipy.sparse import csr_matrix
import matplotlib.pyplot as plt
import os
from temporal_coord import get_temporal_weights

def find_cotweets(df):
    """
    takes what's generated in the data import step and creates a dataframe
    If no authors co-tweet each other at least 3 times, we don't have enough information
    to look at cotweets, and this script will not be run.
    
    df: data import with textnormed and screen_name columns
    """
    # pull out co-tweets and count them
    df['retweets'] = df.text.str.startswith(('RT '))*1
    cotweets = (df[df.retweets < 1].groupby(['textnormed'])
    .size()
    .reset_index()
    .sort_values([0], ascending = False))
    
    # take tweets that were tweeted more than 2 times
    cotweets = cotweets[cotweets[0] >= 2]
    cotweets.columns = ['textnormed', 'count']
    cotweet_df = df[df.textnormed.isin(cotweets.textnormed.values)]
    cotweet_df = cotweet_df[cotweet_df.textnormed != ""]
    # some of the textnormeds match retweets. drop the retweets
    cotweet_df = cotweet_df[cotweet_df.retweets == 0]
    
    # udf2 will be an exportable dataframe for analysis where
    udf = cotweet_df.drop_duplicates(subset=['screen_name', 'textnormed'])
    
    # drop duplicates
    ndf2 = udf[['screen_name', 'textnormed']].drop_duplicates(subset=['screen_name', 'textnormed'])
    
    # pull find all author-pairs who've co-tweeted at least 3 different times
    groupndf = ndf2.groupby('screen_name').size().reset_index()
    groupndf = groupndf[groupndf[0]>=2].screen_name
    
    # this is for creating the edgelist
    ndf2 = ndf2[ndf2.screen_name.isin(groupndf.values)]
    
    # this is for analysts - it contains all cotweeted tweets
    udf2 = udf[udf.screen_name.isin(groupndf.values)]
    return ndf2, udf2

def incidence_edgelist(df):
    """
    filter out any screen names that appear fewer than 2 times. Then 
    reate an agent to agent matrix where nodes are authors and edges are the tweets that connect them.
    
    Pandas is not made for sparse matrix multiplication and it's prohibitively slow. So we'll use
    scipy's sparse matrix format for the dot product calculation
    """
    # create adjacency matrix
    ndf3 = pd.crosstab(df.screen_name, df.textnormed).fillna(0)
    
    # extract node labels
    label_mapping = {idx: val for idx, val in enumerate(ndf3.index)}
    
    # project the matrix to an author-to-author matrix
    ndf4 = csr_matrix(ndf3).dot(csr_matrix(ndf3.transpose())) 
    adj = pd.DataFrame(ndf4.todense())
    
    # create graph object and clean
    G = nx.DiGraph(adj.values)
    G = nx.relabel_nodes(G, label_mapping) #
    G.remove_edges_from(nx.selfloop_edges(G)) # drop self loops
    G = G.to_undirected()
    
    # export edgelist of graph to pandas
    edgelist = nx.to_pandas_edgelist(G)
    return edgelist

def top_n_cotweeters(edgelist, n = 150):
    """
    Takes a weighted edgelist and iteratively filters by weight until there are
    a maximum of 150 rows.
    """
    threshhold = 2
    while len(edgelist) > n:  
        edgelist = edgelist[edgelist['weight'] > threshhold]
        threshhold += 1
    edgelist = edgelist.sort_values(['weight'], ascending = False)
    print(f'Authors at this threshhold co-tweeted at least {edgelist.weight.min()} times')
    G = nx.from_pandas_edgelist(edgelist)
    return G, edgelist

def connected_component_subgraphs(G):
    """
    returns a generator object containing all distinct connected components in our graph
    """
    for c in nx.connected_components(G):
        yield G.subgraph(c)
        
def create_cotweet_graph(G, export_path):
    """
    Creates a graph that colors the 5 largest components of cotweeting authors at
    the threshhold determined by top_n_cotweeters. Return a dictionary of authors 
    and their colors
    """
    
    ## extract the indices of the 5 largest components
    glength = [len(g) for i, g in enumerate(list(connected_component_subgraphs(G)))]
    tograph = pd.DataFrame({'glength':glength}).sort_values(['glength'], ascending= False).index[0:5]
    # plot the figure - we need to iterate over different components to color them differently
    
    fig = plt.figure(figsize=(20,20))
    pos = nx.spring_layout(G, k = .6)
    colorlist = [ 'r', 'g', 'b', 'c', 'm']
    for idx, val in enumerate(tograph):  #there's probably a more elegant approach using zip
        nx.draw_networkx(list(connected_component_subgraphs(G))[val], pos = pos, 
                         edge_color = colorlist[idx], node_color = colorlist[idx])
    plt.tight_layout()
    plt.savefig(export_path + "cotweeters.png", format="PNG", dpi = 300)
    
    ## get a dictionary of authors: colors to map to the dataframe
    color_pre_dict = []
    node_list = []
    color_fullname = {"r":"red", "b":"blue", "g":"green", "m":"magenta", "c":"cyan"}
    for i, val in enumerate(tograph):
        subgraph = list(connected_component_subgraphs(G))[val].nodes()
        for j, node in enumerate(subgraph):
            node_list.append(node)
            color_pre_dict.append(colorlist[i])
    colors_final = [color_fullname.get(c, c) for c in color_pre_dict]
    node_color_dict = {node_list[i]:colors_final[i] for i, val in enumerate(colors_final)}
    return node_color_dict
    
def run_cotweet_net(df, export_path):
    """
    runs all functions in this module.
    
    df: dataframe imported from import_data
    """
    cotweet_edgelist, cotweet_df = find_cotweets(df)
    
    if len(cotweet_df) > 2:
    
        # get temporal weights and merge in those columns
        cotweet_df, tweet_dict, author_dict = get_temporal_weights(cotweet_df)
        cotweet_df['co-author_temporal_score'] = cotweet_df.screen_name.map(author_dict)
        cotweet_df['co-tweet_temporal_score'] = cotweet_df.post_id.map(tweet_dict)
        
        # export the edgelist and the graph
        edgelist = incidence_edgelist(cotweet_df)
        G, t_edgelist = top_n_cotweeters(edgelist)
        
        # if you want the cotweet edgelist
        #t_edgelist.to_csv(export_path + 'cotweet_edgelist.csv', index = False)
        
        node_color_dict = create_cotweet_graph(G, export_path)
        cotweet_df['color'] = cotweet_df.screen_name.map(node_color_dict)
    
    else:
        print('No cotweeting is present in this dataset. \nConsider acquiring more data')
    
    return cotweet_df