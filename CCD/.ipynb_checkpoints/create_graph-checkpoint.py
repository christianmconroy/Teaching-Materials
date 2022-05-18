import networkx as nx
import pandas as pd
from itertools import combinations
import numpy as np


def create_edgelist(df):
    """
    This function imports data and calculates tweet overlap for each author
    """
    
    node_col = 'screen_name'
    edge_col = 'textnormed'
    # don't calculate permutations of connections for tweets 
    # sent by over 500 authors. How many dyadic combinations are there for
    # 500 authors?
    max_content_edges=500
    nodes = {}
    # edgecounts = [] # looks to be not used in any code, probably needs deletion.  needs further review
    exceeded = []

    G = nx.Graph() # creates empty graph object
    
    # Group by on the edge, aggregate and list the accounts
    edgedf = df[[node_col, edge_col]].groupby(edge_col).agg({
        node_col: lambda k: list(k)})
    
    # Per cleaned tweet (textnormed) count the number of occurences of the other vars. 
    # Essentially, how many accounts tweeted this.
    weights = df.groupby(edge_col).count()
    
    # Reset the weights dataframe by creating a new index.  The old index becomes a column, 
    # giving each unique tweet its only index value.  Then sort by the number of screen names 
    # field ascending.
    weights = weights.reset_index().sort_values(node_col).reset_index()
    
    # Creates a 'rank' column.  Each unique is given a unique ranking based upon 
    # the number of times it was tweeted.  The more tweets, the higher the ranking.
    weights['rank'] = weights.index
    # Reset the index to be the textnormed field.    
    weights.set_index(edge_col, inplace=True)

    print('Calculating weights...')
    for idx, row in edgedf.iterrows():
        # takes the rank of the rank of the unique tweet, divides it by the 
        # number of unique tweets in the data set.  The higher the rank 
        # (the more accounts that tweeted the tweet) the higher the pctile
        pctile = 100 * (weights.loc[idx]['rank'] / float(len(weights)))
        # inverts it, so more accounts equals lower pctile
        
        if pctile > 0:
            eweight = 1 / pctile
        elif pctile == 0:
            eweight = 0
         
        # number of accounts associated with the tweet * {the number of accounts associated with the tweet - 1} 
        # and then divided by 2. This calculates total possible undirected edges.
        n_edges = (len(row[node_col]) * (len(row[node_col]) - 1)) / 2.0

        # place tweets that are associated with too many accounts in an error output file
        if n_edges > 500:
            exceeded.append((row, n_edges))
            continue

        # for each unique tweet, creates a list of all possible pairings of the 
        # accounts that sent it
        edges = list(combinations(row[node_col], 2))

        # for each account pair of the shared tweets
        for sn1, sn2 in edges:
            if sn1 == sn2:
                continue

            # if either account is not in the nodes dictionary, add it
            if sn1 not in nodes:
                nodes[sn1] = {}
            if sn2 not in nodes:
                nodes[sn2] = {}


            if sn2 not in nodes[sn1]:
                nodes[sn1][sn2] = {'weight': eweight, 'overlap': 1}
            else:
                nodes[sn1][sn2]['weight'] += eweight
                nodes[sn1][sn2]['overlap'] += 1

            # if account 1 is not in the dictionary, add it along with the eweight.
            # if it is, update the dictionary by adding the weight and increasing the 
            # overlap
            if sn1 not in nodes[sn2]:
                nodes[sn2][sn1] = {'weight': eweight, 'overlap': 1}
            else:
                nodes[sn2][sn1]['weight'] += eweight
                nodes[sn2][sn1]['overlap'] += 1            
          
    return edgedf, nodes

def make_graph(df):
    """
    Builds the graph object and adds the edge attributes we generated in create_edgelist
    """
    edgedf, nodes = create_edgelist(df)
    G = nx.Graph()
    
    # this loops through the nodes dictionary and iteratively updates edge attributes in G
    for sn1, edges in nodes.items():
        for sn2, weight in edges.items():
            if sn1 in G and sn2 in G[sn1]:
                G[sn1][sn2]['weight'] += round(weight['weight'], 3)
                G[sn1][sn2]['overlap'] += weight['overlap']
            else:
                G.add_edge(sn1, sn2, weight=round(weight['weight'], 3),
                           overlap=weight['overlap'])
    
    return G

def connected_component_subgraphs(G):
    """
    returns a generator object containing all distinct connected components in our graph
    """
    for c in nx.connected_components(G):
        yield G.subgraph(c)

def extract_components(df):
    """
    This function takes in a graph object 'G' and extracts the giant component. It will then
    create a list of all authors NOT in the giant component, and create a separate graph 
    object using the smaller components.
    """
    
    # create the graph object
    G = make_graph(df)
    
    ## pull out the giant component
    giant = max(connected_component_subgraphs(G), key=len)
    
    # pull out all components
    connected_components = list(connected_component_subgraphs(G))
    glength = [len(g) for i, g in enumerate(connected_components)]
    connected_components.pop(glength.index(max(glength)))
    
    # get a list of authors outside giant component
    residual_authors = [g.nodes() for i, g in enumerate(connected_components)]
    
    # recompose those authors into a graph object called 'F'. This object can be treated differently
    F = []
    for i in range(len(connected_components)-1):
        connected_components[0] = nx.compose(connected_components[0], connected_components[i+1])
        F = connected_components[0]
    
    # create a dictionary of components excluding the giant. Each one is a "cluster"
    res_names = []
    res_component = []
    for i, x in enumerate(residual_authors):
        for n, z in enumerate(x):
            res_names.append(z)
            res_component.append("Component_" + str(i+1))
    res_dict = dict(zip(res_names, res_component))
    
    return G, F, res_dict


def calc_cluster_coef(G, df):
    """
    Generates coefficients for clusters. takes networkx graph object and a dataframe
    df.clust an integer <= the number of clusters we define in cluster_helper
    
    Path:
    get_all_coef -> cluster_helper -> run_app
    """
    
    
    clusters_unique = df.drop_duplicates(subset=['screen_name', 'cluster_id'])
    # get author totals and tweet-level totals
    author_tots = clusters_unique.groupby(['cluster_id']).size()
    avg_auth_score = clusters_unique.groupby(['cluster_id'])['author_temporal_score'].agg('mean')
    avg_tweet_scores = df.groupby(['cluster_id'])['tweet_temporal_score'].agg('mean')
    
    # calculate graph stats
    gmeta = {}

    for i, cluster in enumerate(df.cluster_id.unique()):
        sns = df[df.cluster_id == cluster].screen_name.values
        subg = G.subgraph(sns)
        elist = list(subg.edges(data=True))
        attrdf = pd.DataFrame([e[2] for e in elist])

        gmeta[cluster] = {}
        gmeta[cluster]['internal_edge_count'] = subg.number_of_edges()
        gmeta[cluster]['internal_author_count'] = len(subg.nodes())

        if subg.number_of_edges() == 0:
            # these nodes were not clustered optimally or arent part of a community
            continue

        gmeta[cluster]['avg_int_author_overlap'] = attrdf.overlap.mean() #calculated in create_nx_filtered

    gmdf = pd.DataFrame(gmeta).transpose()
    
        # create a proportion dataframe
    cstats = pd.DataFrame({'avg_tweet_scores':avg_tweet_scores, 'accs_in_cluster':author_tots,
                      'avg_auth_score':avg_auth_score, 'internal_edge_count':gmdf.internal_edge_count,
                          'internal_author_count':gmdf.internal_author_count,
                           'avg_int_author_overlap':gmdf.avg_int_author_overlap})
    
    return cstats
    