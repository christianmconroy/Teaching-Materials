from sklearn.decomposition import NMF
from sklearn.metrics import silhouette_score
import numpy as np
from scipy.stats import entropy
import pandas as pd
import warnings
warnings.filterwarnings("ignore")

# create cluster eval function

def run_nmf(df, residual_authors):
    """
    This creates a bipartite adjacency Matrix and drops authors outside the main component
    """
    # create bipartite adjacency matrix where authors are indices and textnormed are column names
    # matrices are weighted by temporal scores
    
    print('')
    print('Finding optimal clusters:')
    
    prep = df[['screen_name', 'textnormed', 'L2_norm']].copy()
    prep = pd.crosstab(index = prep.screen_name, columns = prep.textnormed,
                       values = prep.L2_norm, aggfunc = 'mean').fillna(0)
    
    # drop the authors outside the largest component (they are handled 
    # explicitly in create_graph.extract_components())
    to_drop = list(residual_authors.keys())
    prep = prep[~prep.index.isin(to_drop)]
    
    # prepare and run NMF
    
    k_list = []
    mse_list = []
    sil_list = []
    
    #go as high (within reason) or low as you want (you will have to wait)
    for i in range(10, 26, 2): 
        content_model = NMF(n_components=i)
        WC = content_model.fit_transform(prep.fillna(0))
        HC = content_model.components_

        # save clusters and calculate statistics
        clust_df = pd.DataFrame({"screen_name":prep.index})
        clust_df['clust'] = pd.DataFrame(WC).idxmax(axis=1).values
        clust_df['entropy'] = pd.DataFrame(WC).apply(lambda k:entropy(k)/np.log(len(k)),axis=1).values

        # generate reconstruction Matrix to check mse
        err = prep.values - pd.DataFrame(np.dot(WC,HC))
        mse = ((err*err).sum(axis=1)/err.shape[1]).mean()

        # calculate silhouette scores
        cluster_membership = pd.DataFrame(WC).idxmax(axis=1).values
        sil = silhouette_score(prep.fillna(0), cluster_membership)

        # param save the mse
        print(f'cluster: {i}, mse: {mse}, silhouette: {sil}')
        sil_list.append(sil)
        mse_list.append(mse)
        k_list.append(i)
        
    # get the index of the smallest non-negative silhouette score. Silhouette scores look at
    # boundaries of clusters on a scale of [-1, 1]. Scores above 0 mean that clusters are relatively
    # separate. less then 0 means that clusters, on average, are overlapping. 
    
    # silhouette scores are going to be small no matter what we do given that we're working
    # with a connected component. Our optimal cluster choice will be the cluster the closest to 0.
    # this will minimize the non-overlapping boundaries between clusters
    arr_test = np.array(sil_list)
    try:
        valid_idx = np.where(arr_test >= 0)[0]
        out = valid_idx[arr_test[valid_idx].argmin()]
    except:
        out = arr_test.argmax()

    k_list = np.array(k_list)
    # get final number of clusters
    number_of_clusters = k_list[out]
    
    print(f'Silhouette Optimal number of clusters: {number_of_clusters}')
    print('Decomposing with optimal cluster number...')
    
    # rerun NMF with optimal cluster
    content_model = NMF(n_components=number_of_clusters)
    WC = content_model.fit_transform(prep.fillna(0))
    HC = content_model.components_

    # save clusters and calculate statistics
    clust_df = pd.DataFrame({"screen_name":prep.index})
    clust_df['clust'] = pd.DataFrame(WC).idxmax(axis=1).values
    clust_df['entropy'] = pd.DataFrame(WC).apply(lambda k:entropy(k)/np.log(len(k)),axis=1).values

    # generate reconstruction Matrix to check mse
    err = prep.values - pd.DataFrame(np.dot(WC,HC))
    mse = ((err*err).sum(axis=1)/err.shape[1]).mean()

    # calculate silhouette scores
    cluster_membership = pd.DataFrame(WC).idxmax(axis=1).values
    
    # create a dictionary of authors and their clusters
    clusters = pd.Series(cluster_membership,index=prep.index).to_dict()
    
    return clusters