from import_data import import_bw_data, import_ch_data, only_preprocess, import_rtweet
from create_graph import extract_components, calc_cluster_coef
from temporal_coord import get_temporal_weights
from get_cotweets import run_cotweet_net
from detect_clusters import run_nmf
import matplotlib.pylab as plt
import scipy.sparse as sparse
import pandas as pd
import os

def create_contour_plots(df, export_path):
    """
    Creates weighted contour plots for each cluster in the final dataset.
    This only runs on the giant component as only the giant component is assigned clusters.
    """
    
    print('')
    # format subplots
    cluster_num = int(df.cluster_id.dropna().nunique())
    figure_num = int(cluster_num)
    if figure_num % 2 == 0:
        pass
    else:
        figure_num += 1
    figure_rows = int(figure_num/2)
    fig, axs = plt.subplots(nrows=figure_rows, ncols=2, 
                            figsize=(50, 40), facecolor = 'w', edgecolor = 'k', dpi = 200)
    fig.subplots_adjust(hspace = .5, wspace=.05)
    axs = axs.ravel()
    
    for i in range(cluster_num):
        # create bipartite matrices for each cluster
        prep = df[df.cluster_id == i]
        prep = prep[['screen_name', 'textnormed', 'L2_norm']].copy()
        prep = pd.crosstab(index = prep.screen_name, columns = prep.textnormed,
                           values = prep.L2_norm, aggfunc = 'mean').fillna(0)
        
        # handle clusters with only 1 author
        if prep.shape[0] < 2:
            pass
            print(f'only one author in cluster {i} - cannot visualize')
        else:
            # visualize the contours
            axs[i].contour(prep, cmap=plt.cm.inferno)
            axs[i].set_title("cluster " + str(i))
        
    plt.savefig(str(export_path) + 'clusters_weighted.png')


def run_custom_ccd(df, export_path = "../output/"):
    """
    This function runs the entire ccd script and preps the final dataframe for export
    """
    df = only_preprocess(df)
    # extract the giant component
    G, F, residual_authors = extract_components(df)
    # get temporal dictionaries
    df, temporal_tweet_dict, temporal_author_dict = get_temporal_weights(df)    
    # weighted scores based on whether authors co-tweet or co-retweet 
    # within 2 minutes of one another. Higher is more suspicious
    df['tweet_temporal_score'] = df.textnormed.map(temporal_tweet_dict)
    # aggregate tweet_temporal scores by author. High scores mean frequent tweeting 
    # of identical content within 2 minutes of other authors.
    df['author_temporal_score'] = df.screen_name.map(temporal_author_dict)
    
    # run NMF
    cluster_dict = run_nmf(df, residual_authors)
    # clusters calculated by NMF
    df['cluster_id'] = df.screen_name.map(cluster_dict)
    # NMF is only run on the giant component. Additional components are labeled.
    df['components'] = df.screen_name.map(residual_authors).fillna('giant_component')
    
    for_m = df.sort_values('screen_name').groupby('screen_name').first().reset_index()
    for_m['distinct_screen_name']= 1
    df = pd.merge(df, for_m[['screen_name','created_at', 'post_id', 'distinct_screen_name']], how = 'left', on=['screen_name','created_at', 'post_id'])
    df['distinct_screen_name'] = df['distinct_screen_name'].fillna(0)
    
    for_m = df.sort_values('textnormed').groupby('textnormed').first().reset_index()
    for_m['distinct_textnormed']= 1
    df = pd.merge(df, for_m[['screen_name','created_at', 'post_id', 'distinct_textnormed']], how = 'left', on=['screen_name','created_at', 'post_id'])
    df['distinct_textnormed'] = df['distinct_textnormed'].fillna(0)
    
    for_m = df.sort_values('text').groupby('text').first().reset_index()
    for_m['distinct_text']= 1
    df = pd.merge(df, for_m[['screen_name','created_at', 'post_id', 'distinct_text']], how = 'left', on=['screen_name','created_at', 'post_id'])
    df['distinct_text'] = df['distinct_text'].fillna(0)
    
    
    # if you don't specify an output path, this will create a folder called output
    # in the parent directory
    if export_path == "../output/":
        try:
            os.mkdir(export_path)
        except OSError:
            pass
    
    # create cluster contour plots
    create_contour_plots(df, export_path)
    
    # generate cotweet network
    cotweet_df = run_cotweet_net(df, export_path)
    
    # caculate cluster-level stats
    cstats = calc_cluster_coef(G, df)
    
    writer = pd.ExcelWriter(export_path + 'ccd.xlsx', 
                            engine = 'xlsxwriter', options={'strings_to_numbers': False})
    
    cstats.to_excel(writer, sheet_name = 'ClusterStats')
    df.to_excel(writer, sheet_name = 'TweetStats', index=False)
    cotweet_df.to_excel(writer, sheet_name = 'Co_tweets', index = False)  
    writer.save()
    writer.close()
    print('Updated script!')
    print('Done!')
    
    return df