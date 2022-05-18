import pandas as pd
import numpy as np
pd.set_option('mode.chained_assignment', None)


def get_temporal_weights(df):
    """
    This function takes a pandas dataframe. The function iterates
    through the dataframe, and adds weight to authors for posting identical content within
    a short time frame of other authors. Authors with highly positive scores have frequently posted
    at nearly the same time as other authors. Weights increase for subsequent matches. To offset the
    advantage the loop gives the first poster, subsequent authors that meet the criterion are also given
    some weight.
    """
    time_df = df.copy()
    time_df['time_vals'] = 1

    time_df = (time_df[['screen_name','textnormed',  'time_vals', 'created_at', 'post_id']]
               .copy()
               .drop_duplicates(subset=['screen_name','textnormed'])
               .sort_values(['textnormed', 'created_at'], ascending = False)
               .reset_index())
    time_df['created_at'] = pd.to_datetime(time_df['created_at'])
    for i in range(0, (len(time_df)-1), 1):
        # don't run on blanks. This would overweight them
        if time_df.textnormed.iloc[i] != "":
            try:
                # penalize if 2 authors in a row tweet the same content within {0.15, 1, 2} 
                # mins of one another
                # penalties are double-counted and increase through the loop
                if time_df.textnormed.iloc[i] == time_df.textnormed.iloc[i+1]:
                    if (time_df.created_at.iloc[i] - time_df.created_at.iloc[i+1])/np.timedelta64(1,'m') < 0.15:
                        time_df.time_vals.iloc[i] += 3
                        time_df.time_vals.iloc[i+1] += 2
                    elif (time_df.created_at.iloc[i] - time_df.created_at.iloc[i+1])/np.timedelta64(1,'m') < 1:
                        time_df.time_vals.iloc[i] += 2
                        time_df.time_vals.iloc[i+1] += 1
                    elif (time_df.created_at.iloc[i] - time_df.created_at.iloc[i+1])/np.timedelta64(1,'m') < 2:
                        time_df.time_vals.iloc[i] += 1
                        time_df.time_vals.iloc[i+1] += 1

                # penalize for if 3 authors in a row tweet the same content within {0.15, 1, 2} 
                # mins of one another
                if time_df.textnormed.iloc[i] == time_df.textnormed.iloc[i+2]:
                    if (time_df.created_at.iloc[i] - time_df.created_at.iloc[i+2])/np.timedelta64(1,'m') < 0.15:
                        time_df.time_vals.iloc[i] += 3
                        time_df.time_vals.iloc[i+2] += 2
                    elif (time_df.created_at.iloc[i] - time_df.created_at.iloc[i+2])/np.timedelta64(1,'m') < 1:
                        time_df.time_vals.iloc[i] += 2
                        time_df.time_vals.iloc[i+2] += 1
                    elif (time_df.created_at.iloc[i] - time_df.created_at.iloc[i+2])/np.timedelta64(1,'m') < 2:
                        time_df.time_vals.iloc[i] += 1
                        time_df.time_vals.iloc[i+2] += 1

                # if 4 authors in a row tweeted the exact same content within {0.15, 1, 2} 
                # minutes of one another, repeatedly,
                # something strange is going on. This adds a big penalty for a quadruple
                if time_df.textnormed.iloc[i] == time_df.textnormed.iloc[i+3]:
                    if (time_df.created_at.iloc[i] - time_df.created_at.iloc[i+3])/np.timedelta64(1,'m') < 0.15:
                        time_df.time_vals.iloc[i] += 5
                        time_df.time_vals.iloc[i+3] += 5
                    elif (time_df.created_at.iloc[i] - time_df.created_at.iloc[i+3])/np.timedelta64(1,'m') < 1:
                        time_df.time_vals.iloc[i] += 4
                        time_df.time_vals.iloc[i+3] += 4
                    elif (time_df.created_at.iloc[i] - time_df.created_at.iloc[i+3])/np.timedelta64(1,'m') < 2:
                        time_df.time_vals.iloc[i+3] += 3
                        time_df.time_vals.iloc[i] += 3
            except:
                continue
    # get author totals\
    #tdf_agg = time_df.copy().groupby('textnormed')['time_vals'].sum().to_dict()
    #time_df['tmp'] = time_df.textnormed.map(tdf_agg)
    #time_df['time_vals'] = time_df['time_vals']/time_df['tmp']
    #time_df.drop(columns = ['tmp'])
    #time_df['time_vals'] = (time_df['time_vals'])            
    #author_totals = (time_df.groupby(['screen_name'])['time_vals']
    #         .sum()
    #         .reset_index()
    #         .sort_values('time_vals', ascending = False))
    
    # we want to create a transition matrix without wasting memory.
    # the transition matrix will allow us to downweight retweet activity
    rt_deweight = time_df.groupby(['textnormed'])['time_vals'].sum().to_dict()
    ac_deweight = time_df.groupby(['screen_name'])['time_vals'].sum().to_dict()
    

    time_df['post_sum'] = time_df.textnormed.map(rt_deweight)
    time_df['acc_sum'] = time_df.screen_name.map(ac_deweight)
    time_df['post_norm'] = time_df.time_vals/ time_df['post_sum']
    time_df['acc_norm'] = time_df.time_vals/ time_df['acc_sum'] 

    # final stage of the normalization
    time_df['L2'] = np.linalg.norm(time_df[['post_norm','acc_norm']].values,axis=1)
    time_df['normed_score'] = time_df['L2'] * time_df['time_vals']

    # try w/ averaging
    #time_df['normed_score'] = ((time_df['post_norm']+time_df['acc_norm'])/2) * time_df['time_vals']
    
    L2_norm_dict = time_df['normed_score'].to_dict()

    # weights will be the sum of of dimension that was not stochastically normalized
    temporal_author_dict = time_df.groupby(['screen_name'])['post_norm'].sum().to_dict()
    temporal_tweet_dict = time_df.groupby(['textnormed'])['acc_norm'].sum().to_dict()
    df['L2_norm']=df.index.to_series().map(L2_norm_dict)
    # weights will be the sum of of dimension that was not stochastically normalized
    #temporal_author_dict = time_df.groupby(['screen_name'])['post_norm'].sum().to_dict()
    #temporal_tweet_dict = time_df.groupby(['textnormed'])['acc_norm'].sum().to_dict()
    # weights will be the sum of of dimension that was not stochastically normalized
    #temporal_author_dict = time_df.groupby(['screen_name'])['post_norm'].sum().to_dict()
    #temporal_tweet_dict = time_df.groupby(['textnormed'])['acc_norm'].sum().to_dict()
    
    
    # convert to dictionary
    #temporal_author_dict = pd.Series(author_totals.time_vals.values,index=author_totals.screen_name).to_dict()
    #temporal_tweet_dict = pd.Series(time_df.time_vals.values, index = time_df.post_id).to_dict()
    
    return df, temporal_tweet_dict, temporal_author_dict