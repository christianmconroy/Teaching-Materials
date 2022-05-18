import pandas as pd
import numpy as np
import glob
import re
import os
from nltk import word_tokenize
from cosine_similarity import group_by_cosine_similarity

## functions to preprocess text

def pre_normalize(nolink):
    """
    This function is used to posts that ONLY posted weblinks or ONLY posted usernames  
    
    Path:
    normalize -> ch_dataload -> run_app
    """
    # regex to detect ONLY URLs
    reg_only_link = re.compile('^http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\(\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+$')
    reg_user = re.compile('\@\w+')
    # makes this retweet agnostic
    if str(nolink).startswith('rt '):
        nolink = nolink[3:]
        
    # substitute GEC code for accounts that just shared a url.
    nolink = reg_only_link.sub('GEC Internal Code| Only Posted URL| Amaryllis | \
                               Alchemilla | Alstroemeria | Astrantia', str(nolink))
    nolink = reg_user.sub("", str(nolink))
    return nolink
        
        
def normalize(txt):
    """
    This function is used to remove web links, usernames, emojis, punctiation, and most special characters from a tweet.  
    param: a string
    """

    # regex to remove urls
    reg_link = re.compile('http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\(\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+')
    # regex to remove punctuation
    reg_punc = re.compile("""[`\!#$%&'()*\+,\-\.\/\:\;\<\=\>\?\@\[\]^`{|}~…]""")
    # regex to remove numbers
    reg_num = re.compile('[0-9]+')
    # regex to remove users
    reg_user = re.compile('\@\w+')

    # remove urls
    nolink = reg_link.sub('', str(txt).lower())
    # remove ampersands and apostrophes
    nolink = nolink.replace('&amp;', '')
    nolink = nolink.replace("'", '')
    # remove punctuation
    nolink = reg_punc.sub(' ', nolink)
    # remove numbers
    nolink = reg_num.sub('', nolink)
    
    # remove RT
    if nolink.startswith('rt '):
        nolink = nolink[3:]
    
    # sort alphabetically and tokenize each tweet as a set
    nolink = ' '.join(sorted(list(set(word_tokenize(nolink)))))
    
    # verify links have been removed
    nolink = reg_punc.sub(' ', nolink)
    nolink = nolink.lstrip(' ')
    return nolink

## FUNCTIONS TO IMPORT DATA

def import_bw_data(filepath, skiprows = 8):
    """
    Used to load in data from Crimson Hexagon. The function will take any number of files exported from
    Crimson Hexagon and will merge them into a single data frame as long as they are in the same folder.
    The function will then transform the data to the correct structure and normalize the text field.  
    It will also drop all authors (nodes) with less than 2 tweets.  
    
    Path:
    ch_dataload -> run_app
    """
    
    # This is a function intended for importing brandwatch data. As our datasources change,
    # this function should also change.
    print('Loading in data... this can take a while...')
    filelist = glob.glob(str(filepath) + '*.xlsx')
    fields = ['Author', 'Date', 'Full Text', 'Resource Id', 'Twitter Tweets', 'Url', 'Country',
              'Twitter Followers', 'Twitter Following']
    list_of_dfs = [pd.read_excel(flist, skiprows = skiprows, usecols = fields) for flist in
                   filelist]
    df = pd.concat(list_of_dfs, ignore_index=True)

    # rename to standard naming conventions
    df = df.rename(columns={'Date': 'Date (EST)', 'Full Text':'Contents', 
                            'Resource Id':'GUID', 'Twitter Tweets':'Posts',  "Url": 'URL', 
                            "Twitter Followers":"Followers", "Twitter Following": "Following"})
    #Data loaded output summary
    print('Data loaded in: ', len(df), " rows")
    # create dataframe of only necessary fields
    df = df[['Author','Date (EST)', 'Contents', 'GUID', 'Posts', 'Country',
             'URL', 'Followers', 'Following']]

    # rename to standard naming conventions
    df = df.rename(columns={'Author': 'screen_name', 'Date (EST)': 'created_at',
                            'Contents': 'text', 'GUID': 'post_id','Posts': 'statuses'})

    df = df.drop_duplicates()
    
    # preprocess the text -
    print('Cleaning the data...')
    df['tmp'] = df['text'].apply(lambda x: pre_normalize(x))
    df['textnormed'] = df['tmp'].apply(lambda x: normalize(x))
    df.drop('tmp', axis = 1, inplace = True)
    
    
    print('Filtering out noise...')
    
    threshhold = 3
    ## iteratively increase threshhold to only capture top 10k authors. This helps reduce noise. Might
    ## be worth going even lower than 10k for smaller datasets
    while len(set(df.screen_name)) > 10000:
        actives = df.groupby('screen_name').count().query('post_id>%s' % threshhold).index
        df = df[df.screen_name.isin(actives)]
        threshhold += 1
    print(f'Keeping authors with at least {threshhold+1} posts')
    
    threshhold = 3
    ## Now lets do the same for tweets. This helps reduce noise. 
    ## Again, might be worth going lower for smaller datasets.
    while len(set(df.textnormed)) > 10000:
        actives = df.groupby('textnormed').count().query('post_id>%s' % threshhold).index
        df = df[df.textnormed.isin(actives)]
        threshhold += 1
    print(f'Keeping tweets that appeared at least {threshhold+1} times')
    
    df = group_by_cosine_similarity(df)
    return df



def import_ch_data(filepath, skiprows = 1):
    """
    Used to load in data from Crimson Hexagon.  The function will take any number of files 
    exported from Crimson Hexagon and will merge them into a single data frame 
    as long as they are in the same folder.  The function will then 
    transform the data to the correct structure and normalize the text field.
    It will also drop all authors (nodes) with less than 2 tweets.
    
    Path:
    ch_dataload -> run_app
    """
    
    # load in data
    print('Loading in data... this can take a while...')
    if os.path.isdir(filepath):
        dataframes = []
        for filename in os.listdir(filepath):
            dataframe = pd.read_excel(filepath+"/"+filename, skiprows=skiprows)
            dataframes.append(dataframe)

        combined_df = pd.concat(dataframes, ignore_index=True, sort=False)
    else:
        combined_df = pd.read_excel(filepath, skiprows=skiprows)

    #Data loaded output summary
    print('Data loaded in: ', len(combined_df), " rows")
    
    # create dataframe of only necessary fields
    df = combined_df[['Author','Date (EST)', 'Contents', 'GUID', 'Posts',
                      'Country', 'URL', 'Followers', 'Following']]

    # rename to standard naming conventions
    df = df.rename(columns={'Author': 'screen_name', 'Date (EST)': 'created_at',
                            'Contents': 'text', 'GUID': 'post_id','Posts': 'statuses'})
   
    
    df = df.drop_duplicates()
    
    # run text preprocessing steps
    df['tmp'] = df['text'].apply(lambda x: pre_normalize(x))
    df['textnormed'] = df['tmp'].apply(lambda x: normalize(x))
    df.drop('tmp', axis = 1, inplace = True)
    
    # lets trim down our author list
    actives = df.groupby('screen_name').count().query('post_id>%s' % 2).index
    df = df[df.screen_name.isin(actives)]
    
    threshhold = 0
    ## iteratively increase threshhold to only capture top 10k authors. This helps reduce noise. Might
    ## be worth going even lower than 10k for smaller datasets
    while len(set(df.screen_name)) > 10000:
        actives = df.groupby('screen_name').count().query('post_id>%s' % threshhold).index
        df = df[df.screen_name.isin(actives)]
        threshhold += 1
    print(f'keeping authors with at least {threshhold+1} posts')
    
    
    threshhold = 0
    ## Now lets do the same for tweets. This helps reduce noise. 
    ## Again, might be worth going lower for smaller datasets.
    while len(set(df.textnormed)) > 10000:
        actives = df.groupby('textnormed').count().query('post_id>%s' % threshhold).index
        df = df[df.textnormed.isin(actives)]
        threshhold += 1
    print(f'keeping tweets that appeared at least {threshhold+1} times')
    
    df = group_by_cosine_similarity(df)
    return df

def import_rtweet(filepath):
    """[takes path to folder containing feather files]

    Args:
        filepath ([type]): [description]

    Returns:
        [type]: [The function will then 
    transform the data to the correct structure and normalize the text field.
    It will also drop all authors (nodes) with less than 2 tweets.]
    """
    filelist = glob.glob(str(filepath) + '*.feather')
    list_of_dfs = [pd.read_feather(flist) for flist in
                   filelist]
    df = pd.concat(list_of_dfs, ignore_index=True)
    df = df[['screen_name', 'created_at', 'text', 'status_id', 'statuses_count', 'is_retweet']]
    # add a marker for retweets- we'll use these in the cotweet part of the script.
    df['text'] = np.where(df['is_retweet'] == 'TRUE', 'RT ' + df['text'], df['text'])
    df = df[['screen_name', 'created_at', 'text', 'status_id', 'statuses_count']]
    # rename to standard naming conventions
    df = df.rename(columns={'status_id': 'post_id','statuses_count': 'statuses'})
   
    
    df = df.drop_duplicates()
    
    # run text preprocessing steps
    df['tmp'] = df['text'].apply(lambda x: pre_normalize(x))
    df['textnormed'] = df['tmp'].apply(lambda x: normalize(x))
    df.drop('tmp', axis = 1, inplace = True)
    
    # lets trim down our author list
    actives = df.groupby('screen_name').count().query('post_id>%s' % 2).index
    df = df[df.screen_name.isin(actives)]
    
    threshhold = 0
    ## iteratively increase threshhold to only capture top 10k authors. This helps reduce noise. Might
    ## be worth going even lower than 10k for smaller datasets
    while len(set(df.screen_name)) > 10000:
        actives = df.groupby('screen_name').count().query('post_id>%s' % threshhold).index
        df = df[df.screen_name.isin(actives)]
        threshhold += 1
    print(f'keeping authors with at least {threshhold+1} posts')
    
    threshhold = 0
    ## Now lets do the same for tweets. This helps reduce noise. 
    ## Again, might be worth going lower for smaller datasets.
    while len(set(df.textnormed)) > 10000:
        actives = df.groupby('textnormed').count().query('post_id>%s' % threshhold).index
        df = df[df.textnormed.isin(actives)]
        threshhold += 1
    print(f'keeping tweets that appeared at least {threshhold+1} times')
    
    df = group_by_cosine_similarity(df)
    return df

def only_preprocess(df):
    """
    If you want to manually import data from another format, make
    sure it has character post_ids, 
    """
    df = df.drop_duplicates()
    
    # run text preprocessing steps
    df['tmp'] = df['text'].apply(lambda x: pre_normalize(x))
    df['textnormed'] = df['tmp'].apply(lambda x: normalize(x))
    df.drop('tmp', axis = 1, inplace = True)
    
    # lets trim down our author list
    actives = df.groupby('screen_name').count().query('post_id>%s' % 2).index
    df = df[df.screen_name.isin(actives)]
    
    threshhold = 3
    ## iteratively increase threshhold to only capture top 10k authors. This helps reduce noise. Might
    ## be worth going even lower than 10k for smaller datasets
    while len(set(df.screen_name)) > 10000:
        actives = df.groupby('screen_name').count().query('post_id>%s' % threshhold).index
        df = df[df.screen_name.isin(actives)]
        threshhold += 1
    print(f'keeping authors with at least {threshhold+1} posts')
    
    threshhold = 3
    ## Now lets do the same for tweets. This helps reduce noise. 
    ## Again, might be worth going lower for smaller datasets.
    while len(set(df.textnormed)) > 10000:
        actives = df.groupby('textnormed').count().query('post_id>%s' % threshhold).index
        df = df[df.textnormed.isin(actives)]
        threshhold += 1
    print(f'keeping tweets that appeared at least {threshhold+1} times')
    
    df = group_by_cosine_similarity(df)
    return df