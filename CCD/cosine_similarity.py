import numpy as np
import pandas as pd
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.metrics.pairwise import cosine_similarity
from sklearn.metrics.pairwise import linear_kernel

def ngrams(string, n=3):
    """
    This zips documents into ngrams- important for vectorization
    """
    ngrams = zip(*[string[i:] for i in range(n)])
    return [''.join(ngram) for ngram in ngrams]

def find_group(row, col):
    """
    If either the row or the col string have already been given
    a group, return that group. Otherwise return none    
    """

    if row in group_lookup:
        return group_lookup[row]
    elif col in group_lookup:
        return group_lookup[col]
    else:
        return None


def add_vals_to_lookup(group, row, col):
    """
    Iteratively assign textnormed to groups
    """
    group_lookup[row] = group
    group_lookup[col] = group


def add_pair_to_lookup(row, col):
    """
    in this function we'll add both the row and the col to the lookup
    """
    group = find_group(row, col)  # first, see if one has already been added
    if group is not None:
        # if we already know the group, make sure both row and col are in lookup
        add_vals_to_lookup(group, row, col)
    else:
        # if we get here, we need to add a new group.
        # The name is arbitrary, so just make it the row
        add_vals_to_lookup(row, row, col)
        
def cosine_similarity_n_space(m1, m2, batch_size=100):
    """[run cosine similarity over the matrix in batches of 100]

    Args:
        m1 ([sparse matrix or np array]): [description]
        m2 ([sparse matrix or np array]): [description]
        batch_size (int, optional): [description]. Defaults to 100.

    Returns:
        [type]: [cosine similarity matrix]
    """
    assert m1.shape[1] == m2.shape[1]
    ret = np.ndarray((m1.shape[0], m2.shape[0]))
    for row_i in range(0, int(m1.shape[0] / batch_size) + 1):
        start = row_i * batch_size
        end = min([(row_i + 1) * batch_size, m1.shape[0]])
        if end <= start:
            break 
        rows = m1[start: end]
        sim = cosine_similarity(rows, m2) 
        ret[start: end] = sim
    return ret
# create an empty dictionary
group_lookup = {}

#dummytxt = ['laura dylan dylan dylan evan eyosyas',
# 'laura dylan dylan dylan paul ruta',
# 'britany desiree amir',
# 'laura dylan dylan evan eyosyas',
# 'britany desiree evan']


def group_by_cosine_similarity(df):
    """
    Efficiently group tweets by cosine similarity - we group by >= 0.8
    """
    vals = df['textnormed'].unique().astype('U').copy()
    vec = TfidfVectorizer(analyzer = 'word')
    #build the matrix
    tf_idf_matrix = vec.fit_transform(vals) # `X` will now be a TF-IDF representation 
    # get cosine matrix
    cs_mat = cosine_similarity_n_space(tf_idf_matrix, tf_idf_matrix)
    # zero out the diagonal - maybe don't do that?
    #np.fill_diagonal(cs_mat,0)
    # get indices of pairs with cosine similarity >= 0.8
    r,c = np.where(cs_mat >= 0.8)
    for i in range(len(r)):
        add_pair_to_lookup(vals[r[i]], vals[c[i]])
    df = df.copy()
    df['group'] = df['textnormed'].map(group_lookup).fillna(df['textnormed'])
    df.drop('textnormed', axis=1, inplace=True)
    df = df.rename(columns={"group": "textnormed"})
    
    return df