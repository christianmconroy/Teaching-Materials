# Load raw data into Neptune
## Import Packages

import subprocess
import pandas as pd
import boto3
import psycopg2
import numpy as np
import matplotlib.pyplot as plt

import networkx as nx
import pandas as pd
import psycopg2
import os

import igraph as ig
import json
from networkx.readwrite import json_graph

from __future__  import print_function  # Python 2/3 compatibility

from gremlin_python import statics
from gremlin_python.structure.graph import Graph
from gremlin_python.process.graph_traversal import __
from gremlin_python.process.strategies import *
from gremlin_python.driver.driver_remote_connection import DriverRemoteConnection

from gremlin_python.process.traversal import T
from gremlin_python.process.traversal import Cardinality

import boto3
from boto3 import client
from io import StringIO

## Pull in and merge movies data
#GENERATE TOKENS FOR RDS ACCESS

rds_token_cmd = f'aws rds generate-db-auth-token --hostname "mgt-prd-infr-rds-uscis-cluster-0.cargfitmjuj5.us-east-1.rds.amazonaws.com" --port 5432 --region us-east-1 --username "rdspgadm"'
rds_token = str(subprocess.check_output(rds_token_cmd, shell=True, text=True)).strip()

#CONNTECT TO RDS SERVER

conn = psycopg2.connect(
    host="mgt-prd-infr-rds-uscis-cluster-0.cargfitmjuj5.us-east-1.rds.amazonaws.com",
    database="postgres",
    user="rdspgadm",
    password=rds_token)

#TESTING CONNECTION: OUTPUT ALL AVAILABLE TABLES

cursor = conn.cursor()
cursor.execute("""SELECT table_name FROM information_schema.tables
       WHERE table_schema = 'public'""")
for table in cursor.fetchall():
    print(table)
    
#### Bring in Wiki Data

df = pd.read_sql("""SELECT * FROM public.dossier_table""", con=conn)

## Create edge and node list

#### Pull out entities
def splitter(s):
    spl = s.split(",")
    return spl

#Director

df_dir = df[['title', 'director']]
df_dir['director'] = df_dir['director'].str.replace(r"[\"\'\\[\]]", '')
df_dir = df_dir[df_dir['director'].notnull()]
df_dir = df_dir.reset_index(drop=True)
df_new = pd.DataFrame(df_dir["director"].apply(splitter).to_list())
df_melt = pd.merge(df_dir, df_new, left_index=True, right_index=True)

cols = df_melt.columns.tolist()
indices = [0]
id_cols = [cols[i] for i in indices]
v_indices = range(2,len(cols))
value_vars = [cols[i] for i in v_indices]

df_melt_long = df_melt.melt(id_vars=id_cols, value_vars=value_vars, var_name="remove", value_name='to', ignore_index=True)
df_melt_long = df_melt_long[df_melt_long ['to'] != ""]
df_melt_long = df_melt_long[df_melt_long ['to'] != None]
df_melt_long = df_melt_long[df_melt_long ['to'].notnull()]
df_melt_long = df_melt_long.drop(['remove'], axis=1)
df_melt_long = df_melt_long.rename(columns={'title': 'from'})
directors = df_melt_long
directors["edge_type"] = "Directed"
directors["from_type"] = "Movie"
directors["to_type"] = "Director"

# Starring
df_dir = df[['title', 'starring']]
df_dir['starring'] = df_dir['starring'].str.replace(r"[\"\'\\[\]]", '')
df_dir = df_dir[df_dir['starring'].notnull()]
df_dir = df_dir.reset_index(drop=True)

df_new = pd.DataFrame(df_dir["starring"].apply(splitter).to_list())
df_melt = pd.merge(df_dir, df_new, left_index=True, right_index=True)

cols = df_melt.columns.tolist()
indices = [0]
id_cols = [cols[i] for i in indices]
v_indices = range(2,len(cols))
value_vars = [cols[i] for i in v_indices]

df_melt_long = df_melt.melt(id_vars=id_cols, value_vars=value_vars, var_name="remove", value_name='to', ignore_index=True)
df_melt_long = df_melt_long[df_melt_long ['to'] != ""]
df_melt_long = df_melt_long[df_melt_long ['to'] != None]
df_melt_long = df_melt_long[df_melt_long ['to'].notnull()]
df_melt_long = df_melt_long.drop(['remove'], axis=1)
df_melt_long = df_melt_long.rename(columns={'title': 'from'})
starring = df_melt_long
starring['edge_type'] = "Starred In"
starring["from_type"] = "Movie"
starring["to_type"] = "Actor"

starring.head()

# Genres
df_dir = df[['title', 'genres']]
df_dir['genres'] = df_dir['genres'].str.replace(r"[\"\'\\[\]]", '')
df_dir = df_dir[df_dir['genres'].notnull()]
df_dir = df_dir.reset_index(drop=True)

df_new = pd.DataFrame(df_dir["genres"].apply(splitter).to_list())
df_melt = pd.merge(df_dir, df_new, left_index=True, right_index=True)

cols = df_melt.columns.tolist()
indices = [0]
id_cols = [cols[i] for i in indices]
v_indices = range(2,len(cols))
value_vars = [cols[i] for i in v_indices]

df_melt_long = df_melt.melt(id_vars=id_cols, value_vars=value_vars, var_name="remove", value_name='to', ignore_index=True)
df_melt_long = df_melt_long[df_melt_long ['to'] != ""]
df_melt_long = df_melt_long[df_melt_long ['to'] != None]
df_melt_long = df_melt_long[df_melt_long ['to'].notnull()]
df_melt_long = df_melt_long.drop(['remove'], axis=1)
df_melt_long = df_melt_long.rename(columns={'title': 'from'})
genres = df_melt_long
genres['edge_type'] = "Genre"
genres["from_type"] = "Movie"
genres["to_type"] = "Genre"

# Producer
df_dir = df[['title', 'producer']]
df_dir = df_dir[df_dir['producer'].notna()]
df_dir = df_dir[df_dir["producer"].str.contains("\n")]
df_dir['producer'] = df_dir['producer'].str.replace(r"[\"\'\\[\]]", '')
df_dir['producer'] = df_dir['producer'].str.replace(r"\n", ',')
df_dir = df_dir[~df_dir["producer"].str.contains("\.")]
df_dir = df_dir[df_dir['producer'].notnull()]
df_dir = df_dir.reset_index(drop=True)
#df_dir = df_dir[0:285]
#df_dir = df_dir.drop_duplicates(subset=['title'], keep='first')

df_new = pd.DataFrame(df_dir["producer"].apply(splitter).to_list())
df_melt = pd.merge(df_dir, df_new, left_index=True, right_index=True)

cols = df_melt.columns.tolist()
indices = [0]
id_cols = [cols[i] for i in indices]
v_indices = range(2,len(cols))
value_vars = [cols[i] for i in v_indices]

df_melt_long = df_melt.melt(id_vars=id_cols, value_vars=value_vars, var_name="remove", value_name='to', ignore_index=True)
df_melt_long = df_melt_long[df_melt_long ['to'] != ""]
df_melt_long = df_melt_long[df_melt_long ['to'] != None]
df_melt_long = df_melt_long[df_melt_long ['to'].notnull()]
df_melt_long['to'] = df_melt_long['to'].str.replace(r"\,", '')
df_melt_long = df_melt_long.drop(['remove'], axis=1)
df_melt_long = df_melt_long.rename(columns={'title': 'from'})
producer = df_melt_long
producer['edge_type'] = "Produced"
producer["from_type"] = "Movie"
producer["to_type"] = "Producer"

# Writers
df_dir = df[['title', 'writers']]
df_dir['writers'] = df_dir['writers'].str.replace(r"[\"\'\\[\]]", '')
df_dir = df_dir[df_dir['writers'].notnull()]
df_dir = df_dir.reset_index(drop=True)

df_new = pd.DataFrame(df_dir["writers"].apply(splitter).to_list())
df_melt = pd.merge(df_dir, df_new, left_index=True, right_index=True)

cols = df_melt.columns.tolist()
indices = [0]
id_cols = [cols[i] for i in indices]
v_indices = range(2,len(cols))
value_vars = [cols[i] for i in v_indices]

df_melt_long = df_melt.melt(id_vars=id_cols, value_vars=value_vars, var_name="remove", value_name='to', ignore_index=True)
df_melt_long = df_melt_long[df_melt_long ['to'] != ""]
df_melt_long = df_melt_long[df_melt_long ['to'] != None]
df_melt_long = df_melt_long[df_melt_long ['to'].notnull()]
df_melt_long = df_melt_long.drop(['remove'], axis=1)
df_melt_long = df_melt_long.rename(columns={'title': 'from'})
writers = df_melt_long
writers['edge_type'] = "Wrote"
writers["from_type"] = "Movie"
writers["to_type"] = "Writer"

# Characters
df_dir = df[['title', 'characters']]
df_dir['characters'] = df_dir['characters'].str.replace(r"[\"\'\\[\]]", '')
df_dir = df_dir[df_dir['characters'].notnull()]
df_dir = df_dir.reset_index(drop=True)

df_new = pd.DataFrame(df_dir["characters"].apply(splitter).to_list())
df_melt = pd.merge(df_dir, df_new, left_index=True, right_index=True)

cols = df_melt.columns.tolist()
indices = [0]
id_cols = [cols[i] for i in indices]
v_indices = range(2,len(cols))
value_vars = [cols[i] for i in v_indices]

df_melt_long = df_melt.melt(id_vars=id_cols, value_vars=value_vars, var_name="remove", value_name='to', ignore_index=True)
df_melt_long = df_melt_long[df_melt_long ['to'] != ""]
df_melt_long = df_melt_long[df_melt_long ['to'] != None]
df_melt_long = df_melt_long[df_melt_long ['to'].notnull()]
df_melt_long = df_melt_long.drop(['remove'], axis=1)
df_melt_long = df_melt_long.rename(columns={'title': 'from'})
characters = df_melt_long
characters['edge_type'] = "Character In"
characters["from_type"] = "Movie"
characters["to_type"] = "Character"

# Directed-Starred
directed_starred = pd.merge(directors, starring, how="left", on="from")
directed_starred = directed_starred[['to_x','to_y']]
directed_starred = directed_starred.rename(columns={'to_x': 'from', 'to_y':'to'})
directed_starred['edge_type'] = "Directed Actor"
directed_starred["from_type"] = "Director"
directed_starred["to_type"] = "Actor"

# Directed_Genre
directed_genre = pd.merge(directors, genres, how="left", on="from")
directed_genre = directed_genre[['to_x','to_y']]
directed_genre = directed_genre.rename(columns={'to_x': 'from', 'to_y':'to'})
directed_genre['edge_type'] = "Directed Genre"
directed_genre["from_type"] = "Director"
directed_genre["to_type"] = "Genre"

# Produced-Starred
producer_starred = pd.merge(producer, starring, how="left", on="from")
producer_starred = producer_starred[['to_x','to_y']]
producer_starred = producer_starred.rename(columns={'to_x': 'from', 'to_y':'to'})
producer_starred['edge_type'] = "Produced Movie with Actor"
producer_starred["from_type"] = "Producer"
producer_starred["to_type"] = "Actor"

# Produced-Genre
producer_genre = pd.merge(producer, genres, how="left", on="from")
producer_genre = producer_genre[['to_x','to_y']]
producer_genre = producer_genre.rename(columns={'to_x': 'from', 'to_y':'to'})
producer_genre['edge_type'] = "Produced Genre"
producer_genre["from_type"] = "Producer"
producer_genre["to_type"] = "Genre"

# Wrote-Starred
wrote_starred = pd.merge(writers, starring, how="left", on="from")
wrote_starred = wrote_starred[['to_x','to_y']]
wrote_starred = wrote_starred.rename(columns={'to_x': 'from', 'to_y':'to'})
wrote_starred['edge_type'] = "Wrote Movie with Actor"
wrote_starred["from_type"] = "Writer"
wrote_starred["to_type"] = "Actor"


# Wrote-Genre
wrote_genre = pd.merge(writers, genres, how="left", on="from")
wrote_genre = wrote_genre[['to_x','to_y']]
wrote_genre = wrote_genre.rename(columns={'to_x': 'from', 'to_y':'to'})
wrote_genre['edge_type'] = "Wrote Genre"
wrote_genre["from_type"] = "Writer"
wrote_genre["to_type"] = "Genre"


# Starred-Genre
starred_genre = pd.merge(starring, genres, how="left", on="from")
starred_genre = starred_genre[['to_x','to_y']]
starred_genre = starred_genre.rename(columns={'to_x': 'from', 'to_y':'to'})
starred_genre['edge_type'] = "Starred in Genre"
starred_genre["from_type"] = "Actor"
starred_genre["to_type"] = "Genre"


# Directed-Produced
directed_produced = pd.merge(directors, producer, how="left", on="from")
directed_produced = directed_produced[['to_x','to_y']]
directed_produced = directed_produced.rename(columns={'to_x': 'from', 'to_y':'to'})
directed_produced['edge_type'] = "Directed Movie with Producer"
directed_produced["from_type"] = "Director"
directed_produced["to_type"] = "Producer"


# Directred-Wrote
directed_wrote = pd.merge(directors, writers, how="left", on="from")
directed_wrote = directed_wrote[['to_x','to_y']]
directed_wrote = directed_wrote.rename(columns={'to_x': 'from', 'to_y':'to'})
directed_wrote['edge_type'] = "Directed Movie with Writer"
directed_wrote["from_type"] = "Director"
directed_wrote["to_type"] = "Writer"


# Merge all 
full_edge = pd.concat([directors,starring,genres,producer,writers,characters, directed_starred, directed_genre, producer_starred, producer_genre, wrote_starred,wrote_genre,starred_genre,directed_produced,directed_wrote ])
full_edge = full_edge[full_edge['from'].notna()]
full_edge = full_edge[full_edge['to'].notna()]

nodes_n = full_edge[['from','from_type']].drop_duplicates()
nodes_n = nodes_n.rename(columns={'from': 'node', 'from_type':'node_type'})

nodes_t = full_edge[['to','to_type']].drop_duplicates()
nodes_t = nodes_n.rename(columns={'to': 'node', 'to_type':'node_type'})

nodes = pd.concat([nodes_n,nodes_t])
nodes= nodes.drop_duplicates()

nodes.reset_index(drop=True, inplace=True)
nodes.reset_index(inplace=True)
nodes = nodes.rename(columns={'index': 'id'})

nodes['id'] = nodes['id']+1

nodes['id'] = nodes['id'].astype('str')

# Merge node ids with edge list
full_edge_S = pd.merge(full_edge, nodes, how = "left", left_on = ["from"], right_on=["node"])

full_edge_S = pd.merge(full_edge_S, nodes, how = "left", left_on = ["to"], right_on=["node"])

full_edge_S.reset_index(drop=True, inplace=True)
full_edge_S.reset_index(inplace=True)
full_edge_S = full_edge_S.rename(columns={'index': 'id'})

full_edge_S = full_edge_S[['id','id_x','id_y', 'edge_type']]

full_edge_S = full_edge_S.rename(columns={'id_x': 'from', 'id_y':'to'})

full_edge_S['id'] = full_edge_S['id'] + 1
full_edge_S['id'] = "e" + full_edge_S['id'].astype('str')
full_edge_S['from'] = full_edge_S['from'].astype("str")
full_edge_S['to'] = full_edge_S['to'].astype("str")

# # Add Edges and Nodes to Neptune Graph

# Necessary to avoid Cannot run the event loop while another loop is running error
import nest_asyncio
nest_asyncio.apply()

graph = Graph()

remoteConn = DriverRemoteConnection('ws://mgt-prd-infr-neptune-alb-122161610.us-east-1.elb.amazonaws.com:8182/gremlin','g')
g = graph.traversal().withRemote(remoteConn)

# Reset from previous
g.V().drop().iterate()
g.E().drop().iterate()
g.V().count().next()
g.E().count().next()

# Nodes
id = T.id
single = Cardinality.single

ct = 0
for i in range(0,len(nodes)):
    g.addV(nodes['node_type'][i]).property(id, nodes['id'][i]). \
    property('node', nodes['node'][i]). \
    next()
    
    ct = ct + 1
    if ct % 15000 == 0:
        print(ct)
        
        #Reconnect
        graph = Graph()
        remoteConn = DriverRemoteConnection('ws://mgt-prd-infr-neptune-alb-122161610.us-east-1.elb.amazonaws.com:8182/gremlin','g')
        g = graph.traversal().withRemote(remoteConn)
    else:
        pass
       
# Add Edges
ct = 0
for i in range(0,len(full_edge_S)):
    g.V().has(id, full_edge_S['to'][i]).\
    as_('v').V().has(id,full_edge_S['from'][i]).coalesce(
    __.inE(full_edge_S['edge_type'][i]).where(__.outV().as_('v')), 
    __.addE(full_edge_S['edge_type'][i]).property(id, full_edge_S['id'][i]).from_('v')).iterate()
    
    ct = ct + 1
    if ct % 15000 == 0:
        print(ct)
        
        #Reconnect
        graph = Graph()
        remoteConn = DriverRemoteConnection('ws://mgt-prd-infr-neptune-alb-122161610.us-east-1.elb.amazonaws.com:8182/gremlin','g')
        g = graph.traversal().withRemote(remoteConn)
    else:
        pass