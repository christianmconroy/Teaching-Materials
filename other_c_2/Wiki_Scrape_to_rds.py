import numpy as np
import requests
from bs4 import BeautifulSoup
import wikipedia
import boto3
from io import StringIO
import json
import psycopg2
from boto3 import client
import os
import pandas as pd

# wikipedia api has random warnings so ignoring
import warnings
warnings.catch_warnings()
warnings.simplefilter('ignore')

s3 = client('s3')

def main():
    bucket = 'mockdatauscis'
    key = 'wiki_movies/wiki_movies_test.csv'
    local_path = '/tmp/' + key.split('/')[-1]

    s3.download_file(bucket, key, local_path)
    
    conn = psycopg2.connect(user = "rdspgadm", 
                            password = "PW",
                            host = "mgt-prd-infr-rds-uscis-cluster-0.cargfitmjuj5.us-east-1.rds.amazonaws.com",
                            database = "postgres")

    cur = conn.cursor()
    
    # Chunk 1
    cur.execute('DROP TABLE IF EXISTS wiki_movies_5000; CREATE TABLE wiki_movies_5000 (Title varchar, Year varchar, Genre varchar, Director varchar, Starring varchar, Summary varchar, Categories varchar, Plot varchar, Producer varchar, Release_date varchar, Running_Time varchar);')
    conn.commit()
    
    cur.execute("SELECT aws_s3.table_import_from_s3('wiki_movies_5000','', '(format csv, HEADER)',aws_commons.create_s3_uri('mockdatauscis','wiki_movies/wiki_movies_test_5000.csv','us-east-1'));")
    conn.commit()
    
        # Chunk 2
    cur.execute('DROP TABLE IF EXISTS wiki_movies_10000; CREATE TABLE wiki_movies_10000 (Title varchar, Year varchar, Genre varchar, Director varchar, Starring varchar, Summary varchar, Categories varchar, Plot varchar, Producer varchar, Release_date varchar, Running_Time varchar);')
    conn.commit()
    
    cur.execute("SELECT aws_s3.table_import_from_s3('wiki_movies_10000','', '(format csv, HEADER)',aws_commons.create_s3_uri('mockdatauscis','wiki_movies/wiki_movies_test_5000_10000.csv','us-east-1'));")
    conn.commit()
    
    
        # Chunk 3
    cur.execute('DROP TABLE IF EXISTS wiki_movies_15000; CREATE TABLE wiki_movies_15000 (Title varchar, Year varchar, Genre varchar, Director varchar, Starring varchar, Summary varchar, Categories varchar, Plot varchar, Producer varchar, Release_date varchar, Running_Time varchar);')
    conn.commit()
    
    cur.execute("SELECT aws_s3.table_import_from_s3('wiki_movies_15000','', '(format csv, HEADER)',aws_commons.create_s3_uri('mockdatauscis','wiki_movies/wiki_movies_test_510000_15000.csv','us-east-1'));")
    conn.commit()
    
    
        # Chunk 4
    cur.execute('DROP TABLE IF EXISTS wiki_movies_19300; CREATE TABLE wiki_movies_19300 (Title varchar, Year varchar, Genre varchar, Director varchar, Starring varchar, Summary varchar, Categories varchar, Plot varchar, Producer varchar, Release_date varchar, Running_Time varchar);')
    conn.commit()
    
    cur.execute("SELECT aws_s3.table_import_from_s3('wiki_movies_19300','', '(format csv, HEADER)',aws_commons.create_s3_uri('mockdatauscis','wiki_movies/wiki_movies_test_15000_max.csv','us-east-1'));")
    conn.commit()
    
    cur.execute("DROP TABLE IF EXISTS wiki_movies_all; CREATE TABLE wiki_movies_all AS (SELECT * FROM wiki_movies_5000 UNION ALL SELECT * FROM wiki_movies_10000 UNION ALL SELECT  FROM wiki_movies_15000 UNION ALL SELECT * FROM wiki_movies_19300);")
    conn.commit()
    
    conn.close()

if __name__ == "__main__":
    main()  

    



