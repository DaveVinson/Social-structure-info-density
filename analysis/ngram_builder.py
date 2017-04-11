import nltk
import numpy as np
import os, re
import json
from nltk.tokenize import RegexpTokenizer


txt = []
with open('/Users/Dave/Documents/yelp_dataset_mil/yelp_academic_dataset_review.json') as f:
    for line in f:
        a = json.loads(line)
        txt.append(a['text'].lower())

unis = nltk.FreqDist()
bigs = nltk.FreqDist()

tokenizer = RegexpTokenizer(r'\w+')

for line in txt:
    words = tokenizer.tokenize(line)
    big = nltk.bigrams(words)
    for word in words:
        unis[word] += 1
        
    for bigrams in big: 
        bigs[bigrams] += 1

with open("/Users/Dave/Desktop/yelp_unis.csv", "w") as w:
    for item in unis.items():
        w.write('{},{}\n'.format(item[0].encode('utf-8'), item[1]))

with open("/Users/Dave/Desktop/yelp_bigs.csv", "w") as w:
    for item in bigs.items():
        w.write('{},{},{}\n'.format(item[0][0].encode('utf-8'), item[0][1].encode('utf-8'), item[1]))