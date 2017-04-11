# ***************************************************************************************
# ***************************************************************************************
# *********************************** Main script ***************************************
# ******* Written by David W. Vinson and Rick Dale for the purpose of demonstration *****
# ******************** PLEASE DO NOT DISTRIBUTE WITHOUT PERMISSION **********************  
# ***************************************************************************************
# ***************************************************************************************

### Data access 
# Access the data needed for these files here: 'https://github.com/DaveVinson/Social-structure-info-density/tree/master/data'
# you will need to download the .tar.gz file from Yelp.com/dataset_challenge/
# untar this file, and place the .json files inside the data folder under analysis. 

### Set your directory to the 'analysis/data/' folder 
rootFolder = '/Users/Dave/Documents/github/Social-structure-info-density/analysis/data/' 
rootFolder = '~/analysis/data/'
setwd(rootFolder)

### load libraries 
library(network)
library(jsonlite)
library(igraph)
library(plyr)
library(tm)
library(RWeka) #for NgramTokenizer
library(entropy) #pulls the shannon entropy measures
library(qdap) # for converting csv to term doc matrix
library(EntropyEstimation) #necessary for entropy.sd (but not necessary for current analysis)
library(reldist)

### load functions 
source('../net.function10.R')  #description in code
### Build networks and save them to an empty folder 
source('../build.nets.R') # prints details in console
### Build the user list for all those within the networks you just built.
source('../allnet.review.index.R')
### Build information density values by yelp networks 
source('../get.reviews.R')
### Analyze socail network structure and information density
source('../analysis.R')
### Graphs used in the manuscript 
source('../graphs.R')


