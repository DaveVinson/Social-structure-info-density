# ***************************************************************************************
# ***************************************************************************************
# ********** extract network and info measures from Yelp, Inc. Business Reviews *********
# ******* Written by David W. Vinson and Rick Dale for the purpose of demonstration *****
# ******************** PLEASE DO NOT DISTRIBUTE WITHOUT PERMISSION **********************  
# ***************************************************************************************
# ***************************************************************************************

################################################################################
############################### load reviews and unigram/bigrams list ##########
################################################################################
revs = read.csv('yelp_academic_dataset_review.json',sep='\n',quote = "")
colnames(revs) = list('u')  
revs$u = as.character(revs$u)

#### load index within all nets reviews #### 
rand_index1 = read.csv('index_allnets_revs.csv',sep=',')$x
rand_index = sample(rand_index1,length(rand_index1))

##### load across revs uni and bigrams ####
#unigrams
across_uni_matrix <- read.table('unis.csv', sep=',', header=FALSE) #READ OUTSIDE LOOP #load across unigram frequencies
uni_full <- as.matrix(across_uni_matrix[, -1]) #convert to matrix
row.names(uni_full) <- across_uni_matrix[, 1] #name the rows the appropriate words

#bigrams
across_bigram_matrix <- read.table('bigs.csv', sep=',', header=FALSE) #READ OUTSIDE LOOP #load across unigram frequencies
big_rows = as.matrix(paste(across_bigram_matrix$V1,across_bigram_matrix$V2, sep=' ')) # problems with loading bigs #sep by space works.  
big_full <- as.matrix(across_bigram_matrix[,-(1:2)]) #convert to matrix
row.names(big_full) <- big_rows[,1] #name the rows the appropriate words

################################################################################
############################### Build or load user list ########################
################################################################################
# build user lists 
# only do this if you haven't yet built user_list.txt
#write("","user_list.txt",append=F,sep="")
#for (i in 1:dim(revs)[1]) {
#  print(i)
#  json = fromJSON(revs$u[i])
#  if (i==1) { app=F }
#  else { app=T }
#  write(json$user_id,"user_list.txt",append=app,sep="\n")
#}
#load user list
user_list = read.table('user_list.txt',sep='\n',quote="")
colnames(user_list) = list('u')  
user_list = as.character(user_list$u) # as lone strings
#save(user_list,file = paste("user_list.RData",sep=""))

################################################################################
############################### Create results matrix  #########################
################################################################################
results = data.frame(run=1:1000,num_revs=0,length=0,diversity_mean=0,diversity_sd=0,
                     mean_unique_length=0,mean_within_rev_uni_ent=0,
                     sd_within_rev_uni_ent=0,mean_within_rev_info=0,
                     sd_within_rev_info=0,mean_across_rev_uni_ent=0,
                     sd_across_rev_uni_ent=0,mean_across_rev_info=0,
                     sd_across_rev_info=0,within_reviewer_sd_ent_chancap=0,
                     within_reviewer_sd_info_chancap=0,across_reviewer_sd_ent_chancap=0,
                     across_reviewer_sd_info_chancap=0,interconnectivity=0,
                     transitivities=0,degrees=0,scale_test=0,betweens=0,
                     centralization=0,nodes=0,edges=0,path_length=0,gini = 0,
                     alpha=0,xmin=0,loglik=0,KS.stat=0,KS.p=0)


################################################################################
############################### Loop through and get data ######################
################################################################################

for (run in 1:2) { #just test this out first. then run run in 1:1000
  print(run)
  print(length(rand_index))
  load(paste("network_data_1000/users",run,".RData",sep=""))
  load(paste("network_data_1000/net",run,".RData",sep=""))
  load(paste("network_data_1000/edges",run,".RData",sep=""))
  
  #plot(bsk.network,layout=layout.fruchterman.reingold,vertex.size=1,vertex.label=NA)#,vertex.label=labs,vertex.label.cex=.6)#,vertex.label=labs2)#,vertex.label=NA)
  
  colnames(net_users) = list('u')  
  net_users = as.character(net_users$u) # as lone strings
  
  revnum_matr = c() #for gini index
  reviews_desired = c() 
   for (i in 1:length(net_users)) {
     rev_peruser = which(user_list == net_users[i]) #for gini index
     reviews_desired = c(reviews_desired,which(user_list == net_users[i]))
     revnum_matr = c(revnum_matr,length(rev_peruser)) #get matrix of number of reviews per user  
   } # get all those reviews which have user id's in net_users (this net's users)
  
  texts = revs$u[reviews_desired] # get all the reviews
  TNR = data.frame(user_id=net_users,all_reviews="") # TNR = this net's reviews
  TNR$all_reviews = as.character(TNR$all_reviews)
  
  remove = 1:sum(revnum_matr) #this takes the first n'th elements that will be used in the shuffled random index
  r = 1
  for (text in texts) {  
  r = r + 1
     rev = fromJSON(text)
    #comment out to run random
    TNR[TNR$user_id==rev$user_id,]$all_reviews = paste(as.character(rev$text),
     TNR[TNR$user_id==rev$user_id,]$all_reviews,collapse=" ")

#    #uncomment to run random 
#    ran = rand_index[[r]] #get random sample
# #    r.rand_index = rand_index[rand_index==ran] #create var with all instances of sample
# #    r.rand_index = r.rand_index[1:length(r.rand_index)-1] #delete only one instance of sample var
# #    rand_index = c(rand_index[rand_index !=ran],r.rand_index) #bind index back together
#    ran_rev = fromJSON(revs$u[ran])  #pulls sample rev
#    TNR[TNR$user_id==rev$user_id,]$all_reviews = paste(as.character(ran_rev$text),
#    TNR[TNR$user_id==rev$user_id,]$all_reviews,collapse=" ") 
  
  } # collapse for each user (concatenate all user's reviews so they only count once for each)
  
  TNR$all_reviews = tolower(TNR$all_reviews)
  ts = Corpus(VectorSource(TNR$all_reviews)) # let's do some basic NLP in R (with tm)!
  # remove stopwords
  ts <- tm_map(ts, removeWords, stopwords("english"))
  # eliminate extra whitespace
  ts <- tm_map(ts, stripWhitespace)
  # eliminate punctuation
  removepunct <- function(x) { return(gsub("[[:punct:]]","",x)) }
  ts <- tm_map(ts, removepunct)
  # eliminate numbers
  removenum <- function(x) { return(gsub("[0-9]","",x)) }
  ts <- tm_map(ts, removenum)
  
  TNR$all_reviews = sapply(ts, '[', 1)
 
  TNR$length = 0
  TNR$diversity = -1 
  TNR$length_unique = 0
  TNR$within_rev_uni_ent = 0
  TNR$within_rev_uni_ent_sd = 0
  TNR$within_rev_info = 0
  TNR$within_rev_info_sd = 0
  TNR$across_rev_uni_ent = 0
  TNR$across_rev_uni_ent_sd = 0
  TNR$across_rev_big_info = 0
  TNR$across_rev_big_info_sd = 0

  for (i in 1:dim(TNR)[1]) {
    rev = TNR[i,]$all_reviews #takes the i'th reviewers revs in a plain text doc stlye
    if (nchar(rev)>0) { # calculates lexical diversity for each user in this network
      TNR[i,]$length = length(MC_tokenizer(rev))
      
      TNR[i,]$diversity = length(unique(MC_tokenizer(rev)))/length(MC_tokenizer(rev))      
      
      #build unigrams per reviewer
      rev_df <- data.frame(V1 = rev, stringsAsFactors = FALSE)
      rev_corp <- Corpus(DataframeSource(rev_df))
      tdm_unigram <- TermDocumentMatrix(rev_corp)
      #inspect(tdm_unigram)
      tdm_uni_as_matrix = data.matrix(tdm_unigram, rownames.force = NA)
      
      #build bigrams per reviewer
      BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
      tdm_bigrams <- TermDocumentMatrix(rev_corp, control = list(tokenize = BigramTokenizer))
      #inspect(tdm_bigrams)
      tdm_bi_as_matrix = data.matrix(tdm_bigrams, rownames.force = NA)
      
      TNR[i,]$length_unique = length(tdm_uni_as_matrix) #unique words per reviewer
      TNR[i,]$within_rev_uni_ent = entropy::entropy(tdm_uni_as_matrix) #unigram entropy per reviewer
      TNR[i,]$within_rev_info =  entropy::entropy(tdm_bi_as_matrix) #bigram entropy per reviewer
      
      
      # across unigrams
      rows.to.keep<-which(rownames(uni_full) %in% rownames(tdm_uni_as_matrix)) # selects rows to keep (matched)
      across_uni_mat_net = as.matrix(uni_full[rows.to.keep,]) #extracts matrix of matched rows
      TNR[i,]$across_rev_uni_ent = mean(-log2(across_uni_mat_net/sum(uni_full)))

      # across bigrams
      rows.to.keep.bigs<-which(rownames(big_full) %in% rownames(tdm_bi_as_matrix)) # selects rows to keep (matched)
      across_big_mat_net = as.matrix(big_full[rows.to.keep.bigs,]) #extracts matrix of matched rows
      TNR[i,]$across_rev_big_info = mean(-log2(across_big_mat_net/sum(big_full)))

    # channel capacity measures  ### entropy estimation crashes the system. find a better way to estimate SD... 
     TNR[i,]$within_rev_uni_ent_sd = sd(-1*log2(tdm_uni_as_matrix/length(tdm_uni_as_matrix)))
     TNR[i,]$within_rev_info_sd = sd(-1*log2(tdm_bi_as_matrix/length(tdm_bi_as_matrix)))
     TNR[i,]$across_rev_uni_ent_sd =  sd(-1*log2(across_uni_mat_net/sum(uni_full)))
     TNR[i,]$across_rev_big_info_sd =  sd(-1*log2(across_big_mat_net/sum(big_full)))  
    
   
     
    }
  }

  # lexical measures 
  
  TNR$length[is.na(TNR$length)] <- 0

  TNR$diversity[is.na(TNR$diversity)] <- 0
  TNR$length_unique[is.na(TNR$length_unique)] <- 0
  TNR$within_rev_uni_ent[is.na(TNR$within_rev_uni_ent)] <- 0
  TNR$within_rev_info[is.na(TNR$within_rev_info)] <- 0
  TNR$across_rev_uni_ent[is.na(TNR$across_rev_uni_ent)] <- 0
  TNR$across_rev_big_info[is.na(TNR$across_rev_big_info)] <- 0
  
  TNR$within_rev_uni_ent_sd[is.na(TNR$within_rev_uni_ent_sd)] <- 0
  TNR$within_rev_info_sd[is.na(TNR$within_rev_info_sd)] <- 0
  TNR$across_rev_uni_ent_sd[is.na(TNR$across_rev_uni_ent_sd)] <- 0
  TNR$across_rev_big_info_sd[is.na(TNR$across_rev_big_info_sd)] <- 0
  
#   TNR = TNR[TNR$diversity>-1,]
#   TNR = TNR[TNR$length_unique>0,] 
#   TNR = TNR[TNR$within_rev_uni_ent>0,] 
#   TNR = TNR[TNR$within_rev_info>0,] 
#   TNR = TNR[TNR$across_rev_uni_ent>0,] 
#   TNR = TNR[TNR$across_rev_big_info>0,] 
# 
#   TNR = TNR[TNR$within_rev_uni_ent_sd>0,]
#   TNR = TNR[TNR$within_rev_info_sd>0,]
#   TNR = TNR[TNR$across_rev_uni_ent_sd>0,]
#   TNR = TNR[TNR$across_rev_big_info_sd>0,]
#   
  results[run,]$num_revs = length(texts)
  results[run,]$length = mean(TNR$length)
  results[run,]$diversity_mean = mean(TNR$diversity)
  results[run,]$diversity_sd = sd(TNR$diversity) #sd of within reviewer diversity
  results[run,]$mean_unique_length = mean(TNR$length_unique)
  results[run,]$mean_within_rev_uni_ent = mean(TNR$within_rev_uni_ent)
  results[run,]$sd_within_rev_uni_ent = sd(TNR$within_rev_uni_ent) #sd of within reviewer entropy
  results[run,]$mean_within_rev_info = mean(TNR$within_rev_info)
  results[run,]$sd_within_rev_info = sd(TNR$within_rev_info) #sd of within reviewer bigrams

  results[run,]$mean_across_rev_uni_ent = mean(TNR$across_rev_uni_ent)
  results[run,]$sd_across_rev_uni_ent = sd(TNR$across_rev_uni_ent) #sd of within reviewer entropy
  results[run,]$mean_across_rev_info = mean(TNR$across_rev_big_info)
  results[run,]$sd_across_rev_info = sd(TNR$across_rev_big_info) #sd of within reviewer bigrams

  results[run,]$within_reviewer_sd_ent_chancap = mean(TNR$within_rev_uni_ent_sd) #sd of within reviewer bigrams
  results[run,]$within_reviewer_sd_info_chancap = mean(TNR$within_rev_info_sd) #sd of within reviewer bigrams
  results[run,]$across_reviewer_sd_ent_chancap = mean(TNR$across_rev_uni_ent_sd) #sd of within reviewer bigrams
  results[run,]$across_reviewer_sd_info_chancap = mean(TNR$across_rev_big_info_sd) #sd of within reviewer bigrams
 
  # network measures 
  results[run,]$interconnectivity = length(E(bsk.network))/length(V(bsk.network))
  results[run,]$transitivities = transitivity(bsk.network)
  results[run,]$degrees = mean(degree(bsk.network, v=V(bsk.network), loops = TRUE, normalized = FALSE))
  results[run,]$scale_test = mean(degree.distribution(bsk.network, cumulative = FALSE))
  results[run,]$betweens = mean(betweenness(bsk.network, v=V(bsk.network), directed = TRUE, weights = NULL, nobigint = TRUE, normalized = FALSE))
  centrality = centralization.degree (bsk.network,loops = TRUE, normalized = TRUE)
  results[run,]$centralization = centrality$centralization
  results[run,]$nodes = length(V(bsk.network))
  results[run,]$edges = length(E(bsk.network))
  results[run,]$path_length = mean(shortest.paths(bsk.network, v=V(bsk.network), to=V(bsk.network), weights = NULL, algorithm = "automatic"))
  results[run,]$gini = gini(revnum_matr) #use numrev of users to determine gini index. 
  
  deg.dist = degree.distribution(bsk.network, cumulative = FALSE)
  power_law_test = power.law.fit(deg.dist)
  results[run,]$alpha = power_law_test$alpha
  results[run,]$xmin = power_law_test$xmin
  results[run,]$loglik = power_law_test$logLik
  results[run,]$KS.stat = power_law_test$KS.stat
  results[run,]$KS.p = power_law_test$KS.p

rand_index = rand_index[-remove] #this removes the reviews used in this network. 
}

#write.csv(results, file = "new_info_net_baseline.csv", row.names = FALSE)
#write.csv(results, file = "new_info_net_true.csv", row.names = FALSE)
