# ***************************************************************************************
# ***************************************************************************************
# **************** create index of reviews from social networks *************************
# ******* Written by David W. Vinson and Rick Dale for the purpose of demonstration *****
# ******************** PLEASE DO NOT DISTRIBUTE WITHOUT PERMISSION **********************  
# ***************************************************************************************
# ***************************************************************************************

#########################################################################
#########Create index of reviews for only those reivews within networks## 
#########################################################################

#load all reviews
revs = read.csv('/Users/Dave/Documents/yelp_dataset_mil/yelp_academic_dataset_review.json',sep='\n',quote = "")
colnames(revs) = list('u')  
revs$u = as.character(revs$u)

#########################################################################
###################### build or load index user list ####################
#########################################################################
# build index user list
#write("","user_list.txt",append=F,sep="")
#for (i in 1:dim(revs)[1]) {
#  print(i)
#  json = fromJSON(revs$u[i])
#  if (i==1) { app=F }
#  else { app=T }
#  write(json$user_id,"user_list.txt",append=app,sep="\n")
#}
#load index user list 
user_list = read.table('user_list.txt',sep='\n',quote="") #this is the list of all reviews by user! 
colnames(user_list) = list('u')  
user_list = as.character(user_list$u) # as lone strings

#########################################################################
################ create index of all revs in nets #######################
#########################################################################
#get all reviewers from all nets
all_net_users = rbind()
for (run in 1:1000) {#1:number of nets
  load(paste("network_data_1000/users",run,".RData",sep=""))
  all_net_users  = rbind(all_net_users,net_users)
}

colnames(all_net_users) = list('u')  
all_net_users = as.character(all_net_users$u)

#since there are repeated nodes, same user in dif nets...
# to get a true distribution you'll need to keep all of them. 

#all_net_users = unique(all_net_users) #do not do this! 

#### this takes the i'th row placement for all users in all nets reviews
#note: this takes a while! possibly multiple hours (surprisingly)
all_net_revs = c()
for (i in 1:length(all_net_users)) {
  all_net_revs = c(all_net_revs,which(user_list == all_net_users[i])) #this gives and index
} 

#########################################################################
################ Write index to CSV######################################
#########################################################################
write.csv(all_net_revs, file = "index_allnets_revs.csv", row.names = FALSE) #save the indexed list of all nets revs 