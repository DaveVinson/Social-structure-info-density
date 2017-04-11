# ***************************************************************************************
# ***************************************************************************************
# **************** Building social networks from Yelp, Inc. Business Reviews ************
# ******* Written by David W. Vinson and Rick Dale for the purpose of demonstration *****
# ******************** PLEASE DO NOT DISTRIBUTE WITHOUT PERMISSION **********************  
# ***************************************************************************************
# ***************************************************************************************

################################################################################
############################### load users #####################################
################################################################################

#load the userlist provided by Yelp## 
# us = read.csv('yelp_academic_dataset_user.json',sep='\n',quote = "")
us = read.csv('/Volumes/Seagate/yelp/yelp_dataset-jan 2016/yelp_academic_dataset_user.json',sep='\n',quote = "")
colnames(us) = list('u')  
us$u = as.character(us$u)

################################################################################
################### Build user list so looping is easier #######################
################################################################################
# 
# user_list = c()
# for (i in 1:dim(us)[1]) {
#   json = fromJSON(us$u[i])
#   user_list = c(user_list,json$user_id)
# }
#write.csv(user_list, file = "user_list.csv", row.names = FALSE) 
#write to file for later, so you wont have to build again
user_list = as.character(read.csv('~/Desktop/user_list.csv')[,2])#take second column (user_id), first is numbered index

################################################################################
###################### import a variety of functions, or function libraries ####
################################################################################

# source('../net.function10.R')
# #this function selects and individual that has between 11 and 20 friends. It then connects all of them to the same network.
# #it then selects up to 10 friends (randomly) and connects up to 10 of their friends (randomly) to the network. 
# # making the max number 1 + 20 + 10x10 = 121 

################################################################################
############## Run networks and save as .Rdata for network, edges and users ####
################################################################################

for (run in 1:100) {  
  print(run)
  build_networks() 
  setwd('/Users/Dave/Documents/github/Social-structure-info-density/analysis/data/new-networks-100/')

  save(bsk.network,file = paste("net",run,".RData",sep=""))
  save(net_users,file = paste("users",run,".RData",sep=""))
  save(edges2,file = paste("edges",run,".RData",sep=""))
}
setwd(rootFolder)
################################################################################
####################### test nets to make sure they are importing ##############
################################################################################

# load("/Users/Dave/Desktop/testingnet4/users10.RData")
# load("/Users/Dave/Desktop/testingnet4/net10.RData")
# load("/Users/Dave/Desktop/testingnet4/edges10.RData")

#plot(bsk.network,layout=layout.fruchterman.reingold,vertex.size=1,vertex.label=NA)
