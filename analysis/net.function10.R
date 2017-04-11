### functions for network building
#selects and individual that has between 11 and 20 friends. connects them to each other
#it then selects up to 10 friends (randomly) and connects up to 10 of their friends (randomly) to the network. 
# making the max number 1 + 20 + 10x10 = 121 

build_networks = function() {  # this was built to run "us" data.frame for 
  
  us_shuff = us[sample(1:10000),] #shuffles reviews 
  json = fromJSON(us_shuff[1]) #pulls a single review and turns it into a list 
  
  i = 2 #important to scroll through json list 
  while (length(json$friends)<10 | length(json$friends)>20) { #while the review has less than 10 friends or more than 20, shuffle us again 
    json = fromJSON(us_shuff[i]) #take the next person down the line if they don't have friends match. 
    i = i + 1  #to continue looping through this is necessary, or else it'll only choose i 
  } # json is our first entity, if the condition is met, nothing happens. 
  
  edges = matrix(0,nrow=0,ncol=2);
  friends_shuff = json$friends[sample(1:length(json$friends))] #takes a "sample" of friends from the user  (10 total), default size? maybe all of them?
  nodes = c(json$user_id, friends_shuff) #adds the all together 
  
  for (i in 1:length(friends_shuff)) { #loop through sampled (n = 10) friends
    un = which(nodes==json$user_id) #gets the first user id only
    fn = which(nodes==friends_shuff[i]) #gets all the friends ids
    if (un>fn){edges = rbind(edges,c(un,fn))}
    else{edges = rbind(edges,c(fn,un))}              
  }
  
  j = which(user_list==friends_shuff[1]) # build the "user_list" separately rather than doing it each time!
  json = fromJSON(us$u[j]) 

  for (i in 1:10) { #change this line to cycle through more than 10 reviewers (but not possible if the total number of reviewers is below the limited range (above))
    friends_shuff = json$friends[sample(1:length(json$friends))] # find json's friends, shuffle
    for (f in 1:min(c(10,length(friends_shuff)))) { #this takes the n'th number of friends from each of the seeds's friend's friends. 
      #if (runif(1)<.2 | f==1) {
      if (length(which(nodes==friends_shuff[f]))==0) { # make sure they are in "nodes" uniquely (ordered, to use which)
        nodes = c(nodes,friends_shuff[f])
      }      
      un = which(nodes==json$user_id)
      fn = which(nodes==friends_shuff[f])
      if (un>fn){edges = rbind(edges,c(un,fn))}
      else{edges = rbind(edges,c(fn,un))}      
      #}
    }      
    j = which(user_list==friends_shuff[1]) # build the "user_list" separately rather than doing it each time!
    json = fromJSON(us$u[j])
  }
  
  #### this fills the web!
  for (i in 1:length(nodes)) {
    j = which(user_list==nodes[i]) # build the "user_list" separately rather than doing it each time!
    json = fromJSON(us$u[j])  
    for (f in (i+1):length(nodes)) {
      if (length(which(json$friends==nodes[f]))>0) {
        un = i
        fn = f
        if (un>fn){edges = rbind(edges,c(un,fn))}
        else{edges = rbind(edges,c(fn,un))}            
      }
    }
  }
  
  edges = unique(edges) # this gets rid of repeats, but we need to get rid of loop (BIG NB)
  edges2 <<- cbind(edges[,2],edges[,1])
  
  conx2 = data.frame(from=edges[,1],to=edges[,2]) 
  bsk.network <<- graph.data.frame(conx2,directed=F)

  net_users <<- data.frame(nodes)
  
} 



