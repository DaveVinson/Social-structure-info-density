# ***************************************************************************************
# ***************************************************************************************
# **************** Analysis of networks/information from Yelp, Inc. Business Reviews ****
# ******* Written by David W. Vinson and Rick Dale for the purpose of demonstration *****
# ******************** PLEASE DO NOT DISTRIBUTE WITHOUT PERMISSION **********************  
# ***************************************************************************************
# ***************************************************************************************


################################################################################
############################### Load data  #####################################
################################################################################
run = read.csv('unique_net_index.csv')
allData = read.csv('new_info_net_true.csv')[run$x,]
allDataRandom = read.csv('new_info_net_baseline.csv')[run$x,]

allData = allData[complete.cases(allData),]


plot(log(allData$gini),log(allData$num_revs))
cor.test(allData$gini,allData$num_revs)
######################################################################################################################
###################### True network Anaysis ##########################################################################
######################################################################################################################

################################################################################
############################### Simple network analysis [true networks] ########
################################################################################
# center and standardize all independent variables 
allData$nodes = scale(allData$nodes, center = TRUE, scale = TRUE)
allData$edges = scale(allData$edges, center = TRUE, scale = TRUE)
allData$gini = scale(allData$gini, center = TRUE, scale = TRUE)

#num revs
mod= lm(log(allData$num_revs)~allData$gini)
summary(mod)

confint(mod, 'allData$gini', level=0.999)

#mean length 
mod= lm(log(allData$length)~allData$nodes+allData$edges+allData$gini)
summary(mod)
confint(mod, 'allData$edges', level=0.999)
confint(mod, 'allData$gini', level=0.999)

#within reviewer entropy
mod = lm(resid(lm(allData$mean_within_rev_uni_ent~log(allData$length)))  ### issues with NAs when residualized  
           ~allData$edges+allData$nodes+allData$gini)
summary(mod)
confint(mod, 'allData$edges', level=0.999)
confint(mod, 'allData$gini', level=0.999)

#across reviewer entropy
mod = lm(resid(lm(allData$mean_across_rev_uni_ent~log(allData$length)))
           ~allData$nodes+allData$edges+allData$gini)
summary(mod)
confint(mod, 'allData$gini', level=0.999)
confint(mod, 'allData$edges', level=0.999)
confint(mod, 'allData$nodes', level=0.999)

#across reviewer information
mod = lm(resid(lm(allData$mean_across_rev_info~log(allData$length)))~allData$nodes+allData$edges+allData$gini)
summary(mod)
confint(mod, 'allData$gini', level=0.999)
confint(mod, 'allData$edges', level=0.999)

#across reviewer uniform unigram density
mod = lm(resid(lm(log(allData$across_reviewer_sd_ent_chancap)~allData$mean_across_rev_uni_ent+log(allData$length)))
           ~allData$nodes+allData$edges+allData$gini)
summary(mod)
confint(mod, 'allData$gini', level=0.999)
confint(mod, 'allData$edges', level=0.999)
confint(mod, 'allData$nodes', level=0.999)

#across reviewer uniform information density
mod = lm(resid(lm(log(allData$across_reviewer_sd_info_chancap)~allData$mean_across_rev_info+log(allData$length)))
           ~allData$nodes+allData$edges+allData$gini)
summary(mod)
confint(mod, 'allData$gini', level=0.999)
confint(mod, 'allData$edges', level=0.999)

################################################################################
###################### test for collinearity among complext net measures #######
################################################################################
library(car)
# Not correct to do this. 
# between_test<-lm(resid(lm(betweens~nodes+edges))~degrees+scale_test+transitivities+centralization+gini,data=allData)
# vif(between_test)
# 
# degree_test<-lm(resid(lm(log(allData$degrees)~nodes+edges))~betweens+scale_test+transitivities+centralization+gini,data=allData)
# vif(degree_test)
# 
# cent_test<-lm(resid(lm(log(allData$centralization)~nodes+edges))~degrees+betweens+scale_test+transitivities+gini,data=allData)
# vif(cent_test)
# 
# scale<-lm(resid(lm(log(allData$alpha)~nodes+edges))~degrees+betweens+transitivities+centralization+gini,data=allData)
# vif(scale) 
# 
# trans_test<-lm(resid(lm(transitivities~nodes+edges))~scale_test+degrees+betweens+centralization+gini,data=allData)
# vif(trans_test) 

################################################################################
###################### clean complex net measures ##############################
################################################################################
#build residual variables having factored out due nodes and edges 
centralization = resid(lm(log(allData$centralization)~allData$nodes+allData$edges))
betweens = resid(lm(allData$betweens~allData$nodes+allData$edges))
transitivities = resid(lm(allData$transitivities~allData$nodes+allData$edges)) 
power = resid(lm(log(allData$alpha)~allData$nodes+allData$edges))  
degrees = resid(lm(log(allData$degrees)~allData$nodes+allData$edges))

centralization = scale(centralization, center = TRUE, scale = TRUE)
degrees = scale(degrees, center = TRUE, scale = TRUE)
betweens = scale(betweens, center = TRUE, scale = TRUE)
power = scale(power, center = TRUE, scale = TRUE)
transitivities = scale(transitivities, center = TRUE, scale = TRUE)

#determine what variables to log normalize
hist(allData$centralization)
hist(log(allData$degrees))
hist(allData$betweens)
hist(log(allData$alpha))
hist(allData$transitivities)

################################################################################
###################### analyze complex net measures ############################
################################################################################
#Length
mod = lm(log(allData$length)~
             degrees+transitivities+betweens+centralization+power)
summary(mod)
confint(mod, 'transitivities', level=0.999)
confint(mod, 'betweens', level=0.999)
confint(mod, 'centralization', level=0.999)

#RI-Ent 
mod = lm(resid(lm(allData$mean_within_rev_uni_ent~log(allData$length)))~
             degrees+transitivities+betweens+centralization+power)
summary(mod)
confint(mod, 'degrees', level=0.999)
confint(mod, 'transitivities', level=0.999)

#AUI 
mod = lm(resid(lm(allData$mean_across_rev_uni_ent~log(allData$length)))~
             degrees+transitivities+betweens+centralization+power)
summary(mod)
confint(mod, 'degrees', level=0.999)
confint(mod, 'centralization', level=0.999)

#ACI 
mod = lm(resid(lm(allData$mean_across_rev_info~log(allData$length)))~
             degrees+transitivities+betweens+centralization+power)
summary(mod)
confint(mod, 'degrees', level=0.999)
confint(mod, 'transitivities', level=0.999)

#AIV
mod = lm(resid(lm(allData$across_reviewer_sd_ent_chancap~allData$mean_across_rev_uni_ent+log(allData$length)))~
             degrees+transitivities+betweens+centralization+power)
summary(mod)

#CIV
mod = lm(resid(lm(allData$across_reviewer_sd_info_chancap~allData$mean_across_rev_info+log(allData$length)))~
           degrees+transitivities+betweens+centralization+power)
summary(mod)


######################################################################################################################
###################### Compare true nets to random (base) nets #######################################################
######################################################################################################################
#reload data
run = read.csv('~/Dropbox/Dale/yelp/mike jones chapter/data/unique_net_index.csv')
allData = read.csv('~/Dropbox/Dale/yelp/mike jones chapter/data/new_info_net_true.csv')[run$x,]
allDataRandom = read.csv('~/Dropbox/Dale/yelp/mike jones chapter/data/new_info_net_baseline.csv')[run$x,]

allData = allData[complete.cases(allData),]

#create true vs. baseline variable and bind data
allData$original = 1 
allDataRandom$original = 0
allWithRandom = rbind(allData,allDataRandom)

################################################################################
###################### Center and scale variables ##############################
################################################################################
allWithRandom$nodes = scale(allWithRandom$nodes, center = TRUE, scale = TRUE)
allWithRandom$edges = scale(allWithRandom$edges, center = TRUE, scale = TRUE)
allWithRandom$gini = scale(allWithRandom$gini, center = TRUE, scale = TRUE)
allWithRandom$centralization = scale(allWithRandom$centralization, center = TRUE, scale = TRUE)
allWithRandom$degrees = scale(allWithRandom$degrees, center = TRUE, scale = TRUE)
allWithRandom$betweens = scale(allWithRandom$betweens, center = TRUE, scale = TRUE)
allWithRandom$transitivities = scale(allWithRandom$transitivities, center = TRUE, scale = TRUE)
allWithRandom$alpha = scale(allWithRandom$alpha, center = TRUE, scale = TRUE)
allWithRandom$originalC = allWithRandom$original - mean(allWithRandom$original)#no reason to scale

awr = allWithRandom[complete.cases(allWithRandom),]

################################################################################
### compare true vs. base for each signficiant true network measure ############
##################### some examples below ######################################
################################################################################
mod = lm(log(length)~centralization*originalC,data=awr) 
summary(mod)

mod = lm(resid(lm(across_reviewer_sd_info_chancap~log(length)+mean_across_rev_info))
         ~edges*originalC,data=awr)
summary(mod)

colnames(awr)
################################################################################
###################### Interaction Plots #######################################
################################################################################

n = resid(lm(allWithRandom$mean_across_rev_info~allWithRandom$length))


#reset data when making plots
#DO NOT scale/center for plots
allWithRandom = allWithRandom[complete.cases(allWithRandom),]
allWithRandom$original = as.factor(allWithRandom$original)
allWithRandom$original=revalue(allWithRandom$original, c("0"="Baseline", "1"="True Network"))

#load libraries 
library(ggplot2)
library(gridExtra)
library(plyr)
library(scales)
colnames(allWithRandom)
################################################################################
plot1 = ggplot(allWithRandom, aes(y=resid(lm(mean_across_rev_info~length)),x=edges, 
                                 colour=factor(original),fill=factor(original)))+
  geom_point(aes(shape=factor(original),size=2.4),colour="black",size=3)+
  geom_point(aes(color=factor(original)))+
  stat_smooth(method=lm, fullrange=TRUE, alpha = .6,colour="black",size = .5)+
  scale_colour_manual(values =c("darkgrey","darkblue"))+
  scale_fill_manual(values=c("darkgrey","darkblue"))+ # working on making this more transparent.
  theme_bw()+
  ggtitle("A.") + 
  theme(plot.title = element_text(lineheight=1, face="bold",hjust=0))+
  xlab("Edges")+
  ylab("Residual ACI")+
  theme(legend.position="none")+
  theme(axis.title.y = element_text(size = rel(1.4)))+
  theme(plot.margin=unit(c(.5,0,.25,0), "cm"))+
  theme(axis.title.x = element_text(size = rel(1.4)))

################################################################################
plot2 = ggplot(allWithRandom, aes(y=resid(lm(mean_across_rev_info~length)),x=degrees, 
                                  colour=factor(original),fill=factor(original)))+
  geom_point(colour="black",aes(shape=factor(original),size=2.4),size=3)+
  geom_point(aes(color=factor(original)))+
  stat_smooth(method=lm, fullrange=TRUE, alpha = .6,colour="black",size = .5)+
  scale_colour_manual(values =c("darkgrey","darkblue"))+
  scale_fill_manual(values =c("darkgrey","darkblue"))+
  theme_bw()+
  ggtitle("B.") + 
  theme(plot.title = element_text(lineheight=1, face="bold",hjust=0))+
  xlab("Degree")+
  #ylab("Residual ACI")+
  theme(axis.text.y = element_text())+
  theme(legend.title=element_blank(),legend.justification=c(1,0), legend.position=c(1,0),legend.text=element_text(size=14))+
  theme(axis.title.y = element_blank())+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.margin=unit(c(.5,0,.25,0), "cm"))+
  theme(axis.title.x = element_text(size = rel(1.4)))

################################################################################
plot3 = ggplot(allWithRandom, aes(y=resid(lm(mean_across_rev_info~length)),x=resid(lm(transitivities~nodes+edges)), 
                                  colour=factor(original),fill=factor(original)))+
  geom_point(colour="black",aes(shape=factor(original),size=2.4),size=3)+
  geom_point(aes(color=factor(original)))+
  stat_smooth(method=lm, fullrange=TRUE, alpha = .6,colour="black",size = .5)+
  scale_colour_manual(values =c("darkgrey","darkblue"))+
  scale_fill_manual(values =c("darkgrey","darkblue"))+
  theme_bw()+
  ggtitle("C.") + 
  theme(plot.title = element_text(lineheight=1, face="bold",hjust=0))+
  xlab("Residual Transitivity")+
  #ylab("Residual ACI")+
  theme(legend.position="none")+
  theme(axis.title.y = element_blank())+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.margin=unit(c(.5,.25,.25,0), "cm"))+
  theme(axis.title.x = element_text(size = rel(1.4)))


################################################################################  
grid.arrange(plot1,plot2,plot3, nrow=1,ncol=3,
             main=textGrob("True Network vs. Baseline",gp=gpar(fontsize=20), just="top"))
################################################################################


################################################################################
###################### ACI network figure ######################################
################################################################################
library(network)
library(rjson)
library(igraph)

allData = allData[complete.cases(allData),]
allData <- allData[order(transitivities,-degrees,edges),]

testing = allData[with(allData, order(-degrees,transitivities)), ]

head(testing)
head(testing$run)
print(testing$run[500:505],)
tail(testing$run,10)#tail is high
head(allData$degrees)
head(allData)
par(mfrow=c(1,3),cex=.5,cex.axis = 2,cex.main=2,
    cex.lab=2,mgp=c(5,1,0),mar=c(3,3,5,3)+0.1, 
    oma = c(1,1,1,1) + 0.1)

#high ACI
load(paste("/Users/Dave/Documents/yelp_dataset_mil/network_data_1000/users",509,".RData",sep=""))
load(paste("/Users/Dave/Documents/yelp_dataset_mil/network_data_1000/net",509,".RData",sep=""))
load(paste("/Users/Dave/Documents/yelp_dataset_mil/network_data_1000/edges",509,".RData",sep=""))
plot(bsk.network,layout=layout.fruchterman.reingold,vertex.size=1,vertex.label=NA,main="A. High ACI")#,vertex.label=labs,vertex.label.cex=.6)#,vertex.label=labs2)#,vertex.label=NA)

#Middle ACI
load(paste("/Users/Dave/Documents/yelp_dataset_mil/network_data_1000/users",938,".RData",sep=""))
load(paste("/Users/Dave/Documents/yelp_dataset_mil/network_data_1000/net",938,".RData",sep=""))
load(paste("/Users/Dave/Documents/yelp_dataset_mil/network_data_1000/edges",938,".RData",sep=""))
plot(bsk.network,layout=layout.fruchterman.reingold,vertex.size=1,vertex.label=NA,main="B. Middle ACI")#,vertex.label=labs,vertex.label.cex=.6)#,vertex.label=labs2)#,vertex.label=NA)

#Low ACI
load(paste("/Users/Dave/Documents/yelp_dataset_mil/network_data_1000/users",571,".RData",sep=""))
load(paste("/Users/Dave/Documents/yelp_dataset_mil/network_data_1000/net",571,".RData",sep=""))
load(paste("/Users/Dave/Documents/yelp_dataset_mil/network_data_1000/edges",571,".RData",sep=""))
plot(bsk.network,layout=layout.fruchterman.reingold,vertex.size=1,vertex.label=NA,main="C. Low ACI")#,vertex.label=labs,vertex.label.cex=.6)#,vertex.label=labs2)#,vertex.label=NA)
497




#
#
#
#################################################################################
############################## Additional plots for conferences, run code for previous plots first ################
################################################################################
## shows the basline first, then then the actual data next. use invisible box to overlay ##########
base1 = ggplot(allWithRandom, 
               aes(y=resid(lm(mean_across_rev_info~length)),
                   x=edges, ##resid(lm(transitivities~edges+nodes)), resid(lm(degrees~edges+nodes))
                   colour=factor(original),fill=factor(original)))+
  ylim(-1.1,.65)+
 # xlim(-2.5,5)+
  #geom_point(aes(shape=factor(original),size=2.4),colour="black",size=3)+
  #geom_point(aes(color=factor(original)))+
  #geom_point(colour="black",size=3,pch=21)+  #outlines only subsetted point in black
  geom_point(size=3,pch=21)+  #outlines only subsetted point in black
  stat_smooth(method=lm, fullrange=TRUE,data=allWithRandom, alpha = .6,aes(colour = original), size = .5)+
  scale_colour_manual(values =c("black",NA))+ #removes color from line on stat_smooth
  scale_fill_manual(values=c("darkgrey",NA))+ # removes color from subset of points
  theme_bw()+
  theme(plot.title = element_text(lineheight=1, face="bold",hjust=0))+
  xlab("Network Transitivity")+
  ylab("Residual ACI")+
  theme(axis.text.y = element_text())+
  theme(legend.title=element_blank(),legend.justification=c(1,0), legend.position=c(1,0),legend.text=element_text(size=14))+
  theme(axis.title.y = element_text(size = rel(1.4)))+
  theme(axis.title.x = element_text(size = rel(1.4)))


#######
true = ggplot(allWithRandom, 
               aes(y=resid(lm(mean_across_rev_info~length)),
                   x=edges, ##resid(lm(transitivities~edges+nodes)), resid(lm(degrees~edges+nodes))
                   colour=factor(original),fill=factor(original)))+
  ylim(-1.1,.65)+
  #xlim(-2.5,5)+ ## degrees = -1, 1.8: trans = -2.5,2.6?
  #geom_point(aes(shape=factor(original),size=2.4),colour="black",size=3)+
  #geom_point(aes(color=factor(original)))+
  geom_point(colour="black",size=3,pch=21)+  #outlines only subsetted point in black
  #geom_point(size=3,pch=21)+  #outlines only subsetted point in black
  stat_smooth(method=lm, fullrange=TRUE,data=allWithRandom, alpha = .6,aes(colour = original), size = .5)+
  scale_colour_manual(values =c("black","darkblue"))+ #removes color from line on stat_smooth
  scale_fill_manual(values=c("darkgrey","darkblue"))+ # removes color from subset of points
  theme_bw()+
  theme(plot.title = element_text(lineheight=1, face="bold",hjust=0))+
  xlab("Network Transitivity")+
  ylab("Residual ACI")+
  theme(axis.text.y = element_text())+
  theme(legend.title=element_blank(),legend.justification=c(1,0), legend.position=c(1,0),legend.text=element_text(size=14))+
  theme(axis.title.y = element_text(size = rel(1.4)))+
  theme(axis.title.x = element_text(size = rel(1.4)))

base1
true
