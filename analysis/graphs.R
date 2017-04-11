library(gplots)
library(ggplot2)
library(gridExtra)
library(plyr)
library(scales)

plot1 = ggplot(allWithRandom, aes(y=resid(lm(mean_across_rev_info~length)),x=edges, 
                                  colour=factor(original),fill=factor(original)))+
  geom_point(aes(shape=factor(original),size=2.4),colour="black",size=3)+
  geom_point(aes(color=factor(original)))+
  stat_smooth(method=lm, fullrange=TRUE, alpha = .6,colour="black",size = .5)+
  scale_colour_manual(values =c("darkgrey","darkblue"))+
  scale_fill_manual(values=c("darkgrey","darkblue"))+ # working on making this more transparent.
  theme_bw()+
  ggtitle("") + 
  theme(plot.title = element_text(lineheight=1, face="bold",hjust=0))+
  xlab("Edges")+
  ylab("Residual ACI")+
  theme(legend.title=element_blank(),legend.justification=c(1,0), legend.position=c(1,0),legend.text=element_text(size=14))+
  theme(axis.title.y = element_text(size = rel(1.4)))+
  theme(axis.title.x = element_text(size = rel(1.4)))

plot2 = ggplot(allWithRandom, aes(y=across_reviewer_sd_ent_chancap,x=edges, 
                                  colour=factor(original),fill=factor(original)))+
  geom_point(aes(shape=factor(original),size=2.4),colour="black",size=3)+
  geom_point(aes(color=factor(original)))+
  stat_smooth(method=lm, fullrange=TRUE, alpha = .6,colour="black",size = .5)+
  scale_colour_manual(values =c("darkgrey","darkblue"))+
  scale_fill_manual(values=c("darkgrey","darkblue"))+ # working on making this more transparent.
  theme_bw()+
  ggtitle("") + 
  theme(plot.title = element_text(lineheight=1, face="bold",hjust=0))+
  xlab("Edges")+
  ylab("Unigram information variability")+
  theme(legend.title=element_blank(),legend.justification=c(1,0), legend.position=c(1,0),legend.text=element_text(size=14))+
  theme(axis.title.y = element_text(size = rel(1.4)))+
  theme(axis.title.x = element_text(size = rel(1.4)))

plot3 = ggplot(allWithRandom, aes(y=resid(lm(mean_across_rev_info~length)),x=gini, 
                                  colour=factor(original),fill=factor(original)))+
  geom_point(aes(shape=factor(original),size=2.4),colour="black",size=3)+
  geom_point(aes(color=factor(original)))+
  stat_smooth(method=lm, fullrange=TRUE, alpha = .6,colour="black",size = .5)+
  scale_colour_manual(values =c("darkgrey","darkblue"))+
  scale_fill_manual(values=c("darkgrey","darkblue"))+ # working on making this more transparent.
  theme_bw()+
  ggtitle("") + 
  theme(plot.title = element_text(lineheight=1, face="bold",hjust=0))+
  xlab("Gini Coefficient")+
  ylab("Residual ACI")+
  theme(legend.title=element_blank(),legend.justification=c(1,0), legend.position=c(1,0),legend.text=element_text(size=14))+
  theme(axis.title.y = element_text(size = rel(1.4)))+
  theme(axis.title.x = element_text(size = rel(1.4)))

plot4 = ggplot(allWithRandom, aes(y=resid(lm(mean_across_rev_info~length)),x=log(degrees), 
                                  colour=factor(original),fill=factor(original)))+
  geom_point(aes(shape=factor(original),size=2.4),colour="black",size=3)+
  geom_point(aes(color=factor(original)))+
  stat_smooth(method=lm, fullrange=TRUE, alpha = .6,colour="black",size = .5)+
  scale_colour_manual(values =c("darkgrey","darkblue"))+
  scale_fill_manual(values=c("darkgrey","darkblue"))+ # working on making this more transparent.
  theme_bw()+
  ggtitle("") + 
  theme(plot.title = element_text(lineheight=1, face="bold",hjust=0))+
  xlab("Network Degree")+
  ylab("Residual ACI")+
  theme(legend.title=element_blank(),legend.justification=c(1,0), legend.position=c(1,0),legend.text=element_text(size=14))+
  theme(axis.title.y = element_text(size = rel(1.4)))+
  theme(axis.title.x = element_text(size = rel(1.4)))

plot5 = ggplot(allWithRandom, aes(y=resid(lm(mean_across_rev_info~length)),x=log(centralization), 
                                  colour=factor(original),fill=factor(original)))+
  geom_point(aes(shape=factor(original),size=2.4),colour="black",size=3)+
  geom_point(aes(color=factor(original)))+
  stat_smooth(method=lm, fullrange=TRUE, alpha = .6,colour="black",size = .5)+
  scale_colour_manual(values =c("darkgrey","darkblue"))+
  scale_fill_manual(values=c("darkgrey","darkblue"))+ # working on making this more transparent.
  theme_bw()+
  ggtitle("") + 
  theme(plot.title = element_text(lineheight=1, face="bold",hjust=0))+
  xlab("Network Centrality")+
  ylab("Residual ACI")+
  theme(legend.title=element_blank(),legend.justification=c(1,0), legend.position=c(1,0),legend.text=element_text(size=14))+
  theme(axis.title.y = element_text(size = rel(1.4)))+
  theme(axis.title.x = element_text(size = rel(1.4)))

plot1
plot2
plot3
plot4
plot5



