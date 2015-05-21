setwd('/media//cunctation/csun files//csun/')
train=read.csv('economicspcaplot.csv',header=TRUE)
nc=ncol(train)
dat=train[,3:nc-1]
dat.class=as.factor(train$cluster_label)
obs.pca <- dudi.pca(dat,scan=FALSE,center=FALSE,scal=FALSE,nf=10)
obs.bet<- bca(obs.pca, center=FALSE,scal=FALSE,scan=FALSE,dat.class,nf=3)
s.class(obs.bet$ls[0:30,,], fac=dat.class[0:30])



library(ggbiplot)
files <- list.files(pattern = "\\.csv$")
lapply(files,function(x){
  train<-read.csv(x,header=TRUE)
  nc<-ncol(train)
  dat<-train[,3:nc-1]
  dat.class<-as.factor(train$cluster_label)
  wow.pca <- prcomp(dat,scale.=FALSE,center=FALSE)
  ind<-1:ncol(dat)
  ind<-ind[colnames(dat) %in% c("SOM120","MATH150A","ENGL205","ACCT230","ACCT220","BLAW280","BUS302","FIN303")]
  wow <- ggbiplot(wow.pca,num_dir=ind, obs.scale=1,var.scale=1, 
              groups = dat.class, ellipse = FALSE, circle = TRUE,alpha=0.5)
  g <- wow + scale_color_discrete(name = '')
  g <- g + theme(legend.direction = 'horizontal', 
              legend.position = 'top') + ggtitle(paste("Common bottleneck courses in",
         unlist(strsplit(x,"p"))[1]))
  print(g)
})

