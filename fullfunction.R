#############################################################
###------------------------Full algorithm-----------------###
#############################################################

library(dplyr)
library(ggplot2)

##-- Function for only one variable---##
#-Run the function for one variable

kmeans_one<-function(k,x, it=10){
  
  if(class(k)!="numeric"){
    print("number of clusters must be numeric")
    
  } 
  if(class(x)=="matrix"){
    x<-as.data.frame(x)
  }
  if(class(x)!="data.frame"){
    print("x must be a matrix or a data.frame")
    
  }
  seq_it<-1:it
  z <- 0
  withind <- rep(NA,k)
  finalcluster<-matrix(0,nrow=nrow(data), ncol=1)
  finalcenters <- rep(NA,k)
  distance <- matrix(0, nrow=nrow(x), ncol=k)
  cluster <- matrix(0, nrow=nrow(x), ncol=1)
  mincriterio<-NA
  while(z<it){
    z=z+1
    centers <-sample(x[,1], size=k) 
    
    for (n in 1:k){
      for (j in 1:nrow(distance)){
        distance[j,n] <- x[j,1]-centers[n]
      }}
    seq1 <- 1:(k-1)
    for (n in seq1){
      for (j in 1:nrow(distance)){
        cluster[j,1]<- which.min(abs(distance[j,]))
      }
    }
    
    withind_temp <-colSums(distance^2)
    mincriterio_temp <-sum(withind_temp)
    if(mincriterio_temp<=mincriterio|is.na(mincriterio)==TRUE){
      finalcluster<-cluster
      finalcenters<-centers
      withind<-withind_temp
      mincriterio<-mincriterio_temp 
      
    }else{
      finalcluster<-finalcluster
      withind<-withind
      mincriterio<-mincriterio
      finalcenters<-finalcenters
    }
    
  }

  results<<-list(centers=finalcenters, clusters=finalcluster, k=k, max.it=it, z=z, data=x)
  
}




##--run the function for more than one variable--##


kmeansgeneral<-function(k,x, it=1, scale=FALSE){
  if(class(k)!="numeric"){
    print("number of clusters must be numeric")
  } 
  if(class(x)=="matrix"){
    x<-as.data.frame(x)
  }
  if(class(x)!="data.frame"){
    print("x must be a matrix or a data.frame")
  }
  if (scale==FALSE){
    x <- scale(x) 
  }
  seq_it<-1:it
  z <- 0
  finalcluster<-matrix(0,nrow=nrow(data),ncol=1)
  finalcenters <- matrix(NA,nrow=k, ncol=ncol(x))
  centers<- matrix(NA,nrow=k,ncol =ncol(x))
  distance <- matrix(0, nrow=nrow(x), ncol=ncol(x))
  d=ncol(x)+1
  if (k>ncol(x)){
  distance_cluster<- matrix(0, nrow=nrow(x), ncol=k)}else{
  distance_cluster<- matrix(0, nrow=nrow(x), ncol=d)
  }
  cluster <- matrix(0, nrow=nrow(x), ncol=1)
  mincriterio<-NA
  mincriterio_temp <-rep(NA,k) 
  while(z<it){
    z=z+1
    centers[,1] <-sample(x[,1], size=k)
    
    for (n in 1:k){
      centers[n,] <-as.matrix(x[(which((x[,1])==centers[n,1])),])
      centers_temp <-t(as.matrix(centers[n,]))
      centers_temp <-rep(1,(nrow(distance))) %x% centers_temp
      distance<- (x-centers_temp)^2
      distance <- as.data.frame(distance)
      distance_cluster[,n]<-rowSums(distance)
    }
    for (j in 1:nrow(distance_cluster)){
      cluster[j,1]<- which.min(abs(distance_cluster[j,1:ncol(x)]))
      if (k>ncol(x)){
      distance_cluster[,k] <-cluster
    }else{ distance_cluster[,d] <-cluster}}
    
    for (i in 1:k) {
      distance_cluster_temp <- as.data.frame(distance_cluster)
      temp<-filter(distance_cluster_temp, distance_cluster_temp[,d]==i)
      temp <- temp[,-d]
      temp<-sum(colSums(temp))
      mincriterio_temp[i] <- temp
    }
    mincriterio_temp <- sum(mincriterio_temp)
    if(mincriterio_temp<=mincriterio|is.na(mincriterio)==TRUE){
      finalcluster<-cluster
      finalcenters<-centers
      mincriterio<-mincriterio_temp 
      
    }else{
      finalcluster<-finalcluster
      mincriterio<-mincriterio
      finalcenters<-finalcenters
    }}
  results<<-list(centers=finalcenters, clusters=finalcluster, k=k, max.it=it, z=z, data=x)
}


## All combination functions
#ver como dejar seteado por defecto scale FALSE
alt_kmean<-function(k,x,it=10, scale=FALSE){
  
  if((class(x)!="data.frame")){
    x=as.data.frame(x)
  }
  if(k>nrow(x)){
    print("more centers than observations NOT VALID!")
  }
  
  if ((ncol(x))>1){
    kmeansgeneral(k,x,it=10,scale=FALSE)}else{
     scale=TRUE
     kmeans_one(k,x,it)
    }}


##test with two examples 

x_one<-rnorm(100)

set.seed(12)
N1 <- rnorm(100,mean=0, sd=1)
N2 <- rnorm(100,mean=4, sd=0.5)
data<-as.data.frame(cbind(N1,N2))

alt_kmean(k=3,x_one,it=20)
results

alt_kmean(4,data,it=30,scale=FALSE)
