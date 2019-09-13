##-----Kmean algorithm  more than one variable-----##


#load library
library(ggplot2)
library(dplyr)

#1- For programming propouse I create two samples of a normal random variable, the first one called N1 is a random sample 
#for a normal random variable with mean=0 and variance=1, the second one called N2 is a random sample for a normal random variable
# with mean=4 and variance=0.5. 

set.seed(12)
N1 <- rnorm(100,mean=0, sd=1)
N2 <- rnorm(100,mean=4, sd=0.5)

# then I create a dataframe named data that contains the N1 as variable 1 and N2 as variable 2 


data<-as.data.frame(cbind(N1,N2))

# we plot the data

b <-ggplot(data, aes(x =data[,1], y=data[,2]))
b + geom_dotplot(fill = "darkred", color = "darkred") +theme_minimal()+labs(x="N1", y="N2")

# we try with the kmeans command in R and 2 clusters

trial<-kmeans(data,2, iter.max=10) #store the results in trial
data$cluster_ral<-trial$cluster #add to the data the classification obtained by the algorithm (variable= cluster_ral = cluster r algorithm)

c <-ggplot(data, aes(x =data[,1], y=data[,2],fill=factor(cluster_ral))) # we plot the data with the cluster classification obtained by the R k-means algorithm
c + geom_dotplot() +labs(x="N1", y="N2")+theme_minimal()+ scale_fill_discrete(name = "Grupo", labels=c("1", "2"))


data<-data[,-3]
x<-data
k=2
it=1
scale=FALSE

##--run the function for more than one variable--##




kmeansgeneral<-function(k,x, it=10, scale=FALSE){
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



kmeansgeneral(2,x,it=1000,scale=FALSE)


# we plot the data with the resulting classification
data$clusters_al<-as.character(results$clusters)
plot.general <-ggplot(data, aes(x =data[,1], y=data[,2],fill=factor(results$clusters))) # we plot the data with the cluster classification obtained by the R k-means algorithm
plot.general + geom_dotplot() +labs(x="N1", y="N2")+theme_minimal()+ scale_fill_discrete(name = "Grupo", labels=c("1", "2"))


ralgo <- kmeans(x,2,it=1000)
data$cluster_ralg2 <- as.character(ralgo$cluster)

c <-ggplot(data, aes(x =data[,1], y=data[,2],fill=factor(data$cluster_ralg2))) # we plot the data with the cluster classification obtained by the R k-means algorithm
c + geom_dotplot() +labs(x="N1", y="N2")+theme_minimal()+ scale_fill_discrete(name = "Grupo", labels=c("1", "2"))


