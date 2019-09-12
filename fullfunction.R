###------------------------Full algorithm-----------------###

## run the function for only one variable ###
##-----Kmean algorithm  unique variable-----##


#load library
library(ggplot2)

#1- For programming propouse I create two samples of a normal random variable, the first one called N1 is a random sample 
#for a normal random variable with mean=0 and variance=1, the second one called N2 is a random sample for a normal random variable
# with mean=4 and variance=0.5. 

set.seed(12)
N1 <- rnorm(100,mean=0, sd=1)
type1 <- rep(1,100)
type2 <- rep(2,100)
N2 <- rnorm(100,mean=4, sd=0.5)

# then I create a dataframe named data that contains the variable and the type of data (1 if it comes from the sample N1 or 2 if it comens
#from the sambple N2)

data<-c(N1,N2)
type<-c(type1,type2)
data <- as.data.frame(cbind(data,type))
data$type <- as.factor(as.character(data$type))

# we plot both samples wwith different color to see the graphic representation of this two groups.  They are quite different


b <-ggplot(data, aes(x =data, fill=factor(type)))
b + geom_dotplot() +scale_y_continuous(NULL, breaks = NULL)+labs(x="")+theme_minimal()+ scale_fill_discrete(name = "Grupo", labels=c("N1", "N2"))

# the results whit the R function kmeans are the following:

data_ov <- data$data #We define a data frame data_ov (only variable) without the type
x<- as.data.frame(data_ov)

## we try with the kmeans command in R and 2 clusters

trial<-kmeans(x,2, iter.max=19900) #store the results in trial
data$cluster_ral<-trial$cluster #add to the data the classification obtained by the algorithm (variable= cluster_ral = cluster r algorithm)

c <-ggplot(data, aes(x =data, fill=factor(cluster_ral))) # we plot the data with the cluster classification obtained by the R k-means algorithm
c + geom_dotplot() +scale_y_continuous(NULL, breaks = NULL)+labs(x="")+theme_minimal()+ scale_fill_discrete(name = "Grupo", labels=c("1", "2"))


#there are 4 points that are not correctly classified

##--- Function for only one variable---

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
  labels<-as.character(1:k)
  d <-ggplot(x, aes(x =x[,1], fill=factor(finalcluster)))
  d<-d + geom_dotplot() +scale_y_continuous(NULL, breaks = NULL)+labs(x="")+theme_minimal()+ scale_fill_discrete(name = "Grupo", labels=labels)
  results<<-list(centers=finalcenters, clusters=finalcluster, k=k, max.it=it, z=z, data=x, plot=d)
  
}



# we try the function with k=2 and 10 iterations(default)

kmeans_one(2,x)
# graph to see the difference  in classification with the base algorithm


data$finalcluster<-results$clusters


d <-ggplot(data, aes(x =data, fill=factor(finalcluster)))
d + geom_dotplot() +scale_y_continuous(NULL, breaks = NULL)+labs(x="")+theme_minimal()+ scale_fill_discrete(name = "Grupo", labels=c("1", "2"))




##run the function for more than one variable##


kmeansgeneral<-function(k,x, it=1, scale=c(FALSE, TRUE)){
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
  distance_cluster<- matrix(0, nrow=nrow(x), ncol=d)
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
      distance_cluster[,n]<-rowSums(distance)
    }
    for (j in 1:nrow(distance_cluster)){
      cluster[j,1]<- which.min(abs(distance_cluster[j,1:ncol(x)]))
      distance_cluster[,d] <-cluster
    }
    
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
alt_kmean<-function(k,x,it=10, scale=c(FALSE, TRUE)){
if (ncol(x)==1){
  scale=TRUE
  kmeans_one(k,x,it)}else{
    kmeansgeneral(k,x,it,scale)
  }}

x=x


alt_kmean(2,x,it=20)
alt_kmean(2,z,it=30,scale=FALSE)
