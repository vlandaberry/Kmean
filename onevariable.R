##-----Kmean algorithm  unique variable-----##


#libraries
library(ggplot2)

#1- For programming propouse I create two samples of a normal random variable, the first one called N1 is a random sample 
#for a normal random variable with mean=0 and variance=1, the second one called N2 is a random sample for a normal random variable
# with mean=4 and variance=0.5. 

set.seed(12)
N1 <- rnorm(100,mean=0, sd=1)
type1 <- rep(1,100)
type2 <- rep(2,100)
N2 <- rnorm(100,mean=4, sd=0.5)

data<-c(N1,N2)
type<-c(type1,type2)
data <- as.data.frame(cbind(data,type))
data$type <- as.factor(as.character(data$type))

# we plot both samples wwith different color, to see the graphic representation of this two groups.  They are quite different}
#so any algorithm should identify them as different groups. 

b <-ggplot(data, aes(x =data, fill=factor(type)))
b + geom_dotplot() +scale_y_continuous(NULL, breaks = NULL)+labs(x="")+theme_minimal()+ scale_fill_discrete(name = "Grupo", labels=c("N1", "N2"))

# the results whit the R function kmeans are the following:

data_ov <- data$data

##The prior steps for defining the algorithm are:


x<- as.data.frame(data_ov)

## we try with the kmeans command in R
trial<-kmeans(x,2, iter.max=19900)
data$cluster_ral<-trial$cluster

c <-ggplot(data, aes(x =data, fill=factor(cluster_ral)))
c + geom_dotplot() +scale_y_continuous(NULL, breaks = NULL)+labs(x="")+theme_minimal()+ scale_fill_discrete(name = "Grupo", labels=c("1", "2"))


#there are 4 points that are not correctly classified

# Function for only one variable

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
        if (abs(distance[j,n])<=abs(distance[j,n+1])){
          cluster[j,1] <-n
        }else{ 
          cluster[j,1]<-n+1
        }}
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





# graph to see the difference with the base algoritmh


data$finalcluster<-results$clusters


d <-ggplot(data, aes(x =data, fill=factor(finalcluster)))
d + geom_dotplot() +scale_y_continuous(NULL, breaks = NULL)+labs(x="")+theme_minimal()+ scale_fill_discrete(name = "Grupo", labels=c("1", "2"))




