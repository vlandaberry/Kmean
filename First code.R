##-----Kmean algorithm-----##


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

# select the k initial centers 
# for the k initial centers
k<-2
centers <- sample(data$data,2)

# for a function

kmeans_1v<-function(k,x, it=10){
  seq_it<-1:it
   z <- 0
  withind <- rep(NA,k)
  finalcluster<-matrix(0,nrow=nrow(data), ncol=1)
  finalcenters <- rep(NA,k)
  centers <-sample(x[,1], size=k) 
  i=length(centers)
  distance <- matrix(0, nrow=nrow(x), ncol=i)
  cluster <- matrix(0, nrow=nrow(x), ncol=1)
   while (z<it){
 
  for (n in 1:i){
    for (j in 1:nrow(distance)){
  distance[j,n] <- x[j,1]-centers[n]
    }}
  seq1 <- 1:(i-1)
  for (n in seq1){
    for (j in 1:nrow(distance)){
    if (abs(distance[j,n])<=abs(distance[j,n+1])){
    cluster[j,1] <-n
    }else{ 
    cluster[j,1]<-n+1
    }}
  }
  cluster <-cluster 
  distance<-distance
  withind_temp <-colSums(distance^2)
  for (n in (1:k)){
  if(withind_temp[n]<=withind[n]|is.na(withind[n])==TRUE){
      finalcluster<<-cluster
      finalcenters<<-centers
      withind<<-withind_temp
  }}
  z<<-z+1}
  }




# find the k groups assigning each observation to its closer center
x <- as.data.frame(data[,1])
