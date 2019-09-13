# K-mean

This repository contains a kmean function programmed in R:

Whit this function you can calculate the centers and obtain the data clasiffication in clusters using k-mean method. 

- some codes on K-means using the R function kmean.

- real data analysis using k-mean algortihm.


## K-mean function programmed in R

- The onevariable.R code has the code to apply K-mean over 1 variable. 
- The morethanone.R code has the code to apply  K-means with more than one variable
- The fullfunction.R has the code of k-mean algortihm for all cases

- The HowIdidit.Rmd is a file containing  a step by step explanation about what I did in each code and the elements I define.



### Onevariable.R example 

This function is applied for a one variable problem. If you have only one variable and you want classify this variable in clusters using k-means algorithm you can use directly this function. 
kmeans_one(k,x,it) arguments are:
- k= number of clusters 
- x= data frame or vector with the variable data
- it= number of iterations (default it=10)


In the code we generate two samples of two different random variables, and then combine them in one vector. We want to see if the base algorithm provided by r (kmeans) and our algorithm (kmeans_one) can classify the observations according to the random variable that generates the observation. 

*Example*
set.seed(12)
	N1 <- rnorm(100,mean=0, sd=1) 
	N2 <- rnorm(100,mean=4, sd=0.5)
  data<-as.data.frame(c(N1,N2))

For two cluster k=2 and it=100
kmeans_one(2,data,it=100)

In the environment you get a results list witrh the following information:

List of 6:
 - centers : num [1:2] 0.834 1.145 
 - clusters: num [1:200, 1] 1 2 1 1 1 1 1 1 1 1 ...
 - k       : num 2
 - max.it  : num 100
 - z       : num 100
 - data    :'data.frame':	200 obs. of  1 variable:
 - c(N1, N2): num [1:200] -1.481 1.577 -0.957 -0.92 -1.998 ...

- *results$centers:* a vector containing the value of the centers of each cluster. In this examples 1 cluster has a center in 0.834 and the other in 1.145.

- *results$clusters*: a vector with labels for each observation indicating the cluster assigned to that observation. 
- *results$k*: number of clusters
- *results$max.it*: number of iterations defined
- *results$z*: number of iterations actually realized. It should always coincide. Is a control resutl
- *results$data*: original data



### Morethanone.R example

If you have more than one variable in your data frame and you want to aplly kmeans you can use the function kmeansgeneral(k,x, it, scale). The arguments of this function are:
- k= number of clusters 
- x= data frame or vector with the variable data
- it= number of iterations (default it=10)
-scale (default value is FALSE). If the data is not scaled then default should remain as FALSE and the algorithm also 



