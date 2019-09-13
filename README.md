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




### Morethanone.R example
