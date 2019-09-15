##Libraries
#install.packages("cluster")
#install.packages("factoextra")

library(cluster)
library(factoextra)

##  CLUSTERING EXAMPLE
data("USArrests")
df <- scale(USArrests)

#for clustering analysis you have to remove or estimate missing data

df <- na.omit(df) #to remove missing data 

#the second step is to scale data 
#is used to determine standarized values for each element in a data set
#data transformation technique that can be used in clustering analysis it takes the mean of the 
#variable and divide with the sd. New value=(xi-Xmean)/sd
# Example
x <- matrix(1:10, ncol = 2)

plot(x)
x.scale<-scale(x)
plot(x.scale)

x1<-(x[,1]-mean(x[,1]))/ sd(x[,1])
x2<-(x[,2]-mean(x[,2]))/ sd(x[,2])
x.result<-cbind(x1,x2)


#we can olny center the data

x.scaleCenter<-scale(x, scale=FALSE)

#returning to our example


df<-scale(df) ## centering data frame df


head(df, n = 3) # View the firt 3 rows of the data

#plot the four standarized variables
par(mfrow=c(2,2))
plot(df[,1], main="Murder", col = "#009999", pch=19, ylab=" sd. value" , xlab="observation")
plot(df[,2], main="Assault", col = "#0000FF", pch=19, ylab=" sd. value" , xlab="observation")
plot(df[,3], main="UrbanPoP", col = "#000066", pch=19,ylab=" sd. value" , xlab="observation")
plot(df[,4], main="Rape", col = "#33FFFF", pch=19,ylab=" sd. value" , xlab="observation")

#jointplot
plot(df[,1],df[,2]  , col = "#009999", pch=19, ylab=" sd. value assault" , xlab="sd. value murder")
plot(df[,1],df[,3]  , col = "#009999", pch=19, ylab=" sd. value urbanpop" , xlab="sd. value murder")
plot(df[,1],df[,4]  , col = "#009999", pch=19, ylab=" sd. rape" , xlab="sd. value murder")

###########################################
##-------------distances-----------------##
###########################################

#The classification of observations into groups requires some methods for computing
#the distance or the (dis)similarity between each pair of observations. The result of
#this computation is known as a dissimilarity or distance matrix.
#There are many methods to calculate this distance information


set.seed(123)# Subset of the data
ss <- sample(1:50, 15) # Take 15 random rows
df <- USArrests[ss, ] # Subset the 15 rows
df.scaled <- scale(df) # Standardize the variables

#computing euclidean distance using r base dist function

dist.eucl <- dist(df.scaled, method = "euclidean")

# Reformat as a matrix
# Subset the first 3 columns and rows and Round the values
distance<-round(as.matrix(dist.eucl), 1)
cluster<-kmeans(x.scale, 2, iter.max = 10, nstart = 2)

# x is data
#centers= number of clasters (k) or a set of initial cluster centers. If a number, a random set of 
#distinct rows in x is chosen as the inital centers. 
#iter max. maximum number of interation allowed, default is 10
#algorithm = c("Hartigan-Wong", "Lloyd", "Forgy",
#"MacQueen")

str(cluster)


#graphic 
library(factoextra)
fviz_dist(dist.eucl)

######################################################################
##-------------------------Computing k-mean----------------------- ###
######################################################################

data("USArrests") # Loading the data set
df <- scale(USArrests) # Scaling the data
# View the firt 3 rows of the data
head(df, n = 3)
clusters<-kmeans(df,2)
clusters_type<-clusters$cluster

##number of clusters##
library(factoextra)
fviz_nbclust(df, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)


# Compute k-means with k = 4
set.seed(123)
km.res <- kmeans(df, 4, nstart = 25)
# Print the results
print(km.res)

## compute the mean of each cluster using original data

aggregate(USArrests, by=list(cluster=km.res$cluster), mean)

## add the point clasificattion to the original data

dd <- cbind(USArrests, cluster = km.res$cluster)


## resutls of cluster

# Cluster number for each of the observations
km.res$cluster

# Cluster size
km.res$size


# Cluster means
km.res$centers

#cluster plot

fviz_cluster(km.res, data = df,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)


library(usmap)
library(ggplot2)

dfplot<-statepop
dfplot<-dfplot[,-4]

dd$state<-rownames(dd)
library(ggplot2)
library(dplyr)

statesMap <- map_data("state")
dd$region <- tolower(dd$state)
crimeMap <- merge(statesMap, dd, by = "region")
str(crimeMap)


map<-ggplot(crimeMap, aes(x = long, y = lat))+geom_polygon(aes( group = group, fill=cluster)) 
map<-map+ labs(title="Clusters of crime", subtitle="Uscrime data", y="", x="")+theme_minimal()+ theme(axis.text.x=element_blank())+ theme(axis.text.y=element_blank())
map
