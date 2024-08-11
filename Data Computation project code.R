library(dplyr)
library(ggplot2)
library(ggpubr)
library(cluster)
library(tidyverse) 
library(factoextra) 

data("USArrests")
#Data Cleaning - Handling missing values in each 
#counting missing values in each column
sum(is.na(USArrests$Murder))
sum(is.na(USArrests$Assault))
sum(is.na(USArrests$UrbanPop))
sum(is.na(USArrests$Rape))
# There is no missing data
#Detect outliers
summary(USArrests)
boxplot(USArrests)

# from boxplot, we see that the column Rape has outliers
ggplot(USArrests, aes(x="", y=Rape)) + geom_boxplot(fill = "purple")
# now we see that this col has 2 outliers, we can use boxplot.stat to see the outliers
boxplot.stats(USArrests$Rape)$out

# we decide to keep them because they are meaningful to our dataset 
# as seen, we have just 2 states the crime of rape is high but not 
# too much to detect it as outliers

# Since all the variables in the iris data are numeric, 
# I did not have to do any data preprocessing. 

#1.Hierarchical clustering
df <- USArrests 
#As we don't want the clustering algorithm to depend to an arbitrary variable unit, 
#we start by scaling/standardizing the data using the R function scale
df <- scale (df)
head (df)
# Agglomerative Hierarchical clustering 
# Dissimilarity matrix
d<- dist (df, method = "euclidean") 
# Hierarchical clustering using Complete Linkage
hc1<- hclust(d, method = "complete" )
# Plot the obtained dendrogram
plot (hc1, cex = 0.6, hang = - 1 )

# Compute with agnes
hc2<- agnes (df, method = "complete") 

# Agglomerative coefficient
hc2$ac

#methods to assess
m<- c("average", "single", "complete", "ward")
names (m) <- c("average", "single", "complete", "ward") 
# function to compute coefficient

ac <- function(x) {
  agnes (df, method = x ) $ac
} 
map_dbl (m, ac)

#Ward's method identifies the strongest clustering structure of the four methods assessed.
hc3 <- agnes (df, method = "ward")

pltree (hc3, cex = 0.6, hang = -1, main = "Dendrogram of agnes")

# compute divisive hierarchical clustering
hc4 <- diana (df)

# Divise coefficient; amount of clustering structure found
hc4$dc
# plot dendrogram
pltree (hc4, cex = 0.6 , hang = - 1 , main = "Dendrogram of diana")
# Ward's method
hc5<-  hclust(d, method = "ward.D2" )
# Cut tree into 4 groups
sub_grp <- cutree (hc5, k =4)
# Number of members in each cluster
table (sub_grp)
## sub_grp
## 1 2 3 4
## 7 12 19 12
#It's also possible to draw the dendrogram with a border around the 4 clusters.
#The argument border is used to specify the border colors for the rectangles: 
plot (hc5, cex = 0.6 )
rect.hclust (hc5, k = 4 , border = 2:5 )
#fviz_cluster function from the factoextra package to visualize the result in a scatter plot.
fviz_cluster (list(data = df, cluster = sub_grp))

#2.Kmeans clustering

# As per above hierarchical clustering, we can proceed for k-means with 4 centers.
k<- kmeans(df,centers = 4,nstart = 25) 
#nstart = 25. This means that R will try 25 different random starting
# and then select the best results corresponding
#to the one with the lowest within cluster variation

fviz_cluster(k,data = df)  # to visualize the cluster plot

#3.Cluster validation using the silhouette coefficient (Si)

#silhouette method implementation step by step
#create function to calculate Manhattan distance
manhattan_dist <- function(a, b){
  dist <- abs(a-b)
  dist <- sum(dist)
  return(dist)
}
#define defining the points as vectors
a<- c(0,0)
b<- c(0,1)
c<- c(2,3)
d<- c(3,3)
e<- c(3,4)

#calculate b(i)
b1=(manhattan_dist(a, e)+manhattan_dist(a, d))/2.0
b2=(manhattan_dist(b,e)+manhattan_dist(b, d))/2.0
b3=(manhattan_dist(c, e)+manhattan_dist(c, d))/2.0
b4=(manhattan_dist(d,a)+manhattan_dist(d,b)+manhattan_dist(d, c))/3.0
b5=(manhattan_dist(e,a)+manhattan_dist(e,b)+manhattan_dist(e, c))/3.0
b_i=c(b1,b2,b3,b4,b5)

#calculate a(i)
a1=(manhattan_dist(a,b)+manhattan_dist(a,c))/2.0
a2=(manhattan_dist(b,a)+manhattan_dist(b,c))/2.0
a3=(manhattan_dist(c,a)+manhattan_dist(c,b))/2.0
a4=manhattan_dist(e,d)/1.0
a5=manhattan_dist(d,e)/1.0
a_i=c(a1,a2,a3,a4,a5)

#calculate Silhouette s(i)
s1=(b1-a1)/max(a1,b1)
s2=(b2-a2)/max(a2,b2)
s3=(b3-a3)/max(a3,b3)
s4=(b4-a4)/max(a4,b4)
s5=(b5-a5)/max(a5,b5)
s_i=c(s1,s2,s3,s4,s5)

data <- data.frame(b_i= b_i,a_i=a_i, s_i=s_i)
data


sil <- silhouette(k$cluster, dist(df))
fviz_silhouette(sil)#implement cluster silhouette plot
fviz_nbclust(df, kmeans, method = "silhouette")

#4.Determining optimal number of clusters with elbow method
x <- c(runif(100, min=0, max=100))
#calculating total mean
tmean <- mean(x)
tmean

#calculting tss
ss <- NULL
for (i in 1:length(x)){
  square = abs(x[i]-tmean)^2
  ss = c(ss,square)
}
ss
tss = sum(ss)
tss
# we want to see how the elbow curve will be with different number of centers so we make it
# from 1 to 5 clusters to see how it affects within centers distances wss <- NULL

wss <- NULL
for (i in 1:5){
  kmean = kmeans(x,centers = i)
  wss = c(wss, kmean$tot.withinss)
}

#wss values from 1 to 5 clusters
wss

# elbow curve 
plot(1:5, wss, type = "o")

# now we find that k = 4 is the optimal number for k

# When you apply the K-means algorithm in R, 
# the function will help you generate multiple statistics of the model simultaneously, 
# including TSS, BSS, and wss.
# Note: we want to keep wss value as small as posible then BSS to TSS should be high ratio.

k3 = kmeans(x,centers = 3)

#calculating bss of k = 3

bss = tss - k3$tot.withinss
bss / tss  

#5.DB Scan
df <- cbind( df , State = rownames(df))
rownames(df) <- 1:nrow(df)
df
# load our packages
# note: only loading dbscacn, not loading fpc since we're not using it
library(dbscan)

# run dbscan::dbscan() on the first four columns of iris
db <- dbscan::dbscan(df[,1:4],eps =  20 ,minPts = 2)

# create a new data frame by binding the derived clusters to the original data
# this keeps our input and output in the same dataframe for ease of reference
data2 <- bind_cols(df, cluster = factor(db$cluster))

# make a table to confirm it gives the same results as the original code
table(data2$cluster, data2$State)

# using ggplot, make a point plot with "jitter" so each point is visible
# x-axis is species, y-axis is cluster, also coloured according to cluster
ggplot(data2) +
  geom_point(mapping = aes(x=cluster , y = State, colour = cluster),
             position = "jitter") +
  labs(title = "DBSCAN")

