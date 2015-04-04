library(MASS)
library(HSAUR)
library(cluster)
library(fpc)

d <- dist(as.matrix(dmm.train))   # find distance matrix 
hc <- hclust(d)                # apply hirarchical clustering 
plot(hc)                       # plot the dendrogram
(kc <- kmeans(dmm.train, 3) )
table(dmm.train$Objective, kc$cluster)
points(kc$centers[,c(0,1)], col=1:3, pch=8, cex=2)

## Plotting the silhouette
dissE <- daisy(dmm.train) 
dE2   <- dissE^2
sk2   <- silhouette(kc$cl, dE2)
plot(sk2)
plotcluster(dmm.train, kc$cluster)


clusplot(dmm.train, kc$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)
with(dmm.train, pairs(dmm.test, col=c(1:3)[kc$cluster])) 
summary(kc)


wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

#install.packages('NbClust')
library(NbClust)  # used for better cluster analysis. Gives various parameters to analysis and interpret better.

nc <- NbClust(dmm.train, min.nc=3, max.nc=5, method="kmeans")
barplot(table(nc$Best.n[1,]), 
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")

## Selecting 4 clusters to do clustering as per suggested by nb cluster library.
set.seed(1234)
fit.km <- kmeans(dmm.train, 4, nstart=25)                           #4
fit.km$size

fit.km$centers      

aggregate(dmm.train[-1], by=list(cluster=fit.km$cluster), mean)
table(dmm.train$Objective, fit.km$cluster)

plotcluster(dmm.train, fit.km$cluster)

## Plotting the parallel coordinates.
recent.cols<- c ('p01rcy','p02rcy','p03rcy','p04rcy','p06rcy','p07rcy','p08rcy',
                 'p09rcy','p11rcy','p12rcy','p13rcy','p14rcy','p15rcy','p16rcy')
dmm.recency <- dmm.train[,recent.cols]

##Plot parallel co-ordinates usiing objective to understand the relation
# Between the recency.
parcoord(dmm.recency, col=dmm.train$Objective,var.label = TRUE)
parcoord(dmm.recency, col=fit.km$cluster,var.label = TRUE)


fit.km <- kmeans(dmm.train, 3, nstart=25)                           #3
fit.km$size

fit.km$centers      

aggregate(dmm.train[-1], by=list(cluster=fit.km$cluster), mean)
table(dmm.train$Objective, fit.km$cluster)

plotcluster(dmm.train, fit.km$cluster)


## Plotting the parallel coordinates.
recent.cols<- c ('p01rcy','p02rcy','p03rcy','p04rcy','p06rcy','p07rcy','p08rcy',
                 'p09rcy','p11rcy','p12rcy','p13rcy','p14rcy','p15rcy','p16rcy')
dmm.recency <- dmm.train[,recent.cols]

##Plot parallel co-ordinates usiing objective to understand the relation
# Between the recency.
parcoord(dmm.recency, col=dmm.train$Objective,var.label = TRUE)
parcoord(dmm.recency, col=fit.km$cluster,var.label = TRUE)


