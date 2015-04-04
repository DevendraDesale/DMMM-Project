## Creating Profile plots, which shows the variation in each of the variables, 
## by plotting the value of each of the variables for each of the samples.
makeProfilePlot <- function(mylist,names)
{
  require(RColorBrewer)
  # find out how many variables we want to include
  numvariables <- length(mylist)
  # choose 'numvariables' random colours
  colours <- brewer.pal(numvariables,"Set1")
  # find out the minimum and maximum values of the variables:
  mymin <- 1e+20
  mymax <- 1e-20
  for (i in 1:numvariables)
  {
    vectori <- mylist[[i]]
    mini <- min(vectori)
    maxi <- max(vectori)
    if (mini < mymin) { mymin <- mini }
    if (maxi > mymax) { mymax <- maxi }
  }
  # plot the variables
  for (i in 1:numvariables)
  {
    vectori <- mylist[[i]]
    namei <- names[i]
    colouri <- colours[i]
    if (i == 1) { plot(vectori,col=colouri,type="l",ylim=c(mymin,mymax)) }
    else         { points(vectori, col=colouri,type="l")                                     }
    lastxval <- length(vectori)
    lastyval <- vectori[length(vectori)]
    text((lastxval-10),(lastyval),namei,col="black",cex=0.6)
  }
}

mosthighlycorrelated <- function(mydataframe,numtoreport)
{
  # find the correlations
  cormatrix <- cor(mydataframe)
  # set the correlations on the diagonal or lower triangle to zero,
  # so they will not be reported as the highest ones:
  diag(cormatrix) <- 0
  cormatrix[lower.tri(cormatrix)] <- 0
  # flatten the matrix into a dataframe for easy sorting
  fm <- as.data.frame(as.table(cormatrix))
  # assign human-friendly names
  names(fm) <- c("First.Variable", "Second.Variable","Correlation")
  # sort and print the top n correlations
  
  head(fm[order(abs(fm$Correlation),decreasing=T),],n=numtoreport)
}

## Standardise your variables in dataset.
standardise <- function(x) {
  x-mean(x)
}

dmm_data <- read.csv('../project.csv')
summary(dmm_data)
require(car)

variables <- read.csv('../variables.csv',header = TRUE)
colnames(dmm_data) <-variables[,2][1:201]

scatterplotMatrix(dmm_data[2:6])
scatterplotMatrix(dmm_data[c('v200','v137'),])
scatterplotMatrix(dmm_data[,c('v200','v137','v141')])


mosthighlycorrelated(dmm_data, 400)
cor(dmm_data$moy5intep,dmm_data$moy1intep)
ggplot(dmm_data,aes(tf72,tf77)) + geom_line()
ggplot(dmm_data,aes(tf95,tf99)) + geom_line()
ggplot(dmm_data,aes(slg9,tf100)) + geom_line()
ggplot(dmm_data,aes(tf57,tf71)) + geom_line()

histogram(dmm_data$tf57)
histogram(dmm_data$tf71)


require(ggplot2)
require(reshape)
melt_dmm <- melt(dmm_data, id='objective', variable_name='series')

# plot on same grid, each series colored differently -- 
# good if the series have same scale  
ggplot(melt_dmm,aes(objective,value)) + geom_line(aes(colour = series))

# or plot on different plots
ggplot(df, aes(objective,value)) + geom_line() + facet_grid(series ~ .)

require(RColorBrewer)
names <- c('v1','v2','v3','v4')
mylist <- list(dmm_data$v1,dmm_data$v2,dmm_data$v3,dmm_data$v4)
makeProfilePlot(mylist,names)



## Perform principle component analysis

scaledData <- as.data.frame(scale(dmm_data[2:201]))
sapply(scaledData,mean)
head(scaledData)
## PCA analysis
dmm_pca <- prcomp(scaledData)
## see summary of PCA
summary(dmm_pca)

## the standard deviation of each pca and proportion of variance explained by it
dmm_pca$sdev
scatterplot(dmm_pca[,], type = "lines")
dmm_pca$rotation

hist(dmm_data[,1])
hist(dmm_data[,4])
for(i in 2:201){
  hist(dmm_data[,i],xlab = "for variable::v"+as.character(as.numeric(i)))
}
# Add density curve
install.packages("easyGgplot2")
require(devtools)
library(easyGgplot2)



library(MASS)
r <- lda(v1 ~ .,
         data = dmm_data)

## As we can see above, a call to lda returns 
# the prior probability of each class, 
# the counts for each class in the data, 
# the class-specific means for each covariate, 
# the linear combination coefficients (scaling) for each linear discriminant (remember that in this case with 3 classes we have at most two linear discriminants) and the singular values (svd) that gives the ratio of the between- and within-group standard deviations on the linear discriminant variables.
r$prior
r$mean
r$scaling
r$svd
prop = r$svd^2/sum(r$svd^2)
prop

## If we call lda with CV = TRUE it uses a leave-one-out 
# cross-validation and returns a named list with components:
r2 <- lda(formula = Objective ~ ., 
          data = dmm_data, 
          CV = TRUE)
head(r2$class)
head(r2$posterior, 3)

pca <- prcomp(dmm_data[,-201],
              center = TRUE,
              scale. = TRUE) 


dataset = data.frame(species = dmm_data[,"Objective"],
                     pca = pca$x, lda = plda$x)

p1 <- ggplot(dmm_data) + 
  geom_point(aes(lda.LD1, lda.LD2, colour = Objective, shape = Objective), size = 2.5)
+ 
  labs(x = paste("LD1 (", percent(prop.lda[1]), ")", sep=""),
       y = paste("LD2 (", percent(prop.lda[2]), ")", sep=""))
p1

library(randomForest)
cols <- names(dmm.train)[1:201]
clf <- randomForest(factor(Objective) ~ ., data=dmm.train[,cols],
                                ntree=600, nodesize=5, mtry=9)
table(dmm.test$Objective, predict(clf, dmm.test[cols]))
sum(dmm.test$Objective==predict(clf, dmm.test[cols])) / nrow(dmm.test)

## Plotting the OOB values
layout(matrix(c(1,2),nrow=1),
       width=c(4,1)) 
par(mar=c(5,4,4,0)) #No margin on the right side
plot(clf, log="y")
par(mar=c(5,0,4,2)) #No margin on the left side
plot(c(0,1),type="n", axes=F, xlab="", ylab="")
legend("top", colnames(clf$err.rate),col=1:4,cex=0.8,fill=1:4)

summary(clf)

#**************************
#return the rules of a tree
#**************************
getConds<-function(tree){
  #store all conditions into a list
  conds<-list()
  #start by the terminal nodes and find previous conditions
  id.leafs<-which(tree$status==-1)
  j<-0
  for(i in id.leafs){
    j<-j+1
    prevConds<-prevCond(tree,i)
    conds[[j]]<-prevConds$cond
    while(prevConds$id>1){
      prevConds<-prevCond(tree,prevConds$id)
      conds[[j]]<-paste(conds[[j]]," & ",prevConds$cond)
      if(prevConds$id==1){
        conds[[j]]<-paste(conds[[j]]," => ",tree$prediction[i])
        break()
      }
    }
    
  }
  
  return(conds)
}

#**************************
#find the previous conditions in the tree
#**************************
prevCond<-function(tree,i){
  if(i %in% tree$right_daughter){
    id<-which(tree$right_daughter==i)
    cond<-paste(tree$split_var[id],">",tree$split_point[id])
  }
  if(i %in% tree$left_daughter){
    id<-which(tree$left_daughter==i)
    cond<-paste(tree$split_var[id],"<",tree$split_point[id])
  }
  
  return(list(cond=cond,id=id))
}

#remove spaces in a word
collapse<-function(x){
  x<-sub(" ","_",x)
  
  return(x)
}

## install.packages('rminer')
require(rminer)
rminer::fit(Object ~ ., data = dmm.train, model = "randomForest", task = "class",search = 'search')


tree<-getTree(clf, k=1, labelVar=TRUE)
#rename the name of the column
colnames(tree)<-sapply(colnames(tree),collapse)
rules<-getConds(tree)
print(rules)

require('‘randomForestSRC’')
print(clf)
roc(clf$y, as.numeric(clf$predicted))

library(ROCR)
performance(clf)


require(party)
rf2 <- cforest(Objective ~ ., data=dmm.train, controls=cforest_control(mtry=2, mincriterion=0))
summary(rf2)
rf2

decisionTree <- rpart(Objective ~ .,
                   data=dmm.train,
                   method="class",
                   parms=list(split="information"),
                   control=rpart.control(usesurrogate=0, 
                                         maxsurrogate=0))

print(decisionTree)
printcp(decisionTree)
cat("\n")

## Drawing good decision tree
fancyRpartPlot(decisionTree, main="Decision Tree dmm_data.csv $ objective")

## Settin up the seed to a constant value, to get consistent results.

set.seed(1234)
## Generate the training and testing dataset
bound <- floor((nrow(dmm_data)/4)*3)         #define % of training and test set

dmm_data <- dmm_data[sample(nrow(dmm_data)), ]           #sample rows 
dmm.train <- dmm_data[1:bound, ]              #get training set
dmm.test <- dmm_data[(bound+1):nrow(dmm_data), ]    #get test set

# scale the predictor variables to [0,1]
## Scaling the training set
scale.cols <- c("totalspend","totaltrans","cfhuswife","mtenglish","mtsingres",
                "dw86to91","dwmaint","fiinca","fiincm","fsmlabf","hiinca",
                "hiincm","hiincs","hlenglish","hlfrench","improvres",
                "incgovp","ineflowp","infinca","infincm","infincs",
                "inhhlow","inhhlowp","inmfemina","inminca","inmincm",
                "inmincs","knenglish","lfmaunemr","lfmtunemr","lftaempl",
                "lfttempl","lfttunemr","moy5mov","ndtallind","rlcathol",
                "rlrcathol","first","productcount","productcount6","tenure",
                "tf108","tf129","tf27","tf37","tf38","tf39","tf68","tf73",
                "tf74","tf75","tf88","tf89","tf90","tf94","tf95","tf96")

minVals   <- sapply(dmm.train[,scale.cols],min)
ranges <- sapply(dmm.test[,scale.cols],function(x)diff(range(x)))

train.scaled <- as.data.frame(scale(dmm.train[,scale.cols],center=minVals,scale=ranges))

# Scale using the training pararmeters for minVals and Range.
test.scaled <- as.data.frame(scale(dmm.test[,scale.cols],center=minVals,scale=ranges))

dmm.train[,scale.cols] <- train.scaled[,scale.cols]
dmm.test[,scale.cols] <- test.scaled[,scale.cols]

delete.cols<-c("lfmtunemp","lfmtunemr","lfttempl","moy5mov","rlcathol","tf50",
            "tf56","tf70","tf73","tf89","tf91","tf95","tf96","p16rcy","mtsingres",
            "etbritish","etfrench","fiinca","p05spend","hiinca","hlenglish",
            "hlfrench","hlnonoff","improvres","imuk","incgovp","ineflow",
            "ineflowp","inf30plus","inf7to15","infinca","infincm","infincs",
            "inhhlow","inhhlowp","inm15to30","inm30plus","inm7to15","inmfemina",
            "inminca","inmincm","inmincs","knenglish","knfren","lfmaempl",
            "lfmaunemr","lfmtempl")

## Deleting the columns using the correlation, p-test and PCA analysis

train.reduce <- dmm.train[ , -which(names(dmm.train) %in% delete.cols)]
test.reduce <- dmm.test[ , -which(names(dmm.train) %in% delete.cols)]

trainTransformed <- predict(preProcValues, dmm.train[,scale.cols])
testTransformed <- predict(preProcValues, dmm.test[,scale.cols])