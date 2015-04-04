

## Loading the dataset into the working memory
dmm_data <- read.csv('../project.csv')
summary(dmm_data)

## Change the variable names for readability.
variables <- read.csv('../variables.csv',header = TRUE)
colnames(dmm_data) <-variables[,2][1:201]

## Setting up the seed to a constant value, to get consistent results.
set.seed(1234)
## Generate the training and testing dataset
bound <- floor((nrow(dmm_data)/10)*7)         #define % of training and test set

dmm_data <- dmm_data[sample(nrow(dmm_data)), ]           #sample rows 
dmm.train <- dmm_data[1:bound, ]              #get training set
dmm.test <- dmm_data[(bound+1):nrow(dmm_data), ]    #get test set

# scale the predictor variables to [0,1]
** Scaling the training set **
```{r}
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
ranges <- sapply(dmm.train[,scale.cols],function(x)diff(range(x)))

train.scaled <- as.data.frame(scale(dmm.train[,scale.cols],center=minVals,scale=ranges))

# Scale using the training pararmeters for minVals and Range.
test.scaled <- as.data.frame(scale(dmm.test[,scale.cols],center=minVals,scale=ranges))
#Assignin the values to actual variables
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

