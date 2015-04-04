
library(RCurl) # download https data
urlfile <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data'
x <- getURL(urlfile, ssl.verifypeer = FALSE)
adults <- read.csv(textConnection(x), header=F)

# if the above getURL command fails, try this:
# adults <-read.csv('https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data', header=F)

names(adults)=c('Age','Workclass','FinalWeight','Education','EducationNumber',
                'MaritalStatus','Occupation','Relationship','Race',
                'Sex','CapitalGain','CapitalLoss','HoursWeek',
                'NativeCountry','Income')

adults$Income <- ifelse(adults$Income==' <=50K',0,1)
dmy <- dummyVars(" ~ .", data = adults)
adultsTrsf <- data.frame(predict(dmy, newdata = adults))

dummyV
## We borrow two very useful functions from Stephen Turner:
# cor.prob and flattenSquareMatrix. cor.prob will create a correlation matrix 
# along with p-values and flattenSquareMatrix will flatten all 
# the combinations from the square matrix into a data frame of 
# 4 columns made up of row names, column names, 
# the correlation value and the p-value:
## Correlation matrix with p-values. See http://goo.gl/nahmV for documentation of this function
cor.prob <- function (X, dfr = nrow(X) - 2) {
  R <- cor(X, use="pairwise.complete.obs")
  above <- row(R) < col(R)
  r2 <- R[above]^2
  Fstat <- r2 * dfr/(1 - r2)
  R[above] <- 1 - pf(Fstat, 1, dfr)
  R[row(R) == col(R)] <- NA
  R
}

## Use this to dump the cor.prob output to a 4 column matrix
## with row/column indices, correlation, and p-value.
## See StackOverflow question: http://goo.gl/fCUcQ
flattenSquareMatrix <- function(m) {
  if( (class(m) != "matrix") | (nrow(m) != ncol(m))) stop("Must be a square matrix.") 
  if(!identical(rownames(m), colnames(m))) stop("Row and column names must be equal.")
  ut <- upper.tri(m)
  data.frame(i = rownames(m)[row(m)[ut]],
             j = rownames(m)[col(m)[ut]],
             cor=t(m)[ut],
             p=m[ut])
}
?rownames
corMasterList <- flattenSquareMatrix (cor.prob(adultsTrsf))
print(head(corMasterList,10))

## This final format allows you to easily order the
# pairs however you want - for example, 
# by those with the highest absolute correlation value:

corList <- corMasterList[order(-abs(corMasterList$cor)),]
print(head(corList,10))


corMasterList <- flattenSquareMatrix (cor.prob(dmm_data))
print(head(corMasterList,400))

## This final format allows you to easily order the
# pairs however you want - for example, 
# by those with the highest absolute correlation value:

corList <- corMasterList[order(-abs(corMasterList$cor)),]
print(head(corList,300))


## The top correlated pair (sex..Female and sex.Male),
# as seen above, won't be of much use as they are 
# the only two levels of the same factor.
# We need to process this a little further to be of practical use.
# We create a single vector of variable names by filtering those 
# with an absolute correlation of 0.2 or higher 
# against the outcome variable of Income:

selectedSub <- subset(corList, (abs(cor) > 0.2 & j == 'tf56'))
print(selectedSub)

bestSub <- as.character(selectedSub$i)
## Finally we plot the highly correlated pairs using the {psych} package's pair.
# panels plot (this can also be done on the original data as pair.
#panels can handle factor and character variables):
install.packages('psych')
library(psych)
pairs.panels(dmm_data[c(bestSub, 'tf56')])

## The pairs plot, and in particular the last Income column, 
# tell us a lot about our data set. Being never married 
# is the most negatively correlated with income over $50,000/year 
# and Hours Worked and Age are the most postively correlated.
save(dmm_data,file = 'MarketData.csv')

write.table(dmm_data, file = "foo.csv", sep = ",", col.names = NA,
            qmethod = "double")
## for understanding

# get some data from the mtcars built-in dataset
mydata <- mtcars[, c(1,3,4,5,6)]

# correlation matrix
cor(mydata)

# correlation matrix with p-values
cor.prob(mydata)

# "flatten" that table
flattenSquareMatrix(cor.prob(mydata))


## install.packages('PerformanceAnalytics')
# plot the data
library(PerformanceAnalytics)
chart.Correlation(mydata)
