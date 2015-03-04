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
colnames(dmm_data) <-variables[,3][2:202]

scatterplotMatrix(dmm_data[1:10])
scatterplotMatrix(dmm_data[c('v200','v137'),])
scatterplotMatrix(dmm_data[,c('v200','v137','v141')])


mosthighlycorrelated(dmm_data, 400)

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

