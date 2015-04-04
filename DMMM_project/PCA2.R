##We can see now that there may be merit in discarding variables thought
# to be measuring the same underlying (but "latent") aspect
# of a collection of variables, because including the nearly-redundant 
# variables can cause the PCA to overemphasize their contribution. 
# There is nothing mathematically right (or wrong) about such a procedure; 
# it's a judgment call based on the analytical objectives and 
# knowledge of the data. But it should be abundantly clear that setting
# aside variables known to be strongly correlated with others can have
# a substantial effect on the PCA results.

## Here is the R code.

n.cases <- 240               # Number of points.
n.vars <- 200                  # Number of mutually correlated variables.
set.seed(26)                 # Make these results reproducible.
eps <- rnorm(n.vars, 0, 1/4) # Make "1/4" smaller to *increase* the correlations.
x <- matrix(rnorm(n.cases * (n.vars+2)), nrow=n.cases)
beta <- rbind(c(1,rep(0, n.vars)), c(0,rep(1, n.vars)), cbind(rep(0,n.vars), diag(eps)))
y <- x%*%beta                # The variables.
cor(y)                       # Verify their correlations are as intended.
plot(data.frame(y))          # Show the scatterplot matrix.

# Perform PCA on the first 2, 3, 4, ..., n.vars+1 variables.
p <- lapply(2:dim(beta)[2], function(k) prcomp(dmm_data[, 1:k], scale=TRUE))

# Print summaries and display plots.
tmp <- lapply(p, summary)
par(mfrow=c(2,2))
tmp <- lapply(p, plot)





library(MASS)
fit = lm(Sepal.Length ~ .^2,data=iris)
fit <- lm(objective ~ .^2, data = dmm_data)
step <- stepAIC(fit, direction="backward")
step$anova