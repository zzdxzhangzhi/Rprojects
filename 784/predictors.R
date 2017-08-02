# Load libraries
library(R330)
library(ISLR)

# make linear regression base
sp500 = lm(Today ~ . - Direction, data = Smarket)

# do simple prediction and caculate the error
predictions = predict(sp500, data = Smarket)
head(predictions)
actuals = Smarket$Today
mean((predictions - actuals)^2)

# Use cross.val from R330 package to do predict error estimation
cross.val(sp500, nfold = 10, nrep = 20)

####################################################################
cross.val.mod <- function (f, nfold = 10, nrep = 20, ...){
    X <- model.matrix(f$terms, model.frame(f))
    y = fitted.values(f) + residuals(f)
    n <- dim(X)[1]
    CV <- numeric(nrep)
    pred.error <- numeric(nfold)
    m <- n%/%nfold
    for (k in 1:nrep) {
        rand.order <- order(runif(n))
        yr <- y[rand.order]
        Xr <- X[rand.order, ]
        sample <- 1:m
        for (i in 1:nfold) {
              use.mat <- as.matrix(Xr[-sample,])
              test.mat <- as.matrix(Xr[sample,])
              y.use = yr[-sample]
              new.data <- data.frame(test.mat)
              fit <- lm(y.use ~ -1+use.mat)
              my.predict = test.mat%*%coefficients(fit) 
              pred.error[i] <- sum((yr[sample] - my.predict)^2)/m
              sample <- if(i==nfold) (max(sample)+1):n else sample + m

            }
            CV[k] <- mean(pred.error)
        }
mean(CV)
}
############################################# 

# Use many times of average of CV
cvvec = numeric(20)
for (i in 1:20) {
	cvvec[i] = cross.val.mod(sp500, nfold = 10, nrep = 1)
}
mean(cvvec)
sd(cvvec)

# Load libraries
library(bootstrap)
library(caret)

# Use bootstrap method to estimate PE





