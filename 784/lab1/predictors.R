# Load libraries
library(R330)
library(ISLR)

# make linear regression base
head(Smarket)
use = sample(1250, 1000)
sp500.df = Smarket[use,]
rownames(sp500.df) = 1:1000

sp500 = lm(Today ~ . - Direction, data = sp500.df)

# do simple prediction and caculate the test error
newSp500.df = Smarket[-use,]
rownames(newSp500.df) = 1:250
predictions = predict(sp500, newdata = newSp500.df)
head(predictions)
actuals = newSp500.df$Today
mean((predictions - actuals)^2)

# Use cross.val from R330 package to do predict error estimation
cross.val(sp500, nfold = 10, nrep = 20)
cross.val(sp500, nfold = 10, nrep = 50)
cross.val(sp500, nfold = 10, nrep = 100)
cross.val(sp500, nfold = 5, nrep = 20)
cross.val(sp500, nfold = 10, nrep = 50)
cross.val(sp500, nfold = 10, nrep = 100)

# apparent error
mean(residuals(sp500)^2)

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

# Use bootstrap method to estimate PE
err.boot(sp500, B = 50)

# Use boostrap package
library(bootstrap)
theta.fit = function(x, y) {
			lsfit(x, y)
		}
theta.predict = function(fit, x) {
			cbind(1, x)%*%fit$coef
			}
sq.err = function(y, yhat) {
		(y - yhat)^2
		}

# CV10 estimation
y = sp500.df[,1]
x = sp500.df[,-1]
cv10Errs = crossval(x, y, theta.fit, theta.predict, ngroup = 10)
cv10 = mean((y - cv10Errs$cv.fit)^2)
cv10

boot = bootpred(x, y, nboot = 200, theta.fit, theta.predict,
			err.meas = sq.err)

# Use caret package
library(caret)

# CV10 estimation with caret
CV10 = train(Today ~ + Year + Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume - Direction, 
		data = sp500.df, 
		method = "lm",
		trControl = trainControl(method = "cv", number = 10, repeats = 20))
CV10

# bootstrap estimate
boot = train(Today~ . - Direction, 
		data = sp500.df,
		method = "lm", 
		trControl = trainControl(method = "boot", repeats = 200))
boot

# forward selection
null.model = lm(Today~1, data = sp500.df)
selected = step(null.model, 
		scope = formula(sp500), 
		direction = "forward",
		trace = 0)
selected

# backward elimination
selected = step(sp500, 
		scope = formula(sp500),
		direction = "backward", 
		trace = 0)
selected

# stepwise
selected = step(sp500, 
		scope = formula(sp500),
		direction = "both", trace = 0)
selected

# all possible regressions
allpossregs(sp500)








