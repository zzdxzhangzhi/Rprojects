library(MASS)
data(Boston)

ppr.fit = ppr(log(medv) ~ ., data = Boston,
              sm.method = "spline", max.terms = 6, nterms = 4)

summary(ppr.fit)
par(mfrow = c(2, 2))
plot(ppr.fit, cex.lab = 1.5)

RSS = sum(residuals(ppr.fit)^2)
RSS.null = sum((log(Boston$medv) - mean(log(Boston$medv)))^2)
R2 = 1 - RSS/RSS.null

RSS
RSS.null
R2

library(earth)
data(Boston)
mars.fit = earth(log(medv) ~ ., data = Boston,
                 nk = 30, degree = 2)

summary(mars.fit)
par(mfrow = c(2, 2))
plot(mars.fit)

library(nnet)
data(Boston)

nnet.fit = nnet(log(medv) ~ ., data = Boston, size = 6, 
                linout = TRUE, decay = 0.01, maxit = 500)

summary(nnet.fit)
RSS = sum(residuals(nnet.fit)^2)
RSS.null = sum((log(Boston$medv) - mean(log(Boston$medv)))^2)
R2 = 1 - RSS/RSS.null

RSS
RSS.null
R2

library(caret)
my.grid = expand.grid(.decay = c(0.01, 0.001), .size = c(4, 6, 8))
nn.CV = train(log(medv) ~ ., data = Boston, 
              method = "nnet", 
              maxit = 1000,
              tuneGrid = my.grid,
              trace = FALSE,
              linout = TRUE,
              trControl = trainControl(method = "cv",
                                       number = 5,
                                       repeats = 100))

nn.CV

library(rpart)
x = sort(runif(100))
y = 1 + 0.5 * x + 2 * x^2 + 0.5 * rnorm(100)

stuff = rpart(y ~ x, cp = 0.08)
par(mfrow = c(1, 3))
plot(x, y, pch = 19, cex = 1.3, cex.lab = 1.4, cex.axis = 1.4)
abline(h = mean(y), lwd = 2, col = "blue")
abline(v = stuff$splits[1, 4], lty = 2, lwd = 2, col = "red")

R1 = x < stuff$splits[1, 4]
m1 = mean(y[R1])
m2 = mean(y[!R1])

lines(c(-1, stuff$splits[1, 4]), c(m1, m1), col = "blue", lwd = 2)
lines(c(stuff$splits[1, 4], 1.1), c(m2, m2), col = "blue", lwd = 2)

R2 = x < stuff$splits[2, 4]
plot(x, y, pch = 19, cex = 1.3, cex.lab = 1.4, cex.axis = 1.4)
abline(v = stuff$splits[1, 4], lty = 2, lwd = 2, col = "red")
abline(v = stuff$splits[2, 4], lty = 2, lwd = 2, col = "red")

m1 = mean(y[R1])
m2 = mean(y[R2 & !R1])
m3 = mean(y[!R2])

lines(c(-1, stuff$splits[1, 4]), c(m1, m1), col = "blue", lwd = 2)
lines(c(stuff$splits[1, 4], stuff$splits[2, 4]), c(m2, m2), col = "blue", lwd = 2)
lines(c(stuff$splits[2, 4], 1.1), c(m3, m3), col = "blue", lwd = 2)

par(mfrow = c(1, 1))
plot(stuff, branch = 0, lwd = 2, uniform = TRUE)
text(stuff, cex = 1.5, col = "blue")

library(R330)
data(cherry.df)
head(cherry.df)

library(rpart)
my.fit = rpart(volume ~ height + diameter,
               data = cherry.df,
               cp = 0.001, minsplit = 7)
plot(my.fit, lwd = 2, col = "red",
     branch = 0, uniform = TRUE)
text(my.fit, col = "darkblue", cex = 1.3)

RSS.tree = sum((cherry.df$volume - predict(my.fit))^2)

my.fit.lm = lm(volume ~ height + diameter, 
               data = cherry.df)
RSS.lm = sum((cherry.df$volume - predict(my.fit.lm))^2)

par(mfrow = c(1, 1))

h.range = range(cherry.df$height)
d.range = range(cherry.df$diameter)

plot(d.range, h.range, type = "n",
     xlab = "Diameter", ylab = "Height",
     cex.axis = 1.3, cex.main = 1.4,
     cex.lab = 1.3, main = "First split")
  
x1 = c(7.5, 16.15, 16.15, 7.5)
x2 = c(16.15, 21.1, 21.1, 16.15)
y1 = c(58, 58, 92, 92)
y2 = c(58, 58, 92, 92)

my.cols = terrain.colors(8)
polygon(x1, y1, col = my.cols[1])
polygon(x2, y2, col = my.cols[4])
lines(c(16.15, 16.15), h.range + c(-5, 5), lwd = 2)
box(lwd = 2)

points(cherry.df$diameter, cherry.df$height,
       pch = 19, col = "red", cex = 1.3)

mypos = rep(4, 31)
mypos[28] = 2
mypos[8] = 3

text(cherry.df$diameter, cherry.df$height,
     cherry.df$volume, pos = mypos, cex = 0.8)

R1 = cherry.df$diameter < 16.15
m1 = mean(cherry.df$volume[R1])
m2 = mean(cherry.df$volume[!R1])

text(10, 75, m1, col = "darkblue", cex = 1.3)
text(18.5, 75, m2, col = "darkblue", cex = 1.3)

plot(d.range, h.range, type = "n",
     xlab = "Diameter", ylab = "Height",
     cex.axis = 1.3, cex.main = 1.4,
     cex.lab = 1.3, main = "Second split")

x1 = c(12.45,16.15,16.15, 12.45)
x2 = c(16.15,21.1, 21.1,16.15)
x3 = c(7.5,12.45,12.45,7.5)
y1 = c(58,58,92,92)
y2 = c(58,58,92,92)
y3 = c(58,58,92,92)

polygon(x1, y1, col = my.cols[3])
polygon(x2, y2, col = my.cols[4])
polygon(x3, y3, col = my.cols[1])
lines(c(12.45, 12.45), h.range + c(-5, 5), lwd = 2)
lines(c(16.15, 16.15), h.range + c(-5, 5), lwd = 2)
box(lwd = 2)

points(cherry.df$diameter, cherry.df$height,
       pch = 19, col = "red", cex = 1.3)

mypos = rep(4, 31)
mypos[28] = 2
mypos[8] = 3

text(cherry.df$diameter, cherry.df$height,
     cherry.df$volume, pos = mypos, cex = 0.8)

R1 = cherry.df$diameter < 16.15
R2 = cherry.df$diameter < 12.45
m1 = round(mean(cherry.df$volume[R2]), 2)
m2 = round(mean(cherry.df$volume[!R2 & R1]), 2)
m3 = round(mean(cherry.df$volume[!R1]), 2)

text(10, 75, m1, col = "darkblue", cex = 1.3)
text(14, 75, m2, col = "darkblue", cex = 1.3)
text(18.5, 75, m3, col = "darkblue", cex = 1.3)

plot(d.range, h.range, type="n", 
     xlab = "Diameter",ylab = "Height", 
     cex.axis=1.3, cex.main = 1.4, cex.lab = 1.3,main = "Final split")

x1 = c(7.5,9.65,9.65, 7.5)
x2 = c(9.65, 11.05,11.05,9.65)
x3 = c(11.05, 12.45,12.45,11.05)
x4 = c(11.05, 12.45,12.45,11.05)
x5 = c(12.45,13.90,13.90,12.45)
x6 = c(13.90,16.50,16.50,13.90)
x7 = c(16.15,21.1,21.1,16.15)
x8 =  c(16.15,21.1 ,21.1,16.15)


y1 = c(58,58,92,92)
y2 = c(58,58,92,92)
y3 = c(58,58,77.05,77.05)
y4 = c(77.05,77.05,92,92)
y5 = c(58,58,92,92)
y6 = c(58,58,92,92)
y7 = c(58,58,81.5,81.5)
y8 = c(81.5,81.5,92,92)


my.cols = terrain.colors(8)
polygon(x1, y1, col = my.cols[1])
polygon(x2, y2, col = my.cols[2])
polygon(x3, y3, col = my.cols[3])
polygon(x4, y4, col = my.cols[4])
polygon(x5, y5, col = my.cols[5])
polygon(x6, y6, col = my.cols[6])
polygon(x7, y7, col = my.cols[7])
polygon(x8, y8, col = my.cols[8])
box()

lines(c(9.65, 9.65), h.range + c(-5, 5), lwd = 2)
lines(c(11.05, 11.05), h.range + c(-5, 5), lwd = 2)
lines(c(11.05, 12.45), c(77.05, 77.05), lwd = 2)
lines(c(12.45, 12.45), h.range + c(-5, 5), lwd = 2)
lines(c(13.90, 13.90), h.range + c(-5, 5), lwd = 2)
lines(c(16.15, 16.15), h.range + c(-5, 5), lwd = 2)
lines(c(16.15, max(h.range) + 5), c(81.5, 81.5),lwd = 2)
box(lwd = 2)

points(cherry.df$diameter, cherry.df$height,
       pch = 19, col = "red", cex = 1.3)

mypos = rep(4, 31)
mypos[28] = 2
mypos[8] = 3

text(cherry.df$diameter, cherry.df$height,
     cherry.df$volume, pos = mypos, cex = 0.8)

text( 8.70, 75,  10.27, col="darkblue", cex=1.3)
text(10.35,75,  17.74, col="darkblue", cex=1.3)
text(11.75,83,  23.40, col="darkblue", cex=1.3)
text(11.75,70.5,20.54, col="darkblue", cex=1.3)
text(13.17,75,  26.80, col="darkblue", cex=1.3)
text(15.00,75,  35.20, col="darkblue", cex=1.3)
text(18.60,84,  66.35, col="darkblue", cex=1.3)
text(18.60,70.5,51.76, col="darkblue", cex=1.3)

RSS.tree = sum(residuals(my.fit)^2)
my.fit.lm = lm(volume ~ height + diameter,
               data = cherry.df)
RSS.lm = sum(residuals(my.fit.lm)^2)
sum(residuals(my.fit)^2)
RSS.lm
RSS.tree

my.fit = rpart(volume ~ height + diameter, 
               data = cherry.df,
               cp = 0.002,
               minsplit = 7)
plot(my.fit, lwd = 2, col = "red", branch = 0, uniform = TRUE)
text(my.fit, col = "darkblue", cex = 2)

library(MASS)
data(Boston)
library(rpart)
tree.fit = rpart(medv ~ ., data = Boston,
                 cp = 0.005, minsplit = 5)

plotcp(tree.fit)
abline(v = 9, lty = 2, col = "blue", lwd = 2)
abline(v = 7, lty = 2, col = "blue", lwd = 2)

printcp(tree.fit)

subtree.fit = rpart(medv ~ ., data = Boston,
                    cp = 0.009, minsplit = 5)
printcp(subtree.fit)

plot(subtree.fit, branch = 0, uniform = TRUE)
text(subtree.fit, cex = 2, col = "blue")
