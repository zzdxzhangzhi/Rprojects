set.seed(782)
x = matrix(runif(30), nrow = 10)

apply(x, 2, function(x) {c(mean(x), sd(x))})

set.seed(782)
xp = matrix(runif(30), nrow = 10)
xp
rownum = nrow(xp)
colnum = ncol(xp)

for (i in 1:colnum) {
  col.mean = mean(xp[,i])
  col.sd = sd(xp[,i])
  xp[, i] = (xp[, i] - col.mean) / col.sd
}

xp
apply(xp, 2, mean)
apply(xp, 2, sd)

set.seed(782)
xp2 = matrix(runif(30), nrow = 10)
xp2

sweep(sweep(xp2, 2, apply(xp2, 2, mean)), 2, apply(xp2, 2, sd), "/" )

set.seed(782)
x1 = round(rnorm(15), 3)
x1

sort(x1)
sort(x1, decreasing = TRUE)

x2 = matrix(x1, nrow = 5)
x2

o = order(x2[,2], decreasing = TRUE)

x3 = x2[o,]
x3

x4 = apply(x2, 2, function(x) x = x[order(x, decreasing = TRUE)])
x4

xx = rnorm(1000, 4, 2)
min(xx)
max(xx)
hist(xx)
hist(xx, freq = FALSE)

z = seq(-5, 13, length.out = 1000)
xd = dnorm(z, 4, 2)
xd
min(xd)
max(xd)
lines(z, y = xd, type = "l", col = "blue")

xr = rnorm(1000, rep(c(0, 4), c(300, 700)), rep(c(1, 2), c(300, 700)))
xr
hist(xr, breaks = 50, freq = FALSE)