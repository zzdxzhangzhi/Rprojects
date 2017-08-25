ssquare = function(x) {
  n = length(x)
  sum((x - mean(x))^2) / (n - 1)
}

ssquare.2 = function(x) {
  n = length(x)
  (sum(x^2) - n * mean(x)^2) / (n - 1)
}

set.seed(782)
x = rnorm(10)
x

o = cbind(x, outer(x, 1:10, function(x, y) x + 10^y))

result = matrix(c(apply(o, 2, ssquare), 
                  apply(o, 2, ssquare.2), 
                  apply(o, 2, var)), ncol = 3)
print(result, digits = 10)

u = function(x) {
  n = length(x)
  cumsum(x) / (1:n)
}
u(x)

sdsquare = function(x) {
  n = length(x)
  cumsum((x - u(x)) ^ 2) / (1:n)
}
sdsquare(x)

ssqaure.3 = function(x) {
  n = length(x)
  covar = cumsum(1:n / c(1, 1:(n - 1)) * (x - u(x)) ^ 2) / (1:(n - 1))
  covar[n]
}

result = matrix(c(apply(o, 2, ssquare), 
                  apply(o, 2, ssquare.2),
                  apply(o, 2, ssqaure.3),
                  apply(o, 2, var)), ncol = 4)
print(result, digits = 10)
