fib = function(x) {
  s = numeric(x)
  s[1:2] = c(0, 1)
  
  if (x >= 3) {
    for (i in 3:x)
      s[i] = s[i - 2] + s[i - 1]
  }
  
  s
}

fib(1)
fib(2)
fib(3)
fib(10)
fib(20)
fib(50)
fib(100)

fib1 = function(x) {
  s = numeric(x)
  s[1:2] = c(0, 1)
  
  i = 3
  while (i >= 3 && i <= x) {
      s[i] = s[i - 2] + s[i - 1]
      i = i + 1
  }
  
  s
}

fib1(1)
fib1(2)
fib1(3)
fib1(10)
fib1(20)
fib1(50)
fib1(100)

fib2 = function(x) {
  s = numeric(x)
  s[1:2] = c(0, 1)
  
  i = 3
  repeat {
    s[i] = s[i - 2] + s[i - 1]
    i = i + 1
    if  (i > x)
      break
  }
  
  s
}

fib2(1)
fib2(2)
fib2(3)
fib2(10)
fib2(20)
fib2(50)
fib2(100)

library(MASS)
data(geyser)
geyser
?geyser
geyser$duration
geyser$waiting
mean(geyser$duration)

ge4 = length(geyser$duration[geyser$duration >= 4])
ge4
ge4 / length(geyser$duration)

d = geyser$duration >= 4 & geyser$waiting <= 50
d
length(geyser$duration[d]) / length(geyser$duration)
sum(d) / nrow(geyser)
d1 = geyser$duration >= 4.5
mean(geyser$waiting[d1])

lst = list(0:9, 1:10)
for (i in 1:10) {
  d = geyser$duration >= lst[[1]][i] & geyser$duration < lst[[2]][i]
  print(geyser$duration[d])
}

cf.expand = function(x, n = 5) {
  cf = numeric(n)
  for (i in 1:n) {
    cf[i] = round(x)
    delta = x - cf[i]
    x = 1 / delta
  }
  cf
}

cf.expand(pi, 7)

rat.approx = function(x, n) {
  cf = cf.expand(x, n)
  num = cf[n]
  den = 1
  if (n > 1) {
    for (j in (n - 1):1) {
      tmp = num
      num = cf[j] * tmp + den
      den = tmp
    }
  }
  
  if (den > 0) 
    c(num, den)
  else
    c(-num, -den)
}

rat.approx(pi, 7)
print(5419351 / 1725033, digit = 15)

rat.approx2 = function(cf) {
  n = length(cf)
  num = cf[n]
  den = 1
  if (n > 1) {
    for (j in (n - 1):1) {
      tmp = num
      num = cf[j] * tmp + den
      den = tmp
    }
  }
  
  if (den > 0) 
    c(num, den)
  else
    c(-num, -den)
}

cf = cf.expand(pi, 2)
rat.approx2(cf)
cf = cf.expand(pi, 3)
rat.approx2(cf)

cf.expand2 = function(x, n = 5) {
  cf = numeric(n)
  cf[1] = round(x)
  if (n > 1) {
    delta = x - cf[1]
    x =  1 / delta
    cf[2:n] = cf.expand2(x, n - 1)
  }
  cf
}

cf = cf.expand2(pi, 2)
rat.approx2(cf)
cf = cf.expand2(pi, 3)
rat.approx2(cf)

rat.approx3 = function(cf) {
  n = length(cf)
  num = cf[n]
  init = 0
  den = 1
  if (n > 1) {
    if (cf[n - 1] < 0)
      factor = -1
    else
      factor = 1
    prev_appr = rat.approx3(cf[1:(n-1)])
    tmp = num
    if (n > 2) {
      prev_prev_appr = rat.approx3(cf[1:(n-2)])
      den = prev_prev_appr[1]
      init = prev_prev_appr[2]
    }
    num = prev_appr[1] * tmp + factor * den
    den = prev_appr[2] * tmp + factor * init
  }
  
  if (den > 0) 
    c(num, den)
  else
    c(-num, -den)
}

rat.approx3(cf)

cf = cf.expand2(pi, 7)
rat.approx3(cf)

cf = cf.expand2(pi, 7)
rat.approx2(cf)
rat.approx3(cf)

cf = cf.expand(pi, 7)
rat.approx(pi, 7)

