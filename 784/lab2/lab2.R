library(MASS)
data(Boston)
library(mgcv)

gam.stuff = gam(log(medv) ~ s(crim) + s(zn) + s(indus) + factor(chas)
                + s(nox) + s(rm) + s(age) + s(dis) + factor(rad) + s(tax)
                + s(ptratio) + s(black) + s(lstat),
                data = Boston, family = gaussian())

summary(gam.stuff)

par(mfrow = c(3, 4))
plot(gam.stuff)

use = sample(506, 250)
newHouse = Boston[-use,]

newHouse

newHousePred = predict(gam.stuff, newdata = newHouse)
exp(newHousePred)


