x = seq(-1, 1, length = 1001)
y = x^3
plot(x, y, type = "l", axes = FALSE, ann = FALSE)
axis(1, at = c(-1, 1), pos = 0)
axis(2, at = c(-1, 1), pos = 0, las = 2)
lines(x, y, col = "blue", lwd = 2, lty = "longdash")