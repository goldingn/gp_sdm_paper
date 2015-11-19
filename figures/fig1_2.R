# 2D representation of Gaussian random fields in ecology
rm(list = ls())

covfun <- function(x, l = 1) exp(-0.5 * (x ^ 2) / l)

library(GRaF)

set.seed(1)
n <- 20
x <- runif(n, 0, 4)
foo <- function(x) pnorm(-1 + 2 * sin((x + 1) / 1.5))
y <- rbinom(n, 1, foo(x))
xpred <- seq(0, 5, len = 100)
length <- c(0.1, 1, 10)
linetype <- c(1, 2, 4)

darkgrey <- rgb(0.3, 0.3, 0.3)

# figure one
setEPS()
postscript("GRF_expl_fig1.eps",
           width = 12, height = 5.2)

par(mar = c(5, 7, 4, 2), oma = c(0, 0, 0, 5), mfrow = c(1, 2))

# plot data
plot(y ~ x, cex.axis = 1.5, type = 'n',
     ylab = 'probability of presence',
     xlab = expression(paste('temperature ' ^ 'o', 'C')),
     cex.lab = 2, ylim = c(0, 1), xlim = c(0, 5))
lines(foo(xpred) ~ xpred, lwd = 3, lty = 2)
points(y ~ x, pch = 16, col = darkgrey)
mtext('a', adj = 0, cex = 1.5, line = 1)

# plot covariance functions
dist <- seq(0, 5, len = 100)

plot(covfun(dist, length[2]) ~ dist, type = 'l',
     xlab = expression(paste('temperature difference ' ^ 'o', 'C')),
     ylab = 'correlation in\n probability of presence', ylim = c(0, 1), lty = linetype[2],
     cex.axis = 1.5, cex.lab = 2, lwd = 2, xlim = c(0, 5))
lines(covfun(dist, length[1]) ~ dist, lwd = 2, lty = linetype[1])
lines(covfun(dist, length[3]) ~ dist, lwd = 2, lty = linetype[3])

text(0.9, 0.1, paste('l =', length[1]), cex = 1.5, adj = 0)
text(1.6, 0.5, paste('l =', length[2]), cex = 1.5, adj = 0)
text(3, 0.8, paste('l =', length[3]), cex = 1.5, adj = 0)
mtext('b', adj = 0, cex = 1.5, line = 1)

dev.off()



# figure 2
postscript("GRF_expl_fig2.eps",
           width = 12, height = 6)

par(mfrow = c(2, 3), mar = c(5, 7, 4, 2), oma = c(0, 10, 4, 4))

# generate the mean function
for(j in 1:2) {
  if (j == 1) mnfun <- NULL
  else mnfun <- function(x) pnorm(1.3 - 0.47 * x[, 1])
  for(i in 1:3) {
    plot(y ~ x, ylim = c(0, 1), type = 'n', ylab = 'probability of presence',
         xlab = '', cex.lab = 1.8,
         cex.main = 2, cex.axis = 1.5, xlim = c(0, 5))
    title(xlab = expression(paste('temperature ' ^ 'o', 'C')), line = 3.5, cex.lab = 1.8)
    m <- graf(y, as.data.frame(x), l = length[i], prior = mnfun)
    p <- predict(m, as.data.frame(xpred))
    polygon(x = c(xpred, rev(xpred)), y = c(p[, 2], rev(p[, 3])), lty = 0, col = 'light grey')
    lines(m$mnfun(as.data.frame(xpred)) ~ xpred, col = rgb(0.4, 0.4, 0.4), lwd = 2, lty = 3)
    points(y ~ x, pch = 16)
    lines(p[, 1] ~ xpred, lwd = 2, col = darkgrey, lty = 1)#linetype[i])
#     mtext(letters[3 * (j - 1) + i], adj = 0, cex = 1.5, line = 1)
    box()
    if (j == 1) mtext(paste('lengthscale =', length[i]), cex = 2, line = 5)
    
    if (i == 1) {
      if (j == 1) mtext('flat\nmean\nfunction', cex = 2, line = 12, side = 2, las = 2, adj = 0.5)
      else mtext('linear\nmean\nfunction', cex = 2, line = 12, side = 2, las = 2, adj = 0.5)
    }
  }
}

dev.off()