rm(list = ls())

set.seed(123)

n <- 200
x <- seq(0, 10, length.out = n)
X <- expand.grid(x, x)

foo <- function(x) {
  2.5 * (dnorm(x[, 1], 6, 1) * dnorm(x[, 2], 2, 1) +
    dnorm(x[, 1], 5, 0.9) * dnorm(x[, 2], 3, 0.9) +
    dnorm(x[, 1], 4, 0.8) * dnorm(x[, 2], 4, 0.8) +
    dnorm(x[, 1], 3, 0.85) * dnorm(x[, 2], 5, 0.85) +
    dnorm(x[, 1], 4, 0.8) * dnorm(x[, 2], 6, 0.8) +
    dnorm(x[, 1], 5, 0.9) * dnorm(x[, 2], 7, 0.9) +
    dnorm(x[, 1], 6, 1) * dnorm(x[, 2], 8, 1))
}

p <- foo(X)
p.mat <- matrix(p, n)

nobs <- 1000
xobs <- cbind(runif(nobs, 0, 10), runif(nobs, 0, 10))
colnames(xobs) <- c("x1", "x2")
xobs <- data.frame(xobs)
yobs <- rbinom(nobs, 1, foo(xobs))


# fit a gam
library(mgcv)
gam.m <- gam(yobs ~ s(x1) + s(x2), data = xobs, family = binomial)

# and a gbm
library(dismo)
system.time(brt.m <- gbm.step(data = data.frame(cbind(yobs, xobs[, 1:2])), gbm.y = 1, gbm.x = 2:3,
        family = 'bernoulli', tree.complexity = 5, learning.rate = 0.002, bag.fraction = 0.5,
	  plot.main = FALSE))

# and a GP model
library(GRaF)
system.time(grf.m <- graf(yobs, xobs, opt.l = TRUE))

# and predict to the matrix
X.df <- cbind(X, X ^ 2)
colnames(X.df) <- c("x1", "x2", "x1.2", "x2.2")
X.df <- data.frame(X.df)

gam.p <- predict(gam.m, X.df, type = 'response')
gam.p.mat <- matrix(gam.p, n)

brt.p <- predict(brt.m, X.df[, 1:2], type = 'response', n.trees = brt.m$n.trees)
brt.p.mat <- matrix(brt.p, n)

grf.p <- predict(grf.m, X.df[, 1:2])
grf.p.mat <- matrix(grf.p[, 1], n)

# and plot them
phi = 40
theta = 135
r = 100

xlab <- 'temp.'
ylab <-'rainfall'
zlab <- 'probability\npresent'

tickcol = rgb(0.5, 0.5, 0.5)

png("../3d_terms.png",
    type = 'cairo',
    antialias = 'subpixel',
    width = 2400,
    height = 2000,
    pointsize = 30)

par(mfrow = c(2, 2),
    mar = c(5, 10, 0, 0),
    oma = c(5, 0, 0, 10))

# true function
persp(x, x, p.mat, theta = theta, phi = phi, box = F, shade = 0.5,
	xlab = "covariate 1", ylab = "covariate 2",
	zlab = "probability of presence", r = r,
      col = 'light grey', border = NA, zlim = c(0, 1)) -> res

# arrow locations
l1 <- trans3d(c(10, 0), c(11, 11), 0, pmat = res)
l2 <- trans3d(c(11, 11), c(10, 0), 0, pmat = res)
l3 <- trans3d(10, -1, c(0, 1), pmat = res)

arrows(l1$x[1], l1$y[1], l1$x[2], l1$y[2], pch = 16, xpd = NA, lwd = 2, length = 0.1, col = tickcol)
arrows(l2$x[1], l2$y[1], l2$x[2], l2$y[2], pch = 16, xpd = NA, lwd = 2, length = 0.1, col = tickcol)
arrows(l3$x[1], l3$y[1], l3$x[2], l3$y[2], pch = 16, xpd = NA, lwd = 2, length = 0.1, col = tickcol)

text(0, -0.023, "True surface", xpd = NA, cex = 3)
text(0.008, -0.015, ylab, xpd = NA, cex = 2, adj = 0, col = tickcol)
text(-0.008, -0.015, xlab, xpd = NA, cex = 2, adj = 1, col = tickcol)
text(-0.0158, 0, zlab, xpd = NA, cex = 2, adj = 1, col = tickcol)

# gam prediction
persp(x, x, gam.p.mat, theta = theta, phi = phi, box = F, shade = 0.5,
      xlab = "covariate 1", ylab = "covariate 2",
      zlab = "probability of presence", r = r,
      col = 'light grey', border = NA, zlim = c(0, 1))
text(0, -0.023, "GAM model", xpd = NA, cex = 3)

# brt prediction
persp(x, x, brt.p.mat, theta = theta, phi = phi, box = F, shade = 0.5,
	xlab = "covariate 1", ylab = "covariate 2",
	zlab = "probability of presence",
  r = r,
  col = 'light grey',
  border = NA,
  zlim = c(0, 1))
text(0, -0.023, "BRT model", xpd = NA, cex = 3)

# grf prediction
persp(x, x, grf.p.mat, theta = theta, phi = phi, box = F, shade = 0.5,
	xlab = "covariate 1", ylab = "covariate 2",
	zlab = "probability of presence", r = r,
      col = 'light grey', border = NA, zlim = c(0, 1))
text(0, -0.023, "GP model", xpd = NA, cex = 3)

dev.off()


