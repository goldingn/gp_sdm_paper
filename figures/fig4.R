# plot training data, predictions and uncertainty for a BBS data species
rm(list = ls())
set.seed(1)

library(GRaF)
library(raster)
library(RColorBrewer)

source('test/code/functions.R')

# load extracted BBS data
load('test/dave_data/birds.Rdata')

# load rasters a la Dave
worldclim.raster <- getData("worldclim",
                            var = "bio",
                            res = 10)

# which to keep
keep <- which(names(worldclim.raster) %in% colnames(x))
worldclim.raster <- worldclim.raster[[keep]]

# crop to BBS data region
e <- new("Extent"
         , xmin = -170.151570261652
         , xmax = -46.697562902546
         , ymin = 22.2257739512731
         , ymax = 71.8960432313611)

worldclim.raster <- crop(worldclim.raster, e)

# fit the model
i <- 311
species <- colnames(route.presence.absence)[i]
y <- route.presence.absence[in.train, i]
x.fit <- x[in.train, 1:8]
system.time(m <- graf(y, data.frame(x.fit), opt.l = FALSE))

p <- predict(m,
             worldclim.raster,
             type = 'latent',
             CI = 'std')

# par(mfrow = n2mfrow(nlayers(worldclim.raster)),
#     mar = c(4, 4, 1, 1) + 0.1)
# plot(m, peak = TRUE)

# define colours
greys <- colorRampPalette(brewer.pal(9, 'Greys')[-(1:2)])
greens <- colorRampPalette(brewer.pal(9, 'Greens')[-(1:2)])
blues <- colorRampPalette(brewer.pal(9, 'Blues')[-(1:2)])
purples <- colorRampPalette(brewer.pal(9, 'Purples')[-(1:2)])

prescol <- purples(3)[3]
abscol <- purples(3)[1]

trainpres <- in.train & route.presence.absence[, i] == 1
trainabs <- in.train & route.presence.absence[, i] == 0

png('../maps.png',
    width = 2400,
    height = 4500,
    pointsize = 60)

par(mfrow = c(3, 1),
    mar = c(0, 1, 1.5, 3))

pt.cex <- 0.6
cex.main <- 2
title.line <- -1
title.col <- grey(0.3)

plot(worldclim.raster$bio5,
     col = grey(0.8),  #rev(greys(1000)),
     axes = FALSE,
     box = FALSE,
     legend = FALSE,
     maxpixels = Inf)

title(main = paste0('Distribution data - ', species),
      cex.main = cex.main,
      line = title.line,
      col.main = title.col)

# mtext('a', 3, line = -1, adj = 0.1, cex = 2)

borderPoints(latlon[in.test, ],
             border = grey(0.4),
             fill = 'white',
             cex = pt.cex)

borderPoints(latlon[trainabs, ],
             border = 'black',
             fill = abscol,
             cex = pt.cex)

borderPoints(latlon[trainpres, ],
             border = 'black',
             fill = prescol,
             cex = pt.cex)


plot(calc(p$posterior.mean, pnorm),
     axes= FALSE,
     box = FALSE,
     zlim = c(0, 1),
     col = purples(1000),
     legend.lab = 'Probability of presence',
     maxpixels = Inf)

title(main = 'Predicted distribution',
      cex.main = cex.main,
      line = title.line,
      col.main = title.col)

plot(p$posterior.std ^ 2,
     axes= FALSE,
     box = FALSE,
     col = greens(1000),
     zlim = c(0, round(maxValue(p$posterior.std ^ 2), 1) + 0.1),
     legend.lab = 'Variance of GP posterior',
     maxpixels = Inf)

title(main = 'Prediction uncertainty',
      cex.main = cex.main,
      line = title.line,
      col.main = title.col)

dev.off()
