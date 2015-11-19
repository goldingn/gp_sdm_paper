rm(list = ls())

# plotting function

plotModels <- function(m0,
                       m1,
                       df,
                       ylab,
                       ylim) {
  
  # stats for plotting
  getstats <- function(model, df, m) {
    res <- (resid(m) + m$coefficients$fixed)[df$model == model]
    std <- sd(res)
    mn <- mean(res)
    # means(bars), 1sd (boxes) and range  (lines)
    c(min(res), mn - std, mn, mn + std, max(res))
  }
  
  models <- levels(df$model)
  
  # calculate means and standard deviations for the parameter estimates
  wts <- c(0, rep(1, length(models) - 1))
  means <- summary(m1)$tTable[, 1] + wts * summary(m1)$tTable[1, 1]
  sds <- summary(m1)$tTable[, 2] * wts
  
  # nice box plots, lines are means, boxes are 95% confidence intervals 
  b <- boxplot(resid(m1) ~ model,
               df,
               plot = FALSE)
  
  b$stats <- sapply(levels(df$model),
                    getstats,
                    df,
                    m0)
  b$out <- NULL
  
  cols = c(grey(0.85), grey(0.85))
  
  suppressWarnings(bxp(b,
                       ylab = ylab,
                       ylim = ylim,
                       xlab = '',
                       pch = NA,
                       cex.axis = 1.3,
                       cex.lab = 2,
                       boxwex = 0.5,
                       pars = list(whisklty = 0,
                                   whiskcol = cols[1],
                                   whisklwd = 3,
                                   staplelty = 0,
                                   boxlty = 0,
                                   boxfill = cols[1],
                                   medcol = cols[2]),
                       xaxt ='n'))
  axis(1,
       at = 1:length(models),
       labels = models,
       padj = 1,
       cex.axis = 1.3,
       line = 0)
  
  
  abline(h = m1$coefficients$fixed[1],
         lty = 2,
         lwd = 3,
         col = grey(0.6))
  
  for(i in 1:length(models)) {
    
    rect(i - 0.25,
         means[i] - sds[i] * 1.96,
         i + 0.25,
         means[i] + sds[i] * 1.96,
         lty = 0,
         col = grey(0.7))
    
    lines(i + c(-0.25, 0.25),
          rep(means[i], 2),
          lwd = 4,
          col = grey(0.4),
          lend = 1)
    
  }

}

# ~~~~~~~~~~~~
# load data

load('test/results/comparison_models.RData')

# # plot and assess deviances
# setEPS()
# postscript("../deviance_plot.eps",
#            width = 12, height = 11)

# plot accuracy statistics
png('../comparison_plot.png',
    width = 2800, height = 1400,
    pointsize = 28)

par(mfrow = c(1, 2),
    mar = c(4, 5, 5, 2))


plotModels(m0_pa,
           m_pa,
           df_pa,
           ylab = 'log-likelihood',
           ylim = c(-95, -41))

title(main = 'Presence/absence',
      cex.main = 2)

plotModels(m0_po,
           m_po,
           df_po,
           ylab = 'AUC',
           ylim = c(0.815, 0.905))

title(main = 'Presence-only',
      cex.main = 2)

dev.off()


