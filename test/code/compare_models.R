# fit lme models to compare predictive power of models

# clear workspace
rm(list = ls())

# load packages
library(nlme)

# functions
printEval <- function (m) {
  # pretty-print evaluation stats
  coef <- summary(m)$tTable[, 1:5]
  
  coef[, 1] <- round(coef[, 1], 3)
  coef[, 2] <- round(coef[, 2], 3)
  coef[, 4] <- abs(round(coef[, 4], 2))
  coef[, 5] <- round(coef[, 5], 4)
  
  coef
}

# load data
load('results/comparison_dfs.RData')

# switch glms and gams to versions with covariate selection
df_pa <- df_pa[!(df_pa$model %in% c('GLM', 'GAM')), ]
df_po <- df_po[!(df_po$model %in% c('GLM', 'GAM')), ]

df_pa$model[df_pa$model == 'GLM-stepwise'] <- 'GLM'
df_pa$model[df_pa$model == 'GAM-penalized'] <- 'GAM'
df_po$model[df_po$model == 'GLM-stepwise'] <- 'GLM'
df_po$model[df_po$model == 'GAM-penalized'] <- 'GAM'

# fix factor levels
df_po$model <- droplevels(df_po$model, 'GAM-penalized', 'GLM-stepwise')
df_pa$model <- droplevels(df_pa$model, 'GAM-penalized', 'GLM-stepwise')

# presence-absence models

# remove with very bad predictions
# (predictive deviance more than thrice the null deviance)
baddies <- (abs(df_pa$ll) / abs(df_pa$ll_null) ) > 3

# # eyeball these
df_pa_old <- df_pa
# how many
sum(baddies)
# which models
table(df_pa_old$model, baddies)
# look at prevalences
max(df_pa_old$prev[baddies])
tapply(df_pa_old$prev, baddies, mean)
tapply(df_pa_old$prev, baddies, sd)

# remove these
df_pa <- df_pa[!baddies, ]

# null
m0_pa <- lme(ll ~ 1,
            random = ~1|species,
            data = df_pa)

# alternative
m_pa <- lme(ll ~ model,
            random = ~1|species,
            data = df_pa,
            weights = varIdent(form = ~1|model))

# presence-only models

# null
m0_po <- lme(auc ~ 1,
             random = ~1|species,
             data = df_po)

m_po <- lme(auc ~ model,
            random = ~1|species,
            data = df_po,
            weights = varIdent(form = ~1|model))

printEval(m_pa)
printEval(m_po)


# save these for plotting later
save(m0_po, m_po, df_po,
     m0_pa, m_pa, df_pa,
     file = 'results/comparison_models.RData')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# look at deviance R2s

# mean r2
mn <- tapply(df_pa$r2, df_pa$model, mean, na.rm = TRUE)

# sd r2
sd <- tapply(df_pa$r2, df_pa$model, sd, na.rm = TRUE)
n <- tapply(df_pa$r2 ^ 0, df_pa$model, sum, na.rm = TRUE)
se <- sd / sqrt(n)

# printed like this:
round(mn * 100, 2)
round(se * 100, 3)
# ~~~~~~~~~~~~~~
# computation time

# Get per-model times
times$pa <- times$pa / 370
times$po <- times$po / 146

# absolute
round(times, 2)

# relative to GP-fixed
sweep(as.matrix(times), 2, as.matrix(times)['gp', ], '/')

# ~~~~~~~~~~~~~
# look at model residuals

plot(m_pa, ll ~ fitted(.) | model,
     abline = c(0, 1))

plot(m_po, auc ~ fitted(.) | model,
     abline = c(0, 1))

