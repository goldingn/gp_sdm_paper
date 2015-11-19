# load in and assess the fitted models

rm(list = ls())

# read in functions
source('code/functions.R')

# ~~~~~~~~~
# load data

# evaluation data
load('results/eval.RData')

# presence-absence predictions
pa <- combineBatches(path = 'results', pattern = 'pa_')

# get gpo ones
pa_gpo <- combineBatches(path = 'results/gpo', pattern = 'pa_')

# get step/select ones
pa_ss <- combineBatches(path = 'results/step_select', pattern = 'pa_')

# check all species are in there
stopifnot(all.equal(pa$species, pa_gpo$species, pa_ss$species))
stopifnot(all.equal(pa$prevs, pa_gpo$prevs, pa_ss$prevs))

# add the relevant parts to pa
pa$models <- c(pa$models, pa_gpo$models, pa_ss$models)
pa$times <- c(pa$times, pa_gpo$times, pa_ss$times)
pa$preds <- c(pa$preds, pa_gpo$preds, pa_ss$preds)

# presence-only predictions
po <- combineBatches(path = 'results', pattern = 'po_')

# get gpo ones
po_gpo <- combineBatches(path = 'results/gpo', pattern = 'po_')

# and step/select ones
po_ss <- combineBatches(path = 'results/step_select/', pattern = 'po_')

# fix fuckup
idx <- (po_gpo$prevs == po$prevs[1])
po_gpo$times <- po_gpo$times * (length(po$prevs) / length(po_gpo$prevs))
po_gpo$species <- po_gpo$species[idx]
po_gpo$prevs <- po_gpo$prevs[idx]
po_gpo$preds[[1]] <- po_gpo$preds[[1]][, idx]


# check all species are in there
stopifnot(all.equal(po$species, po_gpo$species, po_ss$species))
stopifnot(all.equal(po$prevs, po_gpo$prevs, po_ss$prevs))

# add therelevant parts to po
po$models <- c(po$models, po_gpo$models, po_ss$models)
po$times <- c(po$times, po_gpo$times, po_ss$times)
po$preds <- c(po$preds, po_gpo$preds, po_ss$preds)


# get goodness of fit metrics, and associated data in dataframes

# presence-absence data
nsp <- length(pa$species)
nmodel <- length(pa$models)
df_pa <- data.frame(species = rep(pa$species,
                                  each = nmodel),
                    model = rep(pa$models,
                                nsp),
                    ll = NA,
                    ll_null = NA,
                    prev = rep(pa$prevs,
                               each = nmodel))

pa_eval <- pa_eval[, pa$species]

for (model in pa$models) {
  idx <- df_pa$model == model
  preds <- pa$preds[[model]]
  for (i in 1:nsp) {
    df_pa$ll[idx][i] <- llBern(preds[, i], pa_eval[, i])
    df_pa$ll_null[idx][i] <- llBern(df_pa$prev[i], pa_eval[, i])
  }
}

# get deviance r-squareds
df_pa$r2 <- 1 - (-2 * df_pa$ll) / (-2 * df_pa$ll_null)

# set worse-than-randoms to 0
df_pa$r2[df_pa$r2 < 0] <- 0

# presence-only data
nsp <- length(po$species)
nmodel <- length(po$models)
df_po <- data.frame(species = rep(po$species,
                                  each = nmodel),
                    model = rep(po$models,
                                nsp),
                    auc = NA,
                    prev = rep(po$prevs,
                               each = nmodel))

po_eval <- po_eval[, po$species]

for (model in po$models) {
  idx <- df_po$model == model
  preds <- po$preds[[model]]
  for (i in 1:nsp) {
    df_po$auc[idx][i] <- auc(preds[, i], po_eval[, i])
  }
}


# rename the models
lookup <- matrix(c('gp', 'GP-fixed',
                   'brt', 'BRT',
                   'me', 'MaxEnt',
                   'gam', 'GAM',
                   'glm', 'GLM',
                   'gamSelect', 'GAM-penalized',
                   'glmStep', 'GLM-stepwise',
                   'gpo', 'GP-MAP'),
                 ncol = 2,
                 byrow = TRUE)

o_po <- match(df_po$model, lookup[, 1])
df_po$model <- factor(lookup[o_po, 2])

o_pa <- match(df_pa$model, lookup[, 1])
df_pa$model <- factor(lookup[o_pa, 2])


# relevel factors to make GPfixed the reference
levels_po <- c('GP-fixed', 'GP-MAP', 'BRT', 'MaxEnt', 'GAM-penalized', 'GLM-stepwise', 'GAM', 'GLM')
stopifnot(all(levels_po %in% levels(df_po$model)) &
            all(levels(df_po$model) %in% levels_po))
df_po$model <- factor(df_po$model, levels = levels_po)

levels_pa <- c('GP-fixed', 'GP-MAP', 'BRT', 'GAM-penalized','GLM-stepwise', 'GAM', 'GLM')
stopifnot(all(levels_pa %in% levels(df_pa$model)) &
            all(levels(df_pa$model) %in% levels_pa))
df_pa$model <- factor(df_pa$model, levels = levels_pa)

# combine times data
times <- data.frame(po = po$times)
times$pa <- pa$times[match(rownames(times), names(pa$times))]

# save these
save(df_po, df_pa, times,
     file = 'results/comparison_dfs.RData')
