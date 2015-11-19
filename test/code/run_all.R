# run all model comparisons

# clear workspace
rm(list = ls())

# load packages
library(snowfall)

# load modelling functions
source('code/functions.R')

#~~~~~~~~~~
# load BBS data
load('dave_data/birds.Rdata')

# some light data formatting
x <- data.frame(x)
x_train <- x[in.train, 1:8]
x_test <- x[in.test, 1:8]

y_train <- route.presence.absence[in.train, ]

sfInit(cpus = 10, parallel = TRUE)

sfLibrary(mgcv)
sfLibrary(GRaF)
sfLibrary(dismo)
sfLibrary(gbm)

# which models to run
pa_models <- c('glmStep', 'gamSelect')  # c('glm', 'gam', 'brt', 'gp', 'gpo')
po_models <- c('glmStep', 'gamSelect')  # c('glm', 'gam', 'me', 'brt', 'gp', 'gpo')

# define batches of species to fit at a time
batches <- lapply(1:10, getBatch, 37)

# run the batches
# for (i in 1:length(batches)) {
for (i in 1:10) {
  
  message(paste0('\nrunning batch ',
                 i,
                 '\n'))
  
  # set output file
  outfile <- paste0('results/step_select/pa_batch_ss_',
                    i,
                    '.RData')
  
  # run P/A comparison
  runModels(species = batches[[i]],
            models = pa_models,
            y_train = y_train,
            x_train = x_train,
            x_test = x_test,
            file_out = outfile)
  
}

# ~~~~~~~~~~~
# presence-only estimates

# number of occurrences to use for training the models
n_keep <- 146

# get the number of presences in each training set and testing set
n_pres <- colSums(route.presence.absence[in.train, ])
n_pres_test <- colSums(route.presence.absence[in.test, ])

# find those that have n_keep or fewer to select from and
# some evaluation data
keep <- which(!(n_pres <= n_keep |
                  n_pres_test == 0 |
                  n_pres_test == sum(in.test)))

# remove these
y_po <- route.presence.absence[, keep]

# for each remaining bird, randomly select n_keep presence 
# routes as occurrences and set the rest to 0
for (i in 1:ncol(y_po)) {
  
  # find presences
  which_pres <- which(y_po[in.train, i] == 1)
  
  # sample these without replacement
  which_occ <- sample(which_pres, n_keep, replace = FALSE)
  
  # set all of that bird's training routes to 0
  y_po[in.train, i][] <- 0
  
  # and the occurrences to 1
  y_po[in.train, i][which_occ] <- 1
  
}


# ~~~~~~~~~~~
# run these models

# define batches of species to fit at a time
batches <- lapply(1:5, getBatch, 30)

batches[[5]] <- batches[[5]][batches[[5]] <= ncol(y_po)]

# run the batches
for (i in 1:length(batches)) {
  
  message(paste0('\nrunning batch ',
                 i,
                 '\n'))
  
  # set output file
  outfile <- paste0('results/step_select/po_batch_ss_',
                    i,
                    '.RData')
  
  # run P-O comparison
  runModels(species = batches[[i]],
            models = po_models,
            y_train = y_po[in.train, ],
            x_train = x_train,
            x_test = x_test,
            file_out = outfile)
  
}

# save the evaluation data for each comparison
pa_eval <- route.presence.absence[in.test, ]
pa_eval <- t(apply(pa_eval, 1, as.numeric))
po_eval <- y_po[in.test, ]

save(pa_eval, po_eval, file = 'results/eval.RData')


