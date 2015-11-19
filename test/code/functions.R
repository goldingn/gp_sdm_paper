# model fitting functions

# predict from a glm
glmFit <- function(y,
                   x.train,
                   x.test) {
  
  m <- glm(y ~ .,
           data = x.train,
           family = binomial)
  
  p <- predict(m, x.test, type = 'response')
  
  return (p)
  
}

# predict from a glm fitted with backwards
# stepwise selection to minimise AIC
glmStepFit <- function(y,
                       x.train,
                       x.test) {
  
  m <- glm(y ~ .,
           data = x.train,
           family = binomial)
  
  m <- MASS::stepAIC(m, formula(m), direction = 'backward')
  
  p <- predict(m, x.test, type = 'response')
  
  return (p)
  
}



# predict from a gam
gamFit <- function(y,
                   x.train,
                   x.test) {
  
  f <- formula(paste("y ~ s(",
                     paste(colnames(x.train),
                           collapse = ") + s("),
                     ")",
                     sep = ""))
  
  m <- gam(f,
           data = data.frame(y, x.train),
           family = binomial)
  
  p <- predict(m, x.test, type = 'response')
  
  return (p)
  
}

# predict from a gam
gamSelectFit <- function(y,
                         x.train,
                         x.test) {
  
  f <- formula(paste("y ~ s(",
                     paste(colnames(x.train),
                           collapse = ") + s("),
                     ")",
                     sep = ""))
  
  m <- gam(f,
           data = data.frame(y, x.train),
           family = binomial,
           select = TRUE)
  
  p <- predict(m, x.test, type = 'response')
  
  return (p)
  
}

# predict from a GP model
gpFit <- function(y,
                  x.train,
                  x.test) {
  
  m <- graf(y,
            x.train)
  
  p <- predict(m,
               x.test,
               type = 'response',
               CI = NULL)[, 1]
  
  return (p)
  
}

# predict from a GP model with hyperparameter optimisation
gpoFit <- function(y,
                  x.train,
                  x.test) {
  
  m <- graf(y,
            x.train,
            opt.l = TRUE)
  
  p <- predict(m,
               x.test,
               type = 'response',
               CI = NULL)[, 1]
  
  return (p)
  
}


# predict from a MaxEnt model
meFit <- function(y,
                  x.train,
                  x.test) {
  
  m <- maxent(x.train, y)
  
  p <- predict(m,
               x.test)
  
  return (p)
  
}

# predict from a BRT model
brtFit <- function(y,
                   x.train,
                   x.test) {
  
  
  # fit a BRT with 10,000 trees and Jane Elith's defaults otherwise
  m <- gbm(y ~ .,
           distribution = 'bernoulli',
           data = x.train,
           interaction.depth = 5,
           shrinkage = 0.001,
           n.trees = 10000,
           cv.folds = 5,
           bag.fraction = 0.5,
           verbose = FALSE,
           n.cores = 1)
  
  ntree <- gbm.perf(m,
                    plot.it = FALSE,
                    method = 'cv')
  
  p <- predict(m,
               x.test,
               n.tree = ntree,
               type = 'response')
  
  return (p)
  
}

# bernoulli deviance
llBern <- function(p, y) sum(dbinom(y, 1, p, log = TRUE))

# auc
auc <- function(p, y) {
  require(pROC)
  if (all(y == 1) | all(y == 0)) {
    ans <- NA
  } else {
    ans <- as.numeric(pROC::auc(response = y,
                                predictor = p))
  }
  return (ans)
}
runModels <- function(species,
                      models,
                      y_train,
                      x_train,
                      x_test,
                      file_out) {
  
  # counts
  nsp <- length(species)
  nmodel <- length(models)
  
  # subset occurrence data by speies, and build into a list
  y_train <- lapply(species,
                    function(i, x) x[, i],
                    y_train)
  
  # prevalences
  prevs <- sapply(y_train, mean)
  
  # loop through models running each batch in parallel across species
  
  # structure to record times
  times <- rep(NA, nmodel)
  names(times) <- models
  
  # one to record predictions
  preds <- replicate(nmodel, {})
  names(preds) <- models
  
  for (model in models) {
    
    message(paste0('running ',
                   model,
                   ' models now'))
            
            # get model fitting function
            fun_name <- paste0(model, 'Fit')
            f <- eval(parse(text = fun_name))
            
            # run models
            time <- system.time(
              p <- sfLapply(y_train,
                            f,
                            x_train,
                            x_test)
            )
            
            # get elapsed time
            times[names(times) == model] <- time[3]
            
            # convert prediction to a matrix
            preds[[which(names(preds) == model)]] <- do.call(cbind, p)
            
  }
  
  save(species, preds, times, prevs, models,
       file = file_out)
  
}

# determine batches
getBatch <- function (batch, size) (batch - 1) * size + 1:size

combineBatches <- function(path = 'results',
                           pattern = 'pa_batch') {
  # combine the batch results into one object
  
  batch_files <- list.files(path,
                            pattern = pattern,
                            full.names = TRUE)
  
  load(batch_files[[1]])
  
  # store master versions of the files
  models_ <- models
  times_ <- times
  prevs_ <- prevs
  species_ <- species
  preds_ <- preds
  
  # clear loaded objects
  rm(models, times, prevs, species, preds)
  
  # loop through rest of files
  for (i in 2:length(batch_files)) {
    
    load(batch_files[[i]])
    
    stopifnot(all.equal(models, models_))
    
    # sum times
    times_ <- times_ + times
    
    # append prevalences and species
    prevs_ <- c(prevs_, prevs)
    species_ <- c(species_, species)
    
    # append predictions
    for (model in models_) {
      preds_[[model]] <- cbind(preds_[[model]], preds[[model]])
    }

    # clear loaded objects
    rm(models, times, prevs, species, preds)
    
  } 
  
  # re-order species
  o <- order(species_)
  prevs_ <- prevs_[o]
  species_ <- species_[o]
  for (model in models_) {
    preds_[[model]] <- preds_[[model]][, o]
  }
  
  ans <- list(models = models_,
              times = times_,
              species = species_,
              prevs = prevs_,
              preds = preds_)
  
  return (ans)
  
}

borderPoints <- function(x,
                         border = grey(0.2),
                         fill = grey(0.8),
                         cex = 1,
                         width = 0.1,
                         ...) {
  # plot filled circular points with borders
  cex.inner <- cex * (1 - width)
  for (i in 1:nrow(x)) {
    points(x[i, , drop = FALSE],
           pch = 16,
           cex = cex,
           col = border,
           ...)
    points(x[i, , drop = FALSE],
           pch = 16,
           cex = cex.inner,
           col = fill,
           ...)
    
  }
}