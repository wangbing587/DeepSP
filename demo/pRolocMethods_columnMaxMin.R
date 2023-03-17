library(pRoloc)
library(pRolocdata)
source('Rscript/cross-val.R')
set.seed(1)


pRolocMethods <-function(data,name){
  w <- table(getMarkers(data, verbose = F))
  w <- 1/w[names(w) != "unknown"]
  svmparams <- svmOptimisation(data, fcol = "markers",
                               times = 100, xval = 5,
                               class.weights = w)
  print(svmparams)
  save(svmparams, file = paste0("../data/input_columnMaxmin/", name, '/', name, "svm.rda"))
  
  
  knnparams <- knnOptimisation(data, fcol = "markers",
                               times = 100, xval = 5)
  print(knnparams)
  save(knnparams, file = paste0("../data/input_columnMaxmin/", name, '/', name, "knn.rda"))
  
  
  nnetparams = nnetOptimisation(data, fcol = "markers",
                                times = 100, xval = 5,
                                decay = c(0, 10^(-1:-5)),
                                size = seq(1, 10, 2))
  print(nnetparams)
  save(nnetparams, file = paste0("../data/input_columnMaxmin/", name, '/', name, "nnet.rda"))
  
  
  rfparams = rfOptimisation(data, fcol = "markers",
                            times = 100, xval = 5)
  print(rfparams)
  save(rfparams, file = paste0("../data/input_columnMaxmin/", name, '/', name, "rf.rda"))
  
  
  nbparams = nbOptimisation(data, fcol = "markers",
                            times = 100, xval = 5)
  print(nbparams)
  save(nbparams, file = paste0("../data/input_columnMaxmin/", name, '/', name, "nb.rda"))
  
  
  
  mapparams <- tagmMapCval(object = data, times = 100, test.size = 0.2)
  save(mapparams, file = paste0("../data/input_columnMaxmin/", name, '/', name, "map.rda"))
  
  mcmcparams <- tagmMcmcCvalpar(object = data, times = 100, test.size = 0.2)
  save(mcmcparams, file = paste0("../data/input_columnMaxmin/", name, '/', name, "mcmc.rda"))
  
}

# rm(list = ls())




name <- 'nikolovski2014'
data <- readMSnSet2(paste0('../data/input_columnMaxmin/', name, '/', name, '.csv'), ecol = 2:21)
pRolocMethods(data, name)


name <- 'E14TG2aR'
data <- readMSnSet2(paste0('../data/input_columnMaxmin/', name, '/', name, '.csv'), ecol = 2:9)
pRolocMethods(data, name)


name <- 'itzhak2017'
data <- readMSnSet2(paste0('../data/input_columnMaxmin/', name, '/', name, '.csv'), ecol = 2:31)
pRolocMethods(data, name)


name <- 'hirst2018'
data <- readMSnSet2(paste0('../data/input_columnMaxmin/', name, '/', name, '.csv'), ecol = 2:16)
pRolocMethods(data, name)


name <- 'LOPITDC2019'
data <- readMSnSet2(paste0('../data/input_columnMaxmin/', name, '/', name, '.csv'), ecol = 2:31)
pRolocMethods(data, name)


name <- 'orre2019'
data <- readMSnSet2(paste0('../data/input_columnMaxmin/', name, '/', name, '.csv'), ecol = 2:10)
pRolocMethods(data, name)


name <- 'mulvey2021'
data <- readMSnSet2(paste0('../data/input_columnMaxmin/', name, '/', name, '.csv'), ecol = 2:61)
pRolocMethods(data, name)





#######MCMC########

mcmc_repeat <- function(data, name){
  times <- 100
  cmlist <- vector("list", length = times)
  quadloss <- vector("list", length = times)
  for (time in 1:times){
    mcmcparams <- tagmMcmcCvalpar(object = data, times = 1, test.size = 0.2)
    cmlist[[time]] <- mcmcparams[["cmlist"]][[1]]
    quadloss[[time]] <- mcmcparams[["quadloss"]][[1]]
  }
  
  mcmcparams <- list(cmlist = cmlist, quadloss = quadloss)
  save(mcmcparams, file = paste0("../data/input_columnMaxMin/", name, '/', name, "mcmc.rda"))
}


name <- 'nikolovski2014'
data <- readMSnSet2(paste0('../data/input_columnMaxMin/', name, '/', name, '.csv'), ecol = 2:21)
mcmc_repeat(data, name)


name <- 'E14TG2aR'
data <- readMSnSet2(paste0('../data/input_columnMaxMin/', name, '/', name, '.csv'), ecol = 2:9)
mcmc_repeat(data, name)


name <- 'itzhak2017'
data <- readMSnSet2(paste0('../data/input_columnMaxMin/', name, '/', name, '.csv'), ecol = 2:31)
mcmc_repeat(data, name)


name <- 'hirst2018'
data <- readMSnSet2(paste0('../data/input_columnMaxMin/', name, '/', name, '.csv'), ecol = 2:16)
mcmc_repeat(data, name)


name <- 'LOPITDC2019'
data <- readMSnSet2(paste0('../data/input_columnMaxMin/', name, '/', name, '.csv'), ecol = 2:31)
mcmc_repeat(data, name)


name <- 'orre2019'
data <- readMSnSet2(paste0('../data/input_columnMaxMin/', name, '/', name, '.csv'), ecol = 2:10)
mcmc_repeat(data, name)


name <- 'mulvey2021'
data <- readMSnSet2(paste0('../data/input_columnMaxMin/', name, '/', name, '.csv'), ecol = 2:61)
mcmc_repeat(data, name)

