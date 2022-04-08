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
  save(svmparams, file = paste0("../input/", name, '/', name, "svm.rda"))


  knnparams <- knnOptimisation(data, fcol = "markers",
                                   times = 10, xval = 5)
  print(knnparams)
  save(knnparams, file = paste0("../input/", name, '/', name, "knn.rda"))


  nnetparams = nnetOptimisation(data, fcol = "markers",
                                times = 100, xval = 5,
                                decay = c(0, 10^(-1:-5)),
                                size = seq(1, 10, 2))
  print(nnetparams)
  save(nnetparams, file = paste0("../input/", name, '/', name, "nnet.rda"))


  rfparams = rfOptimisation(data, fcol = "markers",
                                times = 100, xval = 5)
  print(rfparams)
  save(rfparams, file = paste0("../input/", name, '/', name, "rf.rda"))


  nbparams = nbOptimisation(data, fcol = "markers",
                                times = 100, xval = 5)
  print(nbparams)
  save(nbparams, file = paste0("../input/", name, '/', name, "nb.rda"))


  mcmcparams <- tagmMcmcCvalpar(object = data, times = 100, test.size = 0.2)
  save(mcmcparams, file = paste0("../input/", name, '/', name, "mcmc.rda"))
  
  
  mapparams <- tagmMapCval(object = data, times = 100, test.size = 0.2)
  save(mapparams, file = paste0("../input/", name, '/', name, "map.rda"))
}


name <- 'nikolovski2014'
data <- readMSnSet2(paste0('../input/', name, '/', name, '.csv'), ecol = 2:21)
pRolocMethods(data, name)




name <- 'E14TG2aR'
data <- readMSnSet2(paste0('../input/', name, '/', name, '.csv'), ecol = 2:9)
pRolocMethods(data, name)





name <- 'itzhak2017'
data <- readMSnSet2(paste0('../input/', name, '/', name, '.csv'), ecol = 2:31)
pRolocMethods(data, name)


data <- hirst2018
name <- 'hirst2018'
data <- readMSnSet2(paste0('../input/', name, '/', name, '.csv'), ecol = 2:16)



name <- 'LOPITDC2019'
data <- readMSnSet2(paste0('../input/', name, '/', name, '.csv'), ecol = 2:31)
pRolocMethods(data, name)

