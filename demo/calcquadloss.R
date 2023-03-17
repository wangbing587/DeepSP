library(pRoloc)
source('Rscript/quadloss.R')


calcquadloss <- function(object, name){
  print(name)
  load(sprintf('../data/input_rawdata/%s/%sknn.rda', name, name))
  knnquadloss <- knnquadloss(knnparams, object)
  save(knnquadloss, file = sprintf('../data/input_rawdata/%s/%sknnquadloss.rda', name, name))
  print('knn done!')
  
  load(sprintf('../data/input_rawdata/%s/%ssvm.rda', name, name))
  svmquadloss <- svmquadloss(svmparams, data)
  save(svmquadloss, file = sprintf('../data/input_rawdata/%s/%ssvmquadloss.rda', name, name))
  print('svm done!')
  # load(sprintf('../data/input_rawdata/%s/%snb.rda', name, name))
  # nbquadloss <- nbquadloss(nbparams, object)
  # save(nbquadlosss, file = sprintf('../data/input_rawdata/%s/%snbquadloss.rda', name, name))
  # print('nb done!')
  
  load(sprintf('../data/input_rawdata/%s/%snnet.rda', name, name))
  nnetquadloss <- nnetquadloss(nnetparams, object)
  save(nnetquadloss, file = sprintf('../data/input_rawdata/%s/%snnetquadloss.rda', name, name))
  print('nnet done!')
  
  load(sprintf('../data/input_rawdata/%s/%srf.rda', name, name))
  rfquadloss <- rfquadloss(rfparams, data)
  save(rfquadloss, file = sprintf('../data/input_rawdata/%s/%srfquadloss.rda', name, name))
  print('rf done!')
  
}



name <- 'nikolovski2014'
data <- readMSnSet2(paste0('../data/input_rawdata/', name, '/', name, '.csv'), ecol = 2:21)
calcquadloss(data, name)


name <- 'E14TG2aR'
data <- readMSnSet2(paste0('../data/input_rawdata/', name, '/', name, '.csv'), ecol = 2:9)
calcquadloss(data, name)


name <- 'itzhak2017'
data <- readMSnSet2(paste0('../data/input_rawdata/', name, '/', name, '.csv'), ecol = 2:31)
calcquadloss(data, name)



name <- 'hirst2018'
data <- readMSnSet2(paste0('../data/input_rawdata/', name, '/', name, '.csv'), ecol = 2:16)
calcquadloss(data, name)


name <- 'LOPITDC2019'
data <- readMSnSet2(paste0('../data/input_rawdata/', name, '/', name, '.csv'), ecol = 2:31)
calcquadloss(data, name)


name <- 'orre2019'
data <- readMSnSet2(paste0('../data/input_rawdata/', name, '/', name, '.csv'), ecol = 2:10)
calcquadloss(data, name)


name <- 'mulvey2021'
data <- readMSnSet2(paste0('../data/input_rawdata/', name, '/', name, '.csv'), ecol = 2:61)
calcquadloss(data, name)





####################nb####################

source('Rscript/nbquadloss.R')
calcnbquadloss <- function(object, name){

load(sprintf('../data/input_rawdata/%s/%snb.rda', name, name))
nbquadloss <- nbquadloss(nbparams, object)
save(nbquadloss, file = sprintf('../data/input_rawdata/%s/%snbquadloss.rda', name, name))
print('nb done!')
}





name <- 'nikolovski2014'
data <- readMSnSet2(paste0('../data/input_rawdata/', name, '/', name, '.csv'), ecol = 2:21)
calcnbquadloss(data, name)



name <- 'E14TG2aR'
data <- readMSnSet2(paste0('../data/input_rawdata/', name, '/', name, '.csv'), ecol = 2:9)
calcnbquadloss(data, name)



name <- 'itzhak2017'
data <- readMSnSet2(paste0('../data/input_rawdata/', name, '/', name, '.csv'), ecol = 2:31)
calcnbquadloss(data, name)



name <- 'hirst2018'
data <- readMSnSet2(paste0('../data/input_rawdata/', name, '/', name, '.csv'), ecol = 2:16)
calcnbquadloss(data, name)



name <- 'LOPITDC2019'
data <- readMSnSet2(paste0('../data/input_rawdata/', name, '/', name, '.csv'), ecol = 2:31)
calcnbquadloss(data, name)


name <- 'orre2019'
data <- readMSnSet2(paste0('../data/input_rawdata/', name, '/', name, '.csv'), ecol = 2:10)
calcnbquadloss(data, name)


name <- 'mulvey2021'
data <- readMSnSet2(paste0('../data/input_rawdata/', name, '/', name, '.csv'), ecol = 2:61)
calcnbquadloss(data, name)






