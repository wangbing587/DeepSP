library(dplyr)
library(stringr)




calcf1 <- function(data, methodname){
  p <- lapply(data, function(x) MLInterfaces:::precision(x))
  r <- lapply(data, function(x) MLInterfaces:::recall(x))
  p_40 <- NULL
  p_60 <- NULL
  r_40 <- NULL
  r_60 <- NULL
  f_40 <- NULL
  f_60 <- NULL
  for (i in 1:100){
    # 40S Ribosome_60S Ribosome
    p_40[i] <- p[[i]][[1]]
    p_60[i] <- p[[i]][[2]]
    r_40[i] <- p[[i]][[1]]
    r_60[i] <- p[[i]][[2]]
    
    # Cytosol_Proteasome 
    # p_40[i] <- p[[i]][[3]]
    # p_60[i] <- p[[i]][[10]]
    # r_40[i] <- p[[i]][[3]]
    # r_60[i] <- p[[i]][[10]]
    f_40[i] <- MLInterfaces:::.macroF1(p_40[[i]], r_40[[i]], naAs0 = TRUE)
    f_60[i] <- MLInterfaces:::.macroF1(p_60[[i]], r_60[[i]], naAs0 = TRUE)
  }
  print(methodname)
  print(mean(f_40))
  print(mean(f_60))
}

dataname <- 'E14TG2aR'

methodname = 'knn'
load(paste0("../data/input_rawdata/", dataname, '/', dataname, methodname,".rda"))
data <- knnparams@cmMatrices
calcf1(data, methodname)


methodname = 'svm'
load(paste0("../data/input_rawdata/", dataname, '/', dataname, methodname,".rda"))
data <- svmparams@cmMatrices
calcf1(data, methodname)

methodname = 'nnet'
load(paste0("../data/input_rawdata/", dataname, '/', dataname, methodname,".rda"))
data <- nnetparams@cmMatrices
calcf1(data, methodname)



methodname = 'rf'
load(paste0("../data/input_rawdata/", dataname, '/', dataname, methodname,".rda"))
data <- rfparams@cmMatrices
calcf1(data, methodname)

methodname = 'nb'
load(paste0("../data/input_rawdata/", dataname, '/', dataname, methodname,".rda"))
data <- nbparams@cmMatrices
calcf1(data, methodname)


methodname = 'map'
load(paste0("../data/input_rawdata/", dataname, '/', dataname, methodname,".rda"))
data <- mapparams[["cmlist"]]
calcf1(data, methodname)


methodname = 'mcmc'
load(paste0("../data/input_rawdata/", dataname, '/', dataname, methodname,".rda"))
data <- mcmcparams[["cmlist"]]
calcf1(data, methodname)






40S Ribosome_60S Ribosome
[1] "knn"
[1] 0.7501685
[1] 0.9110357
"svm"
[1] 0.7255238
[1] 0.8448135
[1] "nnet"
[1] 0.7936429
[1] 0.8996032
[1] "rf"
[1] 0.8144008
[1] 0.8944336
[1] "nb"
[1] 0.7907619
[1] 0.9067619
[1] "map"
[1] 0.7922143
[1] 0.9018795

[1] "mcmc"
[1] 0.8066746
[1] 0.8641916
'DeepSP'

0.8282629037629031
0.8944518275841804



Cytosol_Proteasome 

[1] "knn"
[1] 0.6833413
[1] 0.5569286
[1] "svm"
[1] 0.6866865
[1] 0.3499167
[1] "nnet"s
[1] 0.6354008
[1] 0.3547857
[1] "rf"
[1] 0.6462341
[1] 0.3758333
[1] "nb"
[1] 0.7192262
[1] 0.4113095
[1] "map"
[1] 0.6034087
[1] 0.2642619
"mcmc"
[1] 0.6025397
[1] 0.004

DeepSP
0.7107725052725047
0.5617402597402592

