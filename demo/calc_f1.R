library(dplyr)
library(stringr)

calcresult <- function(data,dataname, methodname){
  
  p <- lapply(data, function(x) MLInterfaces:::precision(x))
  r <- lapply(data, function(x) MLInterfaces:::recall(x))
  p1 <- sapply(p, mean, na.rm=T)
  r1 <- sapply(r, mean, na.rm=T)
  acc <- sapply(data, function(x) MLInterfaces:::acc(x))
  
  f1 <- vector("numeric", length = 100)
  for(i in 1:100){
    f1[i] <- MLInterfaces:::.macroF1(p[[i]], r[[i]], naAs0 = TRUE)
  }
  
  df = data.frame(data=dataname,
                  method=str_to_upper(methodname),
                  f1score= f1,
                  precision=p1,
                  recall=r1,
                  accuracy= acc)
}
  


out_result <- function(dataname){
methodname = 'knn'
load(paste0("../input/", dataname, '/', dataname, methodname,".rda"))
df <- calcresult(knnparams@cmMatrices, dataname, methodname)
print(methodname)
print(colMeans(df[,-c(1,2)]))

write.csv(df, paste0("../input/", dataname, '/', dataname, methodname,".csv"),
          row.names = F, na = '')


methodname = 'svm'
load(paste0("../input/", dataname, '/', dataname, methodname,".rda"))
df <- calcresult(svmparams@cmMatrices, dataname, methodname)
print(methodname)
print(colMeans(df[,-c(1,2)]))

write.csv(df, paste0("../input/", dataname, '/', dataname, methodname,".csv"),
          row.names = F, na = '')


methodname = 'nnet'
load(paste0("../input/", dataname, '/', dataname, methodname,".rda"))
df <- calcresult(nnetparams@cmMatrices, dataname, methodname)
print(methodname)
print(colMeans(df[,-c(1,2)]))

write.csv(df, paste0("../input/", dataname, '/', dataname, methodname,".csv"),
          row.names = F, na = '')


methodname = 'rf'
load(paste0("../input/", dataname, '/', dataname, methodname,".rda"))
df <- calcresult(rfparams@cmMatrices, dataname, methodname)
print(methodname)
print(colMeans(df[,-c(1,2)]))

write.csv(df, paste0("../input/", dataname, '/', dataname, methodname,".csv"),
          row.names = F, na = '')


methodname = 'nb'
load(paste0("../input/", dataname, '/', dataname, methodname,".rda"))
df <- calcresult(nbparams@cmMatrices, dataname, methodname)
print(methodname)
print(colMeans(df[,-c(1,2)]))

write.csv(df, paste0("../input/", dataname, '/', dataname, methodname,".csv"),
          row.names = F, na = '')


methodname = 'mcmc'
load(paste0("../input/", dataname, '/', dataname, methodname,".rda"))
df <- calcresult(mcmcparams[["cmlist"]], dataname, methodname)
print(methodname)
print(colMeans(df[,-c(1,2)]))

write.csv(df, paste0("../input/", dataname, '/', dataname, methodname,".csv"),
          row.names = F, na = '')


methodname = 'map'
load(paste0("../input/", dataname, '/', dataname, methodname,".rda"))
df0 <- calcresult(mapparams[["cmlist"]], dataname, methodname)
print(methodname)
print(colMeans(df0[,-c(1,2)]))

write.csv(df0, paste0("../input/", dataname, '/', dataname, methodname,".csv"),
          row.names = F, na = '')
}


out_result('nikolovski2014')
out_result('E14TG2aR')
out_result('itzhak2017')
out_result('hirst2018')
out_result('LOPITDC2019')
out_result('orre2019')
out_result('mulvey2021')

################result####################
> out_result('nikolovski2014')
[1] "knn"
f1score precision    recall  accuracy 
0.6161600 0.6739663 0.6137392 0.6372727 
[1] "svm"
f1score precision    recall  accuracy 
0.6273624 0.6519314 0.6433065 0.6252727 
[1] "nnet"
f1score precision    recall  accuracy 
0.6358646 0.6733687 0.6419426 0.6614545 
[1] "rf"
f1score precision    recall  accuracy 
0.6851419 0.7392443 0.6846564 0.7029091 
[1] "nb"
f1score precision    recall  accuracy 
0.5731373 0.5934659 0.6010837 0.5850909 
[1] "mcmc"
f1score precision    recall  accuracy 
0.5741233 0.6194839 0.5793294 0.5736364 
[1] "map"
f1score precision    recall  accuracy 
0.5745387 0.6774317 0.5571955 0.5836364 
> out_result('E14TG2aR')
[1] "knn"
f1score precision    recall  accuracy 
0.7990109 0.8405092 0.8086611 0.8923684 
[1] "svm"
f1score precision    recall  accuracy 
0.7513019 0.7902490 0.7786254 0.8481579 
[1] "nnet"
f1score precision    recall  accuracy 
0.7832546 0.8228180 0.7959683 0.8839474 
[1] "rf"
f1score precision    recall  accuracy 
0.7830516 0.8193086 0.7979802 0.8934211 
[1] "nb"
f1score precision    recall  accuracy 
0.7771900 0.7935749 0.7994048 0.8678947 
[1] "mcmc"
f1score precision    recall  accuracy 
0.7548923 0.8355684 0.7860095 0.8850000 
[1] "map"
f1score precision    recall  accuracy 
0.7833863 0.8166690 0.8040841 0.8863158 
> out_result('itzhak2017')
[1] "knn"
f1score precision    recall  accuracy 
0.7601525 0.8380935 0.7464510 0.8745349 
[1] "svm"
f1score precision    recall  accuracy 
0.7386191 0.7379164 0.8013210 0.8166279 
[1] "nnet"
f1score precision    recall  accuracy 
0.7571095 0.8469234 0.7496753 0.8754651 
[1] "rf"
f1score precision    recall  accuracy 
0.7621097 0.8792700 0.7450883 0.8859302 
[1] "nb"
f1score precision    recall  accuracy 
0.7209065 0.7183846 0.7742109 0.8051163 
[1] "mcmc"
f1score precision    recall  accuracy 
0.7452811 0.7676820 0.7656281 0.8567442 
[1] "map"
f1score precision    recall  accuracy 
0.6950832 0.8771160 0.6680685 0.8520349 
> out_result('hirst2018')
[1] "knn"
f1score precision    recall  accuracy 
0.7122754 0.7729303 0.7146373 0.8605128 
[1] "svm"
f1score precision    recall  accuracy 
0.7233243 0.7575976 0.7568319 0.8065385 
[1] "nnet"
f1score precision    recall  accuracy 
0.7916280 0.8311888 0.7937480 0.8996795 
[1] "rf"
f1score precision    recall  accuracy 
0.7803416 0.8556568 0.7650485 0.9016026 
[1] "nb"
f1score precision    recall  accuracy 
0.6562864 0.6601694 0.7161614 0.7713462 
[1] "mcmc"
f1score precision    recall  accuracy 
0.5342322 0.7012645 0.5309748 0.8134615 
[1] "map"
f1score precision    recall  accuracy 
0.7351284 0.8501339 0.7252568 0.8712179 
> out_result('LOPITDC2019')
[1] "knn"
f1score precision    recall  accuracy 
0.9860129 0.9886826 0.9859910 0.9900000 
[1] "svm"
f1score precision    recall  accuracy 
0.9811545 0.9853654 0.9801280 0.9826667 
[1] "nnet"
f1score precision    recall  accuracy 
0.9846681 0.9886041 0.9833144 0.9881667 
[1] "rf"
f1score precision    recall  accuracy 
0.9866556 0.9930540 0.9836723 0.9918333 
[1] "nb"
f1score precision    recall  accuracy 
0.9841820 0.9924531 0.9801330 0.9907500 
[1] "mcmc"
f1score precision    recall  accuracy 
0.9886545 0.9862019 0.9925821 0.9896667 
[1] "map"
f1score precision    recall  accuracy 
0.9888501 0.9921307 0.9875106 0.9914167 
> out_result('orre2019')
[1] "knn"
f1score precision    recall  accuracy 
0.7188256 0.7315737 0.7140249 0.7310177 
[1] "svm"
f1score precision    recall  accuracy 
0.7016095 0.6996898 0.7190739 0.7130973 
[1] "nnet"
f1score precision    recall  accuracy 
0.7243151 0.7367679 0.7210493 0.7430088 
[1] "rf"
f1score precision    recall  accuracy 
0.7337915 0.7417848 0.7316466 0.7473156 
[1] "nb"
f1score precision    recall  accuracy 
0.7040254 0.7120915 0.7134973 0.7121091 
[1] "mcmc"
f1score precision    recall  accuracy 
0.7265195 0.7475256 0.7188185 0.7429941 
[1] "map"
f1score precision    recall  accuracy 
0.7326825 0.7510895 0.7267869 0.7464897 
> out_result('mulvey2021')
[1] "knn"
f1score precision    recall  accuracy 
0.8429171 0.8804997 0.8335909 0.8885714 
[1] "svm"
f1score precision    recall  accuracy 
0.8581976 0.8597410 0.8687378 0.8913665 
[1] "nnet"
f1score precision    recall  accuracy 
0.8934023 0.9073051 0.8906452 0.9203727 
[1] "rf"
f1score precision    recall  accuracy 
0.8741867 0.8984009 0.8661900 0.9065839 
[1] "nb"
f1score precision    recall  accuracy 
0.7984795 0.8113614 0.7974758 0.8516149 
[1] "mcmc"
f1score precision    recall  accuracy 
0.7802482 0.8929665 0.7786892 0.8738509 
[1] "map"
f1score precision    recall  accuracy 
0.8705195 0.9230288 0.8612820 0.9178882
##############################################


