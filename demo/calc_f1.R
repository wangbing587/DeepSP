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
print(colMeans(df[,-c(1,2)]))

write.csv(df, paste0("../input/", dataname, '/', dataname, methodname,".csv"),
          row.names = F, na = '')


methodname = 'svm'
load(paste0("../input/", dataname, '/', dataname, methodname,".rda"))
df <- calcresult(svmparams@cmMatrices, dataname, methodname)
print(colMeans(df[,-c(1,2)]))

write.csv(df, paste0("../input/", dataname, '/', dataname, methodname,".csv"),
          row.names = F, na = '')


methodname = 'mcmc'
load(paste0("../input/", dataname, '/', dataname, methodname,".rda"))
df <- calcresult(mcmcparams[["cmlist"]], dataname, methodname)
print(colMeans(df[,-c(1,2)]))

write.csv(df, paste0("../input/", dataname, '/', dataname, methodname,".csv"),
          row.names = F, na = '')



methodname = 'map'
load(paste0("../input/", dataname, '/', dataname, methodname,".rda"))
df0 <- calcresult(mapparams[["cmlist"]], dataname, methodname)
print(colMeans(df0[,-c(1,2)]))

write.csv(df0, paste0("../input/", dataname, '/', dataname, methodname,".csv"),
          row.names = F, na = '')

}


out_result('nikolovski2014')
out_result('E14TG2aR')
out_result('itzhak2017')
out_result('hirst2018')
out_result('LOPITDC2019')


