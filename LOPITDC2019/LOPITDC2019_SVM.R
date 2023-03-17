library(pRolocdata)
library(pRoloc)
library(dplyr)

data(lopitdcU2OS2018)


library(pRolocdata)
library(pRoloc)
library(dplyr)
data(lopitdcU2OS2018)


df <- data.frame(lopitdcU2OS2018@assayData[["exprs"]], check.names = F)

df$markers <- lopitdcU2OS2018@featureData@data[["markers_10_classes"]]
df$SVMClassification <- lopitdcU2OS2018@featureData@data[["svm_classification_10_classes"]]
df$SVMScore <- lopitdcU2OS2018@featureData@data[["svm_scores_10_classes"]]
write.csv(df, './LOPITDC2019SVMClassification.csv')



load('../data/input_rawdata/LOPITDC2019/LOPITDC2019nnet.rda')
name <- 'LOPITDC2019'
data <- readMSnSet2(paste0('../data/input_rawdata/', name, '/', name, '.csv'), ecol = 2:31)

data <- nnetClassification(data, nnetparams)
df_nnet <- data.frame(data@assayData[["exprs"]], check.names = F)
rownames(df_nnet) <- data@featureData@data[["X"]]
df_nnet$markers <- data@featureData@data[["markers"]]
df_nnet$NNETClassification <- data@featureData@data[["nnet"]]
df_nnet$NNETScore <- data@featureData@data[["nnet.scores"]]
write.csv(df_nnet, './LOPITDC2019NNETClassification.csv')



