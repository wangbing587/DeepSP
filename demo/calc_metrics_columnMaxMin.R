library(dplyr)
library(ggplot2)
library(ggsci)
library(reshape2)
library(patchwork)
library(stringr)

###calc_metrics####
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
  load(paste0("../data/input_columnMaxMin/", dataname, '/', dataname, methodname,".rda"))
  df <- calcresult(knnparams@cmMatrices, dataname, methodname)
  load(sprintf('../data/input_columnMaxMin/%s/%sknnquadloss.rda', dataname, dataname))
  df$quadloss <- unlist(knnquadloss)
  write.csv(df, paste0("../data/input_columnMaxMin/", dataname, '/', dataname, methodname,".csv"),
            row.names = F, na = '')
  
  methodname = 'svm'
  load(paste0("../data/input_columnMaxMin/", dataname, '/', dataname, methodname,".rda"))
  df <- calcresult(svmparams@cmMatrices, dataname, methodname)
  load(sprintf('../data/input_columnMaxMin/%s/%ssvmquadloss.rda', dataname, dataname))
  df$quadloss <- unlist(svmquadloss)
  write.csv(df, paste0("../data/input_columnMaxMin/", dataname, '/', dataname, methodname,".csv"),
            row.names = F, na = '')
  
  methodname = 'nnet'
  load(paste0("../data/input_columnMaxMin/", dataname, '/', dataname, methodname,".rda"))
  df <- calcresult(nnetparams@cmMatrices, dataname, methodname)
  load(sprintf('../data/input_columnMaxMin/%s/%snnetquadloss.rda', dataname, dataname))
  df$quadloss <- unlist(nnetquadloss)
  write.csv(df, paste0("../data/input_columnMaxMin/", dataname, '/', dataname, methodname,".csv"),
            row.names = F, na = '')
  
  methodname = 'rf'
  load(paste0("../data/input_columnMaxMin/", dataname, '/', dataname, methodname,".rda"))
  df <- calcresult(rfparams@cmMatrices, dataname, methodname)
  load(sprintf('../data/input_columnMaxMin/%s/%srfquadloss.rda', dataname, dataname))
  df$quadloss <- unlist(rfquadloss)
  write.csv(df, paste0("../data/input_columnMaxMin/", dataname, '/', dataname, methodname,".csv"),
            row.names = F, na = '')
  
  methodname = 'nb'
  load(paste0("../data/input_columnMaxMin/", dataname, '/', dataname, methodname,".rda"))
  df <- calcresult(nbparams@cmMatrices, dataname, methodname)
  load(sprintf('../data/input_columnMaxMin/%s/%snbquadloss.rda', dataname, dataname))
  df$quadloss <- unlist(nbquadloss)
  write.csv(df, paste0("../data/input_columnMaxMin/", dataname, '/', dataname, methodname,".csv"),
            row.names = F, na = '')
  
  index <- svmparams@testPartitions[[1]]
  methodname = 'mcmc'
  load(paste0("../data/input_columnMaxMin/", dataname, '/', dataname, methodname,".rda"))
  df <- calcresult(mcmcparams[["cmlist"]], dataname, methodname)
  df$quadloss <- unlist(mcmcparams[["quadloss"]]) / length(index)
  write.csv(df, paste0("../data/input_columnMaxMin/", dataname, '/', dataname, methodname,".csv"),
            row.names = F, na = '')
  
  methodname = 'map'
  load(paste0("../data/input_columnMaxMin/", dataname, '/', dataname, methodname,".rda"))
  df <- calcresult(mapparams[["cmlist"]], dataname, methodname)
  df$quadloss <- unlist(mapparams[["quadloss"]]) / length(index)
  write.csv(df, paste0("../data/input_columnMaxMin/", dataname, '/', dataname, methodname,".csv"),
            row.names = F, na = '')
}

names <- c('nikolovski2014', 'E14TG2aR', 'itzhak2017', 'hirst2018',
           'LOPITDC2019', 'orre2019', 'mulvey2021')
for (name in names){
  out_result(name)
}



#######mergeresult#####

loadresult <- function(name){
  df_KNN <- read.csv(sprintf('../data/input_columnMaxMin/%s/%sknn.csv', name, name))
  df_SVM <- read.csv(sprintf('../data/input_columnMaxMin/%s/%ssvm.csv', name, name))
  df_NNET <- read.csv(sprintf('../data/input_columnMaxMin/%s/%snnet.csv', name, name))
  df_RF <- read.csv(sprintf('../data/input_columnMaxMin/%s/%srf.csv', name, name))
  df_NB <- read.csv(sprintf('../data/input_columnMaxMin/%s/%snb.csv', name, name))
  df_MAP <- read.csv(sprintf('../data/input_columnMaxMin/%s/%smap.csv', name, name))
  df_MCMC <- read.csv(sprintf('../data/input_columnMaxMin/%s/%smcmc.csv', name, name))
  df_DeepSP <- read.csv(sprintf('../data/input_columnMaxMin/%s/%sDeepSP_test.csv', name, name))
  return(rbind(df_KNN, df_SVM, df_NNET, df_RF, df_NB, df_MAP, df_MCMC, df_DeepSP))
}

totalresult <- NULL
for (name in names){
  totalresult <- rbind(totalresult, loadresult(name))
}

colnames(totalresult) <- c("Data", "Method", "F1-score","Precision",
                           "Recall","Accuracy", "Quadloss")

totalresult[which(totalresult$Method == "ColumnMaxMin"), 'Method'] = 'DeepSP'
write.csv(totalresult, '../output/Classification_columnMaxMin_testresult.csv', row.names = F, na = '')




##########Mean_Std#########
datanames <- c('nikolovski2014', 'E14TG2aR', 'itzhak2017', 'hirst2018',
               'LOPITDC2019', 'orre2019', 'mulvey2021')
methodnames <- c('KNN', 'SVM', 'NNET','RF', 
                 'NB', 'MAP', 'MCMC', 'DeepSP')

totalresult <- read.csv('../output/Classification_columnMaxMin_testresult.csv', 
                        check.names = F)

totalresult$Data <-factor(totalresult$Data, levels = datanames)
totalresult$Method <- factor(totalresult$Method, levels = methodnames)



totalresult_mean <- data.frame(summarise(group_by(totalresult, Data, Method),
                                         `F1-score_mean` = mean(`F1-score`),
                                         Precision_mean = mean(Precision), 
                                         Recall_mean= mean(Recall), 
                                         Accuracy_mean = mean(Accuracy),
                                         Quadloss_mean = mean(Quadloss)), check.names = F)




totalresult_sd <-  data.frame(summarise(group_by(totalresult, Data, Method),
                                        `F1-score_sd` = sd(`F1-score`),
                                        Precision_sd = sd(Precision), 
                                        Recall_sd= sd(Recall), 
                                        Accuracy_sd = sd(Accuracy),
                                        Quadloss_sd = sd(Quadloss)), check.names = F)


total_table <- data.frame(Data = totalresult_mean$Data,
                          Method = totalresult_mean$Method,
                          `F1-score`= paste0(round(totalresult_mean$`F1-score_mean`, 3), '±', round(totalresult_sd$`F1-score_sd`, 3)), 
                          Precision=paste0(round(totalresult_mean$Precision_mean, 3), '±', round(totalresult_sd$Precision_sd, 3)),
                          Recall=paste0(round(totalresult_mean$Recall_mean, 3), '±', round(totalresult_sd$Recall_sd, 3)),
                          Accuracy=paste0(round(totalresult_mean$Accuracy_mean, 3), '±', round(totalresult_sd$Accuracy_sd, 3)),
                          Quadloss=paste0(round(totalresult_mean$Quadloss_mean, 3), '±', round(totalresult_sd$Quadloss_sd, 3)))




library(export)
table2excel(x=total_table,
            file='../output/Classification_columnMaxMin_testresult_stat.csv',
            sheetName='testresult',
            append = FALSE,
            add.rownames=FALSE, 
            fontName="Calibri",
            border=c("top", "bottom"))


##########################Plot################################
datanames <- c('nikolovski2014', 'E14TG2aR', 'itzhak2017', 'hirst2018',
               'LOPITDC2019', 'orre2019', 'mulvey2021')
methodnames <- c('KNN', 'SVM', 'NNET','RF', 
                 'NB', 'MAP', 'MCMC', 'DeepSP')

totalresult <- read.csv('../output/Classification_columnMaxMin_testresult.csv', 
                        check.names = F)
totalresult$Data <-factor(totalresult$Data, 
                          levels = datanames)

totalresult$Method <- factor(totalresult$Method,
                             levels = methodnames)



gF <-  ggplot(totalresult, aes(x = Method, y = `F1-score`,fill=Method)) + 
  geom_boxplot(alpha = 0.8, outlier.size=0.5) + 
  stat_boxplot(geom="errorbar") +
  facet_grid(.~Data, scales = "free_y") +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(plot.title = element_text(color="black", size=14),
        axis.title.x = element_text(color="black", size=12),
        axis.title.y = element_text(color="black", size=12))+
  theme(axis.text.x = element_text(color="black",size=10),
        axis.text.y = element_text( color="black",size=10)) +
  theme(axis.text.x = element_blank())+
  theme(legend.position = "bottom", 
        legend.direction = "horizontal",
        legend.title = element_blank()) + 
  theme(legend.position="none") + labs(x = NULL)



gP <- ggplot(totalresult, aes(x = Method, y = Precision,fill=Method)) + 
  geom_boxplot(alpha = 0.8, outlier.size=0.5) + 
  stat_boxplot(geom="errorbar") +
  facet_grid(.~Data, scales = "free_y") +
  # scale_fill_npg() +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(plot.title = element_text(color="black", size=14),
        axis.title.x = element_text(color="black", size=12),
        axis.title.y = element_text(color="black", size=12))+
  theme(axis.text.x = element_text(color="black",size=10),
        axis.text.y = element_text( color="black",size=10)) +
  theme(axis.text.x = element_blank())+
  theme(legend.position = "bottom", 
        legend.direction = "horizontal",
        legend.title = element_blank()) + 
  theme(legend.position="none") + labs(x = NULL)



gR <- ggplot(totalresult, aes(x = Method, y = Recall,fill=Method)) + 
  geom_boxplot(alpha = 0.8, outlier.size=0.5) + 
  stat_boxplot(geom="errorbar") +
  facet_grid(.~Data, scales = "free_y") +
  # scale_fill_npg() +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(plot.title = element_text(color="black", size=14),
        axis.title.x = element_text(color="black", size=12),
        axis.title.y = element_text(color="black", size=12))+
  theme(axis.text.x = element_text(color="black",size=10),
        axis.text.y = element_text( color="black",size=10)) +
  theme(axis.text.x = element_blank())+
  theme(legend.position = "bottom", 
        legend.direction = "horizontal",
        legend.title = element_blank())  + 
  theme(legend.position="none") + labs(x = NULL)




gA <- ggplot(totalresult, aes(x = Method, y = Accuracy,fill=Method)) + 
  geom_boxplot(alpha = 0.8, outlier.size=0.5) +  
  stat_boxplot(geom="errorbar") +
  facet_grid(.~Data, scales = "free_y") +
  # labs(x = "Method" , y = "Accuracy")+
  # scale_fill_npg() +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(plot.title = element_text(color="black", size=14),
        axis.title.x = element_text(color="black", size=12),
        axis.title.y = element_text(color="black", size=12))+
  theme(axis.text.x = element_text(color="black",size=10),
        axis.text.y = element_text( color="black",size=10)) +
  theme(axis.text.x = element_blank())+
  theme(legend.position = "bottom", 
        legend.direction = "horizontal",
        legend.title = element_blank())  + 
  theme(legend.position="none") + labs(x = NULL)





gQ <- ggplot(totalresult, aes(x = Method, y = Quadloss,fill=Method)) + 
  geom_boxplot(alpha = 0.8, outlier.size=0.5) +  
  stat_boxplot(geom="errorbar") +
  facet_grid(.~Data, scales = "free_y") +
  labs(x = "Method" , y = "Quadloss")+
  # scale_fill_npg() +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(plot.title = element_text(color="black", size=14),
        axis.title.x = element_text(color="black", size=12),
        axis.title.y = element_text(color="black", size=12))+
  theme(axis.text.x = element_text(color="black",size=10),
        axis.text.y = element_text( color="black",size=10)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(legend.position = "bottom", 
        legend.direction = "horizontal",
        legend.title = element_blank()) + 
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))





library(patchwork)

g <- gF/gA/gP/gR/ gR / gQ



ggsave(g, filename = '../figure/Classification_columnMaxMin.pdf',
       height =15, width = 12)


ggsave(g, filename = '../figure/Classification_columnMaxMin.jpg',
       height =15, width = 12, dpi = 300)

