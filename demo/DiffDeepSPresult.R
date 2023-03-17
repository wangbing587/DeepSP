library(dplyr)
library(ggplot2)
library(ggsci)
library(reshape2)

###################Merge###########################
loadresult <- function(name){
  df_nocnn_cv <- read.csv(sprintf('../data/input_rawdata/%s/%sDeepSP_nocnn_train.csv', name, name))
  df_nocnn_test <- read.csv(sprintf('../data/input_rawdata/%s/%sDeepSP_nocnn_test.csv', name, name))
  df_noatt_cv <- read.csv(sprintf('../data/input_rawdata/%s/%sDeepSP_noatt_train.csv', name, name))
  df_noatt_test <- read.csv(sprintf('../data/input_rawdata/%s/%sDeepSP_noatt_test.csv', name, name))
  df_DeepSP_cv <- read.csv(sprintf('../data/input_rawdata/%s/%sDeepSP_train.csv', name, name))
  df_DeepSP_test <- read.csv(sprintf('../data/input_rawdata/%s/%sDeepSP_test.csv', name, name))
  df_cv <- rbind(df_nocnn_cv, df_noatt_cv, df_DeepSP_cv)
  df_cv$data_split <- 'Cross_validation'
  df_test <- rbind(df_nocnn_test, df_noatt_test, df_DeepSP_test)
  df_test$data_split <- 'Test'
  return(rbind(df_cv, df_test))
}


names <- c('nikolovski2014', 'E14TG2aR', 'itzhak2017', 'hirst2018',
           'LOPITDC2019', 'orre2019', 'mulvey2021')
totalresult <- NULL
for (name in names){
  totalresult <- rbind(totalresult, loadresult(name))
}

colnames(totalresult) <- c("Data", "Method", "F1-score","Precision",
                           "Recall","Accuracy", "Quadloss", 'DataSplit')
totalresult <- totalresult[,c("Data", "Method", 'DataSplit', "F1-score",
                              "Precision", "Recall","Accuracy", "Quadloss")]



write.csv(totalresult, '../output/Classification_DiffDeepSP_result.csv', row.names = F, na = '')



########################Mean±SD#########################

totalresult <- read.csv('../output/Classification_DiffDeepSP_result.csv', check.names = F)


datanames <- c('nikolovski2014', 'E14TG2aR', 'itzhak2017', 'hirst2018',
               'LOPITDC2019', 'orre2019', 'mulvey2021')
methodnames <- c('WithoutCNN', 'WithoutAttention', 'DeepSP')




totalresult$Data <-factor(totalresult$Data, levels = datanames)
totalresult$Method <- factor(totalresult$Method, levels = methodnames)
totalresult$DataSplit <- factor(totalresult$DataSplit, levels = c('Cross_validation', 'Test'))


totalresult_mean <- data.frame(summarise(group_by(totalresult, Data, Method, DataSplit),
                                         `F1-score_mean` = mean(`F1-score`),
                                         Precision_mean = mean(Precision), 
                                         Recall_mean= mean(Recall), 
                                         Accuracy_mean = mean(Accuracy),
                                         Quadloss_mean = mean(Quadloss)), check.names = F)




totalresult_sd <-  data.frame(summarise(group_by(totalresult, Data, Method, DataSplit),
                                        `F1-score_sd` = sd(`F1-score`),
                                        Precision_sd = sd(Precision), 
                                        Recall_sd= sd(Recall), 
                                        Accuracy_sd = sd(Accuracy),
                                        Quadloss_sd = sd(Quadloss)), check.names = F)


total_table <- data.frame(Data = totalresult_mean$Data,
                          Method = totalresult_mean$Method,
                          DataSplit = totalresult_mean$DataSplit,
                          `F1-score`= paste0(round(totalresult_mean$`F1-score_mean`, 3), '±', round(totalresult_sd$`F1-score_sd`, 3)), 
                          Precision=paste0(round(totalresult_mean$Precision_mean, 3), '±', round(totalresult_sd$Precision_sd, 3)),
                          Recall=paste0(round(totalresult_mean$Recall_mean, 3), '±', round(totalresult_sd$Recall_sd, 3)),
                          Accuracy=paste0(round(totalresult_mean$Accuracy_mean, 3), '±', round(totalresult_sd$Accuracy_sd, 3)),
                          Quadloss=paste0(round(totalresult_mean$Quadloss_mean, 3), '±', round(totalresult_sd$Quadloss_sd, 3)))


# write.csv(total_table, '../output/Classification_testresult_stat.csv', 
#           row.names = F, na = '')


library(export)
table2excel(x=total_table,
            file='../output/Classification_DiffDeepSP_stat.csv',
            sheetName='DiffDeepSPresult',
            append = FALSE,
            add.rownames=FALSE, 
            fontName="Calibri",
            border=c("top", "bottom"))




##########wilcox.test#########

testsign <- function(name){
  df_nocnn_cv <- read.csv(sprintf('../data/input_rawdata/%s/%sDeepSP_nocnn_train.csv', name, name))
  df_nocnn_test <- read.csv(sprintf('../data/input_rawdata/%s/%sDeepSP_nocnn_test.csv', name, name))
  df_noatt_cv <- read.csv(sprintf('../data/input_rawdata/%s/%sDeepSP_noatt_train.csv', name, name))
  df_noatt_test <- read.csv(sprintf('../data/input_rawdata/%s/%sDeepSP_noatt_test.csv', name, name))
  df_DeepSP_cv <- read.csv(sprintf('../data/input_rawdata/%s/%sDeepSP_train.csv', name, name))
  df_DeepSP_test <- read.csv(sprintf('../data/input_rawdata/%s/%sDeepSP_test.csv', name, name))

  t_cv_nocnn <- wilcox.test(df_nocnn_cv$f1score, df_DeepSP_cv$f1score, 
                    alternative= 'less')[["p.value"]]
  t_test_nocnn <- wilcox.test(df_nocnn_test$f1score, df_DeepSP_test$f1score, 
                            alternative= 'less')[["p.value"]]
  t_cv_noatt <- wilcox.test(df_noatt_cv$f1score, df_DeepSP_cv$f1score, 
                            alternative= 'less')[["p.value"]]
  t_test_noatt <- wilcox.test(df_noatt_test$f1score, df_DeepSP_test$f1score, 
                            alternative= 'less')[["p.value"]]
  testresult <- data.frame(Data=name, DataSplit=c('Cross_validation','Test'),
             `pvale(WithoutCNN/DeeSP)`=c(t_cv_nocnn, t_test_nocnn),
             `pvalue(WithoutAttention/DeepSP`=c(t_cv_noatt, t_test_noatt),
             check.names = F)
  return(testresult)
}

names <- c('nikolovski2014', 'E14TG2aR', 'itzhak2017', 'hirst2018',
           'LOPITDC2019', 'orre2019', 'mulvey2021')
testresult <- NULL
for (name in names){
  testresult <- rbind(testresult, testsign(name))
}

write.csv(testresult, '../output/SignificanceResult.csv', row.names = F)




################Plot################################

datanames <- c('nikolovski2014', 'E14TG2aR', 'itzhak2017', 'hirst2018',
               'LOPITDC2019', 'orre2019', 'mulvey2021')
methodnames <- c('WithoutCNN', 'WithoutAttention', 'DeepSP')


totalresult <- read.csv('../output/Classification_DiffDeepSP_result.csv', check.names = F)
totalresult$Method <- factor(totalresult$Method, levels =methodnames)
totalresult$Data <- factor(totalresult$Data, levels = datanames)


df <- melt(totalresult, id.vars = c('Data', 'Method', 'DataSplit'),
           variable.name="metrics",
           value.name="value")


df$metrics <- factor(df$metrics, levels = c('F1-score',"Precision", "Recall",
                                            'Accuracy', 'Quadloss'))
table(df$metrics)


df_CV <- df[which(df$DataSplit == 'Cross_validation'), ]
g_cv <- ggplot(df_CV[which(df_CV$metrics != 'Quadloss'), ],
               aes(x = metrics, y = value,fill=Method)) + 
  geom_boxplot(alpha = 0.8, outlier.size=0.5) +  
  stat_boxplot(geom="errorbar") +
  facet_grid(.~Data, scales = "free_y") +
  labs(x = "Metrics" , y = "CV")+
  scale_fill_npg() +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(plot.title = element_text(color="black", size=14),
        axis.title.x = element_text(color="black", size=12),
        axis.title.y = element_text(color="black", size=12))+
  theme(axis.text.x = element_text(color="black",size=10),
        axis.text.y = element_text( color="black",size=10)) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))+
  theme(legend.position = "bottom", 
        legend.direction = "horizontal",
        legend.title = element_blank())
g_cv

ggsave(g_cv, filename = '../figure/Classification_DiffDeeSP_CV.pdf',
       height = 6, width = 12)



df_test <- df[which(df$DataSplit == 'Test'), ]
g_test <- ggplot(df_test[which(df_test$metrics != 'Quadloss'), ],
               aes(x = metrics, y = value,fill=Method)) + 
  geom_boxplot(alpha = 0.8, outlier.size=0.5) +  
  stat_boxplot(geom="errorbar") +
  facet_grid(.~Data, scales = "free_y") +
  labs(x = "Metrics" , y = "Test")+
  scale_fill_npg() +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(plot.title = element_text(color="black", size=14),
        axis.title.x = element_text(color="black", size=12),
        axis.title.y = element_text(color="black", size=12))+
  theme(axis.text.x = element_text(color="black",size=10),
        axis.text.y = element_text( color="black",size=10)) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))+
  theme(legend.position = "bottom", 
        legend.direction = "horizontal",
        legend.title = element_blank())
g_test

ggsave(g_test, filename = '../figure/Classification_DiffDeeSP_test.pdf',
       height = 6, width = 12)

library(patchwork)
g <- (g_cv + theme(legend.position="none") + labs(x = NULL)) / g_test

g

ggsave(g, filename = '../figure/Classification_DiffDeepSP.pdf',
       height = 9, width = 12)

ggsave(g, filename = '../figure/Classification_DiffDeepSP.jpg',
       height = 9, width = 12, dpi = 500)


