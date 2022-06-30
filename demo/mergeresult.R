library(dplyr)
library(ggplot2)
library(ggsci)

df1_KNN <- read.csv('../input/nikolovski2014/nikolovski2014knn.csv')
df1_SVM <- read.csv('../input/nikolovski2014/nikolovski2014svm.csv')
df1_NNET <- read.csv('../input/nikolovski2014/nikolovski2014nnet.csv')
df1_RF <- read.csv('../input/nikolovski2014/nikolovski2014rf.csv')
df1_NB <- read.csv('../input/nikolovski2014/nikolovski2014nb.csv')
df1_MAP <- read.csv('../input/nikolovski2014/nikolovski2014map.csv')
df1_MCMC <- read.csv('../input/nikolovski2014/nikolovski2014mcmc.csv')
df1_DeepSP <- read.csv('../input/nikolovski2014/nikolovski2014DeepSP.csv')


df2_KNN <- read.csv('../input/E14TG2aR/E14TG2aRknn.csv')
df2_SVM <- read.csv('../input/E14TG2aR/E14TG2aRsvm.csv')
df2_NNET <- read.csv('../input/E14TG2aR/E14TG2aRnnet.csv')
df2_RF <- read.csv('../input/E14TG2aR/E14TG2aRrf.csv')
df2_NB <- read.csv('../input/E14TG2aR/E14TG2aRnb.csv')
df2_MAP <- read.csv('../input/E14TG2aR/E14TG2aRmap.csv')
df2_MCMC <- read.csv('../input/E14TG2aR/E14TG2aRmcmc.csv')
df2_DeepSP <- read.csv('../input/E14TG2aR/E14TG2aRDeepSP.csv')


df3_KNN <- read.csv('../input/itzhak2017/itzhak2017knn.csv')
df3_SVM <- read.csv('../input/itzhak2017/itzhak2017svm.csv')
df3_NNET <- read.csv('../input/itzhak2017/itzhak2017nnet.csv')
df3_RF <- read.csv('../input/itzhak2017/itzhak2017rf.csv')
df3_NB <- read.csv('../input/itzhak2017/itzhak2017nb.csv')
df3_MAP <- read.csv('../input/itzhak2017/itzhak2017map.csv')
df3_MCMC <- read.csv('../input/itzhak2017/itzhak2017mcmc.csv')
df3_DeepSP <- read.csv('../input/itzhak2017/itzhak2017DeepSP.csv')

df4_KNN <- read.csv('../input/hirst2018/hirst2018knn.csv')
df4_SVM <- read.csv('../input/hirst2018/hirst2018svm.csv')
df4_NNET <- read.csv('../input/hirst2018/hirst2018nnet.csv')
df4_RF <- read.csv('../input/hirst2018/hirst2018rf.csv')
df4_NB <- read.csv('../input/hirst2018/hirst2018nb.csv')
df4_MAP <- read.csv('../input/hirst2018/hirst2018map.csv')
df4_MCMC <- read.csv('../input/hirst2018/hirst2018mcmc.csv')
df4_DeepSP <- read.csv('../input/hirst2018/hirst2018DeepSP.csv')


df5_KNN <- read.csv('../input/LOPITDC2019/LOPITDC2019knn.csv')
df5_SVM <- read.csv('../input/LOPITDC2019/LOPITDC2019svm.csv')
df5_NNET <- read.csv('../input/LOPITDC2019/LOPITDC2019nnet.csv')
df5_RF <- read.csv('../input/LOPITDC2019/LOPITDC2019nb.csv')
df5_NB <- read.csv('../input/LOPITDC2019/LOPITDC2019rf.csv')
df5_MAP <- read.csv('../input/LOPITDC2019/LOPITDC2019map.csv')
df5_MCMC <- read.csv('../input/LOPITDC2019/LOPITDC2019mcmc.csv')
df5_DeepSP <- read.csv('../input/LOPITDC2019/LOPITDC2019DeepSP.csv')

df6_KNN <- read.csv('../input/orre2019/orre2019knn.csv')
df6_SVM <- read.csv('../input/orre2019/orre2019svm.csv')
df6_NNET <- read.csv('../input/orre2019/orre2019nnet.csv')
df6_RF <- read.csv('../input/orre2019/orre2019nb.csv')
df6_NB <- read.csv('../input/orre2019/orre2019rf.csv')
df6_MAP <- read.csv('../input/orre2019/orre2019map.csv')
df6_MCMC <- read.csv('../input/orre2019/orre2019mcmc.csv')
df6_DeepSP <- read.csv('../input/orre2019/orre2019DeepSP.csv')


df7_KNN <- read.csv('../input/mulvey2021/mulvey2021knn.csv')
df7_SVM <- read.csv('../input/mulvey2021/mulvey2021svm.csv')
df7_NNET <- read.csv('../input/mulvey2021/mulvey2021nnet.csv')
df7_RF <- read.csv('../input/mulvey2021/mulvey2021nb.csv')
df7_NB <- read.csv('../input/mulvey2021/mulvey2021rf.csv')
df7_MAP <- read.csv('../input/mulvey2021/mulvey2021map.csv')
df7_MCMC <- read.csv('../input/mulvey2021/mulvey2021mcmc.csv')
df7_DeepSP <- read.csv('../input/mulvey2021/mulvey2021DeepSP.csv')




totalresult <- rbind(df1_KNN, df1_SVM, df1_NNET, df1_RF, df1_NB, df1_MAP, df1_MCMC, df1_DeepSP, 
                     df2_KNN, df2_SVM, df2_NNET, df2_RF, df2_NB, df2_MAP, df2_MCMC, df2_DeepSP, 
                     df3_KNN, df3_SVM, df3_NNET, df3_RF, df3_NB, df3_MAP, df3_MCMC, df3_DeepSP, 
                     df4_KNN, df4_SVM, df4_NNET, df4_RF, df4_NB, df4_MAP, df4_MCMC, df4_DeepSP, 
                     df5_KNN, df5_SVM, df5_NNET, df5_RF, df5_NB, df5_MAP, df5_MCMC, df5_DeepSP,
                     df6_KNN, df6_SVM, df6_NNET, df6_RF, df6_NB, df6_MAP, df6_MCMC, df6_DeepSP,
                     df5_KNN, df5_SVM, df5_NNET, df5_RF, df5_NB, df5_MAP, df5_MCMC, df5_DeepSP, 
                     df7_KNN, df7_SVM, df7_NNET, df7_RF, df7_NB, df7_MAP, df7_MCMC, df7_DeepSP)

write.csv(totalresult, '../output/Classification_result.csv', row.names = F, na = '')

####################################################
totalresult <- read.csv('../output/Classification_result.csv')
totalresult$data <-factor(totalresult$data, 
                          levels = c('nikolovski2014', 'E14TG2aR',
                                     'itzhak2017', 'hirst2018', 'LOPITDC2019',
                                     'orre2019', 'mulvey2021'))

totalresult$method <- factor(totalresult$method,
                             levels = c('KNN', 'SVM', 'NNET','RF', 
                                        'NB', 'MAP', 'MCMC', 'DeepSP'))



colnames(totalresult)

g1 = ggplot(totalresult, aes(x = method, y = f1score,fill=method)) + 
  geom_boxplot(alpha = 0.8, outlier.size=0.5) + 
  # scale_fill_npg() +
  facet_grid(.~data, scales = "free_y") +
  labs(x = "Method" , y = "F1-score")+
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(plot.title = element_text(color="black", size=14),
        axis.title.x = element_text(color="black", size=12),
        axis.title.y = element_text(color="black", size=12))+
  theme(axis.text.x = element_text(color="black",size=10),
        axis.text.y = element_text( color="black",size=10)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(legend.position = "bottom", 
        legend.direction = "horizontal",
        legend.title = element_blank())
  
g1

ggsave(g1, filename = '../figure/Classification_F1score.pdf',
       height = 6, width = 12)


g2 = ggplot(totalresult, aes(x = method, y = precision,fill=method)) + 
  geom_boxplot(alpha = 0.8, outlier.size=0.5) + 
  facet_grid(.~data, scales = "free_y") +
  labs(x = "Method" , y = "Precision")+
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
        legend.title = element_blank()) 
g2

ggsave(g2, filename = '../figure/Classification_Precision.pdf',
       height = 6, width = 12)



g3 = ggplot(totalresult, aes(x = method, y = recall,fill=method)) + 
  geom_boxplot(alpha = 0.8, outlier.size=0.5) + 
  facet_grid(.~data, scales = "free_y") +
  labs(x = "Method" , y = "Recall")+
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
        legend.title = element_blank())

g3

ggsave(g3, filename = '../figure/Classification_Recall.pdf',
       height = 6, width = 12)



g4 = ggplot(totalresult, aes(x = method, y = accuracy,fill=method)) + 
  geom_boxplot(alpha = 0.8, outlier.size=0.5) +  
  facet_grid(.~data, scales = "free_y") +
  labs(x = "Method" , y = "Accuracy")+
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
        legend.title = element_blank())

g4


ggsave(g4, filename = '../figure/Classification_Accuracy.pdf',
       height = 6, width = 12)




totalresult_mean <- data.frame(summarise(group_by(totalresult, data, method),
                     f1score_mean = mean(f1score),
                     precision_mean = mean(precision), 
                     recall_mean= mean(recall), 
                     accuracy_mean = mean(accuracy)))

# totalresult_median <- data.frame(summarise(group_by(totalresult, data, method),
#                                          f1score_median = median(f1score),
#                                          precision_median = median(precision), 
#                                          recall_median= median(recall), 
#                                          accuracy_median = median(accuracy)))



write.csv(totalresult_mean, '../output/Classification_result_mean.csv', 
          row.names = F, na = '')


