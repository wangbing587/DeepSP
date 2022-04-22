library(dplyr)
library(ggplot2)
library(ggsci)

df1_KNN <- read.csv('../input/nikolovski2014/nikolovski2014knn.csv')
df1_SVM <- read.csv('../input/nikolovski2014/nikolovski2014svm.csv')
df1_MAP <- read.csv('../input/nikolovski2014/nikolovski2014map.csv')
df1_MCMC <- read.csv('../input/nikolovski2014/nikolovski2014mcmc.csv')
df1_DeepSP <- read.csv('../input/nikolovski2014/nikolovski2014DeepSP.csv')


df2_KNN <- read.csv('../input/E14TG2aR/E14TG2aRknn.csv')
df2_SVM <- read.csv('../input/E14TG2aR/E14TG2aRsvm.csv')
df2_MAP <- read.csv('../input/E14TG2aR/E14TG2aRmap.csv')
df2_MCMC <- read.csv('../input/E14TG2aR/E14TG2aRmcmc.csv')
df2_DeepSP <- read.csv('../input/E14TG2aR/E14TG2aRDeepSP.csv')


df3_KNN <- read.csv('../input/itzhak2017/itzhak2017knn.csv')
df3_SVM <- read.csv('../input/itzhak2017/itzhak2017svm.csv')
df3_MAP <- read.csv('../input/itzhak2017/itzhak2017map.csv')
df3_MCMC <- read.csv('../input/itzhak2017/itzhak2017mcmc.csv')
df3_DeepSP <- read.csv('../input/itzhak2017/itzhak2017DeepSP.csv')

df4_KNN <- read.csv('../input/hirst2018/hirst2018knn.csv')
df4_SVM <- read.csv('../input/hirst2018/hirst2018svm.csv')
# df4_MAP <- read.csv('../input/hirst2018/hirst2018map.csv')
df4_MCMC <- read.csv('../input/hirst2018/hirst2018mcmc.csv')
df4_DeepSP <- read.csv('../input/hirst2018/hirst2018DeepSP.csv')


df5_KNN <- read.csv('../input/LOPITDC2019/LOPITDC2019knn.csv')
df5_SVM <- read.csv('../input/LOPITDC2019/LOPITDC2019svm.csv')
df5_MAP <- read.csv('../input/LOPITDC2019/LOPITDC2019map.csv')
df5_MCMC <- read.csv('../input/LOPITDC2019/LOPITDC2019mcmc.csv')
df5_DeepSP <- read.csv('../input/LOPITDC2019/LOPITDC2019DeepSP.csv')



totalresult <- rbind(df1_KNN, df1_SVM, df1_MAP, df1_MCMC, df1_DeepSP, 
                     df2_KNN, df2_SVM, df2_MAP, df2_MCMC, df2_DeepSP, 
                     df3_KNN, df3_SVM, df3_MAP, df3_MCMC, df3_DeepSP, 
                     df4_KNN, df4_SVM, df4_MCMC, df4_DeepSP, 
                     df5_KNN, df5_SVM, df5_MAP, df5_MCMC, df5_DeepSP)

write.csv(totalresult, '../output/Classification_result.csv', row.names = F, na = '')

####################################################
totalresult <- read.csv('../output/Classification_result.csv')
totalresult$data <-factor(totalresult$data, 
                          levels = c('nikolovski2014', 'E14TG2aR',
                                     'itzhak2017', 'hirst2018', 'LOPITDC2019'))

totalresult$method <- factor(totalresult$method,
                             levels = c('KNN', 'SVM', 'MAP', 'MCMC', 'DeepSP'))


colnames(totalresult)

g1 = ggplot(totalresult, aes(x = method, y = f1score,fill=method)) + 
  geom_boxplot() + 
  facet_grid(.~data, scales = "free_y") +
  labs(x = "Method" , y = "F1-score")+
  scale_fill_npg() +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(plot.title = element_text(color="black", size=14),
        axis.title.x = element_text(color="black", size=12),
        axis.title.y = element_text(color="black", size=12))+
  theme(axis.text.x = element_text(color="black",size=9),
        axis.text.y = element_text( color="black",size=9)) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))+
  theme(legend.position = "bottom", 
        legend.direction = "horizontal",
        legend.title = element_blank())
  
g1

ggsave(g1, filename = '../figure/Classification_F1score.pdf',
       height = 3.5, width = 10)




g2 = ggplot(totalresult, aes(x = method, y = precision,fill=method)) + 
  geom_boxplot() + 
  facet_grid(.~data, scales = "free_y") +
  labs(x = "Method" , y = "Precision")+
  scale_fill_npg() +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(plot.title = element_text(color="black", size=14),
        axis.title.x = element_text(color="black", size=12),
        axis.title.y = element_text(color="black", size=12))+
  theme(axis.text.x = element_text(color="black",size=9),
        axis.text.y = element_text( color="black",size=9)) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))+
  theme(legend.position = "bottom", 
        legend.direction = "horizontal",
        legend.title = element_blank())
g2

ggsave(g2, filename = '../figure/Classification_Precision.pdf',
       height = 3.5, width = 10)



g3 = ggplot(totalresult, aes(x = method, y = recall,fill=method)) + 
  geom_boxplot() + 
  facet_grid(.~data, scales = "free_y") +
  labs(x = "Method" , y = "Recall")+
  scale_fill_npg() +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(plot.title = element_text(color="black", size=14),
        axis.title.x = element_text(color="black", size=12),
        axis.title.y = element_text(color="black", size=12))+
  theme(axis.text.x = element_text(color="black",size=9),
        axis.text.y = element_text( color="black",size=9)) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))+
  theme(legend.position = "bottom", 
        legend.direction = "horizontal",
        legend.title = element_blank())

g3

ggsave(g3, filename = '../figure/Classification_Recall.pdf',
       height = 3.5, width = 10)





g4 = ggplot(totalresult, aes(x = method, y = accuracy,fill=method)) + 
  geom_boxplot() + 
  facet_grid(.~data, scales = "free_y") +
  labs(x = "Method" , y = "Accuracy")+
  scale_fill_npg() +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(plot.title = element_text(color="black", size=14),
        axis.title.x = element_text(color="black", size=12),
        axis.title.y = element_text(color="black", size=12))+
  theme(axis.text.x = element_text(color="black",size=9),
        axis.text.y = element_text( color="black",size=9)) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))+
  theme(legend.position = "bottom", 
        legend.direction = "horizontal",
        legend.title = element_blank())

g4

ggsave(g4, filename = '../figure/Classification_Accuracy.pdf',
       height = 3.5, width = 10)




totalresult_mean <- data.frame(summarise(group_by(totalresult, data, method),
                     f1score_mean = mean(f1score),
                     precision_mean = mean(precision), 
                     recall_mean= mean(recall), 
                     accuracy_mean = mean(accuracy)))



write.csv(totalresult_mean, '../output/Classification_result_mean.csv', 
          row.names = F, na = '')



























