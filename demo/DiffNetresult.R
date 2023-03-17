library(dplyr)
library(ggplot2)
library(ggsci)
library(reshape2)

methodnames <- c('InceptionNet', 'DenseNet', 'DeepSP')
name <- names[1]
###################Merge###########################
loadresult <- function(name){
  df_IN <- read.csv(sprintf('../data/input_otherNet/%s/%sInceptionNet_test.csv', name, name))
  df_DN <- read.csv(sprintf('../data/input_otherNet/%s/%sDenseNet_test.csv', name, name))
  df_DP <- read.csv(sprintf('../data/input_rawdata/%s/%sDeepSP_test.csv', name, name))
  return(rbind(df_IN, df_DN, df_DP))
}


names <- c('nikolovski2014', 'E14TG2aR', 'hirst2018')
totalresult <- NULL
for (name in names){
  totalresult <- rbind(totalresult, loadresult(name))
}

colnames(totalresult) <- c("Data", "Method", "F1-score","Precision",
                           "Recall","Accuracy", "Quadloss")



write.csv(totalresult, '../output/Classification_DiffNet_result.csv', row.names = F, na = '')



########################Mean盨D#########################

totalresult <- read.csv('../output/Classification_DiffNet_result.csv', check.names = F)


datanames <- c('nikolovski2014', 'E14TG2aR', 'hirst2018')
methodnames <- c('InceptionNet', 'DenseNet', 'DeepSP')




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
                          `F1-score`= paste0(round(totalresult_mean$`F1-score_mean`, 3), '?', round(totalresult_sd$`F1-score_sd`, 3)), 
                          Precision=paste0(round(totalresult_mean$Precision_mean, 3), '?', round(totalresult_sd$Precision_sd, 3)),
                          Recall=paste0(round(totalresult_mean$Recall_mean, 3), '?', round(totalresult_sd$Recall_sd, 3)),
                          Accuracy=paste0(round(totalresult_mean$Accuracy_mean, 3), '?', round(totalresult_sd$Accuracy_sd, 3)),
                          Quadloss=paste0(round(totalresult_mean$Quadloss_mean, 3), '?', round(totalresult_sd$Quadloss_sd, 3)))


# write.csv(total_table, '../output/Classification_testresult_stat.csv', 
#           row.names = F, na = '')


library(export)
table2excel(x=total_table,
            file='../output/Classification_DiffNet_stat.csv',
            sheetName='DiffDeepSPresult',
            append = FALSE,
            add.rownames=FALSE, 
            fontName="Calibri",
            border=c("top", "bottom"))







# ################Plot################################
# 
# datanames <- c('nikolovski2014', 'E14TG2aR', 'hirst2018')
# methodnames <- c('LessLayer', 'MoreLayer','SmallFilter', 'LargeFilter',
#                  'LessChannel', 'MoreChannel', 'DeepSP')
# 
# 
# 
# totalresult <- read.csv('../output/Classification_DiffModel_result.csv', check.names = F)
# totalresult$Method <- factor(totalresult$Method, levels =methodnames)
# totalresult$Data <- factor(totalresult$Data, levels = datanames)
# 
# df <- totalresult
# 
# 
# gF <-  ggplot(totalresult, aes(x = Method, y = `F1-score`,fill=Method)) + 
#   geom_boxplot(alpha = 0.8, outlier.size=0.5) + 
#   stat_boxplot(geom="errorbar") +
#   facet_grid(.~Data, scales = "free_y") +
#   theme_bw() + theme(panel.grid=element_blank()) +
#   theme(plot.title = element_text(color="black", size=14),
#         axis.title.x = element_text(color="black", size=12),
#         axis.title.y = element_text(color="black", size=12))+
#   theme(axis.text.x = element_text(color="black",size=10),
#         axis.text.y = element_text( color="black",size=10)) +
#   theme(axis.text.x = element_blank())+
#   theme(legend.position = "bottom", 
#         legend.direction = "horizontal",
#         legend.title = element_blank()) + 
#   theme(legend.position="none") + labs(x = NULL)
# 
# 
# 
# gP <- ggplot(totalresult, aes(x = Method, y = Precision,fill=Method)) + 
#   geom_boxplot(alpha = 0.8, outlier.size=0.5) + 
#   stat_boxplot(geom="errorbar") +
#   facet_grid(.~Data, scales = "free_y") +
#   # scale_fill_npg() +
#   theme_bw() + theme(panel.grid=element_blank()) +
#   theme(plot.title = element_text(color="black", size=14),
#         axis.title.x = element_text(color="black", size=12),
#         axis.title.y = element_text(color="black", size=12))+
#   theme(axis.text.x = element_text(color="black",size=10),
#         axis.text.y = element_text( color="black",size=10)) +
#   theme(axis.text.x = element_blank())+
#   theme(legend.position = "bottom", 
#         legend.direction = "horizontal",
#         legend.title = element_blank()) + 
#   theme(legend.position="none") + labs(x = NULL)
# 
# 
# 
# gR <- ggplot(totalresult, aes(x = Method, y = Recall,fill=Method)) + 
#   geom_boxplot(alpha = 0.8, outlier.size=0.5) + 
#   stat_boxplot(geom="errorbar") +
#   facet_grid(.~Data, scales = "free_y") +
#   # scale_fill_npg() +
#   theme_bw() + theme(panel.grid=element_blank()) +
#   theme(plot.title = element_text(color="black", size=14),
#         axis.title.x = element_text(color="black", size=12),
#         axis.title.y = element_text(color="black", size=12))+
#   theme(axis.text.x = element_text(color="black",size=10),
#         axis.text.y = element_text( color="black",size=10)) +
#   theme(axis.text.x = element_blank())+
#   theme(legend.position = "bottom", 
#         legend.direction = "horizontal",
#         legend.title = element_blank())  + 
#   theme(legend.position="none") + labs(x = NULL)
# 
# 
# 
# 
# gA <- ggplot(totalresult, aes(x = Method, y = Accuracy,fill=Method)) + 
#   geom_boxplot(alpha = 0.8, outlier.size=0.5) +  
#   stat_boxplot(geom="errorbar") +
#   facet_grid(.~Data, scales = "free_y") +
#   # labs(x = "Method" , y = "Accuracy")+
#   # scale_fill_npg() +
#   theme_bw() + theme(panel.grid=element_blank()) +
#   theme(plot.title = element_text(color="black", size=14),
#         axis.title.x = element_text(color="black", size=12),
#         axis.title.y = element_text(color="black", size=12))+
#   theme(axis.text.x = element_text(color="black",size=10),
#         axis.text.y = element_text( color="black",size=10)) +
#   theme(axis.text.x = element_blank())+
#   theme(legend.position = "bottom", 
#         legend.direction = "horizontal",
#         legend.title = element_blank())  + 
#   theme(legend.position="none") + labs(x = NULL)
# 
# 
# 
# 
# 
# gQ <- ggplot(totalresult, aes(x = Method, y = Quadloss,fill=Method)) + 
#   geom_boxplot(alpha = 0.8, outlier.size=0.5) +  
#   stat_boxplot(geom="errorbar") +
#   facet_grid(.~Data, scales = "free_y") +
#   labs(x = "Method" , y = "Quadloss")+
#   # scale_fill_npg() +
#   theme_bw() + theme(panel.grid=element_blank()) +
#   theme(plot.title = element_text(color="black", size=14),
#         axis.title.x = element_text(color="black", size=12),
#         axis.title.y = element_text(color="black", size=12))+
#   theme(axis.text.x = element_text(color="black",size=10),
#         axis.text.y = element_text( color="black",size=10)) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))+
#   theme(legend.position = "bottom", 
#         legend.direction = "horizontal",
#         legend.title = element_blank()) + 
#   guides(fill = guide_legend(nrow = 1, byrow = TRUE))
# 
# 
# 
# 
# 
# library(patchwork)
# 
# g <- gF/gA/gP/gR/ gR / gQ
# 
# 
# 
# ggsave(g, filename = '../figure/Classification_DiffModel.pdf',
#        height =15, width = 12)
# 
# 
# ggsave(g, filename = '../figure/Classification_DiffModel.jpg',
#        height =15, width = 12, dpi = 300)
