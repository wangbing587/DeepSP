library(dplyr)
library(ggplot2)
library(ggsci)
library(reshape2)

df1 <- read.csv('../input/nikolovski2014/AttentionModule/nikolovski2014_AttentionModule.csv',
                check.names = F, stringsAsFactors = F)
df2 <- read.csv('../input/E14TG2aR/AttentionModule/E14TG2aR_AttentionModule.csv',
                check.names = F, stringsAsFactors = F)
df3 <- read.csv('../input/itzhak2017/AttentionModule/itzhak2017_AttentionModule.csv',
                check.names = F, stringsAsFactors = F)
df4 <- read.csv('../input/hirst2018/AttentionModule/hirst2018_AttentionModule.csv',
                check.names = F, stringsAsFactors = F)
df5 <- read.csv('../input/LOPITDC2019/AttentionModule/LOPITDC2019_AttentionModule.csv',
                check.names = F, stringsAsFactors = F)
df6 <- read.csv('../input/orre2019/AttentionModule/orre2019_AttentionModule.csv',
                check.names = F, stringsAsFactors = F)
df7 <- read.csv('../input/mulvey2021/AttentionModule/mulvey2021_AttentionModule.csv',
                check.names = F, stringsAsFactors = F)



df <- rbind(df1, df2, df3, df4, df5, df6, df7)
write.csv(df, '../output/AttentionModule_result.csv', row.names = F, na = '')

df$method <- factor(df$method, levels = c("WithoutAttention", "WithAttention"))
df$data <- factor(df$data, levels = unique(df$data))

colnames(df)
df_cv <- df[which(df$result == 'Cross_validation'), ]

df_cv1 <- melt(df_cv, id.vars = c('data', 'method', 'result'),
               variable.name="metrics",
               value.name="value")




df_test <- df[which(df$result == 'Test'), ]
df_test1 <- melt(df_test, id.vars = c('data', 'method', 'result'),
               variable.name="metrics",
               value.name="value")


g_cv = ggplot(df_cv1, aes(x = metrics, y = value,fill=method)) + 
  geom_bar(stat ="identity",width = 0.6,position = "dodge")+
  facet_grid(.~data, scales = "free_y") +
  labs(x = NULL , y = NULL)+
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

ggsave(g_cv, filename = '../figure/AttentionModule_CV.pdf',
       height = 5, width = 12)



g_test = ggplot(df_test1, aes(x = metrics, y = value,fill=method)) + 
  geom_bar(stat ="identity",width = 0.6,position = "dodge")+
  facet_grid(.~data, scales = "free_y") +
  labs(x = NULL , y = NULL)+
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

ggsave(g_test, filename = '../figure/AttentionModule_Test.pdf',
       height = 5, width = 12)



