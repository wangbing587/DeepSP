library(ggplot2)
library(ggsci)
library(scales)
library(ggrepel)
library(reshape2)

data <- read.csv('./DeepSP/testresult.csv')




g <- ggplot(data,aes(x = `pred`, y = `score`)) + 
  geom_boxplot(alpha = 0.8, outlier.size=0.5, fill='grey') +  
  stat_boxplot(geom="errorbar") +
  labs(x = "PSL Label" , y = "Predicted Score")+
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(plot.title = element_text(color="black", size=14),
        axis.title.x = element_text(color="black", size=12),
        axis.title.y = element_text(color="black", size=12))+
  theme(axis.text.x = element_text(color="black",size=10),
        axis.text.y = element_text( color="black",size=10)) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))+
  theme(legend.position = "bottom", 
        legend.direction = "horizontal",
        legend.title = element_blank()) + 
  geom_hline(aes(yintercept=data[1,c('cutoff')]),colour="red", linetype="dashed")


g

ggsave('../figure/nikolovski2014_DeepSP_pred.pdf', 
       height = 4.8, width = 5)












