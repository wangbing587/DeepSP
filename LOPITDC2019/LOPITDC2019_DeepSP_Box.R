library(dplyr)
library(ggplot2)

dftest <- read.csv('./DeepSP/testresult.csv', row.names = 1,
                   check.names = F, stringsAsFactors = F)

cutoff <- dftest[1, 'cutoff']

?geom_boxplot

plotbox <- function(data, cutoff){
  p <- ggplot(data, aes(x = pred, y = score)) +
    geom_boxplot(fill='grey',notchwidth = 0.3) + 
    theme_bw()+ theme(panel.grid=element_blank()) +
    labs(x='Markers',y='Predicted Score')+
    theme(plot.title=element_text(hjust=0.5))+ 
    theme(axis.title.x=element_text(size=14,color="black"),
          axis.text.x=element_text(size=10,angle =45, color="black",
                                   hjust = 0.5, vjust = 0.5)) +
    theme(axis.title.y=element_text(size=14,color="black"),
          axis.text.y=element_text(size=11,color="black")) +
    geom_hline(yintercept=cutoff, color="red",linetype="dashed")
  
}
p =plotbox(dftest, cutoff)

p
ggsave(p, 
       filename = '../figure/LOPITDC2019_DeepSP_Box.pdf',
       width = 5, height = 6)
