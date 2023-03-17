library(ggplot2)
library(ggsci)
library(scales)
library(ggrepel)



data <- read.csv('LOPITDC2019SVM_DeepSP.csv', row.names = 1,
                 check.names = F)
colnames(data)


# "SVM.classification"         "SVM.score"  "Subcellular.markers"                     
# [35] "Curated.SVM.predictions"    "DeepSP.classification"     
# [37] "DeepSP.score"               "Curated.DeepSP.predictions"



col <- c("#E41A1C","#377EB8","#238B45","#FF7F00","#FFD700",
         "#333333","#00CED1","#A65628","#F781BF","#984EA3",
         "#9ACD32","#B0C4DE","#00008A","#8B795E","#FDAE6B",
         "#66C2A5","#276419","#CD8C95","#6A51A3","#EEAD0E",
         "#0000FF","#9ACD32","#CD6090","#CD5B45","#8E0152",
         "#808000","#67000D","#3F007D","#6BAED6","#FC9272")


col[7] = col[13]
col[6] = '#9ACD32'
col[11] = 'gray'




exprs <- data[,c(1:30)]


P=c(1,2)
df <- data.frame(PC1 = pca$x[,1], 
                 PC2 = pca$x[,2], 
                 `PSL Label` = data[, 'DeepSPClassification'],
                 Score = data$`DeepSPScore`,
                 row.names = rownames(data),
                 check.names = F)



m <-sort(unique(df$`PSL Label`))
# m <- c(m[m != 'unknown'], 'unknown')
df$`PSL Label` <- factor(df$`PSL Label`, levels = m)

PC <- round(pca$sdev **2 / sum(pca$sdev ** 2) *100, 2)

p <- ggplot(df, aes(x=PC1, y=PC2, color=`PSL Label`,
                    size=Score, alpha=`PSL Label`)) +
  geom_point() + 
  theme_bw() + theme(panel.grid=element_blank()) +
  scale_color_manual(values = col[1:11])+
  # scale_shape_manual(values = c(rep(16, 10),1))+
  scale_alpha_manual(values = c(rep(0.8, 11), 0.4)) +
  # scale_x_continuous(breaks = c(-5, 0,  5)) +
  # scale_y_continuous(breaks = c(-5, 0,  5))+
  # theme(plot.title=element_text(hjust=0.5))+ 
  theme(axis.title.x=element_text(size=40),
        axis.text.x=element_text(size=36)) +
  theme(axis.title.y=element_text(size=40),
        axis.text.y=element_text(size=36)) +
  theme(legend.title = element_text(size= 30),
        legend.text = element_text(size=25))+
  labs(# title = 'PCA', 
    x = paste0('PC1 (', PC[1], '%)'),
    y = paste0('PC2 (', PC[2], '%)')) +
  theme(legend.position = "bottom", 
        legend.justification = "center") + 
  guides(size = FALSE, alpha = FALSE) +
  guides(color = guide_legend(override.aes = list(size=5))) +
  theme(legend.key.height = unit(4, "line"))

p


proteins <- c('Q9Y324', 'Q9H9L3','Q9Y3C7', 'P42696',
              'Q7Z412', 'Q9Y2S0-2', 'Q9BY49', 'Q9UKR5')
for (i in proteins){
  p <- p + geom_point(x =  df[i, 'PC1'], 
                      y =  df[i, 'PC2'], 
                      color='black', size=8, show.legend = FALSE)
}

p <- p + geom_text_repel(data = df[proteins,],
                  aes(label = rownames(df[proteins,])),
                  fontface="bold", color='black',alpha=1,
                  size = 10,
                  segment.color = "black", show.legend = FALSE)
  
p
ggsave(p, filename = '../figure/LOPITDC2019_DeepSP_PCA.pdf',
       width = PC[1]/2, height = PC[2]/2 + 2.2)



