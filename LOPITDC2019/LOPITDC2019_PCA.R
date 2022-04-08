library(ggplot2)
library(ggsci)
library(scales)
library(ggrepel)

data <- read.csv('LOPITDC2019_DeepSCP_SVM_result.csv', row.names = 1)
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



col[6] = '#9ACD32'
col[11] = 'gray'




exprs <- data[,c(1:30)]
pca <- prcomp(exprs, scale = TRUE, center = TRUE)
df <- data.frame(PC1 = pca$x[,1], 
                 PC2 = pca$x[,2], 
                 markers = data[, 'Subcellular.markers'],
                 row.names = rownames(data))


m <-sort(unique(df$markers))
m <- c(m[m != 'unknown'], 'unknown')
df$markers <- factor(df$markers, levels = m)
PC = round(pca$sdev **2 / sum(pca$sdev ** 2) *100, 2)

P=c(1,2)



p0 <- ggplot(df, aes(x=PC1, y=PC2, color=markers, 
               shape=markers,alpha=markers)) +
  geom_point(size=1.5) + 
  theme_bw() + theme(panel.grid=element_blank()) +
  scale_color_manual(values = col[1:11])+
  scale_shape_manual(values = c(rep(16, 10),1))+
  scale_alpha_manual(values = c(rep(0.6, 10), 0.2)) + 
  # scale_x_continuous(breaks = c(-5, 0,  5)) +
  # scale_y_continuous(breaks = c(-5, 0,  5))+
  theme(plot.title=element_text(hjust=0.5))+ 
  theme(axis.title.x=element_text(size=13),
        axis.text.x=element_text(size=13)) +
  theme(axis.title.y=element_text(size=13),
        axis.text.y=element_text(size=13)) +
  theme(legend.title = element_text(size = 11), 
        legend.text = element_text(size = 11)) +
  labs(
    # title = 'PCA', 
    x = paste0('PC1 (', PC[1], '%)'),
    y = paste0('PC2 (', PC[2], '%)')) 

p0
ggsave(p0, filename = '../figure/LOPITDC2019_rawPCA.pdf', 
       height = 5, width = 7.15)





df <- data.frame(PC1 = pca$x[,1], 
                 PC2 = pca$x[,2], 
                 markers = data[, 'Curated.DeepSP.predictions'],
                 row.names = rownames(data))


m <-sort(unique(df$markers))
m <- c(m[m != 'unknown'], 'unknown')
df$markers <- factor(df$markers, levels = m)
PC = round(pca$sdev **2 / sum(pca$sdev ** 2) *100, 2)

P=c(1,2)


p <- ggplot(df, aes(x=PC1, y=PC2, color=markers, 
                     shape=markers,alpha=markers)) +
  geom_point(size=1.5) + 
  theme_bw() + theme(panel.grid=element_blank()) +
  scale_color_manual(values = col[1:11])+
  scale_shape_manual(values = c(rep(16, 10),1))+
  scale_alpha_manual(values = c(rep(0.6, 10), 0.2)) + 
  # scale_x_continuous(breaks = c(-5, 0,  5)) +
  # scale_y_continuous(breaks = c(-5, 0,  5))+
  theme(plot.title=element_text(hjust=0.5))+ 
  theme(axis.title.x=element_text(size=13),
        axis.text.x=element_text(size=13)) +
  theme(axis.title.y=element_text(size=13),
        axis.text.y=element_text(size=13)) +
  theme(legend.title = element_text(size = 11), 
        legend.text = element_text(size = 11)) +
  labs(
    # title = 'PCA', 
    x = paste0('PC1 (', PC[1], '%)'),
    y = paste0('PC2 (', PC[2], '%)')) 


proteins <- c('Q9Y324','Q16394','Q7L4I2')
for (i in proteins){
  p <- p + geom_point(x =  df[i, 'PC1'], 
                      y =  df[i, 'PC2'], 
                      color='black', size=2, show.legend = FALSE)
}

p <- p + geom_text_repel(data = df[proteins,],
                  aes(label = rownames(df[proteins,])),
                  fontface="bold", color='black',alpha=1,
                  size = 4,
                  segment.color = "black", show.legend = FALSE)
  
p
ggsave(p, filename = '../figure/LOPITDC2019_DeepSP_PCA.pdf',
       height = 5, width = 7.15)




