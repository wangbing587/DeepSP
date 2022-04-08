library(ggplot2)
library(ggsci)
library(scales)
library(ggrepel)

data <- read.csv('./DeepSP_att/nikolovski2014_DeepSCP_result.csv',
                 row.names = 1)


data$markers = factor(data$markers, 
                      levels = c('Cytosol', 'ER', 
                                 'Golgi', 'Mitochondrion',
                                 'PM', 'Plastid', 'Ribosome',
                                 'Vacuole', 'unknown'))

exprs <- data[,-c((ncol(data) -4): ncol(data))]
pca <- prcomp(exprs, scale = TRUE, center = TRUE)
df <- data.frame(PC1 = pca$x[,1], 
                 PC2 = pca$x[,2], 
                 markers = data[, 'markers'],
                 row.names = rownames(data))



P=c(1,2)


col <- c("#E41A1C","#377EB8","#238B45","#FF7F00","#FFD700",
         "#333333","#00CED1","#A65628","#F781BF","#984EA3",
         "#9ACD32","#B0C4DE","#00008A","#8B795E","#FDAE6B",
         "#66C2A5","#276419","#CD8C95","#6A51A3","#EEAD0E",
         "#0000FF","#9ACD32","#CD6090","#CD5B45","#8E0152",
         "#808000","#67000D","#3F007D","#6BAED6","#FC9272")




show_col(col)


col[6] = '#9ACD32'
col[length(levels(data$markers))] = 'gray'
# show_col(col)



df <- data.frame(PC1 = pca$x[,1], 
                 PC2 = pca$x[,2], 
                 markers = data[, 'markers'],
                 row.names = rownames(data))

m <-sort(unique(df$markers))
m <- c(m[m != 'unknown'], 'unknown')
df$markers <- factor(df$markers, levels = m)



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
        axis.text.x=element_text(size=11)) +
  theme(axis.title.y=element_text(size=13),
        axis.text.y=element_text(size=11))


p0
ggsave(p0, filename = '../figure/nikolovski2014_rawPCA.pdf',
       height = 4, width = 5.3)




df <- data.frame(PC1 = pca$x[,1], 
                 PC2 = pca$x[,2], 
                 markers = data[, 'Curated.DeepSP.predictions'],
                 row.names = rownames(data))

m <-sort(unique(df$markers))
m <- c(m[m != 'unknown'], 'unknown')
df$markers <- factor(df$markers, levels = m)


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
        axis.text.x=element_text(size=11)) +
  theme(axis.title.y=element_text(size=13),
        axis.text.y=element_text(size=11))

p
ggsave(p, filename = '../figure/nikolovski2014_DeepSP_PCA.pdf', 
       height = 5, width = 6)







