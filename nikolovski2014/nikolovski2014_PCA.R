library(ggplot2)
library(ggsci)
library(scales)
library(ggrepel)

data <- read.csv('./DeepSP/nikolovski2014_DeepSCP_result.csv',
                 row.names = 1, check.names = F)

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

PC <- round(pca$sdev **2 / sum(pca$sdev ** 2) *100, 2)


col <- c("#E41A1C","#377EB8","#238B45","#FF7F00","#FFD700",
         "#333333","#00CED1","#A65628","#F781BF","#984EA3",
         "#9ACD32","#B0C4DE","#00008A","#8B795E","#FDAE6B",
         "#66C2A5","#276419","#CD8C95","#6A51A3","#EEAD0E",
         "#0000FF","#9ACD32","#CD6090","#CD5B45","#8E0152",
         "#808000","#67000D","#3F007D","#6BAED6","#FC9272")




show_col(col)

col[7] = col[13]
col[6] = '#9ACD32'
col[2] = "#FF7F00"
col[4] = "#377EB8"

col[length(levels(data$markers))] = 'gray'
# show_col(col)



df <- data.frame(PC1 = pca$x[,1], 
                 PC2 = pca$x[,2], 
                 markers = data[, 'markers'],
                 row.names = rownames(data))

m <-sort(unique(df$markers))
m <- c(as.character(m[m != 'unknown']), 'unknown')
df$markers <- factor(df$markers, levels = m)





p0 <- ggplot(df, aes(x=PC1, y=PC2, color=markers, 
                     shape=markers,alpha=markers)) +
  geom_point(size=1.5) + 
  theme_bw() + theme(panel.grid=element_blank()) +
  scale_color_manual(values = col[1:11], name = "PSL Label")+
  scale_shape_manual(values = c(rep(16, 10),1))+
  scale_alpha_manual(values = c(rep(0.8, 8), 0.4)) + 
  # scale_x_continuous(breaks = c(-5, 0,  5)) +
  # scale_y_continuous(breaks = c(-5, 0,  5))+
  # theme(plot.title=element_text(hjust=0.5))+ 
  theme(axis.title.x=element_text(size=13),
        axis.text.x=element_text(size=11)) +
  theme(axis.title.y=element_text(size=13),
        axis.text.y=element_text(size=11)) + 
  labs(# title = 'PCA', 
    x = paste0('PC1 (', PC[1], '%)'),
    y = paste0('PC2 (', PC[2], '%)'))+
  theme(legend.position = "bottom", 
        legend.justification = "center")



p0


ggsave(p0, filename = '../figure/nikolovski2014_rawPCA.pdf',
       width = PC[1] /8, height = PC[2] / 8 +1.18)



df <- data.frame(PC1 = pca$x[,1], 
                 PC2 = pca$x[,2], 
                 `PSL Label` = data[, 'DeepSP classification'],
                 Score = data$`DeepSP score`,
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
  theme(legend.title = element_text(size= 40),
        legend.text = element_text(size=35))+
  labs(# title = 'PCA', 
    x = paste0('PC1 (', PC[1], '%)'),
    y = paste0('PC2 (', PC[2], '%)')) +
  theme(legend.position = "bottom", 
        legend.justification = "center") + 
  guides(size = FALSE, alpha = FALSE) +
  guides(color = guide_legend(override.aes = list(size=5))) +
  theme(legend.key.height = unit(4, "line"))
        
p
ggsave(p, filename = '../figure/nikolovski2014_DeepSP_PCA.pdf', 
       width = PC[1] /2, height = PC[2] / 2 +2.4)



50.87/17.44








