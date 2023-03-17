library(pRoloc)
source('Rscript/quadloss.R')


calcquadloss <- function(object, name){
  print(name)
  load(sprintf('../data/input_rowMaxMin/%s/%sknn.rda', name, name))
  knnquadloss <- knnquadloss(knnparams, object)
  save(knnquadloss, file = sprintf('../data/input_rowMaxMin/%s/%sknnquadloss.rda', name, name))
  print('knn done!')
  
  load(sprintf('../data/input_rowMaxMin/%s/%ssvm.rda', name, name))
  svmquadloss <- svmquadloss(svmparams, data)
  save(svmquadloss, file = sprintf('../data/input_rowMaxMin/%s/%ssvmquadloss.rda', name, name))
  print('svm done!')
  # load(sprintf('../data/input_rowMaxMin/%s/%snb.rda', name, name))
  # nbquadloss <- nbquadloss(nbparams, object)
  # save(nbquadlosss, file = sprintf('../data/input_rowMaxMin/%s/%snbquadloss.rda', name, name))
  # print('nb done!')
  
  load(sprintf('../data/input_rowMaxMin/%s/%snnet.rda', name, name))
  nnetquadloss <- nnetquadloss(nnetparams, object)
  save(nnetquadloss, file = sprintf('../data/input_rowMaxMin/%s/%snnetquadloss.rda', name, name))
  print('nnet done!')
  
  load(sprintf('../data/input_rowMaxMin/%s/%srf.rda', name, name))
  rfquadloss <- rfquadloss(rfparams, data)
  save(rfquadloss, file = sprintf('../data/input_rowMaxMin/%s/%srfquadloss.rda', name, name))
  print('rf done!')
  
}



name <- 'nikolovski2014'
data <- readMSnSet2(paste0('../data/input_rowMaxMin/', name, '/', name, '.csv'), ecol = 2:21)
calcquadloss(data, name)


name <- 'E14TG2aR'
data <- readMSnSet2(paste0('../data/input_rowMaxMin/', name, '/', name, '.csv'), ecol = 2:9)
calcquadloss(data, name)


name <- 'itzhak2017'
data <- readMSnSet2(paste0('../data/input_rowMaxMin/', name, '/', name, '.csv'), ecol = 2:31)
calcquadloss(data, name)



name <- 'hirst2018'
data <- readMSnSet2(paste0('../data/input_rowMaxMin/', name, '/', name, '.csv'), ecol = 2:16)
calcquadloss(data, name)


name <- 'LOPITDC2019'
data <- readMSnSet2(paste0('../data/input_rowMaxMin/', name, '/', name, '.csv'), ecol = 2:31)
calcquadloss(data, name)


name <- 'orre2019'
data <- readMSnSet2(paste0('../data/input_rowMaxMin/', name, '/', name, '.csv'), ecol = 2:10)
calcquadloss(data, name)


name <- 'mulvey2021'
data <- readMSnSet2(paste0('../data/input_rowMaxMin/', name, '/', name, '.csv'), ecol = 2:61)
calcquadloss(data, name)





####################nb####################

source('Rscript/nbquadloss.R')
calcnbquadloss <- function(object, name){
  
  load(sprintf('../data/input_rowMaxMin/%s/%snb.rda', name, name))
  nbquadloss <- nbquadloss(nbparams, object)
  save(nbquadloss, file = sprintf('../data/input_rowMaxMin/%s/%snbquadloss.rda', name, name))
  print('nb done!')
}





name <- 'nikolovski2014'
data <- readMSnSet2(paste0('../data/input_rowMaxMin/', name, '/', name, '.csv'), ecol = 2:21)
calcnbquadloss(data, name)



name <- 'E14TG2aR'
data <- readMSnSet2(paste0('../data/input_rowMaxMin/', name, '/', name, '.csv'), ecol = 2:9)
calcnbquadloss(data, name)



name <- 'itzhak2017'
data <- readMSnSet2(paste0('../data/input_rowMaxMin/', name, '/', name, '.csv'), ecol = 2:31)
calcnbquadloss(data, name)



name <- 'hirst2018'
data <- readMSnSet2(paste0('../data/input_rowMaxMin/', name, '/', name, '.csv'), ecol = 2:16)
calcnbquadloss(data, name)



name <- 'LOPITDC2019'
data <- readMSnSet2(paste0('../data/input_rowMaxMin/', name, '/', name, '.csv'), ecol = 2:31)
calcnbquadloss(data, name)


name <- 'orre2019'
data <- readMSnSet2(paste0('../data/input_rowMaxMin/', name, '/', name, '.csv'), ecol = 2:10)
calcnbquadloss(data, name)


name <- 'mulvey2021'
data <- readMSnSet2(paste0('../data/input_rowMaxMin/', name, '/', name, '.csv'), ecol = 2:61)
calcnbquadloss(data, name)



##########################Plot################################
datanames <- c('nikolovski2014', 'E14TG2aR', 'itzhak2017', 'hirst2018',
               'LOPITDC2019', 'orre2019', 'mulvey2021')
methodnames <- c('KNN', 'SVM', 'NNET','RF', 
                 'NB', 'MAP', 'MCMC', 'DeepSP')

totalresult <- read.csv('../output/Classification_rowMaxMin_testresult.csv', 
                        check.names = F)
totalresult$Data <-factor(totalresult$Data, 
                          levels = datanames)

totalresult$Method <- factor(totalresult$Method,
                             levels = methodnames)



gF <-  ggplot(totalresult, aes(x = Method, y = `F1-score`,fill=Method)) + 
  geom_boxplot(alpha = 0.8, outlier.size=0.5) + 
  stat_boxplot(geom="errorbar") +
  facet_grid(.~Data, scales = "free_y") +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(plot.title = element_text(color="black", size=14),
        axis.title.x = element_text(color="black", size=12),
        axis.title.y = element_text(color="black", size=12))+
  theme(axis.text.x = element_text(color="black",size=10),
        axis.text.y = element_text( color="black",size=10)) +
  theme(axis.text.x = element_blank())+
  theme(legend.position = "bottom", 
        legend.direction = "horizontal",
        legend.title = element_blank()) + 
  theme(legend.position="none") + labs(x = NULL)



gP <- ggplot(totalresult, aes(x = Method, y = Precision,fill=Method)) + 
  geom_boxplot(alpha = 0.8, outlier.size=0.5) + 
  stat_boxplot(geom="errorbar") +
  facet_grid(.~Data, scales = "free_y") +
  # scale_fill_npg() +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(plot.title = element_text(color="black", size=14),
        axis.title.x = element_text(color="black", size=12),
        axis.title.y = element_text(color="black", size=12))+
  theme(axis.text.x = element_text(color="black",size=10),
        axis.text.y = element_text( color="black",size=10)) +
  theme(axis.text.x = element_blank())+
  theme(legend.position = "bottom", 
        legend.direction = "horizontal",
        legend.title = element_blank()) + 
  theme(legend.position="none") + labs(x = NULL)



gR <- ggplot(totalresult, aes(x = Method, y = Recall,fill=Method)) + 
  geom_boxplot(alpha = 0.8, outlier.size=0.5) + 
  stat_boxplot(geom="errorbar") +
  facet_grid(.~Data, scales = "free_y") +
  # scale_fill_npg() +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(plot.title = element_text(color="black", size=14),
        axis.title.x = element_text(color="black", size=12),
        axis.title.y = element_text(color="black", size=12))+
  theme(axis.text.x = element_text(color="black",size=10),
        axis.text.y = element_text( color="black",size=10)) +
  theme(axis.text.x = element_blank())+
  theme(legend.position = "bottom", 
        legend.direction = "horizontal",
        legend.title = element_blank())  + 
  theme(legend.position="none") + labs(x = NULL)




gA <- ggplot(totalresult, aes(x = Method, y = Accuracy,fill=Method)) + 
  geom_boxplot(alpha = 0.8, outlier.size=0.5) +  
  stat_boxplot(geom="errorbar") +
  facet_grid(.~Data, scales = "free_y") +
  # labs(x = "Method" , y = "Accuracy")+
  # scale_fill_npg() +
  theme_bw() + theme(panel.grid=element_blank()) +
  theme(plot.title = element_text(color="black", size=14),
        axis.title.x = element_text(color="black", size=12),
        axis.title.y = element_text(color="black", size=12))+
  theme(axis.text.x = element_text(color="black",size=10),
        axis.text.y = element_text( color="black",size=10)) +
  theme(axis.text.x = element_blank())+
  theme(legend.position = "bottom", 
        legend.direction = "horizontal",
        legend.title = element_blank())  + 
  theme(legend.position="none") + labs(x = NULL)





gQ <- ggplot(totalresult, aes(x = Method, y = Quadloss,fill=Method)) + 
  geom_boxplot(alpha = 0.8, outlier.size=0.5) +  
  stat_boxplot(geom="errorbar") +
  facet_grid(.~Data, scales = "free_y") +
  labs(x = "Method" , y = "Quadloss")+
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
        legend.title = element_blank()) + 
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))





library(patchwork)

g <- gF/gA/gP/gR/ gR / gQ

g

ggsave(g, filename = '../figure/Classification_rowMaxMin.pdf',
       height =15, width = 12)


ggsave(g, filename = '../figure/Classification_rowMaxMin.jpg',
       height =15, width = 12, dpi = 300)



