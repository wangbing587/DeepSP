

trainresult <- read.csv('../LOPITDC/DeepSP/trainresult.csv')


table(trainresult$markers, trainresult$pred)
