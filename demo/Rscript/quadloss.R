library(pRoloc)



knnquadloss <- function(knnparams, object){
  
  knnquadloss <- vector("list", length = 100) #quadratic loss to save
  
  for(iLoss in seq_along(knnquadloss)){
    
    markerhl <- markerMSnSet(object)
    
    index <- knnparams@testPartitions[[iLoss]]
    trueclasses <- fData(markerhl)$markers[index]
    
    levels(fData(markerhl)$markers) <- c(getMarkerClasses(object), "unknown") #change levels
    fData(markerhl)$markers[index] <- "unknown" #hide markers
    
    nbr <- knnparams@results[iLoss,2]
    train <- exprs(markerhl)[-index,]
    test <- exprs(markerhl)[index,]
    
    K <- length(getMarkerClasses(object))
    
    res <- FNN::knn(train, test, cl = fData(markerhl)$markers[-index], k = nbr, prob = TRUE) #knn classification
    nn.index <- attr(res, "nn.index")
    distMat <- attr(res, "nn.dist") #get atrributes
    
    scoresmatrix <- matrix(0, length(index), length(getMarkerClasses(object)))
    colnames(scoresmatrix) <- getMarkerClasses(object) #change names
    
    irates <- tabulate(fData(markerhl)$markers[-index])/sum(tabulate(fData(markerhl)$markers[-index]))
    
    
    for(i in seq_along(index)){
      scoresmatrix[i, ] <- (0.5 * irates * K)/(nbr + 0.5 * K)
      
      for(j in seq(1:K)[table(fData(markerhl)$markers[-index][nn.index[i,]])!=0]){
        count <- as.numeric(table(fData(markerhl)$markers[-index][nn.index[i,]])[j])
        scoresmatrix[i,j] <-  (count + 0.5 * irates[j] * K)/(nbr + 0.5 * K)
        
      }
    }
    
    scoresmatrix <- scoresmatrix/rowSums(scoresmatrix)
    
    allocmatrix <- matrix(0, length(index), length(unique(trueclasses)))
    
    #create allocation matrix
    for(j in seq_along(index)){
      allocmatrix[j, as.numeric(factor(trueclasses), seq(1,length(unique(trueclasses))))[j]] <- 1 
    }
    
    knnquadloss[[iLoss]] <- sum((scoresmatrix[,getMarkerClasses(markerhl)] - allocmatrix)^2) / length(index) #compute quad loss
  }
  
  return(knnquadloss = knnquadloss)
}




svmquadloss <- function(svmparams, object){
  
  svmquadloss <- vector("list", length = 100) #quadratic loss to save
  
  for(i in seq_along(svmparams@f1Matrices)){
    
    index <- svmparams@testPartitions[[i]] #gettestpartitions
    
    
    markerhl <- markerMSnSet(object) #create markerset
    trueclasses <- fData(markerhl)$markers[index] #save true markers
    
    levels(fData(markerhl)$markers) <- c(getMarkerClasses(object), "unknown") #change levels
    fData(markerhl)$markers[index] <- "unknown" #hide markers
    
    sigma <- as.numeric(svmparams@results[i, 2]) #get sigma
    cost <- as.numeric(svmparams@results[i, 3]) #getcost
    
    res <- svmClassification(markerhl, fcol= "markers", sigma = sigma, cost = cost, scores = "all" ) #svm classification
    scoresmatrix <- fData(res)[index, "svm.all.scores"] #get scores as matrix
    colnames(scoresmatrix) <- unique(trueclasses) #change names
    
    allocmatrix <- matrix(0, length(index), length(unique(trueclasses)))
    
    #create allocation matrix
    for(j in seq_along(index)){
      allocmatrix[j, as.numeric(factor(trueclasses), seq(1,length(unique(trueclasses))))[j]] <- 1 
    }
    
    svmquadloss[[i]] <- sum((scoresmatrix[,getMarkerClasses(markerhl)] - allocmatrix)^2) / length(index) #compute quad loss
  }
  
  
  return(svmquadloss = svmquadloss)
}



nnetquadloss <- function(nnetparams, object){
  
  nnetquadloss <- vector("list", length = 10) #quadratic loss to save
  
  for(i in seq_along(nnetparams@f1Matrices)){
    index <- nnetparams@testPartitions[[i]] #gettestpartitions
    
    
    markerhl <- markerMSnSet(object) #create markerset
    trueclasses <- fData(markerhl)$markers[index] #save true markers
    
    levels(fData(markerhl)$markers) <- c(getMarkerClasses(object), "unknown") #change levels
    fData(markerhl)$markers[index] <- "unknown" #hide markers
    decay <- as.numeric(nnetparams@results[i, 2]) #get sigma
    size <- as.numeric(nnetparams@results[i, 3]) #getcost
    
    res <- nnetClassification(markerhl, fcol= "markers",
                              decay = decay, size= size,
                              scores = "all", verbose = FALSE) #nnet classification
    scoresmatrix <- fData(res)[index, "nnet.all.scores"] #get scores as matrix
    colnames(scoresmatrix) <- unique(trueclasses) #change names
    
    allocmatrix <- matrix(0, length(index), length(unique(trueclasses)))
    
    #create allocation matrix
    for(j in seq_along(index)){
      allocmatrix[j, as.numeric(factor(trueclasses), seq(1,length(unique(trueclasses))))[j]] <- 1 
    }
    
    nnetquadloss[[i]] <- sum((scoresmatrix[,getMarkerClasses(markerhl)] - allocmatrix)^2) / length(index) #compute quad loss
  }
  
  return(nnetquadloss = nnetquadloss)
}



rfquadloss <- function(rfparams, object){
  
  rfquadloss <- vector("list", length = 10) #quadratic loss to save

  for(i in seq_along(rfparams@f1Matrices)){
    index <- rfparams@testPartitions[[i]] #gettestpartitions
    
    
    markerhl <- markerMSnSet(object) #create markerset
    trueclasses <- fData(markerhl)$markers[index] #save true markers
    
    levels(fData(markerhl)$markers) <- c(getMarkerClasses(object), "unknown") #change levels
    fData(markerhl)$markers[index] <- "unknown" #hide markers
    mtry <- as.numeric(rfparams@results[i, 2]) 

    res <- rfClassification(markerhl, fcol= "markers",
                            mtry = mtry, 
                            scores = "all", verbose = FALSE) #nnet classification
    scoresmatrix <- fData(res)[index, "rf.all.scores"] #get scores as matrix
    colnames(scoresmatrix) <- unique(trueclasses) #change names
    
    allocmatrix <- matrix(0, length(index), length(unique(trueclasses)))
    
    #create allocation matrix
    for(j in seq_along(index)){
      allocmatrix[j, as.numeric(factor(trueclasses), seq(1,length(unique(trueclasses))))[j]] <- 1 
    }
    
    rfquadloss[[i]] <- sum((scoresmatrix[,getMarkerClasses(markerhl)] - allocmatrix)^2) / length(index) #compute quad loss
  }
  
  return(rfquadloss = rfquadloss)
}




# nbquadloss <- function(nbparams, object){
#   
#   nbquadloss <- vector("list", length = 100) #quadratic loss to save
#   
#   for(i in seq_along(nbparams@f1Matrices)){
#     i = 2
#     index <- nbparams@testPartitions[[i]] #gettestpartitions
#     
#     markerhl <- markerMSnSet(object) #create markerset
#     trueclasses <- fData(markerhl)$markers[index] #save true markers
#     
#     levels(fData(markerhl)$markers) <- c(getMarkerClasses(object), "unknown") #change levels
#     fData(markerhl)$markers[index] <- "unknown" #hide markers
#     laplace <- as.numeric(nbparams@results[i, 2])
#     res <- nbClassification(markerhl, fcol= "markers", laplace = laplace, scores = "pred") #nb classification
#     scoresmatrix <- fData(res)[index, "all.scores"] #get scores as matrix
#     colnames(scoresmatrix) <- unique(trueclasses) #change names
#     
#     allocmatrix <- matrix(0, length(index), length(unique(trueclasses)))
#     
#     #create allocation matrix
#     for(j in seq_along(index)){
#       allocmatrix[j, as.numeric(factor(trueclasses), seq(1,length(unique(trueclasses))))[j]] <- 1 
#     }
#     
#     nbquadloss[[i]] <- sum((scoresmatrix[,getMarkerClasses(markerhl)] - allocmatrix)^2) / length(index) #compute quad loss
#   }
#   
#   return(nbquadloss = nbquadloss)
# }


