##' Calculate Cross-validation Metrics using TAGM-MAP


tagmMapCval <- function(object,
                         times = 10,
                         test.size = 0.2
                         ){
  
  
  marker.data <- markerMSnSet(object)
  X <- pRoloc:::subsetAsDataFrame(marker.data, "markers", train = TRUE)
  K <- length(getMarkerClasses(object))
  
  .testPartitions <- .cmMatrices <- vector("list", length = times)
  f1score <- matrix(0, times)
  .f1Matrices <- matrix(0, times)
  cmlist <- vector("list", length = times)
  quadloss <- vector("list", length = times)
  
  
  for (i in seq_along(cmlist)) {
    
    .size <- ceiling(table(fData(marker.data)$markers) * test.size) ## get sizes
    .size <- .size[unique(fData(marker.data)$markers)] ## strata needs size to be ordered as they appear in data
    
    test.idx <- sampling::strata(X, "markers", size = .size, method = "srswor")$ID_unit ## get strata indexes
    
    .test1   <- MSnSet(exprs(marker.data)[test.idx, ],
                       fData(marker.data[test.idx, ]), pData(marker.data)) ## 'unseen' test set
    .train1  <- MSnSet(exprs(marker.data)[-test.idx, ],
                       fData(marker.data[-test.idx, ]), pData(marker.data)) ## 'seen' training set
    
    test.markers <- fData(.test1)$markers ## save true marker labels
    mydata <- MSnbase::combine(.test1, .train1) ## create new combined MSnset
    levels(fData(mydata)$markers) <- c(levels(fData(mydata)$markers), "unknown")
    fData(mydata)[rownames(.test1), "markers"] <- "unknown" ## hide marker labels
    
    params <- tagmMapTrain(object = mydata, numIter = 100) #optimsiation
    
    if (abs(params@posteriors$logposterior[100]-params@posteriors$logposterior[99])>0.5){
      print("EM algorithm has not converged")
    }
    
    res <- tagmMapPredict(object = mydata, params = params, probJoint = TRUE)
    
    data <- factor(fData(res)$tagm.map.allocation[seq_len(nrow(.test1))], levels = getMarkerClasses(mydata))
    reference <- factor(test.markers, levels = getMarkerClasses(mydata))
    
    cmlist[[i]] <- conf <- caret::confusionMatrix(data = data, reference = reference)$table
    
    allocmatrix <- matrix(0, length(test.idx), length(getMarkerClasses(mydata)))
    
    #create allocation matrix
    for (j in seq_along(test.idx)) {
      allocmatrix[j, as.numeric(factor(test.markers), seq(1,length(unique(test.markers))))[j]] <- 1
    }
    quadloss[[i]] <- sum((allocmatrix - fData(res)$tagm.map.joint[rownames(.test1),])^2) #compute quad loss
    
  }
  
  return(list(cmlist = cmlist, quadloss = quadloss))
}

#' Calculate Cross-validation Metrics 
#' 
#' 
#' 
#' 
#'@params
#'
#'
#'@return
#'
#'
#'

tagmMcmcCval <- function(test.idx,
                        object,
                        fcol = "markers",
                        numIter = 1000L,
                        burnin = 100L,
                        thin = 5L,
                        mu0 = NULL,
                        lambda0 = 0.01,
                        nu0 = NULL,
                        S0 = NULL,
                        beta0 = NULL,
                        u = 2,
                        v = 10
){
  
  suppressPackageStartupMessages({
    require(pRoloc)
    require(MSnbase)
  })
  
  
  marker.data <- markerMSnSet(object)
  X <- pRoloc:::subsetAsDataFrame(marker.data, "markers", train = TRUE)
  
  .test1   <- MSnSet(exprs(marker.data)[test.idx, ], fData(marker.data[test.idx, ]), pData(marker.data)) ## 'unseen' test set
  .train1  <- MSnSet(exprs(marker.data)[-test.idx, ], fData(marker.data[-test.idx, ]), pData(marker.data)) ## 'seen' training set
  
  test.markers <- fData(.test1)$markers #save true marker labels
  mydata <- MSnbase::combine(.test1, .train1) #create new combined MSnset
  levels(fData(mydata)$markers) <- c(levels(fData(mydata)$markers), "unknown")
  fData(mydata)[rownames(.test1), "markers"] <- "unknown" #hide marker labels
  
  params <- tagmMcmcTrain(object = mydata, numIter = numIter, burnin = burnin, thin = thin,
                          mu0 = mu0,
                          lambda0 = lambda0,
                          nu0 = nu0,
                          S0 = S0,
                          beta0 = beta0,
                          u = u,
                          v = v) ## optimsiation
  
  N <- nrow(X[test.idx, ])
  K <- length(getMarkerClasses(mydata))
  
  params <- tagmMcmcProcess(params)
  mydata <- tagmMcmcPredict(object = mydata, params = params, probJoint = TRUE)
  
  data <- factor(fData(mydata[rownames(.test1),])$tagm.mcmc.allocation, levels = getMarkerClasses(mydata))
  reference <- factor(test.markers, levels = getMarkerClasses(mydata))
  
  conf <- caret::confusionMatrix(data = data, reference = reference)$table
  
  quad <- vector("numeric", length = length(test.idx))
  allocmatrix <- matrix(0, length(test.idx), K)
  
  for (j in seq_along(test.idx)) {
    allocmatrix[j, as.numeric(factor(test.markers), seq(1, K))[j]] <- 1 
  }
  quadloss <- sum(rowSums((allocmatrix - fData(mydata[rownames(.test1),])$tagm.mcmc.joint)^2))
  
  return(list(conf = conf, quadloss = quadloss))
} 


#' Parallel function for running cross-validation MCMC 
#' 
#' 
#' 
#' 
#'@params
#'
#'
#'@return
#'
#'
#'

tagmMcmcCvalpar <- function(object,
                              times = 100,
                              test.size = 0.2,
                              numIter = 1000L,
                              burnin = 100L,
                              thin = 5L,
                              mu0 = NULL,
                              lambda0 = 0.01,
                              nu0 = NULL,
                              S0 = NULL,
                              beta0 = NULL,
                              u = 2,
                              v = 10,
                              BPPARAM = bpparam()
){
  
  
  marker.data <- markerMSnSet(object)
  X <- pRoloc:::subsetAsDataFrame(marker.data, "markers", train = TRUE)
  K <- length(getMarkerClasses(object))
  
  .testPartitions <- .cmMatrices <- vector("list", length = times)
  f1score <- matrix(0, times)
  .f1Matrices <- matrix(0, times)
  cmlist <- vector("list", length = times)
  quadloss <- vector("list", length = times) 
  .size <- ceiling(table(fData(marker.data)$markers) * test.size) #get sizes
  .size <- .size[unique(fData(marker.data)$markers)] #strata needs size to be ordered as they appear in data
  test.idx <- vector("list", length = times)
  
  
  for(i in seq_along(cmlist)){
    test.idx[[i]] <- sampling::strata(X, "markers", size = .size, method = "srswor")$ID_unit #get strata indexes
  } 
  
  .res <- bplapply(test.idx, tagmMcmcCval,
                   object = object,
                   fcol = "markers",
                   numIter = numIter,
                   burnin = burnin,
                   thin = thin,
                   mu0 = mu0,
                   lambda0 = lambda0,
                   nu0 = nu0 ,
                   S0 = S0,
                   beta0 = beta0,
                   u = u,
                   v = v,
                   BPPARAM = BPPARAM
  )
  
  for(i in seq_along(cmlist)){
    cmlist[[i]] <- .res[[i]]$conf
    quadloss[[i]] <- .res[[i]]$quadloss
  }
  
  
  return(list(cmlist = cmlist, quadloss = quadloss))
}   





