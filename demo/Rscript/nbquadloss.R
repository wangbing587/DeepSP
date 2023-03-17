library(pRoloc)

nbClassification1 <- function(object,
                              assessRes,
                              scores = c("prediction", "all", "none"),
                              laplace,
                              fcol = "markers",
                              ...) {
  scores <- match.arg(scores)  
  if (missing(assessRes)) {
    if (missing(laplace))
      stop("First run 'nbOptimisation' or set 'laplace' manually.")
    params <- c("laplace" = laplace)
  } else {
    params <- getParams(assessRes)
    if (is.na(params["laplace"]))
      stop("No 'laplace' found.")    
  }
  trainInd <- which(fData(object)[, fcol] != "unknown")
  form <- as.formula(paste0(fcol, " ~ ."))
  ans <- MLearn(form, t(object), naiveBayesI,
                trainInd,
                laplave = params["laplace"],
                ...)
  fData(object)$naiveBayes <- predictions(ans)
  if (scores == "all") {
    scoreMat <- rbind(ans@trainScores, ans@testScores)
    colnames(scoreMat) <- paste0(colnames(scoreMat), ".naiveBayes.scores")
    fData(object)$naiveBayes.all.scores <- scoreMat
  } else if (scores == "prediction") {
    fData(object)$naiveBayes.scores <- predScore(ans)
  } ## else scores is "none" 
  object@processingData@processing <- c(processingData(object)@processing,
                                        paste0("Performed naiveBayes prediction (", 
                                               paste(paste(names(params), params, sep = "="),
                                                     collapse = " "), ") ",
                                               date()))
  if (validObject(object))
    return(object)
}




nbquadloss <- function(nbparams, object){
  
  nbquadloss <- vector("list", length = 100) #quadratic loss to save
  
  for(i in seq_along(nbparams@f1Matrices)){
    index <- nbparams@testPartitions[[i]] #gettestpartitions
    
    markerhl <- markerMSnSet(object) #create markerset
    trueclasses <- fData(markerhl)$markers[index] #save true markers
    
    levels(fData(markerhl)$markers) <- c(getMarkerClasses(object), "unknown") #change levels
    fData(markerhl)$markers[index] <- "unknown" #hide markers
    laplace <- as.numeric(nbparams@results[i, 2])
    res <- nbClassification1(markerhl, fcol= "markers", laplace = laplace, scores = "all") #nb classification
    scoresmatrix <- fData(res)[index, "naiveBayes.all.scores"] #get scores as matrix
    colnames(scoresmatrix) <- unique(trueclasses) #change names
    
    allocmatrix <- matrix(0, length(index), length(unique(trueclasses)))
    
    #create allocation matrix
    for(j in seq_along(index)){
      allocmatrix[j, as.numeric(factor(trueclasses), seq(1,length(unique(trueclasses))))[j]] <- 1 
    }
    
    nbquadloss[[i]] <- sum((scoresmatrix[,getMarkerClasses(markerhl)] - allocmatrix)^2) / length(index) #compute quad loss
  }
  
  return(nbquadloss = nbquadloss)
}