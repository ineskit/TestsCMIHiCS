library(subcon)
library(data.table)
library(foreign)
library(doSNOW)
library(dplyr)

Log <- function(text, ...) {
  msg <- sprintf(paste0(as.character(Sys.time()), ": ", text, "\n"), ...)
  cat(msg)
}

importData <- function(inputPath, fileName){
  if(grepl(".csv", fileName)){
    dataset_labeled <- fread(paste0(inputPath, "/", fileName))
  }else{
    dataset_labeled <- as.data.table(read.arff(paste0(inputPath,"/", fileName)))
  }
  if("id" %in% names(dataset_labeled)){
    dataset_labeled <- dataset_labeled[,!"id", with=F]
  }
  if("outlier" %in% names(dataset_labeled)) {
    names(dataset_labeled)[which(names(dataset_labeled)=="outlier")] <- "class"
  }
  if(!("class" %in% names(dataset_labeled))) {
    names(dataset_labeled)[ncol(dataset_labeled)] <- "class"
  }
  if("yes" %in% levels(dataset_labeled$class)){
    dataset_labeled$class <- as.numeric(dataset_labeled$class == "yes")
  }else{
    if(is.factor(dataset_labeled$class)){
      dataset_labeled$class <- as.numeric(levels(dataset_labeled$class))[dataset_labeled$class]
    }
    dataset_labeled[,class:=ifelse(class>0, 1, 0),]
  }
  dataset_labeled
}

runLOF <- function(subspaces, data, minPts){
  lapply(subspaces, function(x) {
    tmp <- LOF(data[,unlist(x), with=F], k = minPts)
    tmp[!is.finite(tmp)] <- 1
    return(tmp)
  })
}

applyLOF <- function(outputSpaces, data, label, maxMinPts, input, algorithm){
  tmpResult <- foreach(k = seq(10, min(nrow(data) - 1, maxMinPts), by= 10)) %do% {
    Log(paste0("input: ", input, " algorithm: ", algorithm, " k: ", k))
    # outlier detection
    uniqueOutputSpaces <- unique(lapply(outputSpaces, sort)) # remove exact duplicate subspaces
    lofactors <- runLOF(subspaces = uniqueOutputSpaces, data = data, minPts = k)

    # summarize the number of subspaces that hold the max score for an object
    numberMaxScoreSubspaces <- data.table(matrix(unlist(lofactors), ncol=length(label), byrow = T)) %>%
      summarize_all(which.max) %>%
      as.numeric %>%
      unique %>%
      length

    finalScoreMax <- Reduce(maxCombination, lofactors)
    finalScoreSum <- Reduce(sumCombination, lapply(lofactors, identity))

    Rprecision_adj_sum <- precisionAtN(label, finalScoreSum, n = sum(label), adjusted = T)
    Rprecision_adj_max <- precisionAtN(label, finalScoreMax, n = sum(label), adjusted = T)

    auc_sum <- redundancyAUC(scores = lofactors, label = label, combinationFun = sumCombination, scaleFun = identity)
    auc_max <- redundancyAUC(scores = lofactors, label = label, combinationFun = maxCombination, scaleFun = identity)

    data.table("AUC_sum" = auc_sum$initialAUC,
               "Rprecision_adj_sum" = Rprecision_adj_sum,
               "numberRemainingSpaces_sum" = auc_sum$numberRemainingSpaces,
               "AUC_removedRedundant_sum" = auc_sum$maximumAUC,

               "AUC_max" = auc_max$initialAUC,
               "Rprecision_adj_max" = Rprecision_adj_max,
               "numberRemainingSpaces_max" = auc_max$numberRemainingSpaces,
               "AUC_removedRedundant_max" = auc_max$maximumAUC,

               "totalNumberSubspaces" = length(uniqueOutputSpaces),
               "numberMaxScoreSubspaces" = numberMaxScoreSubspaces,
               "minPts" = k)
  }

  data.table(Reduce(rbind, tmpResult))
}

subspaceSearch <- function(data, alpha, numRuns, algorithm, topkSearch, topkOutput){
  sampleSize <- 1000
  if(nrow(data)>sampleSize){
    sampledata <- data[sample(.N, sampleSize)]
    Log(paste0("Data set has ", nrow(data)," rows. Sampling down to ", sampleSize, " rows."))
  }
  else{
    sampledata <- data
  }
  indexMatrix <- sortedIndexMatrix(sampledata)
  numMatrix <- data.matrix(data)

  addTrivialSpaces <- function(subspaces, numDim){
    subspaces[[length(subspaces) + 1]] <- c(1:numDim) # add full space
    subspaces <- c(subspaces, 1:numDim) # add single dimensions
    subspaces
  }

  resList <- list()
  switch(algorithm,
         GMD = {
           resList$outputSpaces <- GMD(indexMap = indexMatrix, alpha, numRuns)
         },
         HiCS = {
           capture.output(hicsSearchResult <- HiCSSearch(indexMap = indexMatrix, alpha, numRuns, topkSearch = topkSearch, topkOutput = topkOutput), file = NULL)

           resList$outputSpaces <- hicsSearchResult$subspaces
           resList$contrast     <- hicsSearchResult$contrast
         },
         FS = {
           outputSpaces <- list(1:ncol(sampledata))
         },
         HiCSSO = {
           capture.output(hicsSearchResult <- HiCSSearch(indexMap = indexMatrix, alpha, numRuns, topkSearch = topkSearch, topkOutput = topkOutput), file = NULL)
           outputSpaces <- hicsSearchResult$subspaces
           outputSpaces <- addTrivialSpaces(outputSpaces, ncol(sampledata))
           contrast     <- hicsSearchResult$contrast
         },
         GMDSO = {
           outputSpaces <- GMD(indexMap = indexMatrix, alpha, numRuns)
           outputSpaces <- addTrivialSpaces(outputSpaces, ncol(sampledata))
         },
         CMI = {
           CMIResult = CMISearch(data = numMatrix, numCluster = 10, topkSearch = topkSearch, topkOutput = topkOutput)
           resList$outputSpaces <- CMIResult$subspaces
           resList$contrast     <- CMIResult$contrast
         }
  )
  Log(paste0(algorithm, ": found ", length(resList$outputSpaces), " subspaces."))
  return (resList)
}

runExperiments <- function(inputPath,
                           outputFolder="results",
                           outputFile="experimentResult.RData",
                           numCores = 1,
                           alpha = 0.1,
                           numRuns = 100,
                           maxMinPts = 100,
                           logFile="",
                           topkSearch = 500,
                           topkOutput = 100){

  # setup
  Log("starting experiments")
  inputs <- list.files(path=inputPath, recursive = T)
    # algorithms <- c("HiCS", "CMI", "GMD")
    algorithms <- c("HiCS")

  experiments <- expand.grid("algorithm" = algorithms, "input" = inputs, stringsAsFactors = FALSE)

  resultSet <- data.table()

  if(numCores > 1){
    Log(paste0("setting up parallel cluster with ", numCores, " cores..."))
    cluster <- makeCluster(numCores, type = "SOCK", outfile=logFile)
    registerDoSNOW(cluster)
  }else{
    Log(paste0("running in sequential mode..."))
    registerDoSEQ()
  }

  t0 <- proc.time()

  parallelResult <- foreach(i=1:nrow(experiments),
                            .packages=c("foreign", "data.table", "subcon", "foreach", "dplyr"),
                            .export=c("subspaceSearch", "applyLOF", "runLOF", "Log", "importData")) %do% {

                              experiment <- experiments[i,]

                              dataset_labeled <- importData(inputPath, experiment$input)
                              dt <- data.table(dataset_labeled)
                              label <- dt$class
                              dt <- dt[,-c("class"), with=F]

                              timer_start <- proc.time()
                              rL <- subspaceSearch(data = dt, alpha, numRuns, algorithm = experiment$algorithm, topkSearch, topkOutput)
                              timer_end <- proc.time()

                              timer_start_LOF <- proc.time()
                              result <- applyLOF(outputSpaces = rL$outputSpaces, data=dt, label=label, maxMinPts = maxMinPts, input = experiment$input, algorithm = experiment$algorithm)
                              timer_end_LOF <- proc.time()

                              topkSSV <- NULL
                              topkcontrasts <- NULL
                              for (i in 1: topkOutput) {
                                subspV_tmp <- rL$outputSpaces[i]
                                topkSSV <- cbind(topkSSV, subspV_tmp)
                                colnames(topkSSV)[i] <- c(paste0("subsp",i))
                                
                                contrV_tmp <- rL$contrast[i]
                                topkcontrasts <- cbind(topkcontrasts, contrV_tmp)
                                colnames(topkcontrasts)[i] <- c(paste0("contr",i))
                              }
                              topkSSV <- data.table(topkSSV)
                              topkcontrasts <- data.table(topkcontrasts)
                              

                              if (experiment$algorithm != "GMD"){
                                data.table(cbind(algorithm = experiment$algorithm, dataset = experiment$input, duationSS = (timer_end -timer_start)["elapsed"],
                                                 durationLOF = (timer_end_LOF - timer_start_LOF)["elapsed"], result, topkSSV, topkcontrasts))
                              }
                              else{
                                data.table(cbind(algorithm = experiment$algorithm, dataset = experiment$input, duationSS = (timer_end -timer_start)["elapsed"],
                                                 durationLOF = (timer_end_LOF - timer_start_LOF)["elapsed"], result, topkSSV, Highestcontrast ="0",
                                                 contrast2 ="0", contrast3 ="0", contrast4 ="0", contrast5 ="0"))
                              }
                              
                            }

  combinedResult <- data.table(Reduce(rbind, parallelResult))

  print(paste0("total duration experiment: ", (proc.time() - t0)["elapsed"]))
  # store result

  save(combinedResult, file=paste0(outputFolder,"/",outputFile))

 
  if(numCores > 1){
    stopCluster(cluster)
  }
  combinedResult
}

  # finalResult <- runExperiments(inputPath = "datasets", maxMinPts = 100, numCores=1, topkSearch = 500, topkOutput = 100)
  
