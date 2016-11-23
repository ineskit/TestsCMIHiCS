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

    # evaluation
    # auc_sum <- combinedScoreAUC(combinationFun = sumCombination, scores = lofactors, label = label)
    # auc_max <- combinedScoreAUC(combinationFun = maxCombination, scores = lofactors, label = label)

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
   algorithms <- c("HiCS", "CMI", "GMD")


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

                              # for (i in 1:5) {
                              #   subspX <- sapply(outputSpaces[i], function(x) paste0("[",paste(x, collapse = ","),"]"))
                              #   header<-paste0("Top_",i)
                              #   top5SS$header <- subspX
                              #   #top5SS <- data.table(header = subspX)
                              # }
                              subsp1 <- sapply(rL$outputSpaces[1], function(x) paste0("[",paste(x, collapse = ","),"]"))
                              subsp2 <- sapply(rL$outputSpaces[2], function(x) paste0("[",paste(x, collapse = ","),"]"))
                              subsp3 <- sapply(rL$outputSpaces[3], function(x) paste0("[",paste(x, collapse = ","),"]"))
                              subsp4 <- sapply(rL$outputSpaces[4], function(x) paste0("[",paste(x, collapse = ","),"]"))
                              subsp5 <- sapply(rL$outputSpaces[5], function(x) paste0("[",paste(x, collapse = ","),"]"))
                              subsp6 <- sapply(rL$outputSpaces[6], function(x) paste0("[",paste(x, collapse = ","),"]"))
                              subsp7 <- sapply(rL$outputSpaces[7], function(x) paste0("[",paste(x, collapse = ","),"]"))
                              subsp8 <- sapply(rL$outputSpaces[8], function(x) paste0("[",paste(x, collapse = ","),"]"))
                              subsp9 <- sapply(rL$outputSpaces[9], function(x) paste0("[",paste(x, collapse = ","),"]"))
                              subsp10 <- sapply(rL$outputSpaces[10], function(x) paste0("[",paste(x, collapse = ","),"]"))
                              subsp11 <- sapply(rL$outputSpaces[11], function(x) paste0("[",paste(x, collapse = ","),"]"))
                              subsp12 <- sapply(rL$outputSpaces[12], function(x) paste0("[",paste(x, collapse = ","),"]"))
                              subsp13 <- sapply(rL$outputSpaces[13], function(x) paste0("[",paste(x, collapse = ","),"]"))
                              subsp14 <- sapply(rL$outputSpaces[14], function(x) paste0("[",paste(x, collapse = ","),"]"))
                              subsp15 <- sapply(rL$outputSpaces[15], function(x) paste0("[",paste(x, collapse = ","),"]"))
                              subsp16 <- sapply(rL$outputSpaces[16], function(x) paste0("[",paste(x, collapse = ","),"]"))
                              subsp17 <- sapply(rL$outputSpaces[17], function(x) paste0("[",paste(x, collapse = ","),"]"))
                              subsp18 <- sapply(rL$outputSpaces[18], function(x) paste0("[",paste(x, collapse = ","),"]"))
                              subsp19 <- sapply(rL$outputSpaces[19], function(x) paste0("[",paste(x, collapse = ","),"]"))
                              subsp20 <- sapply(rL$outputSpaces[20], function(x) paste0("[",paste(x, collapse = ","),"]"))
                              top20SS <- data.table(subsp1, subsp2, subsp3, subsp4, subsp5, subsp6, subsp7, subsp8, subsp9, subsp10,
                                                   subsp11, subsp12, subsp13, subsp14, subsp15, subsp16, subsp17, subsp18, subsp19, subsp20)
                        
                              subsp1V <- data.table(rL$outputSpaces[1])
                              subsp2V <- data.table(rL$outputSpaces[2])
                              subsp3V <- data.table(rL$outputSpaces[3])
                              subsp4V <- data.table(rL$outputSpaces[4])
                              subsp5V <- data.table(rL$outputSpaces[5])
                              subsp6V <- data.table(rL$outputSpaces[6])
                              subsp7V <- data.table(rL$outputSpaces[7])
                              subsp8V <- data.table(rL$outputSpaces[8])
                              subsp9V <- data.table(rL$outputSpaces[9])
                              subsp10V <- data.table(rL$outputSpaces[10])
                              subsp11V <- data.table(rL$outputSpaces[11])
                              subsp12V <- data.table(rL$outputSpaces[12])
                              subsp13V <- data.table(rL$outputSpaces[13])
                              subsp14V <- data.table(rL$outputSpaces[14])
                              subsp15V <- data.table(rL$outputSpaces[15])
                              subsp16V <- data.table(rL$outputSpaces[16])
                              subsp17V <- data.table(rL$outputSpaces[17])
                              subsp18V <- data.table(rL$outputSpaces[18])
                              subsp19V <- data.table(rL$outputSpaces[19])
                              subsp20V <- data.table(rL$outputSpaces[20])
                              
                              top20SSV <- data.table(subsp1V, subsp2V, subsp3V, subsp4V, subsp5V, subsp6V, subsp7V, subsp8V, subsp9V, subsp10V,
                                                    subsp11V, subsp12V, subsp13V, subsp14V, subsp15V, subsp16V, subsp17V, subsp18V, subsp19V, subsp20V)
                              
                              if (experiment$algorithm != "GMD"){
                                data.table(cbind(algorithm = experiment$algorithm, dataset = experiment$input, duationSS = (timer_end -timer_start)["elapsed"],
                                                 durationLOF = (timer_end_LOF - timer_start_LOF)["elapsed"], result, top20SS, top20SSV, Highestcontrast =rL$contrast[1],
                                                 contrast2 =rL$contrast[2], contrast3 =rL$contrast[3], contrast4 =rL$contrast[4], contrast5 =rL$contrast[5]))
                              }
                              else{
                                data.table(cbind(algorithm = experiment$algorithm, dataset = experiment$input, duationSS = (timer_end -timer_start)["elapsed"],
                                                 durationLOF = (timer_end_LOF - timer_start_LOF)["elapsed"], result, top20SS, top20SSV, Highestcontrast ="0",
                                                 contrast2 ="0", contrast3 ="0", contrast4 ="0", contrast5 ="0"))
                              }
                              # top 5 subspaces:
                              #top5SS<-data.table(cbind(subspaces = outputSpaces[1:5]), contrast = contrastCMI)
                            }

  combinedResult <- data.table(Reduce(rbind, parallelResult))

  print(paste0("total duration experiment: ", (proc.time() - t0)["elapsed"]))
  # store result

  save(combinedResult, file=paste0(outputFolder,"/",outputFile))

  #outputFileEnh <- paste0(outputFile, "_enh")
  #save(top5SS, file=paste0(outputFolder,"/",outputFileEnh))
  # cleanup
  if(numCores > 1){
    stopCluster(cluster)
  }
  combinedResult
}

  # finalResult <- runExperiments(inputPath = "datasets", maxMinPts = 100, numCores=1, topkSearch = 500, topkOutput = 100)
  # finalResult <- runExperiments(inputPath = "datasets", maxMinPts = 100, numCores=1, topkSearch = 50, topkOutput = 20)

