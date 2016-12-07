library(tidyverse)
library(forcats)
library(stringr)
library(data.table)
library(tikzDevice)

## ---- loadData
colSSVec <- c(1,2,36:55) # top 20 subspaces as vectors
colTop5SS <- c(1,2,16:20)   # top 5 subspaces as characters

load("results/Param_100_050_020_SSVec/experimentResult_ann_161129.RData")
annSS <-combinedResult[,colSSVec, with=FALSE]
annSSU <-annSS[!duplicated(annSS$algorithm)]
ann5SS <-combinedResult[!duplicated(combinedResult[,colTop5SS, with=FALSE]),colTop5SS, with=FALSE]
tmp <- combinedResult 

# load("results/Param_100_050_020_SSVec/experimentResult_ann_161129.RData")
# breastSS <-combinedResult[,colSSVec, with=FALSE]
# breastU <-breastSS[!duplicated(breastSS$algorithm)]
# breast5SS <-combinedResult[!duplicated(combinedResult[,colTop5SS, with=FALSE]),colTop5SS, with=FALSE]
# tmp <- rbind(tmp, combinedResult)

load("results/Param_100_050_020_SSVec/experimentResult_diabetes_161206.RData")
diabSS <-combinedResult[,colSSVec, with=FALSE]
diabSSU <-diabSS[!duplicated(diabSS$algorithm)]
diab5SS <-combinedResult[!duplicated(combinedResult[,colTop5SS, with=FALSE]),colTop5SS, with=FALSE]
tmp <- rbind(tmp, combinedResult)

load("results/Param_100_050_020_SSVec/experimentResult_glass_161129.RData")
glassSS <-combinedResult[,colSSVec, with=FALSE]
glassSSU <-glassSS[!duplicated(glassSS$algorithm)]
glass5SS <-combinedResult[!duplicated(combinedResult[,colTop5SS, with=FALSE]),colTop5SS, with=FALSE]
tmp <- rbind(tmp, combinedResult)

load("results/Param_100_050_020_SSVec/experimentResult_ion_161129.RData")
ionSS <-combinedResult[,colSSVec, with=FALSE]
ionSSU <-ionSS[!duplicated(ionSS$algorithm)]
ion5SS <-combinedResult[!duplicated(combinedResult[,colTop5SS, with=FALSE]),colTop5SS, with=FALSE]
tmp <- rbind(tmp, combinedResult)

load("results/Param_100_050_020_SSVec/experimentResult_lymph_161206.RData")
lympSS <-combinedResult[,colSSVec, with=FALSE]
lympSSU <-lympSS[!duplicated(lympSS$algorithm)]
lymp5SS <-combinedResult[!duplicated(combinedResult[,colTop5SS, with=FALSE]),colTop5SS, with=FALSE]
tmp <- rbind(tmp, combinedResult)

load("results/Param_100_050_020_SSVec/experimentResult_pen_161206.RData")
penSS <-combinedResult[,colSSVec, with=FALSE]
penSSU <-penSS[!duplicated(penSS$algorithm)]
pen5SS <-combinedResult[!duplicated(combinedResult[,colTop5SS, with=FALSE]),colTop5SS, with=FALSE]
tmp <- rbind(tmp, combinedResult)

load("results/Param_100_050_020_SSVec/experimentResult_segmentn_161206.RData")
segmSS <-combinedResult[,colSSVec, with=FALSE]
segmSSU <-segmSS[!duplicated(segmSS$algorithm)]
segm5SS <-combinedResult[!duplicated(combinedResult[,colTop5SS, with=FALSE]),colTop5SS, with=FALSE]
combinedResult <- rbind(tmp, combinedResult)

## ---- end



## ---- auc
  
  combinedResult %>%   
  select(dataset, algorithm, AUC_sum) %>%
  
  group_by(dataset, algorithm) %>%
  slice(which.max(AUC_sum)) %>%
  group_by(dataset, algorithm) %>%
  summarize(mean.AUC = mean(AUC_sum), sd.AUC = sd(AUC_sum)) %>%
  select(dataset, algorithm, mean.AUC, sd.AUC) %>%
  
  ggplot(aes(x = factor(dataset), y = mean.AUC, fill = algorithm)) +
  geom_bar(position = position_dodge(), stat="identity", width = 0.7) +
  geom_errorbar(aes(ymin=mean.AUC - sd.AUC, ymax=mean.AUC + sd.AUC), width=.2, position = position_dodge(.7)) +
  xlab(label = "Dataset") +
  ylab(label = "AUC") +
  scale_fill_grey(start = 0.1, end = 0.7)

## ---- end
  
## ---- runtime
  
  combinedResult %>%   
    select(dataset, algorithm, durationLOF, duationSS) %>%
    
    group_by(dataset, algorithm) %>%
    slice(which.max(sum(duationSS, durationLOF))) %>%
    group_by(dataset, algorithm) %>%
    
    mutate(runtime = sum(duationSS, durationLOF)) %>%
    
    ggplot(aes(x = factor(dataset), y = runtime, fill = algorithm)) +
    geom_bar(position = position_dodge(), stat="identity", width = 0.7) +
    xlab(label = "Dataset") +
    ylab(label = "Runtime in sec.") +
    scale_fill_brewer(palette = "Set2") 
## ---- end
  
  
  
  
  
  
## ---- meanSizeTop20SS 
  ssv <- rbind(annSSU, diabSSU, glassSSU, ionSSU, lympSSU, penSSU, segmSSU)
  
  # gather first 
  long_DF <- ssv %>% gather(name, subsp, subsp1V:subsp20V)
  
  
  long_DF <- long_DF[order(long_DF$algorithm),]
  
  # subspace length
  lengthSS <- c(length(long_DF$subsp[[1]]))
  #   .. add length as new column "length"
  for (i in 2:length(long_DF$subsp)){
    lengthSS <- c(lengthSS, length(long_DF$subsp[[i]]))
  }
  subspaces <- mutate(long_DF, length = lengthSS) 
  
  # mean per algorithm per dataset
  algdsDistinct <- subspaces %>% select(algorithm, dataset) %>% distinct
  
  dfMean <- data.frame()
  for (i in 1:nrow(algdsDistinct)){
    
    tmpdf <- filter(subspaces, subspaces$algorithm == algdsDistinct$algorithm[i] & subspaces$dataset == algdsDistinct$dataset[i])             %>% select(length)
    
    tmpMean <- mean(tmpdf$length)
    # print(tmpMean)
    v <- data.frame(dataset = algdsDistinct$dataset[i], algorithm = algdsDistinct$algorithm[i], mean = tmpMean)
    dfMean  <- rbind(dfMean, v)
    
  }
  
  
  ggplot(dfMean, aes(factor(dataset), mean, fill=algorithm, x=dataset, y=mean, gg)) +
    geom_bar(stat="identity", position = "dodge") + 
    scale_fill_brewer(palette = "Set2") +
    labs(title ="Average size of Top20-Subspaces", y = "Mean size") 
## ---- end
  
  
  
## ---- meanSizeTop5SS
  
  top5Length <- filter(subspaces, subspaces$name == "subsp1V" | subspaces$name == "subsp2V" | subspaces$name == "subsp3V"
                       | subspaces$name == "subsp4V" | subspaces$name == "subsp5V") 
  
  dfMean5 <- data.frame()
  for (i in 1:nrow(algdsDistinct)){
    
    tmpdf5 <- filter(top5Length, top5Length$algorithm == algdsDistinct$algorithm[i] & top5Length$dataset == algdsDistinct$dataset[i])             %>% select(length)
    
    tmpMean5 <- mean(tmpdf5$length)
    # print(tmpMean)
    v <- data.frame(dataset = algdsDistinct$dataset[i], algorithm = algdsDistinct$algorithm[i], mean = tmpMean5)
    dfMean5  <- rbind(dfMean5, v)
  }
  
  ggplot(dfMean5, aes(factor(dataset), mean, fill=algorithm, x=dataset, y=mean, gg)) +
    geom_bar(stat="identity", position = "dodge") + 
    scale_fill_brewer(palette = "Set2") +
    ggtitle("Average size of Top5-Subspaces")
  
## ---- end
  
## ---- sampleSS 
  colNam <- c("Algorithm","Dataset","Subspace 1","Subspace 2","Subspace 3","Subspace 4","Subspace 5")
  
  kable(glass5SS, caption = "Top-5-Subspaces - Glass-Dataset", col.names = colNam)
  kable(ion5SS, caption = "Top-5-Subspaces - Ion-Dataset", col.names = colNam)
  kable(pen5SS, caption = "Top-5-Subspaces - Pendigits-Dataset", col.names = colNam)
## ---- end
  
  
  
  
## ---- meanSizealg
    meanAlg <- data.frame()
  SSCMI <- filter(subspaces, subspaces$algorithm == "CMI")
  meanCMI <- mean(SSCMI$length)
  nL <- data.frame(algorithm = "CMI", mean = meanCMI)
  meanAlg <- rbind(meanAlg, nL)
  
  SSHics <- filter(subspaces, subspaces$algorithm == "HiCS")
  meanHics <- mean(SSHics$length)
  nL <- data.frame(algorithm = "HiCS", mean = meanHics)
  meanAlg <- rbind(meanAlg, nL)
  
  SSGMD <- filter(subspaces, subspaces$algorithm == "GMD")
  meanGMD <- mean(SSGMD$length)
  nL <- data.frame(algorithm = "GMD", mean = meanGMD)
  meanAlg <- rbind(meanAlg, nL)
  
  ggplot(meanAlg, mapping = aes(factor(algorithm), mean, x=algorithm, y=mean, gg)) +
    geom_bar(stat="identity", position = "dodge") + 
    scale_color_brewer(palette = "Set2") +
    labs(title ="Average size of Top20-Subspaces per algorithm", y = "Mean size") 
## ---- end
  