
# Comparing CMI, HiCS and GMD Subspace Search using real-world datasets

Algorithms for subspace search are implemented in R-subcon package, see [1]

## Structure of folders

#### datasets folder
- contains the datasets which are considered during the run

#### datasets_tmp_notUsed folder
- contains all datasets which have been considered, but are not relevant for current run

#### results folder
- contains the results of the tests, classified by folders with different parameters
-	Param_100_050_020 means: maxMinPts = 100, topkSearch = 50, topkOutput = 20


## Most important files

#### experimentCMIHiCSResults.Rmd 
-	R Markdown file that describes test results and how new experiments can be conducted
-	package knitr is needed to create reproducible reports with r-code chunks, see [2]


#### generatePlots.R
-	contains the code chunks that are included in the file experimentCMIHiCSResults.Rmd
-	mainly code to read and plot results

#### experimentCMIHiCSResults.html  
-	generated html-file from experimentCMIHiCSResults.Rmd

## Conducting new tests
1. Download the dataset into the *datasets* folder

2. Choose algorithms, if necessary apply changes in line:
```R
  algorithms <- c("HiCS", "CMI", "GMD")
```  
*Notes:*
-	please make sure that you've got 1 RData-resultfile per dataset
  If you don’t want to change the general behaviour of the markdown, run for every dataset a single experiment
-	there can be several algorithms within 1 RData-file

3. Source the *HICSvsCMI_Experiments.R* script
4. Start experiment by calling **runExperiments**-function, for example:
```R
  finalResult <- runExperiments(inputPath = "datasets", maxMinPts = 100, numCores=1, topkSearch = 500, topkOutput = 100)
```

5. file experimentResult.RData is generated with the results in folder “result” 
6. load your new results in the file  generatePlots.R under 
```R
## ---- loadData
load("results/Param_100_050_020_SSVec/experimentResult_ann_161129.RData")
```



##References
The package *R-Subcon* containing several subspace search algorithm [1]

The package *knitr* [2] 

[1]: https://github.com/holtri/Rsubcon
[2]: https://cran.r-project.org/web/packages/knitr/index.html

