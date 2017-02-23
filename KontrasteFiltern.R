library(dplyr)

load("D:/Uni/SS16/Hiwi/github/TestsCMIHiCS/results/experimentResult_Hics_100_100_100_20170222.RData")
tmp <- combinedResult

# madelonnorm = extra file:
load("D:/Uni/SS16/Hiwi/github/TestsCMIHiCS/results/experimentResult_madelon_Hics_100_100_100_20170222.RData")
combinedResult <- rbind(tmp, combinedResult)


# filter relevant columns & delete duplicates

top100SSHics <- combinedResult %>% select(dataset, subsp1:subsp100, contr1:contr100) %>% group_by(dataset) %>% filter(row_number() == 1)
ncol(top100SSHics) # 201 (inkl. Dataset)

Kontraste <- select(top100SSHics,... = c(102:201))
