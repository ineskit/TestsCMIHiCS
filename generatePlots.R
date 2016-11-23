library(tidyverse)
library(forcats)
library(stringr)
library(data.table)
library(tikzDevice)


## ---- plotTest

 load("results/Param_100_050_020/experimentResult_glass_161102_100_50_20.RData") # glass
 tmp <- combinedResult 
 load("results/Param_100_050_020/experimentResult_ann_diabet_ion_161102_100_50_20.RData") # ann, diabetes, ion
 tmp <- rbind(tmp, combinedResult)
 load("results/Param_100_050_020/experimentResult_pen_lymph_segm_161102_100_50_20.RData") # pen, lymph, segment
 tmp <- rbind(tmp, combinedResult)
 combinedResult <- tmp
 
 
 plotThemeReal <- theme_bw() +
   theme(plot.title = element_text(size=40, vjust = 2.5, face = "bold"),
         axis.title.x = element_text(margin = margin(15,0,0,0)),
         axis.title.y = element_text(margin = margin(0,15,0,0)),
         axis.text = element_text(margin = margin(0,0,0,0)),
         axis.ticks.y = element_blank(),
         axis.ticks.x = element_blank(),
         plot.margin = unit(c(0, 0, 0.3, 0), "lines"),
         panel.border = element_blank(),
         panel.grid.major.x = element_blank(),
         panel.grid.major.y = element_line( size=.3, color="black"),
         legend.key = element_rect(size=1, color="white"),
         legend.key.size = unit(1, "lines"),
         legend.key.height=unit(1,"lines"),
         legend.title = element_blank(),
         legend.position = "right",
         strip.background = element_blank(),
         strip.text = element_text(size = 12),
         panel.margin.x=unit(1.5, "lines"))
 
 ggplot(combinedResult, aes(x=dataset , y=AUC_sum, fill=algorithm)) +
   geom_boxplot() +
   facet_wrap(~dataset) + 
   xlab(label = "Datasets") +
   ylab(label = "AUC") +
   plotThemeReal  +
   scale_y_continuous(limits = c(0.5,1)) # +
   # scale_fill_grey(start = 0.1, end = 0.7) 
 
 
## ---- end

## ---- plotRealWorldData
# 
# load("results/Param_100_050_020/experimentResult_glass_161102_100_50_20.RData") # glass
# tmp <- combinedResult 
# load("results/Param_100_050_020/experimentResult_ann_diabet_ion_161102_100_50_20.RData") # ann, diabetes, ion
# tmp <- rbind(tmp, combinedResult)
# load("results/Param_100_050_020/experimentResult_pen_lymph_segm_161102_100_50_20.RData") # pen, lymph, segment
# tmp <- rbind(tmp, combinedResult)
# combinedResult <- tmp
# 
# 
# plotThemeReal <- theme_bw() +
#   theme(plot.title = element_text(size=40, vjust = 2.5, face = "bold"),
#         axis.title.x = element_text(margin = margin(15,0,0,0)),
#         axis.title.y = element_text(margin = margin(0,15,0,0)),
#         axis.text = element_text(margin = margin(0,0,0,0)),
#         axis.ticks.y = element_blank(),
#         axis.ticks.x = element_blank(),
#         plot.margin = unit(c(0, 0, 0.3, 0), "lines"),
#         panel.border = element_blank(),
#         panel.grid.major.x = element_blank(),
#         panel.grid.major.y = element_line( size=.3, color="black"),
#         legend.key = element_rect(size=1, color="white"),
#         legend.key.size = unit(1, "lines"),
#         legend.key.height=unit(1,"lines"),
#         legend.title = element_blank(),
#         legend.position = "right",
#         strip.background = element_blank(),
#         strip.text = element_text(size = 12),
#         panel.margin.x=unit(1.5, "lines"))
# 
# plotReal <- combinedResult %>%
#   select(dataset, algorithm, AUC = AUC_sum) %>%
#   mutate(AUC = 0.5 + abs(0.5 - AUC)) %>%
#   group_by(dataset, algorithm) %>%
#   slice(which.max(AUC)) # %>%
#   # mutate(percentageDisplay = paste0("[", outlierPercentage, "%]")) %>%
# 
#    ggplot(aes(x=dataset , y=AUC, fill=algorithm)) +
#   #ggplot(aes(x=percentageDisplay , y=AUC, fill=algorithm)) +
#   geom_boxplot() +
#   # facet_wrap(~data.name, nrow=1, scales = "free_x") +
#   xlab(label = "Datasets") +
#   ylab(label = "AUC") +
#   plotThemeReal +
#   scale_y_continuous(limits = c(0.5,1)) +
#   scale_fill_grey(start = 0.1, end = 0.7) 

## ---- end
   
   
filename <- "experiments_compos.tex"
tikz(filename, standAlone = F, width=7, height=3)

plotReal
dev.off()

plotReal


# ## ---- (begin) Subspace Size
plotSSSize <- combinedResult %>% 

  # Boxplot of MPG by Car Cylinders
  boxplot(combinedResult~subsp1,data=combinedResult, main="Subspace Size",
          xlab="Subspace 1", ylab="Size") 

plotBoxSSSize
dev.off()
plotBoxSSSize

# ## ---- (end) Subspace Size








