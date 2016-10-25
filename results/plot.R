library(data.table)
library(ade4)
# (tab.zahlen <- table(zahlen <- rpois(100, lambda=5)))
# 
# #0  1  2  3  4  5  6  7  8  9 10 12 
# #2  3  8 15 14 13 16 12  6  7  3  1 
# farbe <- rainbow(20)
# (bplot <- barplot(tab.zahlen, col=farbe))
# 
# text(bplot, tab.zahlen+1,
#      # x-y Koordinaten
#      labels=tab.zahlen,
#      # Text
#      xpd=TRUE)



# sssize <- matrix(c(5,3.2,6.2,2.6,8.2,2.4,6.6,2.6),ncol=2,byrow=TRUE)
# colnames(sssize) <- c("CMI","HiCS")
# zeilen <- c("Thyroid","Glass","Segment", "Lympho")
# rownames(sssize) <- zeilen
# sssize <- as.table(sssize)
# 
# bplot <- barplot(sssize$CMI, sssize$HiCS)



dataset = c("Thyroid","Thyroid","Glass","Glass","Ion", "Ion", "Pendigits","Pendigits","Segment","Segment", "Lympho","Lympho") 
algorithm = c("CMI", "HiCS","CMI", "HiCS","CMI", "HiCS","CMI", "HiCS", "CMI", "HiCS", "CMI", "HiCS")
values = c(5, 3.2, 6.2, 2.6, 7.2, 2.4, 6.4, 3.2, 8.2, 2.4, 6.6, 2.2) 
df = data.frame(dataset, algorithm, values) 
barplot(df$values, 
        names.arg = dataset,
        main = "Average size of Top5-Subspaces",
        xlab="Dataset", 
        ylab="Average Subspace-Size",
        col=c("darkblue","deepskyblue"),
        legend = c("CMI","HiCS"))

table.value(df)

dataset_ann_th <- fread("D:/Uni/SS16/Hiwi/Paper/CMI_Daten/madelonnorm.csv")
plot(V1~V3, data=dataset_ann_th, main="Dataset Thyroid", xlab="Dimension 1", ylab="Dimension 3")
#plot(V1~V3~V4, data=dataset_ann_th, main="Dataset Thyroid", xlab="Dimension 1", ylab="Dimension 3")
