library(data.table)

(tab.zahlen <- table(zahlen <- rpois(100, lambda=5)))

#0  1  2  3  4  5  6  7  8  9 10 12 
#2  3  8 15 14 13 16 12  6  7  3  1 
farbe <- rainbow(20)
(bplot <- barplot(tab.zahlen, col=farbe))

text(bplot, tab.zahlen+1,
     # x-y Koordinaten
     labels=tab.zahlen,
     # Text
     xpd=TRUE)



sssize <- matrix(c(5,3.2,6.2,2.6,8.2,2.4,6.6,2.6),ncol=2,byrow=TRUE)
colnames(sssize) <- c("CMI","HiCS")
zeilen <- c("Thyroid","Glass","Segment", "Lympho")
rownames(sssize) <- zeilen
sssize <- as.table(sssize)

bplot <- barplot(sssize$CMI, sssize$HiCS)


dataset = c("T","G","S","L" ) 
cmi = c(5, 6.2, 8.2, 6.6) 
hics = c(3.2,2.6,2.4, 2.2) 
df = data.frame(dataset, cmi, hics) 
barplot(df$cmi, xlab="Datasets", ylab="Average size of subspaces", beside = FALSE, xpd=TRUE)

# Grouped Bar Plot
counts <- table(df$cmi, df$hics)
barplot(counts, main="Subspace Size",
        xlab="Datasets", col=c("darkblue","green"),
        legend = rownames(counts), beside=TRUE)

dataset_ann_th <- fread("D:/Uni/SS16/Hiwi/Paper/CMI_Daten/madelonnorm.csv")
plot(V1~V3, data=dataset_ann_th, main="Dataset Thyroid", xlab="Dimension 1", ylab="Dimension 3")
#plot(V1~V3~V4, data=dataset_ann_th, main="Dataset Thyroid", xlab="Dimension 1", ylab="Dimension 3")
