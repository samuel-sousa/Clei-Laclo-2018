library('xlsx')
library('pvclust')
library('factoextra')
library('cluster')
library('amap')
library('fpc')
library('dbscan')
library('fossil')
library("cclust")

#Normalization function

normalize <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))) }

#Data reading

datasus <- read.xlsx("C:\\Users\\samue\\OneDrive\\Documentos\\UNIFESP\\Artigos\\CLEI\\Data//DADOS_DATASUS.xlsx",1, header = TRUE)
datasus[,2:14] <- apply(datasus[,2:14], MARGIN = c(1,2), as.numeric)
pnud <- read.xlsx("C:\\Users\\samue\\OneDrive\\Documentos\\UNIFESP\\Artigos\\CLEI\\Data//DADOS_UNESCO.xlsx",1, header = TRUE)
pnud[,2:8] <- apply(pnud[,2:8], MARGIN = c(1,2), as.numeric)
ibge <- read.xlsx("C:\\Users\\samue\\OneDrive\\Documentos\\UNIFESP\\Artigos\\CLEI\\Data//DADOSIBGE.xlsx",1, header = TRUE)
ibge <- ibge[1:246,]
ibge[1:246,2:3] <- apply(ibge[1:246,2:3], MARGIN = c(1,2), as.double)

ideb <- read.xlsx("C:\\Users\\samue\\OneDrive\\Documentos\\UNIFESP\\Artigos\\CLEI\\Data//IDEB.xlsx",1, header = TRUE)
ideb[,2:7] <- apply(ideb[,2:7], MARGIN = c(1,2), as.numeric)

View(datasus)
View(pnud)
View(ibge)
View(ideb)


#agregations

mhr <- rowSums(datasus[,2:14])
datasus <- data.frame('municipio' = datasus$municipio, 'overallmhr' = mhr)
mideb <- rowMeans(ideb[,2:6], na.rm = TRUE)
mhrdataset <- data.frame('municipality' = datasus$municipio, 'population' = ibge$populacao, 'demogdensity' = ibge$densidadedemo, 'lifeexpectation' = pnud$ESPVIDA, 'gini' = pnud$GINI, 'incomerihest10' = pnud$PREN10RICOS, 'educlevel' = pnud$I_ESCOLARIDADE, 'mhdi' = pnud$IDHM, 'mhdie' = pnud$IDHM_E, 'mhdil' = pnud$IDHM_L, 'mhdii' = pnud$IDHM_R, 'ideb' = mideb, 'mhr' = datasus$overallmhr) #, 'v1' = ideb$municipio, 'v2'= pnud$municipio, 'v3' = ibge$municipio)

mhrdataset[,2:13] <- apply(mhrdataset[,2:13], MARGIN = c(2), as.double)
View(mhrdataset)

#correlations

cor.test(mhrdataset$ideb,mhrdataset$mhr, method = "pearson")
cor.test(mhrdataset$mhr,mhrdataset$ideb, method = "spearman")
cor.test(mhrdataset$mhr,mhrdataset$ideb, method = "kendall")

#Normalization

mhrdatasetnormalized <- data.frame('municipality' = mhrdataset$municipality, apply(mhrdataset[,2:13], MARGIN = 2, normalize))
mhrdatasetnormalized <- na.omit(mhrdatasetnormalized[,1:13])

rownames(mhrdatasetnormalized) <- mhrdatasetnormalized[,1]
mhrdatasetnormalized <- mhrdatasetnormalized[,2:13]

View(mhrdatasetnormalized)

#Scatteplots with linear and local regression

plot(mhrdatasetnormalized$population,mhrdatasetnormalized$mhr)

plot(mhrdatasetnormalized$lifeexpectation, mhrdatasetnormalized$mhr, main="Test", 
     xlab="Life expectation", ylab="MHR ", pch=20, abline(lm(mhrdatasetnormalized$mhr~mhrdatasetnormalized$lifeexpectation), col="red"), lines(lowess(mhrdatasetnormalized$lifeexpectation,mhrdatasetnormalized$mhr), col="blue"))
plot(mhrdatasetnormalized$demogdensity,mhrdatasetnormalized$mhr,  main="", 
     xlab="Demographic Density ", ylab="MHR ", pch=20, abline(lm(mhrdatasetnormalized$mhr~mhrdatasetnormalized$densdem), col="red"), lines(lowess(mhrdatasetnormalized$densdem,mhrdatasetnormalized$mhr), col="blue"))
plot(mhrdatasetnormalized$ideb, mhrdatasetnormalized$mhr, main="", 
     xlab="IDEB ", ylab="MHR ", pch=20, abline(lm(mhrdatasetnormalized$mhr~mhrdatasetnormalized$ideb), col="red"), lines(lowess(mhrdatasetnormalized$ideb,mhrdatasetnormalized$mhr), col="blue"))
plot(mhrdatasetnormalized$gini, mhrdatasetnormalized$mhr, main="", 
     xlab="Gini index ", ylab="MHR ", pch=20, abline(lm(mhrdatasetnormalized$mhr~mhrdatasetnormalized$gini), col="red"), lines(lowess(mhrdatasetnormalized$gini,mhrdatasetnormalized$mhr), col="blue"))
plot(mhrdatasetnormalized$pern10, mhrdatasetnormalized$mhr, main="", 
     xlab="Income held by richest 10%", ylab="MHR ", pch=20, abline(lm(mhrdatasetnormalized$mhr~mhrdatasetnormalized$pern10), col="red"), lines(lowess(mhrdatasetnormalized$pern10,mhrdatasetnormalized$mhr), col="blue"))
plot(mhrdatasetnormalized$escolaridade, mhrdatasetnormalized$mhr, main="", 
     xlab="Educational Level", ylab="MHR ", pch=20, abline(lm(mhrdatasetnormalized$mhr~mhrdatasetnormalized$escolaridade), col="red"), lines(lowess(mhrdatasetnormalized$escolaridade,mhrdatasetnormalized$mhr), col="blue"))
plot(mhrdatasetnormalized$idhm, mhrdatasetnormalized$mhr, main="", 
     xlab="MHDI", ylab="MHR ", pch=20, abline(lm(mhrdatasetnormalized$mhr~mhrdatasetnormalized$idhm), col="red"), lines(lowess(mhrdatasetnormalized$idhm,mhrdatasetnormalized$mhr), col="blue"))
plot(mhrdatasetnormalized$idhme, mhrdatasetnormalized$mhr, main="", 
     xlab="MHDI (Education)", ylab="MHR ", pch=20, abline(lm(mhrdatasetnormalized$mhr~mhrdatasetnormalized$idhme), col="red"), lines(lowess(mhrdatasetnormalized$idhme,mhrdatasetnormalized$mhr), col="blue"))
plot(mhrdatasetnormalized$idhml, mhrdatasetnormalized$mhr, main="", 
     xlab="MHDI (Longevity)", ylab="MHR ", pch=20, abline(lm(mhrdatasetnormalized$mhr~mhrdatasetnormalized$idhml), col="red"), lines(lowess(mhrdatasetnormalized$idhml,mhrdatasetnormalized$mhr), col="blue"))
plot(mhrdatasetnormalized$idhmr, mhrdatasetnormalized$mhr, main="", 
     xlab="MHDI (Income)", ylab="MHR ", pch=20, abline(lm(mhrdatasetnormalized$mhr~mhrdatasetnormalized$idhmr), col="red"), lines(lowess(mhrdatasetnormalized$idhmr,mhrdatasetnormalized$mhr), col="blue"))

#######################
## PROXIMITY
## CALCULATION
#######################


#Distance Matrix
distance <- get_dist(t(mhrdatasetnormalized))
fviz_dist(distance, gradient = list(low = "#DEB887", mid = "white", high = "#800080"))

d <- dist(mhrdatasetnormalized, method = "manhattan")

#######################
## Hierarchical
## Aglomerative
#######################

#chosen!
fit <- hclust(d, method = "single")

plot(fit, cex = 0.6, labels = FALSE)
rect.hclust(fit, k = 3, border =c("#FF0000", "#FFFF00","#4B0082"))
#------------

fit <- hclust(d, method = "average")
plot(fit)

fit <- hclust(d, method = "complete")
plot(fit)

#######################
## K-means
## Cluster
#######################


#Optimal clusters number

fviz_nbclust(mhrdatasetnormalized, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)

#######################
## K-means
## K = 4, W/ Canberra D.
#######################

km.res <- Kmeans(mhrdatasetnormalized, 4, iter.max = 100, nstart = 1, method = "canberra")
fviz_cluster(km.res, data = mhrdatasetnormalized,
             palette = c("#DEB887", "#DC143C", "#000000", "#800080"), #"#ff1a1a"), "#FFD700", "#FF69B4","#4B0082","#000000"
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)

View(km.res$cluster)
table(km.res$size)


#######################
## DBSCAN
## Cluster
#######################

#DEtermining the optimum eps

dbscan::kNNdistplot(mhrdatasetnormalized, k =  13) #minPts is often set to be dimensionality of the data plus one or higher.
abline(h = 0.5, lty = 2)

#DBSCAN

set.seed(123)
#res.fpc <- fpc::dbscan(mhrdatasetnormalized[,1:12], eps = 0.5, MinPts = 12)
res.db <- dbscan::dbscan(mhrdatasetnormalized, 0.5, 13, borderPoints = FALSE) #Border points are arbitrarily assigned to clusters in the original algorithm. DBSCAN* (see Campello et al 2013) treats all border points as noise points. This is implemented with borderPoints = FALSE.
res.db$cluster

#all(res.fpc$cluster == res.db)


fviz_cluster(res.db, data = mhrdatasetnormalized,
             geom = "text",
             ellipse.type = "norm", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow),
             labelsize = 5,
             outlier.color = "black",
             outlier.shape = 16,
             ggtheme = theme_minimal())

########################
##Validation
##
########################
fviz_nbclust(mhrdatasetnormalized, amap::Kmeans, dist(mhrdatasetnormalized, method = "canberra"), method = "gap_stat", verbose = TRUE)+
geom_hline(yintercept = 0.717, linetype = 2)

gap_stat <- clusGap(mhrdatasetnormalized, FUN = dbscan::dbscan, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

gap_stat_hcut <- clusGap(mhrdatasetnormalized, FUN = hcut, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat_hcut)

#Silhouette

fviz_nbclust(mhrdatasetnormalized, FUN = dbscan::dbscan, method = "silhouette")

fviz_nbclust(mhrdatasetnormalized, FUN = hcut, method = "silhouette")

#WSS

clustIndex(km.res, mhrdatasetnormalized, index = "ball")

fviz_nbclust(mhrdatasetnormalized, FUN = dbscan::dbscan, method = "wss")+
  geom_hline(yintercept = 66.71, linetype = 2)

fviz_nbclust(mhrdatasetnormalized, FUN = hcut, method = "wss")+
geom_hline(yintercept = 50, linetype = 2)










