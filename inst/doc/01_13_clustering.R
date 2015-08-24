## ----style, results = 'asis',include=FALSE-------------------------------
BiocStyle::markdown()

## ----global_options,include=FALSE----------------------------------------
## see ch. 10 Hooks of Xie's knitr book
knit_hooks$set(setPch = function(before, options, envir) {
  if(before) par(pch = 19)
})
opts_chunk$set(setPch = TRUE)
library(RSkittleBrewer)
# Make the colors pretty
trop = RSkittleBrewer("tropical")
palette(trop)

## ------------------------------------------------------------------------
con =url("http://bowtie-bio.sourceforge.net/recount/ExpressionSets/bodymap_eset.RData")
load(file=con)
close(con)
bm = bodymap.eset
pdata=pData(bm)
edata=as.data.frame(exprs(bm))
fdata = fData(bm)
ls()

## ------------------------------------------------------------------------
edata = edata[rowMeans(edata) > 5000,]
edata = log2(edata + 1)

# By default calculates the distance between rows
dist1 = dist(t(edata))

## Look at distance matrix
library(RSkittleBrewer)
trop = RSkittleBrewer("tropical")
colramp = colorRampPalette(c(trop[3],"white",trop[2]))(9)
heatmap(as.matrix(dist1),col=colramp,Colv=NA,Rowv=NA)

## ------------------------------------------------------------------------
hclust1 = hclust(dist1)
plot(hclust1)

## ------------------------------------------------------------------------
plot(hclust1,hang=-1)

## ------------------------------------------------------------------------
library(dendextend)
dend = as.dendrogram(hclust1)
dend = color_labels(hclust1,4,col=trop)
plot(dend)

## ------------------------------------------------------------------------
labels_colors(dend) = c(rep(trop[1],10),rep(trop[2],9))
plot(dend)

## ------------------------------------------------------------------------
kmeans1 = kmeans(edata,centers=3)
names(kmeans1)

## ------------------------------------------------------------------------
matplot(t(kmeans1$centers),col=1:3,type="l",lwd=3)

## ------------------------------------------------------------------------
table(kmeans1$cluster)

## ------------------------------------------------------------------------
heatmap(as.matrix(edata)[order(kmeans1$cluster),],col=colramp,Colv=NA,Rowv=NA)

## ------------------------------------------------------------------------
kmeans2 = kmeans(edata,centers=3)
table(kmeans1$cluster,kmeans2$cluster)

## ----session_info--------------------------------------------------------
devtools::session_info()

