## ----global_palette, results = 'asis'------------------------------------
rm(list=ls())
tropical=  c('darkorange', 'dodgerblue', 'hotpink', 'limegreen', 'yellow')
palette(tropical)

## ----global_options,warning=FALSE,message=FALSE--------------------------
## see ch. 10 Hooks of Xie's knitr book
library(knitr)
knit_hooks$set(setPch = function(before, options, envir) {
  if(before) par(pch = 19)
})
opts_chunk$set(setPch = TRUE)

## ----global_plot,warning=FALSE, message=FALSE----------------------------
knitr::opts_chunk$set(fig.width=5, fig.height=5, size="footnotesize",
                      warning=FALSE, message=FALSE)
knitr::knit_hooks$set(small.mar = function(before, options, envir) {
  if (before) graphics::par(mar = c(5,5,1.5,1))
})

## ----load_hidden, echo=FALSE, results="hide", warning=FALSE--------------
suppressPackageStartupMessages({
  library(devtools)
  library(Biobase)
  library(dendextend)
})

## ----load----------------------------------------------------------------
  library(devtools)
  library(Biobase)
  library(dendextend)

## ----install_packages, eval=FALSE----------------------------------------
#  install.packages(c("devtools","dendextend"))
#  source("http://www.bioconductor.org/biocLite.R")
#  biocLite(c("Biobase"))

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
colramp = colorRampPalette(c(3,"white",2))(9)
heatmap(as.matrix(dist1),col=colramp,Colv=NA,Rowv=NA)

## ------------------------------------------------------------------------
hclust1 = hclust(dist1)
plot(hclust1)

## ------------------------------------------------------------------------
plot(hclust1,hang=-1)

## ------------------------------------------------------------------------
dend = as.dendrogram(hclust1)
dend = color_labels(hclust1,4,col=1:4)
plot(dend)

## ------------------------------------------------------------------------
labels_colors(dend) = c(rep(1,10),rep(2,9))
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

