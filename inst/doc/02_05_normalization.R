## ----style, results = 'asis',include=FALSE-------------------------------
BiocStyle::markdown()

## ----global_options------------------------------------------------------
## see ch. 10 Hooks of Xie's knitr book
knit_hooks$set(setPch = function(before, options, envir) {
  if(before) par(pch = 19)
})
opts_chunk$set(setPch = TRUE)

## Make the colors pretty
library(RSkittleBrewer)
trop = RSkittleBrewer("tropical")
palette(trop)

## ------------------------------------------------------------------------
con =url("http://bowtie-bio.sourceforge.net/recount/ExpressionSets/montpick_eset.RData")
load(file=con)
close(con)
mp = montpick.eset
pdata=pData(mp)
edata=as.data.frame(exprs(mp))
fdata = fData(mp)
ls()

## ------------------------------------------------------------------------
edata = log2(edata + 1)
edata = edata[rowMeans(edata) > 3, ]
colramp = colorRampPalette(c(trop[3],"white",trop[2]))(20)
plot(density(edata[,1]),col=colramp[1],lwd=3,ylim=c(0,.30))
for(i in 2:20){lines(density(edata[,i]),lwd=3,col=colramp[i])}

## ------------------------------------------------------------------------
library(preprocessCore)
norm_edata = normalize.quantiles(as.matrix(edata))
plot(density(norm_edata[,1]),col=colramp[1],lwd=3,ylim=c(0,.20))
for(i in 2:20){lines(density(norm_edata[,i]),lwd=3,col=colramp[i])}

## ------------------------------------------------------------------------
plot(norm_edata[1,],col=as.numeric(pdata$study))

## ------------------------------------------------------------------------
svd1 = svd(norm_edata - rowMeans(norm_edata))
plot(svd1$v[,1],svd1$v[,2],xlab="PC1",ylab="PC2",
     col=as.numeric(pdata$study))

## ----session_info--------------------------------------------------------
devtools::session_info()

