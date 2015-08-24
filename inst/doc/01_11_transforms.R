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
hist(rnorm(1000),col=2)

## ------------------------------------------------------------------------
hist(edata[,1],col=2,breaks=100)

## ------------------------------------------------------------------------
hist(log(edata[,1]),col=2,breaks=100)

## ------------------------------------------------------------------------
min(log(edata))

## ------------------------------------------------------------------------
min(log(edata[,1] + 1))
hist(log(edata[,1] + 1),breaks=100,col=2)

## ------------------------------------------------------------------------
hist(log2(edata[,1] + 1),breaks=100,col=2)

## ------------------------------------------------------------------------
hist(log2(edata[,1] + 1),breaks=100,col=2,xlim=c(1,15),ylim=c(0,400))

## ------------------------------------------------------------------------
hist(rowSums(edata==0),col=2)

## ------------------------------------------------------------------------
low_genes = rowMeans(edata) < 5
table(low_genes)
filt_edata = filter(edata,!low_genes)
dim(filt_edata)

low_genes2 = rowMedians(as.matrix(edata)) < 5
table(low_genes2,low_genes)
filt_edata2 = filter(edata,!low_genes2)
dim(filt_edata2)

## ------------------------------------------------------------------------
hist(log2(filt_edata[,1] + 1),col=2)

## ----session_info--------------------------------------------------------
devtools::session_info()

