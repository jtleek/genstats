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
con =url("http://bowtie-bio.sourceforge.net/recount/ExpressionSets/montpick_eset.RData")
load(file=con)
close(con)
mp = montpick.eset
pdata=pData(mp)
edata=as.data.frame(exprs(mp))
fdata = fData(mp)
ls()

## ------------------------------------------------------------------------
edata_centered = edata - rowMeans(edata)
svd1 = svd(edata_centered)
names(svd1)

## ------------------------------------------------------------------------
plot(svd1$d,ylab="Singular value",col=2)
plot(svd1$d^2/sum(svd1$d^2),ylab="Percent Variance Explained",col=2)

## ------------------------------------------------------------------------
par(mfrow=c(1,2))
plot(svd1$v[,1],col=2,ylab="1st PC")
plot(svd1$v[,2],col=2,ylab="2nd PC")

## ------------------------------------------------------------------------
plot(svd1$v[,1],svd1$v[,2],col=2,ylab="2nd PC",xlab="1st PC")

## ------------------------------------------------------------------------
plot(svd1$v[,1],svd1$v[,2],ylab="2nd PC",
     xlab="1st PC",col=as.numeric(pdata$study))

## ------------------------------------------------------------------------
boxplot(svd1$v[,1] ~ pdata$study,border=c(1,2))
points(svd1$v[,1] ~ jitter(as.numeric(pdata$study)),col=as.numeric(pdata$study))

## ------------------------------------------------------------------------
pc1 = prcomp(edata)
plot(pc1$rotation[,1],svd1$v[,1])

## ------------------------------------------------------------------------
edata_centered2 = t(t(edata) - colMeans(edata))
svd2 = svd(edata_centered2)

## ----session_info--------------------------------------------------------
devtools::session_info()

