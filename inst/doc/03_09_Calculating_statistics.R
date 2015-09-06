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
  library(limma)
  library(edge)
  library(genefilter)
})

## ----load----------------------------------------------------------------
  library(devtools)
  library(Biobase)
  library(limma)
  library(edge)
  library(genefilter)

## ----install_packages, eval=FALSE----------------------------------------
#  install.packages(c("devtools"))
#  source("http://www.bioconductor.org/biocLite.R")
#  biocLite(c("Biobase","limma","genefilter","jdstorey/edge"))

## ------------------------------------------------------------------------
con =url("http://bowtie-bio.sourceforge.net/recount/ExpressionSets/bottomly_eset.RData")
load(file=con)
close(con)
bot = bottomly.eset
pdata=pData(bot)
edata=as.matrix(exprs(bot))
fdata = fData(bot)
ls()

## ------------------------------------------------------------------------
edata = log2(as.matrix(edata) + 1)
edata = edata[rowMeans(edata) > 10, ]

## ------------------------------------------------------------------------
tstats_obj = rowttests(edata,pdata$strain)
names(tstats_obj)
hist(tstats_obj$statistic,col=2)

## ------------------------------------------------------------------------
fstats_obj = rowFtests(edata,as.factor(pdata$lane.number))
names(fstats_obj)
hist(fstats_obj$statistic,col=2)

## ------------------------------------------------------------------------
mod = model.matrix(~ pdata$strain)
fit_limma = lmFit(edata,mod)
ebayes_limma = eBayes(fit_limma)
head(ebayes_limma$t)

## ------------------------------------------------------------------------
plot(ebayes_limma$t[,2],-tstats_obj$statistic,col=4,
     xlab="Moderated T-stat",ylab="T-stat")
abline(c(0,1),col="darkgrey",lwd=3)

## ------------------------------------------------------------------------
mod_adj = model.matrix(~ pdata$strain + as.factor(pdata$lane.number))
fit_limma_adj = lmFit(edata,mod_adj)
ebayes_limma_adj = eBayes(fit_limma_adj)
head(ebayes_limma_adj$t)

## ------------------------------------------------------------------------
plot(ebayes_limma_adj$t[,2],-tstats_obj$statistic,col=3,
     xlab="Moderated T-stat",ylab="T-stat")
abline(c(0,1),lwd=3,col="darkgrey")

## ------------------------------------------------------------------------
mod_lane = model.matrix(~ as.factor(pdata$lane.number))
fit_limma_lane = lmFit(edata,mod_lane)
ebayes_limma_lane = eBayes(fit_limma_lane) 
head(ebayes_limma_lane$t)

## ------------------------------------------------------------------------
top_lane = topTable(ebayes_limma_lane, coef=2:7,
                    number=dim(edata)[1],sort.by="none")
head(top_lane)

## ------------------------------------------------------------------------
plot(top_lane$F,fstats_obj$statistic,
     xlab="Moderated F-statistic",ylab="F-statistic",col=3)

## ------------------------------------------------------------------------
edge_study = build_study(edata, grp = as.factor(pdata$lane.number))
de_obj = lrt(edge_study)
qval = qvalueObj(de_obj)
plot(qval$stat,fstats_obj$statistic,col=4,
      xlab="F-stat from edge",ylab="F-stat from genefilter")

## ------------------------------------------------------------------------
edge_study2 = build_study(edata, grp = as.factor(pdata$lane.number),
                        adj.var=pdata$strain)
de_obj2 = lrt(edge_study2)
qval2 = qvalueObj(de_obj2)
plot(qval2$stat,fstats_obj$statistic,col=4,
      xlab="F-stat from edge",ylab="F-stat from genefilter")

## ----session_info--------------------------------------------------------
devtools::session_info()

