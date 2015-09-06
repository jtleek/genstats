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
hist(tstats_obj$statistic,col=2,xlim=c(-5,2))

## ------------------------------------------------------------------------
set.seed(135)
strain = pdata$strain
strain0 = sample(strain)
tstats_obj0 = rowttests(edata,strain0)
hist(tstats_obj0$statistic,col=2,xlim=c(-5,2))

## ------------------------------------------------------------------------
quantile(tstats_obj0$statistic)
quantile(tstats_obj$statistic)

## ------------------------------------------------------------------------
set.seed(3333)
B = 1000
tstat0 = matrix(NA,nrow=dim(edata)[1],ncol=B)
tstat = tstats_obj$statistic
for(i in 1:B){
  strain0 = sample(strain)
  tstat0[,i] = rowttests(edata,strain0)$statistic
}

pvals = empPvals(tstat,tstat0)
hist(pvals,col=2)

## ----session_info--------------------------------------------------------
devtools::session_info()

