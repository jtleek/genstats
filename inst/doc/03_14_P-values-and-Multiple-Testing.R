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
  library(qvalue)
})

## ----load----------------------------------------------------------------
  library(devtools)
  library(Biobase)
  library(limma)
  library(edge)
  library(genefilter)
  library(qvalue)

## ----install_packages, eval=FALSE----------------------------------------
#  install.packages(c("devtools"))
#  source("http://www.bioconductor.org/biocLite.R")
#  biocLite(c("Biobase","limma","genefilter","jdstorey/edge","qvalue"))

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
fstats_obj = rowFtests(edata,as.factor(pdata$strain))
hist(fstats_obj$p.value,col=2)

## ------------------------------------------------------------------------

edge_study = build_study(edata, grp = pdata$strain, 
                         adj.var = as.factor(pdata$lane.number))
de_obj = lrt(edge_study)
qval = qvalueObj(de_obj)
hist(qval$pvalues,col=3)

## ------------------------------------------------------------------------
mod = model.matrix(~ pdata$strain + pdata$lane.number)
fit_limma = lmFit(edata,mod)
ebayes_limma = eBayes(fit_limma)
limma_pvals = topTable(ebayes_limma,number=dim(edata)[1])$P.Value
hist(limma_pvals,col=4)

## ------------------------------------------------------------------------
set.seed(3333)
B = 1000
tstats_obj = rowttests(edata,pdata$strain)
tstat0 = matrix(NA,nrow=dim(edata)[1],ncol=B)
tstat = tstats_obj$statistic
strain = pdata$strain
for(i in 1:B){
  strain0 = sample(strain)
  tstat0[,i] = rowttests(edata,strain0)$statistic
}

emp_pvals = empPvals(tstat,tstat0)
hist(emp_pvals,col=2)

## ------------------------------------------------------------------------
fp_bonf = p.adjust(fstats_obj$p.value,method="bonferroni")
hist(fp_bonf,col=3)
quantile(fp_bonf)

fp_bh = p.adjust(fstats_obj$p.value,method="BH")
hist(fp_bh,col=3)
quantile(fp_bh)

## ------------------------------------------------------------------------
limma_pvals_adj = topTable(ebayes_limma,number=dim(edata)[1])$adj.P.Val
hist(limma_pvals_adj,col=2)
quantile(limma_pvals_adj)

## ------------------------------------------------------------------------
qval_limma = qvalue(limma_pvals)
summary(qval_limma)
qval$pi0

## ------------------------------------------------------------------------
qval = qvalueObj(de_obj)
summary(qval)

## ----session_info--------------------------------------------------------
devtools::session_info()

