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
  library(snpStats)
  library(broom)
  library(MASS)
  library(DESeq2)
})

## ----load----------------------------------------------------------------
  library(devtools)
  library(Biobase)
  library(snpStats)
  library(broom)
  library(MASS)
  library(DESeq2)

## ----install_packages, eval=FALSE----------------------------------------
#  install.packages(c("devtools","broom","MASS"))
#  source("http://www.bioconductor.org/biocLite.R")
#  biocLite(c("Biobase","snpStats","DESeq2"))

## ------------------------------------------------------------------------
data(for.exercise)
use <- seq(1, ncol(snps.10), 10)
sub.10 <- snps.10[,use]

## ------------------------------------------------------------------------
xxmat <- xxt(sub.10, correct.for.missing=FALSE)
evv <- eigen(xxmat, symmetric=TRUE)
pcs <- evv$vectors[,1:5]

## ------------------------------------------------------------------------
snpdata = sub.10@.Data
status = subject.support$cc
snp1 = as.numeric(snpdata[,1])
snp1[snp1==0] = NA
glm1 = glm(status ~ snp1,family="binomial")
tidy(glm1)

## ------------------------------------------------------------------------
snp1_dom = (snp1 == 1)
glm1_dom = glm(status ~ snp1_dom,family="binomial")
tidy(glm1_dom)
tidy(glm1)

## ------------------------------------------------------------------------
glm2 = glm(status ~ snp1 + pcs[,1:5],family="binomial")
tidy(glm2)

## ------------------------------------------------------------------------
glm_all = snp.rhs.tests(status ~ 1,snp.data=sub.10)
slotNames(glm_all)
qq.chisq(chi.squared(glm_all),df=1)

## ------------------------------------------------------------------------
glm_all_adj = snp.rhs.tests(status ~ pcs,snp.data=sub.10)
qq.chisq(chi.squared(glm_all_adj),df=1)

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
edata = edata[rowMeans(edata) > 10, ]

## ------------------------------------------------------------------------
glm3 = glm(edata[1, ] ~ pdata$strain,family="poisson")
tidy(glm3)

## ------------------------------------------------------------------------
glm.nb1 = glm.nb(edata[1, ] ~ pdata$strain)
tidy(glm.nb1)

## ------------------------------------------------------------------------
de = DESeqDataSetFromMatrix(edata, pdata, ~strain)
glm_all_nb = DESeq(de)
result_nb = results(glm_all_nb)
hist(result_nb$stat)

## ----session_info--------------------------------------------------------
devtools::session_info()

