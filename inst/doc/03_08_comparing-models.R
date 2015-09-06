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
  library(sva)
  library(bladderbatch)
})

## ----load----------------------------------------------------------------
  library(devtools)
  library(Biobase)
  library(sva)
  library(bladderbatch)

## ----install_packages, eval=FALSE----------------------------------------
#  install.packages(c("devtools"))
#  source("http://www.bioconductor.org/biocLite.R")
#  biocLite(c("Biobase","sva","bladderbatch"))

## ------------------------------------------------------------------------
library(sva)
library(bladderbatch)
data(bladderdata)

## ------------------------------------------------------------------------
pheno = pData(bladderEset)
edata = exprs(bladderEset)

## ------------------------------------------------------------------------
## Setting seed so the jitter will be the same
set.seed(123)

cancerjit = jitter(as.numeric(pheno$cancer))
lm1 = lm(edata[1,] ~ 1)
lm2 = lm(edata[1,] ~ pheno$cancer)

par(mfrow=c(1,2))

plot(edata[1,] ~ cancerjit,type="p",xaxt="n",xlab="Cancer Status",ylab="Expression",pch=19,col=as.numeric(pheno$cancer))
axis(1,at=c(1,2,3),c("Biopsy","Cancer","Normal"))
abline(lm1,col="darkgrey",lwd=5)


plot(edata[1,] ~ cancerjit,type="p",xaxt="n",xlab="Cancer Status",ylab="Expression",pch=19,col=as.numeric(pheno$cancer))
axis(1,at=c(1,2,3),c("Biopsy","Cancer","Normal"))
boxplot(lm2$fitted~pheno$cancer,add=T,border=1:3)


## ------------------------------------------------------------------------
par(mfrow=c(1,2))
plot(lm1$residuals ~ cancerjit,type="p",xaxt="n",xlab="Cancer Status",ylab="Expression",pch=19,col=as.numeric(pheno$cancer),ylim=c(-1.1,1.1))
axis(1,at=c(1,2,3),c("Biopsy","Cancer","Normal"))

plot(lm2$residuals ~ cancerjit,type="p",xaxt="n",xlab="Cancer Status",ylab="Residuals",pch=19,col=as.numeric(pheno$cancer),ylim=c(-1.1,1.1))
axis(1,at=c(1,2,3),c("Biopsy","Cancer","Normal"))

## ----session_info--------------------------------------------------------
devtools::session_info()

