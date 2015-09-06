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
  library(snpStats)
})

## ----load----------------------------------------------------------------
  library(devtools)
  library(Biobase)
  library(sva)
  library(bladderbatch)
  library(snpStats)

## ----install_packages, eval=FALSE----------------------------------------
#  install.packages(c("devtools"))
#  source("http://www.bioconductor.org/biocLite.R")
#  biocLite(c("Biobase","sva","bladderbatch","snpStats"))

## ------------------------------------------------------------------------
data(bladderdata)

## ------------------------------------------------------------------------
pheno = pData(bladderEset)
edata = exprs(bladderEset)

## ------------------------------------------------------------------------
mod = model.matrix(~as.factor(cancer) + as.factor(batch),data=pheno)
fit = lm.fit(mod,t(edata))
hist(fit$coefficients[2,],col=2,breaks=100)

## ------------------------------------------------------------------------
table(pheno$cancer,pheno$batch)

## ------------------------------------------------------------------------
batch = pheno$batch
modcombat = model.matrix(~1, data=pheno)
modcancer = model.matrix(~cancer, data=pheno)
combat_edata = ComBat(dat=edata, batch=batch, mod=modcombat, par.prior=TRUE, prior.plots=FALSE)
combat_fit = lm.fit(modcancer,t(combat_edata))
hist(combat_fit$coefficients[2,],col=2,breaks=100)

## ------------------------------------------------------------------------
plot(fit$coefficients[2,],combat_fit$coefficients[2,],col=2,
      xlab="Linear Model",ylab="Combat",xlim=c(-5,5),ylim=c(-5,5))
abline(c(0,1),col=1,lwd=3)

## ------------------------------------------------------------------------
mod = model.matrix(~cancer,data=pheno)
mod0 = model.matrix(~1, data=pheno)
sva1 = sva(edata,mod,mod0,n.sv=2)

## ------------------------------------------------------------------------
summary(lm(sva1$sv ~ pheno$batch))
boxplot(sva1$sv[,2] ~ pheno$batch)
points(sva1$sv[,2] ~ jitter(as.numeric(pheno$batch)),col=as.numeric(pheno$batch))

## ------------------------------------------------------------------------
modsv = cbind(mod,sva1$sv)
fitsv = lm.fit(modsv,t(edata))

## ------------------------------------------------------------------------
par(mfrow=c(1,2))
plot(fitsv$coefficients[2,],combat_fit$coefficients[2,],col=2,
      xlab="SVA",ylab="Combat",xlim=c(-5,5),ylim=c(-5,5))
abline(c(0,1),col=1,lwd=3)
plot(fitsv$coefficients[2,], fit$coefficients[2,],col=2,
      xlab="SVA",ylab="linear model",xlim=c(-5,5),ylim=c(-5,5))
abline(c(0,1),col=1,lwd=3)

## ------------------------------------------------------------------------
data(for.exercise)
controls <- rownames(subject.support)[subject.support$cc==0]
use <- seq(1, ncol(snps.10), 10)
ctl.10 <- snps.10[controls,use]

## ------------------------------------------------------------------------
xxmat <- xxt(ctl.10, correct.for.missing=FALSE)
evv <- eigen(xxmat, symmetric=TRUE)
pcs <- evv$vectors[,1:5]

## ------------------------------------------------------------------------
pop <- subject.support[controls,"stratum"]
plot(pcs[,1],pcs[,2],col=as.numeric(pop),
      xlab="PC1",ylab="PC2")
legend(0,0.15,legend=levels(pop),pch=19,col=1:2)

## ----session_info--------------------------------------------------------
devtools::session_info()

