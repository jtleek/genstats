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
})

## ----load----------------------------------------------------------------
  library(devtools)
  library(Biobase)
  library(limma)
  library(edge)

## ----install_packages, eval=FALSE----------------------------------------
#  install.packages(c("devtools"))
#  source("http://www.bioconductor.org/biocLite.R")
#  biocLite(c("Biobase","limma","jdstorey/edge"))

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
mod = model.matrix(~ pdata$strain)
fit = lm.fit(mod,t(edata))
names(fit)

## ------------------------------------------------------------------------
fit$coefficients[,1]
tidy(lm(as.numeric(edata[1, ]) ~ pdata$strain))

## ------------------------------------------------------------------------
par(mfrow=c(1,2))
hist(fit$coefficients[1,],breaks=100,col=2,xlab="Intercept")
hist(fit$coefficients[2,],breaks=100,col=2,xlab="Strain")
abline(v=0,lwd=3,col=1)

## ------------------------------------------------------------------------
par(mfrow=c(1,2))
plot(fit$residuals[,1],col=2)
plot(fit$residuals[,2],col=2)

## ------------------------------------------------------------------------
mod_adj = model.matrix(~ pdata$strain + as.factor(pdata$lane.number))
fit_adj = lm.fit(mod_adj,t(edata))
fit_adj$coefficients[,1]

## ------------------------------------------------------------------------
fit_limma = lmFit(edata,mod_adj)
names(fit_limma)
fit_limma$coefficients[1,]
fit_adj$coefficients[,1]

## ------------------------------------------------------------------------
edge_study = build_study(data=edata,grp=pdata$strain,adj.var=as.factor(pdata$lane.number))
fit_edge = fit_models(edge_study)
summary(fit_edge)
fit_edge@beta.coef[1,]
fit_limma$coefficients[1,]

## ----session_info--------------------------------------------------------
devtools::session_info()

