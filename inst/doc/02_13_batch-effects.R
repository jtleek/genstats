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
library(sva)
library(bladderbatch)
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

## ----session_info--------------------------------------------------------
devtools::session_info()

