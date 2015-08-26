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
edata = log2(as.matrix(edata) + 1)
edata = edata[rowMeans(edata) > 10, ]

## ------------------------------------------------------------------------
library(edge)
mod = model.matrix(~ pdata$num.tech.reps)
fit = lm.fit(mod,t(edata))
names(fit)

## ------------------------------------------------------------------------
fit$coefficients[,1]
tidy(lm(edata[1, ] ~ pdata$num.tech.reps))

## ------------------------------------------------------------------------
par(mfrow=c(1,2))
hist(fit$coefficients[1,],breaks=100,col=2,xlab="Intercept")
hist(fit$coefficients[2,],breaks=100,col=2,xlab="Tech Reps")
abline(v=0,lwd=3,col=1)

## ------------------------------------------------------------------------
par(mfrow=c(1,2))
plot(fit$residuals[,1],col=2)
plot(fit$residuals[,2],col=2)

## ----session_info--------------------------------------------------------
devtools::session_info()

