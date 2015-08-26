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
library(broom)
edata = as.matrix(edata)
lm1 = lm(edata[1,] ~ pdata$age)
tidy(lm1)

## ------------------------------------------------------------------------
plot(pdata$age,edata[1,], col=1)
abline(lm1$coeff[1],lm1$coeff[2], col=2,lwd=3)

## ------------------------------------------------------------------------
pdata$gender
table(pdata$gender)

## ------------------------------------------------------------------------
boxplot(edata[1,] ~ pdata$gender)
points(edata[1,] ~ jitter(as.numeric(pdata$gender)),
       col=as.numeric(pheno_data$gender))

## ------------------------------------------------------------------------
dummy_m = pdata$gender=="M"
dummy_m

dummy_f = pdata$gender=="F"
dummy_f

## ------------------------------------------------------------------------
lm2 = lm(edata[1,] ~ pdata$gender)
tidy(lm2)


## ------------------------------------------------------------------------
mod2 = model.matrix(~pdata$gender)
mod2

## ------------------------------------------------------------------------
table(pheno_data$tissue.type)
pdata$tissue.type == "adipose"
pdata$tissue.type == "adrenal"

## ------------------------------------------------------------------------
tidy(lm(edata[1,] ~ pdata$tissue.type ))

## ------------------------------------------------------------------------
table(pheno_data$num.tech.reps)
lm3 = lm(edata[1,] ~ pdata$age + pdata$gender)
tidy(lm3)

## ------------------------------------------------------------------------
lm4 = lm(edata[6,] ~ pdata$age)
plot(pdata$age,edata[6,],col=2)
abline(lm4,col=1,lwd=3)

## ------------------------------------------------------------------------
index = 1:19
lm5 = lm(edata[6,] ~ index)
plot(index,edata[6,],col=2)
abline(lm5,col=1,lwd=3)

lm6 = lm(edata[6,-19] ~ index[-19])
abline(lm6,col=3,lwd=3)

legend(5,1000,c("With outlier","Without outlier"),col=c(1,3),lwd=3)


## ------------------------------------------------------------------------
par(mfrow=c(1,2))
hist(lm6$residuals,col=2)
hist(lm5$residuals,col=3)

## ------------------------------------------------------------------------
gene1 = log2(edata[1,]+1)
lm7 = lm(gene1 ~ index)
hist(lm7$residuals,col=4)

## ------------------------------------------------------------------------
lm8 = lm(gene1 ~ pdata$tissue.type + pdata$age)
tidy(lm8)

## ------------------------------------------------------------------------
library(RSkittleBrewer)
trop = RSkittleBrewer("tropical")
colramp = colorRampPalette(c(trop[1:4]))(17)
lm9 = lm(edata[2,] ~ pdata$age)
plot(lm9$residuals,col=colramp[as.numeric(pdata$tissue.type)])

## ----session_info--------------------------------------------------------
devtools::session_info()

