## ----style, results = 'asis',include=FALSE-------------------------------
BiocStyle::markdown()

## ----global_options,include=FALSE----------------------------------------
## see ch. 10 Hooks of Xie's knitr book
knit_hooks$set(setPch = function(before, options, envir) {
  if(before) par(pch = 19)
})
opts_chunk$set(setPch = TRUE)

## ------------------------------------------------------------------------
con=url("http://bowtie-bio.sourceforge.net/recount/ExpressionSets/bodymap_eset.RData")
load(file=con)
close(con)

## ------------------------------------------------------------------------
bm = bodymap.eset
bm

## ------------------------------------------------------------------------
exp_data = exprs(bm)
dim(exp_data)
head(exp_data,n=5)

## ------------------------------------------------------------------------
pheno_data = pData(bm)
dim(pheno_data)
head(pheno_data)

## ------------------------------------------------------------------------
feature_data = fData(bm)
dim(fData(bodymap.eset))
fData(bodymap.eset)[1:10,,1]

## ----session_info--------------------------------------------------------
devtools::session_info()

