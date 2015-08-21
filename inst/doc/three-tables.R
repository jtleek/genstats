## ----style, echo = FALSE, results = 'asis'-------------------------------
BiocStyle::markdown()

## ----global_options, include=FALSE---------------------------------------
knitr::opts_chunk$set(warning=FALSE, message=FALSE)
palette(RSkittleBrewer::RSkittleBrewer("tropical"))

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
sessionInfo()
devtools::session_info()

