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
})

## ----load----------------------------------------------------------------
  library(devtools)
  library(Biobase)

## ---- eval=FALSE---------------------------------------------------------
#  install.packages("devtools")
#  source("http://www.bioconductor.org/biocLite.R")
#  biocLite(c("Biobase"))

## ----load_data-----------------------------------------------------------
con=url("http://bowtie-bio.sourceforge.net/recount/ExpressionSets/bodymap_eset.RData")
load(file=con)
close(con)

## ----expression_set------------------------------------------------------
bm = bodymap.eset
bm

## ----expression_data-----------------------------------------------------
exp_data = exprs(bm)
dim(exp_data)
head(exp_data,n=5)

## ----pheno_data----------------------------------------------------------
pheno_data = pData(bm)
dim(pheno_data)
head(pheno_data)

## ----feature_data--------------------------------------------------------
feature_data = fData(bm)
dim(fData(bodymap.eset))
fData(bodymap.eset)[1:10,,1]

## ----session_info--------------------------------------------------------
devtools::session_info()

