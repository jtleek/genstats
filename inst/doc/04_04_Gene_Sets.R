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
  library(goseq)
  library(DESeq2)
})

## ----load----------------------------------------------------------------
  library(devtools)
  library(Biobase)
  library(goseq)
  library(DESeq2)

## ----install_packages, eval=FALSE----------------------------------------
#  install.packages(c("devtools","MatrixEQTL"))
#  source("http://www.bioconductor.org/biocLite.R")
#  biocLite(c("Biobase","goseq","DESeq2"))

## ------------------------------------------------------------------------
supportedGenomes()
supportedGeneIDs()

## ------------------------------------------------------------------------
temp_data =read.table(system.file("extdata","Li_sum.txt",
                                     package="goseq"),sep="\t",
                                     header=TRUE,
                                     stringsAsFactors=FALSE)
expr= temp_data[,-1]
rownames(expr) = temp_data[,1]
expr = expr[rowMeans(expr) > 5,]
grp=factor(rep(c("Control","Treated"),times=c(4,3)))
pdata  = data.frame(grp)

## ------------------------------------------------------------------------
de = DESeqDataSetFromMatrix(expr, pdata, ~grp)
de_fit = DESeq(de)
de_results = results(de_fit)

## ------------------------------------------------------------------------
genes = as.integer(de_results$padj < 0.05)
not_na = !is.na(genes)
names(genes) = rownames(expr)
genes = genes[not_na]

## ------------------------------------------------------------------------
head(supportedGenomes(),n=12)[,1:4]

## ------------------------------------------------------------------------
pwf=nullp(genes,"hg19","ensGene")
head(pwf)

## ------------------------------------------------------------------------
GO.wall=goseq(pwf,"hg19","ensGene")
head(GO.wall)

## ------------------------------------------------------------------------
GO.MF=goseq(pwf,"hg19","ensGene",test.cats=c("GO:MF"))
head(GO.MF)

## ----session_info--------------------------------------------------------
devtools::session_info()

