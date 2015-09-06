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
  library(MatrixEQTL)
})

## ----load----------------------------------------------------------------
  library(devtools)
  library(Biobase)
  library(MatrixEQTL)

## ----install_packages, eval=FALSE----------------------------------------
#  install.packages(c("devtools","MatrixEQTL"))
#  source("http://www.bioconductor.org/biocLite.R")
#  biocLite(c("Biobase"))

## ------------------------------------------------------------------------
base.dir = find.package("MatrixEQTL")
SNP_file_name = paste(base.dir, "/data/SNP.txt", sep="");
expression_file_name = paste(base.dir, "/data/GE.txt", sep="")
covariates_file_name = paste(base.dir, "/data/Covariates.txt", sep="")
output_file_name = tempfile()

## ------------------------------------------------------------------------
expr = read.table(expression_file_name,sep="\t",
                  header=T,row.names=1)
expr[1,]

snps = read.table(SNP_file_name,sep="\t",
                  header=T,row.names=1)
snps[1,]

cvrt = read.table(covariates_file_name,sep="\t",
                  header=T,row.names=1)

## ------------------------------------------------------------------------
e1 = as.numeric(expr[1,])
s1 = as.numeric(snps[1,])
lm1 = lm(e1 ~ s1)
tidy(lm1)

## ------------------------------------------------------------------------
plot(e1 ~ jitter(s1),
     col=(s1+1),xaxt="n",xlab="Genotype",ylab="Expression")
axis(1,at=c(0:2),labels=c("AA","Aa","aa"))
lines(lm1$fitted ~ s1,type="b",pch=15,col="darkgrey")

## ------------------------------------------------------------------------
pvOutputThreshold = 1e-2
errorCovariance = numeric()
useModel = modelLINEAR

## ------------------------------------------------------------------------
snps = SlicedData$new()
snps$fileDelimiter = "\t"     # the TAB character
snps$fileOmitCharacters = "NA" # denote missing values;
snps$fileSkipRows = 1          # one row of column labels
snps$fileSkipColumns = 1       # one column of row labels
snps$fileSliceSize = 2000     # read file in pieces of 2,000 rows
snps$LoadFile( SNP_file_name )

## ------------------------------------------------------------------------
gene = SlicedData$new()
gene$fileDelimiter = "\t"      # the TAB character
gene$fileOmitCharacters = "NA" # denote missing values;
gene$fileSkipRows = 1          # one row of column labels
gene$fileSkipColumns = 1      # one column of row labels
gene$fileSliceSize = 2000      # read file in pieces of 2,000 rows
gene$LoadFile(expression_file_name)

## ------------------------------------------------------------------------
cvrt = SlicedData$new()

## ------------------------------------------------------------------------
me = Matrix_eQTL_engine(
    snps = snps,
    gene = gene,
    cvrt = cvrt,
    output_file_name = NULL,
    pvOutputThreshold = pvOutputThreshold,
    useModel = useModel, 
    errorCovariance = errorCovariance, 
    verbose = TRUE,
    pvalue.hist = TRUE,
    min.pv.by.genesnp = FALSE,
    noFDRsaveMemory = FALSE);

## ------------------------------------------------------------------------
plot(me)

## ------------------------------------------------------------------------
me$all$neqtls
me$all$eqtls

## ----session_info--------------------------------------------------------
devtools::session_info()

