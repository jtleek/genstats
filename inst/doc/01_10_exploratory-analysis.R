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
  library(RSkittleBrewer)
  library(gplots)
  library(dplyr)
  library(AnnotationDbi)
})

## ----load----------------------------------------------------------------
  library(gplots)
  library(devtools)
  library(Biobase)
  library(RSkittleBrewer)
  library(org.Hs.eg.db)
  library(AnnotationDbi)

## ----install_packages, eval=FALSE----------------------------------------
#  install.packages(c("devtools","gplots"))
#  source("http://www.bioconductor.org/biocLite.R")
#  biocLite(c("Biobase","org.Hs.eg.db","AnnotationDbi"))
#  biocLite("alyssafrazee/RSkittleBrewer")

## ----pretty, eval=FALSE--------------------------------------------------
#  library(RSkittleBrewer)
#  # Make the colors pretty
#  trop = RSkittleBrewer("tropical")
#  palette(trop)
#  par(pch=19)

## ----load_data-----------------------------------------------------------
con = url("http://bowtie-bio.sourceforge.net/recount/ExpressionSets/bodymap_eset.RData")
load(file=con)
close(con)
bm = bodymap.eset
pdata=pData(bm)
edata=exprs(bm)
fdata = fData(bm)
ls()

## ----tables--------------------------------------------------------------
table(pdata$gender)
table(pdata$gender,pdata$race)

## ----summary-------------------------------------------------------------
summary(edata)

## ----missing-------------------------------------------------------------
# Use option useNA to include NA's in table
table(pdata$age,useNA="ifany")

# is.na checks for NA values
table(is.na(pdata$age))

# Check for other common missing names
sum(pdata$age==" ")

# Check genomic data for NAs
sum(is.na(edata))

# Make the distribution of NA's by genes
gene_na = rowSums(is.na(edata))
table(gene_na)

# Make the distribution of NA's by samples
sample_na = rowSums(is.na(edata))
table(sample_na)


## ----dimensions----------------------------------------------------------
dim(fdata)
dim(pdata)
dim(edata)

## ----boxplot-------------------------------------------------------------
boxplot(log2(edata+1),col=2,range=0)

## ----histograms----------------------------------------------------------
par(mfrow=c(1,2))
hist(log2(edata[,1]+1),col=2)
hist(log2(edata[,2]+1),col=2)

## ----densities-----------------------------------------------------------
plot(density(log2(edata[,1]+1)),col=2)
lines(density(log2(edata[,2]+1)),col=3)

## ------------------------------------------------------------------------
qqplot(log2(edata[,1]+1), log2(edata[,2]+1),col=3)

## ------------------------------------------------------------------------
mm = log2(edata[,1]+1) - log2(edata[,2]+1)
aa = log2(edata[,1]+1) + log2(edata[,2]+1)
plot(aa,mm,col=2)

## ------------------------------------------------------------------------
edata = as.data.frame(edata)
filt_edata = filter(edata,rowMeans(edata) > 1)
boxplot(as.matrix(log2(filt_edata+1)),col=2)

## ------------------------------------------------------------------------
aeid = as.character(fdata[,1])
chr = AnnotationDbi::select(org.Hs.eg.db,keys=aeid,keytype="ENSEMBL",columns="CHR")
head(chr)

## ------------------------------------------------------------------------
dim(chr)
dim(edata)
# Take non-duplicated chromsomes
chr = chr[!duplicated(chr[,1]),]

# Confirm that the annotation still is in the right order
all(chr[,1] == rownames(edata))

# Select the chromosome Y samples
edatay = dplyr::filter(edata,chr$CHR=="Y")

# Males have Y chromsome expression as expected
boxplot(colSums(edatay) ~ pdata$gender)
points(colSums(edatay) ~ jitter(as.numeric(pdata$gender)),
        col=as.numeric(pdata$gender),
        pch=19)


## ------------------------------------------------------------------------
ematrix = as.matrix(edata)[rowMeans(edata) > 10000,]
heatmap(ematrix)

## ------------------------------------------------------------------------
colramp = colorRampPalette(c(3,"white",2))(9)
heatmap(ematrix,col=colramp)

## ------------------------------------------------------------------------
heatmap(ematrix,col=colramp,Rowv=NA,Colv=NA)

## ------------------------------------------------------------------------
heatmap.2(ematrix,col=colramp,Rowv=NA,Colv=NA,
          dendrogram="none", scale="row",trace="none")

## ----session_info--------------------------------------------------------
devtools::session_info()

