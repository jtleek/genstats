## ----style, echo = FALSE, results = 'asis'-------------------------------
BiocStyle::markdown()

## ----global_options, include=FALSE---------------------------------------
knitr::opts_chunk$set(warning=FALSE, message=FALSE)
palette(RSkittleBrewer::RSkittleBrewer("tropical"))

## ---- eval=FALSE---------------------------------------------------------
#  devtools::install_github("alyssafrazee/RSkittleBrewer")

## ----pretty--------------------------------------------------------------
library(RSkittleBrewer)
# Make the colors pretty
trop = RSkittleBrewer("tropical")
palette(trop)

## ------------------------------------------------------------------------
library(Biobase)
con=url("http://bowtie-bio.sourceforge.net/recount/ExpressionSets/bodymap_eset.RData")
load(file=con)
close(con)
bm = bodymap.eset
pdata=pData(bm)
edata=exprs(bm)
fdata = fData(bm)
ls()

## ------------------------------------------------------------------------
table(pdata$gender)
table(pdata$gender,pdata$race)

## ------------------------------------------------------------------------
summary(edata)

## ------------------------------------------------------------------------
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


## ------------------------------------------------------------------------
dim(fdata)
dim(pdata)
dim(edata)

## ------------------------------------------------------------------------
boxplot(log2(edata+1),col=2,range=0)

## ------------------------------------------------------------------------
library(dplyr)
edata = as.data.frame(edata)
filt_edata = filter(edata,rowMeans(edata) > 1)
boxplot(log2(filt_edata+1),col=2)

## ----eval=FALSE----------------------------------------------------------
#  source("http://bioconductor.org/biocLite.R")
#  biocLite("org.Hs.eg.db")

## ------------------------------------------------------------------------
library(org.Hs.eg.db)
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


