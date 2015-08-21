## ----style, echo = FALSE, results = 'asis'-------------------------------
BiocStyle::markdown()

## ----global_options, include=FALSE---------------------------------------
knitr::opts_chunk$set(warning=FALSE, message=FALSE)
palette(RSkittleBrewer::RSkittleBrewer("tropical"))

## ------------------------------------------------------------------------
firstName = "jeff"
class(firstName)
firstName

## ------------------------------------------------------------------------
heightCM = 188.2
class(heightCM)
heightCM

## ------------------------------------------------------------------------
numberSons = 1L
class(numberSons)
numberSons

## ------------------------------------------------------------------------
teachingCoursera = TRUE
class(teachingCoursera)
teachingCoursera

## ------------------------------------------------------------------------
heights = c(188.2, 181.3, 193.4)
heights

firstNames = c("jeff","roger","andrew","brian")
firstNames


## ------------------------------------------------------------------------
vector1 = c(188.2, 181.3, 193.4)
vector2 = c("jeff","roger","andrew","brian")
myList = list(heights=vector1,firstNames=vector2)
myList


## ------------------------------------------------------------------------
myMatrix = matrix(c(1,2,3,4),byrow=T,nrow=2)
myMatrix


## ------------------------------------------------------------------------
vector1 = c(188.2,181.3,193.4,192.3)
vector2 = c("jeff","roger","andrew","brian")
myDataFrame = data.frame(heights=vector1,firstNames=vector2)
myDataFrame

## ------------------------------------------------------------------------
smoker = c("yes","no","yes","yes")
smokerFactor = as.factor(smoker)
smokerFactor


## ------------------------------------------------------------------------
vector1 = c(188.2,181.3,193.4,NA)
vector1
is.na(vector1)

## ------------------------------------------------------------------------
vector1 = c(188.2,181.3,193.4,192.3)
vector2 = c("jeff","roger","andrew","brian")
myDataFrame = data.frame(heights=vector1,firstNames=vector2)

vector1[1]
vector1[c(1,2,4)]

## ------------------------------------------------------------------------
myDataFrame[1,1:2]
myDataFrame$firstNames


## ------------------------------------------------------------------------
myDataFrame[myDataFrame$firstNames=="jeff",]
myDataFrame[myDataFrame$heights < 190,]

## ------------------------------------------------------------------------
myHeightCM = 188

## ------------------------------------------------------------------------
my_height_cm = 188

## ------------------------------------------------------------------------
my.height.cm = 188

