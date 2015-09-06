## ------------------------------------------------------------------------
library(devtools)

## ---- eval=FALSE---------------------------------------------------------
#  install.packages("devtools")

## ------------------------------------------------------------------------
## Color scheme inspired by the RSkittleBrewer package
## https://github.com/alyssafrazee/RSkittleBrewer
tropical=  c('darkorange', 'dodgerblue', 'hotpink', 'limegreen', 'yellow')
palette(tropical)
par(pch=19)

## ----global_options,warning=FALSE,message=FALSE--------------------------
## see ch. 10 Hooks of Xie's knitr book
library(knitr)
knit_hooks$set(setPch = function(before, options, envir) {
  if(before) par(pch = 19)
})
opts_chunk$set(setPch = TRUE)

## ----global-plot,warning=FALSE, message=FALSE----------------------------
knitr::opts_chunk$set(fig.width=5, fig.height=5, size="footnotesize",
                      warning=FALSE, message=FALSE)
knitr::knit_hooks$set(small.mar = function(before, options, envir) {
  if (before) graphics::par(mar = c(5,5,1.5,1))
})

## ----chunk1--------------------------------------------------------------
x = rnorm(100)
plot(x,col=3)

## ----chunk2, fig.height=5, fig.align="center"----------------------------
x = rnorm(100)
plot(x,col=3,pch=19)

## ----chunk4, echo=FALSE--------------------------------------------------
x = rnorm(100)
plot(x,col=3,pch=19)

## ----longtime, cache=FALSE-----------------------------------------------
#Sys.sleep(10)

## ----session_info--------------------------------------------------------
sessionInfo()
devtools::session_info()

