## ----style, results = 'asis'---------------------------------------------
BiocStyle::markdown()

## ----global_options------------------------------------------------------
## see ch. 10 Hooks of Xie's knitr book
knit_hooks$set(setPch = function(before, options, envir) {
  if(before) par(pch = 19)
})
opts_chunk$set(setPch = TRUE)

## ----chunk1--------------------------------------------------------------
x = rnorm(100)
plot(x,col=3)

## ----chunk2, fig.height=5, fig.align="center"----------------------------
x = rnorm(100)
plot(x,col=3,pch=19)

## ----chunk4, echo=FALSE--------------------------------------------------
x = rnorm(100)
plot(x,col=3,pch=19)

## ----session_info--------------------------------------------------------
sessionInfo()
devtools::session_info()

