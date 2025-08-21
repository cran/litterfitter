## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(litterfitter)

## ----results="hide",warning=FALSE,message = FALSE-----------------------------
fit <- fit_litter(time=c(0,1,2,3,4,5,6),
                  mass.remaining =c(1,0.9,1.01,0.4,0.6,0.2,0.01),
                  model="weibull",
                  iters=500)

class(fit)

## ----fig.height=6,results='hide',fig.keep=TRUE,warning=FALSE,message = FALSE----
plot_multiple_fits(time=c(0,1,2,3,4,5,6),
                   mass.remaining=c(1,0.9,1.01,0.4,0.6,0.2,0.01),
                   model=c("neg.exp","weibull"),
                   iters=500)

## ----fig.keep=TRUE------------------------------------------------------------
   plot(fit)

## ----echo=FALSE,fig.keep=TRUE-------------------------------------------------
   summary(fit)

## ----echo=FALSE,fig.keep=TRUE-------------------------------------------------
   post<-bootstrap_parameters(fit)
   plot(post)

