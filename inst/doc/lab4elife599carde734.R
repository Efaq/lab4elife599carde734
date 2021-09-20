## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=FALSE--------------------------------------------------------
library(lab4elife599carde734)

## -----------------------------------------------------------------------------
data("iris")
my_linreg_object = linreg(Petal.Length ~ Petal.Width + Sepal.Length + Sepal.Width, data = iris)

## -----------------------------------------------------------------------------
my_linreg_object$print()

## -----------------------------------------------------------------------------
my_linreg_object$summary()

## -----------------------------------------------------------------------------
head(my_linreg_object$resid())
head(my_linreg_object$pred())
my_linreg_object$coef()

## -----------------------------------------------------------------------------
my_linreg_object$plot()

