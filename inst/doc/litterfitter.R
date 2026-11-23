## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(litterfitter)
set.seed(1)

## ----results="hide",warning=FALSE,message = FALSE-----------------------------
fit <- fit_litter(
  time = c(0, 1, 2, 3, 4, 5, 6),
  mass.remaining = c(1, 0.9, 1.01, 0.4, 0.6, 0.2, 0.01),
  model = "weibull",
  iters = 500
)

class(fit)

## ----fig.height=6,results='hide',fig.keep=TRUE,warning=FALSE,message = FALSE----
plot_multiple_fits(
  time = c(0, 1, 2, 3, 4, 5, 6),
  mass.remaining = c(1, 0.9, 1.01, 0.4, 0.6, 0.2, 0.01),
  model = c("neg.exp", "weibull"),
  iters = 500
)

## ----fig.keep=TRUE------------------------------------------------------------
plot(fit)

## ----echo=FALSE,fig.keep=TRUE-------------------------------------------------
summary(fit)

## ----fig.keep=TRUE,warning=FALSE----------------------------------------------
post <- bootstrap_parameters(fit, nboot = 500)
plot(post)

## -----------------------------------------------------------------------------
summary(post)

## -----------------------------------------------------------------------------
fit
coef(fit)
residuals(fit)
predict(fit, newdata = c(7, 8, 9))

## -----------------------------------------------------------------------------
time_to_prop_mass_remaining(fit, threshold.mass = 0.5)
steady_state(fit)

## ----warning=FALSE------------------------------------------------------------
data(pineneedles)
fits <- fit_litter_all(
  time = pineneedles$Year,
  mass.remaining = pineneedles$Mass.remaining,
  models = c("neg.exp", "weibull", "discrete.parallel", "cont.quality"),
  iters = 500
)
fits
glance(fits)[, c("fit", "nparams", "logLik", "AICc", "delta_AICc", "time_to_50")]

## ----fig.height = 4, warning = FALSE------------------------------------------
boot_neg <- bootstrap_parameters(fits$neg.exp, nboot = 500)
plot(fits$neg.exp, boot = boot_neg)
plot(fits)

## ----fig.height = 5-----------------------------------------------------------
plot(boot_neg, coef.index = "all")

## ----eval = requireNamespace("ggplot2", quietly = TRUE), fig.height = 4-------
library(ggplot2)
autoplot(fits$neg.exp, boot = boot_neg)
autoplot(fits) + theme_bw()
autoplot(boot_neg)

## -----------------------------------------------------------------------------
best <- fits$neg.exp
boot <- bootstrap_parameters(best, nboot = 500)
tidy(best, boot = boot)
grid <- augment(best, newdata = seq(0, 5, length.out = 100), boot = boot)
head(grid)

## ----eval = requireNamespace("ggplot2", quietly = TRUE), fig.height = 4-------
library(ggplot2)
ggplot(grid, aes(time)) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), fill = "grey85") +
  geom_line(aes(y = .fitted)) +
  geom_point(data = augment(best), aes(y = mass)) +
  labs(x = "Time (years)", y = "Proportion mass remaining")

## ----eval = requireNamespace("ggplot2", quietly = TRUE), fig.height = 4-------
curves <- augment(fits, newdata = seq(0, 5, length.out = 100))
ggplot(curves, aes(time, .fitted, colour = fit)) +
  geom_line() +
  geom_point(data = augment(best), aes(y = mass), colour = "black") +
  labs(x = "Time (years)", y = "Proportion mass remaining", colour = "Model")

## -----------------------------------------------------------------------------
efit <- fit_element(
  mass.remaining = pineneedles$Mass.remaining,
  element.remaining = c(1.1, 1.2, 1.3, 1, 0.8, 0.6),
  iters = 500, quiet = TRUE
)
efit

## ----fig.height = 4-----------------------------------------------------------
plot(efit)

## ----warning = FALSE----------------------------------------------------------
glance(efit)[, c("model", "AICc", "peak_element", "mass_at_peak")]
eboot <- bootstrap_parameters(efit, nboot = 500)
summary(eboot)

