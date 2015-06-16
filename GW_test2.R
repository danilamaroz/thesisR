rm(list = ls())
load("fitted.errs2.RData")
load("msfe.all.RData")
load("fcst.errs.RData")


GWtest <- function(f.errs, fit.errs, modelf = 1, modelg = 2, variable = 1, h = 1, regq = 10) {
  # f and g = arrays of the squared forecasting models
  f <- f.errs[modelf, variable, h, ]
  g <- f.errs[modelg, variable, h, ]
  # f.fit and g.fit = matrices of prediction error differences between the models (quantity = regq)
  f.fit <- fit.errs[modelf, variable, h, , 1:regq]
  g.fit <- fit.errs[modelg, variable, h, , 1:regq]
  summary <- summary(lm((f-g)~(f.fit - g.fit)))
  
  # write the full code of giacomini test!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  n = 
  Tstat <- n*solve(n^-1)
  
  return(summary)
}

GWtest(fcst.errs, fitted.errs2, modelf = 1, modelg = 2, variable = 1, h = 1, regq = 10)

GWstat <- function(h =)
Z_bar <- 
Tst <- n*(t(Z_bar)*solve(Sigma_bar)*Z_bar)
