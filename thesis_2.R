#NOTES
#the difference between the years is put in the t+1 cell (instead of t cell) !?
source("BVARBGR.R")
load("macro1.RData")
data <- data[, -21] #subtracting stat discrepancy
library(lars)
library(vars)
library(forecast)
library(MASS)
data0 <- data[-1,]
for (i in 3:ncol(data)) {
  data0[, i] <- diff(log(data[, i]))
}
# data made stationary-like
# models
# RW, BVAR, VAR, ARMA
# figure out a simple DFM model??

T = nrow(data0)
m = ncol(data0)
dimnames = c("GNPK", "CPIIT", "X3MBOT")
# forecasts matrices
fcst.results <- array(0, dim = c(5, 3, 8, T-40), dimnames = list(c("RW", "BVAR", "VAR", "ARMA", "???"), c("GNP", "infl", "3MBoT"), c("1p ahead", "2p ahead", "3p ahead", "4p ahead", "5p ahead", "6p ahead", "7p ahead", "8p ahead"), data0[41:T, 1]))
fcst.BVAR <- array(0, dim = c(3, 8, T-40), dimnames = list(c("GNP", "infl", "3MBoT"), c("1p ahead", "2p ahead", "3p ahead", "4p ahead", "5p ahead", "6p ahead", "7p ahead", "8p ahead"), data0[41:T, 1]))
fcst.RW <- array(0, dim = c(3, 8, T-40), dimnames = list(c("GNP", "infl", "3MBoT"), c("1p ahead", "2p ahead", "3p ahead", "4p ahead", "5p ahead", "6p ahead", "7p ahead", "8p ahead"), data0[41:T, 1]))
fcst.VAR <- array(0, dim = c(3, 8, T-40), dimnames = list(c("GNP", "infl", "3MBoT"), c("1p ahead", "2p ahead", "3p ahead", "4p ahead", "5p ahead", "6p ahead", "7p ahead", "8p ahead"), data0[41:T, 1]))
fcst.ARMA <- array(0, dim = c(3, 8, T-40), dimnames = list(c("GNP", "infl", "3MBoT"), c("1p ahead", "2p ahead", "3p ahead", "4p ahead", "5p ahead", "6p ahead", "7p ahead", "8p ahead"), data0[41:T, 1]))
#fcst.DFM?????

# fill the forecast matrices with rolling window forecasts
#RW
for (i in 1:3) {
  for (ii in 1:8) {
    for (iii in 1:(T-40)) {
      fcst.RW[i, ii, iii] <- data0[(40+iii-ii), dimnames[i]]
    }
  }
}
#BVAR (do it only once) !! rolling winndow (ask if applicable to BVAR) !! lambda choice
for (ii in 1:8) {
  for (iii in 1:(T-40)) {
    fcst.BVAR[, ii, iii] <- BVARBGR(data0[(iii:(iii+40-ii)), c(-1,-2)], 5, lambda = 0.03, predictive = F)$fcast[ii, dimnames]
  }
}
# VAR 3variables
for (ii in 1:8) {
  for (iii in 1:(T-40)) {
    for (i in 1:3) {
      fcst.VAR[i, ii, iii] <- predict(VAR(data0[(iii:(iii+40-ii)), c(3, 22, 23)], p = 5), n.ahead = 8)$fcst[[i]][ii, 1]
    }
  }
}
# ARMA (already integrated)
for (i in 1:3) {
  for (ii in 1:8) {
    for (iii in 1:(T-40)) {
      fcst.ARMA[i, ii, iii] <- predict(auto.arima(data0[(iii:(iii+40-ii)), c(3, 22, 23)][, i]), n.ahead = 8)$pred[ii]
    }
  }
}
# ?????
fcst.results[1, , , ] <- fcst.RW
fcst.results[2, , , ] <- fcst.BVAR
fcst.results[3, , , ] <- fcst.VAR
fcst.results[4, , , ] <- fcst.ARMA

fcst.errs <- array(0, dim = c(5, 3, 8, T-40), dimnames = list(c("RW", "BVAR", "VAR", "ARMA", "???"), c("GNP", "infl", "3MBoT"), c("1p ahead", "2p ahead", "3p ahead", "4p ahead", "5p ahead", "6p ahead", "7p ahead", "8p ahead"), data0[41:T, 1]))
#calculate SFEs for models and squared residuals errors
# SFEs
for (i in 1:4) {
  for (ii in 1:3) {
    for (iii in 1:8) {
      for (iiii in 1:(T-40)){
        fcst.errs[i, ii, iii, iiii] <- (fcst.results[i, ii, iii, iiii] - data0[40 + iiii, dimnames[ii]])^2
      }
    }
  }
}

msfe.all <- apply(fcst.errs, c(1,2,3), mean) # check if correct!!!! returns mean over methods, variables and n.ahead forecasts
