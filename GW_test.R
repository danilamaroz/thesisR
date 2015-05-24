load("macro1.RData")
source("BVARBGR.R")
# data adjustment
data <- data[, -21]
data0 <- data[-1,]
for (i in 3:(ncol(data)-1)) {
  data0[, i] <- diff(log(data[, i]))
}

# create arrays for the values of predicted error for 30 last observations of the window
# dim = model, variable, h-step ahead, quantity of rolling windows, 20
T = nrow(data0)
m = ncol(data0)
dimnames = c("GNPK", "CPIIT", "X3MBOT")
modelnames = c("RW", "BVAR", "VAR", "ARIMA", "empty")
hnames = c("1p ahead", "2p ahead", "3p ahead", "4p ahead", "5p ahead", "6p ahead", "7p ahead", "8p ahead")
fitted.errs <- array(0, dim = c(5, 3, 8, T-40, 20), dimnames = c(modelnames, dimnames, hnames, data0[41:T, 1], 1:30))
# errors span from ones closer to the forecasting date to the ones further in time

for (i in 1:3) {
  for (ii in 1:8) {
    for (iii in 1:(T-40)) {
      # fcst.RW[i, ii, iii] <- data0[(40+iii-ii), dimnames[i]]
      for (iiii in 1:20){
        fitted.errs[1, i, ii, iii, iiii] <- data0[(40+iii-ii), dimnames[i]] - data0[(40+iii-iiii), dimnames[i]]        
      }
    }
  }
}
#BVAR (do it only once) !! rolling winndow (ask if applicable to BVAR) !! lambda choice
for (ii in 1:8) {
  for (iii in 1:(T-40)) {
    #fcst.BVAR[, ii, iii] <- BVARBGR(data0[(iii:(iii+40-ii)), c(-1,-2)], 5, lambda = 0.03, predictive = F)$fcast[ii, dimnames]
    # residuals = data - fitted 
    # residuals go from older ones to newer ones (opposite of what i need)
    # checked
    fitted.errs[2, , ii, iii, ] <- apply(BVARBGR(data0[(iii:(iii+40-ii)), c(-1,-2)], 5, lambda = 0.03, predictive = F)$residuals[, dimnames], 2, rev)[1:20, ]
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