# the optimal lambda choice
source("BVARBGR.R")
load("macro1.RData")
data <- data[, -21]
data0 <- data[-1,]
for (i in 3:(ncol(data)-1)) {
  data0[, i] <- diff(log(data[, i]))
}
data0[, 24] <- data0[, 24]/100

T = nrow(data0)
m = ncol(data0)
dimnames = c("GNPK", "CPIIT", "X3MBOT")
h.names = c("1p ahead", "2p ahead", "3p ahead", "4p ahead", "5p ahead", "6p ahead", "7p ahead", "8p ahead")


fcst.BVAR <- array(0, dim = c(3, 8, T-40), dimnames = list(dimnames, h.names, data0[41:T, 1]))
sfe.BVAR <- array(0, dim = c(3, 8, T-40), dimnames = list(dimnames, h.names, data0[41:T, 1]))
lambda.msfe <- array(0, dim = c(100, 3, 8), dimnames = list((1:100)/1000, dimnames, h.names))

for (l in 1:100) {
for (ii in 1:8) {
  for (iii in 1:(T-40)) {
    fcst.BVAR[, ii, iii] <- BVARBGR(data0[(iii:(iii+40-ii)), c(-1,-2)], 5, lambda = l/1000, predictive = F)$fcast[ii, dimnames]
  }
}



for (i in 1:3) {
  for (ii in 1:8) {
    for (iii in 1:(T-40)){
      sfe.BVAR[i, ii, iii] <- (fcst.BVAR[i, ii, iii] - data0[40+iii, dimnames[i]])^2
    }
  }
}

lambda.msfe[l, , ] <- apply(sfe.BVAR, c(1, 2), mean)
}
# save the results!!!!!!!!!!!!

