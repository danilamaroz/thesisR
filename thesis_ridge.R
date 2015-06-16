rm(list=ls())
# Ridge lambda development path
load("old_macro_data.RData")
require(MASS)
require(ggplot2)
load("macro1.RData")
regs<-c(3:25)
data_old <- data
attach(data_old)

X1 <- as.matrix(data_old[-nrow(data_old), regs])
Y <- GNPK[-1]
# dimnames(data_old)[[2]][regs]

# matrix for the ridge coeff depending on lambda
ridge.coef <- matrix(0, 100, 23, dimnames = list(1:100, dimnames(data_old)[[2]][regs]))
ridge.coef <- as.data.frame(ridge.coef)
lm(GNPK ~ X1)
lm.ridge(Y ~ X1, lambda = 100)
plot(lm.ridge(Y ~ X1, lambda = 100)$coef)

for (i in 1:100) {
  ridge.coef[i, c(1, 2, 3)] <- lm.ridge(Y ~ X1[, c(1, 22, 23)], lambda = i*10)$coef
}

for (i in 1:1000) {
  ridge.coef[i, ] <- lm.ridge(Y ~ X1[, ], lambda = i*10)$coef
}

pl1 <- ggplot(data = ridge.coef, mapping = aes(x = 1:1000, y = ridge.coef[, 1]))
pl1 + geom_line() + theme_bw()
