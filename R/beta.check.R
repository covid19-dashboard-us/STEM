beta.check <- function(mfit, aq = c(0.001, 0.999)) {
  BQ2 = mfit$BQ2
  n.B = ncol(BQ2)
  coef.names1 = paste0("beta0", 1:n.B)
  coef.names2 = paste0("beta1", 1:n.B)
  coef.B1 = mfit$coefficients[coef.names1]
  coef.B2 = mfit$coefficients[coef.names2]

  beta0.hat = as.matrix(BQ2 %*% coef.B1)
  beta1.hat = as.matrix(BQ2 %*% coef.B2)

  thrd1 = quantile(beta0.hat, probs = min(aq))
  thrd2 = quantile(beta0.hat, probs = max(aq))

  index.1 = which(beta0.hat < thrd1)
  index.2 = which(beta0.hat > thrd2)
  index.3 = which(beta1.hat < 0)
  index.4 = which(beta1.hat > 1)

  list(index.1 = index.1, index.2 = index.2,
       index.3 = index.3, index.4 = index.4,
       thrd1 = thrd1, thrd2 = thrd2)
}
