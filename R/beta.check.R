beta.check <- function(mfit, BQ2, b1.type = "cons", aq = c(0.001, 0.999)){
  nq = ncol(BQ2)
  if(b1.type == "vary"){
    coef.B1 = mfit$coefficients[1:nq]
    coef.B2 = mfit$coefficients[(nq + 1) : (2 * nq)]
    beta0 = BQ2 %*% coef.B1
    beta1 = BQ2 %*% coef.B2
    index.3 = which(beta1 < 0)
    index.4 = which(beta1 > 1)
  }else{
    coef.B1 = mfit$coefficients[1:nq]
    beta0 = BQ2 %*% coef.B1
    index.3 = NULL
    index.4 = NULL
  }
  thrd1 = quantile(beta0, probs = min(aq))
  thrd2 = quantile(beta0, probs = max(aq))
  index.1 = which(beta0 < thrd1)
  index.2 = which(beta0 > thrd2)

  list(index.1 = index.1, index.2 = index.2,
    index.3 = index.3, index.4 = index.4,
    thrd1 = thrd1, thrd2 = thrd2)
}
