beta.adjust <- function(mfit, new.data, index){
  mpred.terms <- predict(mfit, new.data,
    type = "terms", se.fit = FALSE)
  W = log(as.matrix(new.data$I) + 1)
  mpred.terms[index$index.1, 1] = index$thrd1
  mpred.terms[index$index.2, 1] = index$thrd2
  mpred.terms[index$index.3, 2] = 0
  mpred.terms[index$index.4, 2] = W[index$index.4]

  mpred.gsvcm = rowSums(mpred.terms)
  mpred.gsvcm = mpred.gsvcm + log(new.data$S.ratio)

  theta <- mfit$family$getTheta()
  p.gsvcm <- 1 - exp(-exp(theta[1] + exp(theta[2]) * mpred.gsvcm ))

  pred = p.gsvcm  * exp(mpred.gsvcm)
  return(pred)
}
