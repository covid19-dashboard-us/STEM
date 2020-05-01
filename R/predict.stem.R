predict.stem <- function(mfit, dat.pred = list(), adjust = FALSE, aq = c(0.001,0.999)) {

  # one head prediction
  mpred <- predict(mfit, dat.pred, type = "response", se.fit = TRUE)

  # se
  mpred.se = mpred$se.fit

  # adjust value
  if(adjust == TRUE){
    index = beta.check(mfit, aq = aq)
    mpred = beta.adjust(mfit, dat.pred, index)
  }else{
    mpred = as.matrix(mpred$fit)
  }

  list(mpred = mpred, mpred.se = mpred.se)
}
