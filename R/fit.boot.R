#' SCB via bootstrap for Spatio-Temporal Epidemic Model (STEM)
#'
#' @import mgcv
#'
#' @return

fit.boot <- function(iter, mfit.stem, cov.infec, cov.death,
  dat = list(), date.est, est.h, R.rate, file.path){
  cov.names = union(cov.infec, cov.death)
  mfit.infec = mfit.stem$mfit.stem.infec
  mfit.death = mfit.stem$mfit.stem.death
  b1.type = mfit.stem$b1.type
  cov.type = mfit.stem$cov.type
  cat('Bootstrap Sample:', iter, '\r')

  # generate bootstrap sample
  dat.b = data.boot(seed.iter = iter, mfit.infec = mfit.infec, mfit.death = mfit.death,
    dat = dat, date.start = date.est - est.h, R.rate = R.rate, est.h = est.h, cov.names = cov.names)

  dat.boot = dat
  # update arguments
  dat.fit.boot = dat$dat.fit
  dat.fit.boot$y.infec = dat.b$y.infec
  dat.fit.boot$y.death = dat.b$y.death
  dat.fit.boot$S.ratio = dat.b$S.ratio
  dat.fit.boot$BQ2_all.X.infec = dat.b$BQ2_all.X.infec
  dat.fit.boot$BQ2_all.X.death = dat.b$BQ2_all.X.death

  dat.boot$dat.fit = dat.fit.boot

  # fit model
  mfit.boot = fit.stem(cov.infec, cov.death, dat.boot, b1.type, cov.type)
  mfit.infec.boot <- mfit.boot$mfit.stem.infec
  mfit.death.boot <- mfit.boot$mfit.stem.death

  # output: temrs and theta
  terms.infec.b <- Terms.Fit(mfit = mfit.infec.boot, dat = dat.boot, cov.names)
  terms.death.b <- Terms.Fit(mfit = mfit.death.boot, dat = dat.boot, cov.names)
  theta.infec.b <- mfit.infec.boot$family$getTheta()
  theta.death.b <- mfit.death.boot$family$getTheta()

  A = list(terms.infec.b = terms.infec.b,
       terms.death.b = terms.death.b,
       theta.infec.b = theta.infec.b,
       theta.death.b = theta.death.b)

  save(file = paste0(file.path, iter, '.RData'), A)
  return(A)
}
