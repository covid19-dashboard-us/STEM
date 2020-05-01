predict.rolling.stem <- function(mfit.infec, mfit.death, data.new, cov.names,
  R.rate = 0.1, pred.h = 7, aq = c(0.001,0.999)) {

  I_new = dat.pred[, paste0('I_', as.character(date.est))]
  S.ratio = as.matrix(
    1 - dat.pred[,paste0('P_', as.character(date.est))]/dat.pred$population
  )
  dat.pred.infec <- list(
    I = I_new, lI = log(I_new+1), S.ratio = S.ratio,
    Control2 = dat.pred[, paste0("C2_", as.Date(date.est) + 1)],
    Control1 = dat.pred[, paste0("C1_", as.Date(date.est) + 1)],
    beta0 = as.matrix(mfit.infec$BQ2),
    beta1 = as.matrix(kr(log(as.matrix(I_new) + 1),
      as.matrix(mfit.infec$BQ2), byrow = T))
  )
  p <- length(dat.pred.infec)
  tmp01 = names(dat.pred.infec)
  for (iter in (p+1):(p+length(cov.names))) {
    dat.pred.infec[[iter]] <- dat.pred[,cov.names[iter-p]]
  }
  names(dat.pred.infec) <- c(tmp01, cov.names)

  dat.pred.death <- list(
    I = I_new, lI = log(I_new+1), S.ratio = S.ratio,
    Control2 = dat.pred[, paste0("C2_", as.Date(date.est) + 1)],
    Control1 = dat.pred[, paste0("C1_", as.Date(date.est) + 1)],
    beta0 = as.matrix(mfit.death$BQ2)
  )
  p <- length(dat.pred.death)
  tmp01 = names(dat.pred.death)
  for (iter in (p+1):(p+length(cov.names))) {
    dat.pred.death[[iter]] <- dat.pred[,cov.names[iter-p]]
  }
  names(dat.pred.death) <- c(tmp01, cov.names)

  # prediction
  mpred.infec <- predict(mfit.infec, dat.pred.infec,
    type = "response", se.fit = TRUE)
  mpred.death <- predict(mfit.death, dat.pred.death,
    type = "response", se.fit = TRUE)

  # se
  mpred.infec.se = mpred.infec$se.fit
  mpred.death.se = mpred.death$se.fit

  # adjust value
  index.infec = beta.check(mfit.infec, aq = aq)
  mpred.infec = beta.adjust(mfit.infec, dat.pred.infec, index.infec)
  mpred.death = as.matrix(mpred.death$fit)

  # rolling prediction
  I_new = dat.pred[,paste0('P_', as.Date(date.est))]
  D_new = dat.pred[,paste0('D_', as.Date(date.est))]
  I_update = I_new
  for(iter in 1:pred.h) {
    Y.new.infec = mpred.infec
    Y.new.death = mpred.death

    dat.pred[, paste0("P_", as.Date(date.est) + iter)] =
      Y.new.infec +  dat.pred[, paste0("P_", as.Date(date.est) + iter - 1)]
    dat.pred[, paste0("R_", as.Date(date.est) + iter)] =
      I_update * R.rate + dat.pred[, paste0('R_', as.Date(date.est) + iter - 1)]
    dat.pred[, paste0('D_', as.Date(date.est) + iter)] =
      Y.new.death + dat.pred[, paste0('D_',as.Date(date.est) + iter - 1)]
    I_update = dat.pred[, paste0("P_", as.Date(date.est) + iter)] -
      dat.pred[, paste0("R_", as.Date(date.est) + iter)] -
      dat.pred[, paste0('D_', as.Date(date.est) + iter)]
    I_update[I_update < 0] <- 0
    dat.pred[, paste0("I_", as.Date(date.est) + iter)] = I_update

    I_new = as.vector(I_update)
    S.ratio = as.vector(1 - dat.pred[,paste0('P_', as.Date(date.est) + iter)]/dat.pred$population)
    S.ratio [S.ratio < 1e-6] = 1e-6
    dat.pred.infec <- list(
      I = I_new, lI = log(I_new+1), S.ratio = S.ratio,
      Control2 = dat.pred[, paste0("C2_", as.Date(date.est) + iter)], # fixed
      Control1 = dat.pred[, paste0("C1_", as.Date(date.est) + iter)],
      beta0 = as.matrix(mfit.infec$BQ2),
      beta1 = as.matrix(kr(log(as.matrix(I_new) + 1),
                           as.matrix(mfit.infec$BQ2), byrow = T))
    )
    tmp01 = names(dat.pred.infec)
    p <- length(dat.pred.infec)
    for (j in (p+1):(p+length(cov.names))) {
      dat.pred.infec[[j]] <- dat.pred[,cov.names[j-p]]
    }
    names(dat.pred.infec) <- c(tmp01,cov.names)

    dat.pred.death <- list(
      I = I_new, lI = log(I_new+1), S.ratio = S.ratio,
      Control2 = dat.pred[, paste0("C2_", as.Date(date.est) + 1)],
      Control1 = dat.pred[, paste0("C1_", as.Date(date.est) + 1)],
      beta0 = as.matrix(mfit.death$BQ2)
    )
    p <- length(dat.pred.death)
    tmp01 = names(dat.pred.death)
    for (iter in (p+1):(p+length(cov.names))) {
      dat.pred.death[[iter]] <- dat.pred[,cov.names[iter-p]]
    }
    names(dat.pred.death) <- c(tmp01, cov.names)

    mpred.infec = beta.adjust(mfit.infec, dat.pred.infec, index.infec)
    mpred.death = as.matrix(predict(mfit.death, dat.pred.death,
      type = "response", se.fit = FALSE))
  }

  p.names = paste0('P_',as.Date(date.est)+1:pred.h)
  P.pred = dat.pred[, c('ID', 'State', 'County', p.names)]

  d.names = paste0('D_',as.Date(date.est)+1:pred.h)
  D.pred = dat.pred[, c('ID', 'State', 'County', d.names)]

  list(P.pred = P.pred, D.pred = D.pred)
}
