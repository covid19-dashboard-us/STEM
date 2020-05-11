#' Plugin prediction for Spatio-Temporal Epidemic Model (STEM)
#'
#' @import mgcv
#'
plugin.predict <- function(mfit.stem, dat = list(), cov.names,
  date.est = date.est, R.rate = 0.1, pred.h = 7, aq = c(0.001,0.999)){

  Cases_sub = dat$Cases_sub
  BQ2.infec = dat$BQ2.infec
  BQ2.death = dat$BQ2.death
  mfit.infec = mfit.stem$mfit.stem.infec
  mfit.death = mfit.stem$mfit.stem.death
  b1.type = mfit.stem$b1.type
  # adjust extreme beta value
  index.infec = beta.check(mfit.infec, BQ2 = BQ2.infec, b1.type, aq = aq)
  index.death = beta.check(mfit.death, BQ2 = BQ2.death, "cons", aq = aq)

  # one step head prediction
  I_new = Cases_sub[,paste0('I_', date.est)]
  S.ratio = as.matrix(
    1 - Cases_sub[,paste0('P_', date.est)]/Cases_sub$population
  )
  new.data <- list(
    I = I_new, lI = log(I_new+1), S.ratio = S.ratio,
    Control2 = Cases_sub[, paste0("C2_", as.Date(date.est) + 1)],
    Control1 = Cases_sub[, paste0("C1_", as.Date(date.est) + 1)],
    BQ2_all.infec = as.matrix(BQ2.infec),
    BQ2_all.death = as.matrix(BQ2.death),
    BQ2_all.X.infec = as.matrix(
      kr(log(as.matrix(I_new) + 1), BQ2.infec, byrow = T)),
    BQ2_all.X.death = as.matrix(
      kr(log(as.matrix(I_new) + 1), BQ2.death, byrow = T))
  )
  p <- length(new.data)
  tmp01 = names(new.data)
  for (iter in (p+1):(p+length(cov.names))) {
    new.data[[iter]] <- Cases_sub[,cov.names[iter-p]]
  }
  names(new.data) <- c(tmp01, cov.names)

  # prediction
  mpred.infec.pred <- predict(mfit.infec, new.data,
    type = "response", se.fit = TRUE)
  mpred.death.pred <- predict(mfit.death, new.data,
    type = "response", se.fit = TRUE)

  # se
  mpred.infec.se = mpred.infec.pred$se.fit
  mpred.death.se = mpred.death.pred$se.fit

  # adjust value
  mpred.infec = mean.adjust(mfit.infec, new.data, index.infec,
    b1.type, adj.type = "infec")
  mpred.death = mean.adjust(mfit.death, new.data, index.death,
    "cons", adj.type = "death")

  # output: elements for one-day ahead prediction interval.
  I_new = Cases_sub[, paste0('P_', date.est)]
  P.pred.iterval = data.frame(ID = Cases_sub$ID, County = Cases_sub$County,
    State = Cases_sub$State, predicted.new = mpred.infec + I_new,
    predicted.new.CI.Upper = mpred.infec.se^2,
    predicted.new.CI.Lower = mpred.infec, I = I_new)

  D_new = Cases_sub[,paste0('D_', date.est)]
  D.pred.iterval = data.frame(ID = Cases_sub$ID, County = Cases_sub$County,
    State = Cases_sub$State, predicted.new = mpred.death + D_new,
    predicted.new.CI.Upper = mpred.death.se^2,
    predicted.new.CI.Lower = mpred.death,
                              I = D_new)

  I_update = I_new
  for(iter in 1:pred.h) {
    cat("Prediction for ", iter, " ahead.\n")
    Y.new.infec = mpred.infec
    Y.new.death = mpred.death

    # add I(t)+D(t)
    Cases_sub[, paste0("P_", as.Date(date.est) + iter)] =
      Y.new.infec +  Cases_sub[, paste0("P_", as.Date(date.est) + iter - 1)]
    Cases_sub[, paste0("R_", as.Date(date.est) + iter)] =
      I_update * R.rate + Cases_sub[, paste0('R_', as.Date(date.est) + iter - 1)]
    Cases_sub[,paste0('D_',as.Date(date.est) + iter)] <- Y.new.death + Cases_sub[, paste0('D_',as.Date(date.est) + iter - 1)]
    I_update = Cases_sub[, paste0("P_", as.Date(date.est) + iter)] -
      Cases_sub[, paste0("R_", as.Date(date.est) + iter)] -
      Cases_sub[, paste0('D_', as.Date(date.est) + iter)]
    I_update[I_update < 0] <- 0
    Cases_sub[, paste0("I_", as.Date(date.est) + iter)] = I_update

    I_new = I_update
    S.ratio = as.matrix(
      1 - Cases_sub[,paste0('P_', as.Date(date.est) + iter)]/Cases_sub$population
    )
    S.ratio [S.ratio < 1e-6] = 1e-6
    new.data <- list(
      I = as.matrix(I_new), lI = log(as.numeric(I_new)+1), S.ratio = S.ratio,
      Control2 = Cases_sub[, paste0("C2_", as.Date(date.est) + iter)], # fixed
      Control1 = Cases_sub[, paste0("C1_", as.Date(date.est) + iter)],
      BQ2_all.infec = as.matrix(BQ2.infec),
      BQ2_all.death = as.matrix(BQ2.death),
      BQ2_all.X.infec = as.matrix(kr(log(as.matrix(I_new) + 1), BQ2.infec, byrow = T)),
      BQ2_all.X.death = as.matrix(kr(log(as.matrix(I_new) + 1), BQ2.death, byrow = T))
    )
    tmp01 = names(new.data)
    p <- length(new.data)
    for (j in (p+1):(p+length(cov.names))) {
      new.data[[j]] <- Cases_sub[,cov.names[j-p]]
    }
    names(new.data) <- c(tmp01,cov.names)

    # prediction
    # mpred.gsvcm.infec <- predict(result.gsvcm.infec, new.data,
    #   type = "response", se.fit = FALSE)
    # mpred.gsvcm.death <- predict(result.gsvcm.death, new.data,
    #   type = "response", se.fit = FALSE)
    mpred.infec = mean.adjust(mfit.infec, new.data, index.infec, b1.type, adj.type = "infec")
    mpred.death = mean.adjust(mfit.death, new.data, index.death, "cons", adj.type = "death")
  }

  p.names = paste0('P_', as.Date(date.est) + 1:pred.h)
  P.pred = Cases_sub[, c('ID', 'State', 'County', p.names)]

  d.names = paste0('D_', as.Date(date.est) + 1:pred.h)
  D.pred = Cases_sub[, c('ID', 'State', 'County', d.names)]

  list(P.pred = P.pred, D.pred = D.pred,
       P.pred.iterval = P.pred.iterval, D.pred.iterval = D.pred.iterval)
}
