plugin.correct <- function(terms.infec,
  terms.death, theta.infec, theta.death,
  date = date.est, dat, R.rate = 0.1, pred.h = 60) {

  Cases_sub = dat$Cases_sub
  all.names = names(dat$dat.fit)
  # one head prediction --------------------------------------------------------------
  I.new = Cases_sub[,paste0('I_', date)]
  S.ratio = as.matrix(
    1 - Cases_sub[,paste0('P_', date)]/Cases_sub$population
  )
  I.new.log = log(I.new + 1)

  # infectious
  if (ncol(terms.infec) == 2) {
    link.infec = terms.infec[, 2] * I.new.log + terms.infec[, 1] + log(S.ratio)
  } else {
    link.infec = terms.infec[, 2] * I.new.log + rowSums(terms.infec[, -2]) + log(S.ratio)
  }
  mpred.gsvcm.infec <- exp(link.infec)
  p.mpred.gsvcm.infec <- 1 - exp(-exp(theta.infec[1] + exp(theta.infec[2]) * link.infec ))

  # death
  if (ncol(terms.death) == 2) {
    link.death = terms.death[, 2] * I.new.log + terms.death[, 1]
  } else {
    link.death = terms.death[, 2] * I.new.log + rowSums(terms.death[, -2])
  }
  mpred.gsvcm.death <- exp(link.death)
  p.mpred.gsvcm.death <- 1 - exp(-exp(theta.death[1] + exp(theta.death[2]) * link.death ))
  mpred.gsvcm.death [mpred.gsvcm.death < 10e-4] = 10e-4
  p.mpred.gsvcm.death [p.mpred.gsvcm.death < 10e-4] = 10e-4
  # recover
  mpred.recover <- R.rate * I.new

  for(iter in 1:pred.h) {
    # cat('iter:', iter, '\n')
    Y.new = p.mpred.gsvcm.infec * mpred.gsvcm.infec
    Y.new.death = p.mpred.gsvcm.death * mpred.gsvcm.death
    # print(which(is.na(Y.new.death)))

    Y.new.recover = rpois(n = length(mpred.recover), mpred.recover)

    # add I(t)+D(t)
    Cases_sub[, paste0("P_", as.Date(date) + iter)] =
      Y.new +  Cases_sub[, paste0("P_", as.Date(date) + iter - 1)]
    Cases_sub[, paste0("R_", as.Date(date) + iter)] =
      Y.new.recover + Cases_sub[, paste0('R_', as.Date(date) + iter - 1)]
    Cases_sub[,paste0('D_',as.Date(date) + iter)] <- Y.new.death + Cases_sub[,paste0('D_',as.Date(date) + iter - 1)]
    I.update = Cases_sub[, paste0("P_", as.Date(date) + iter)] -
      Cases_sub[, paste0("R_", as.Date(date) + iter)] -
      Cases_sub[,paste0('D_',as.Date(date) + iter)]
    I.update[I.update < 0] <- 0
    Cases_sub[, paste0("I_", as.Date(date) + iter)] = I.update

    I.new = I.update
    S.ratio = as.matrix(
      1 - Cases_sub[,paste0('P_', as.Date(date) + iter)]/Cases_sub$population
    )
    S.ratio [S.ratio < 0.000001] = 0.000001
    I.new.log = log(I.new + 1)

    # infectious
    if (ncol(terms.infec) == 2) {
      link.infec = terms.infec[, 2] * I.new.log + terms.infec[, 1] + log(S.ratio)
    } else {
      link.infec = terms.infec[, 2] * I.new.log + rowSums(terms.infec[, -2]) + log(S.ratio)
    }
    mpred.gsvcm.infec <- exp(link.infec)
    p.mpred.gsvcm.infec <- 1 - exp(-exp(theta.infec[1] + exp(theta.infec[2]) * link.infec ))

    # death
    if (ncol(terms.death) == 2) {
      link.death = terms.death[, 2] * I.new.log + terms.death[, 1]
    } else {
      link.death = terms.death[, 2] * I.new.log + rowSums(terms.death[, -2])
    }
    mpred.gsvcm.death <- exp(link.death)
    p.mpred.gsvcm.death <- 1 - exp(-exp(theta.death[1] + exp(theta.death[2]) * link.death ))
    mpred.gsvcm.death [mpred.gsvcm.death < 10e-4] = 10e-4
    p.mpred.gsvcm.death [p.mpred.gsvcm.death < 10e-4] = 10e-4

    # recover
    mpred.recover <- R.rate * I.new
  }

  p.names = paste0('P_',as.Date(date)+1:pred.h)
  P.pred = Cases_sub[, c('ID', 'State', 'County', p.names)]

  d.names = paste0('D_',as.Date(date)+1:pred.h)
  D.pred = Cases_sub[, c('ID', 'State', 'County', d.names)]

  r.names = paste0('R_',as.Date(date)+1:pred.h)
  R.pred = Cases_sub[, c('ID', 'State', 'County', r.names)]

  list(P.pred = P.pred, D.pred = D.pred, R.pred = R.pred)
}
