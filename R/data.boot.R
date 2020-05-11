data.boot <- function(seed.iter, mfit.infec, mfit.death, dat, date.start, 
  R.rate, est.h, cov.names) {
  
  set.seed(seed.iter)
  date.all = as.character(date.start + est.h:0)
  BQ2.infec = dat$BQ2.infec
  BQ2.death = dat$BQ2.death
  Cases_sub = dat$Cases_sub
  all.names = names(dat$dat.fit)
  
  # obtain estimated value for each term --------------------
  terms.infec = Terms.Fit(mfit = mfit.infec, dat = dat, cov.names = cov.names)
  terms.death = Terms.Fit(mfit = mfit.death, dat = dat, cov.names = cov.names)
  theta.infec <- mfit.infec$family$getTheta()
  theta.death <- mfit.death$family$getTheta()
  
  # generate mean -------------------------------------------------------
  I.new = Cases_sub[, paste0('I_', date.start)]
  S.ratio = 1- Cases_sub[, paste0('P_', date.start)]/Cases_sub$population
  I.new.log = log(I.new + 1)
  
  # infectious 
  if ('Control2' %in% colnames(terms.infec)) {
    index.C2 = which(colnames(terms.infec) == 'Control2')
    link.infec = terms.infec[, 2] * I.new.log + log(S.ratio) + rowSums(terms.infec[, - c(2, index.C2)]) +
      terms.infec[, index.C2] * Cases_sub[, paste0('C2_', date.start + 1)]
  } else {
    link.infec = terms.infec[, 2] * I.new.log + log(S.ratio) + rowSums(terms.infec[, -2]) 
  }
  
  mpred.gsvcm.infec <- exp(link.infec)
  p.mpred.gsvcm.infec <- 1 - exp(-exp(theta.infec[1] + exp(theta.infec[2]) * link.infec ))
  
  # death
  if ('Control2' %in% colnames(terms.death)) {
    index.C2 = which(colnames(terms.death) == 'Control2')
    link.death = terms.death[, 2] * I.new.log + rowSums(terms.death[, - c(2, index.C2)]) +
      terms.death[, index.C2] * Cases_sub[, paste0('C2_', date.start + 1)]
  } else {
    if (ncol(terms.death) == 2) {
      link.death = terms.death[, 2] * I.new.log + terms.death[, 1]
    } else {
      link.death = terms.death[, 2] * I.new.log + rowSums(terms.death[, -2])
    }
  }
  mpred.gsvcm.death <- exp(link.death)
  p.mpred.gsvcm.death <- 1 - exp(-exp(theta.death[1] + exp(theta.death[2]) * link.death ))
  
  mpred.gsvcm.death [mpred.gsvcm.death < 10e-4] = 10e-4
  p.mpred.gsvcm.death [p.mpred.gsvcm.death < 10e-4] = 10e-4
  
  # recover
  mpred.recover <- R.rate * I.new
  
  date = date.start
  n.ober <- length(p.mpred.gsvcm.infec) 
  
  for(iter in 1:est.h) {
    # cat('iter:', iter, '\n')
    Y.new.zero = rbinom(n = n.ober, size = 1, prob = p.mpred.gsvcm.infec)
    Y.new = rztpois(n = n.ober, lambda = mpred.gsvcm.infec)
    Y.new = Y.new*Y.new.zero
    
    Y.new.zero = rbinom(n = n.ober, size = 1, prob = p.mpred.gsvcm.death)
    Y.new.death = rztpois(n = n.ober, lambda = mpred.gsvcm.death)
    Y.new.death = Y.new.death*Y.new.zero
    
    Y.new.recover = rpois(n = n.ober, mpred.recover)
    
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
    
    if ('Control2' %in% colnames(terms.infec)) {
      index.C2 = which(colnames(terms.infec) == 'Control2')
      link.infec = terms.infec[, 2] * I.new.log + log(S.ratio) + rowSums(terms.infec[, - c(2, index.C2)]) +
        terms.infec[, index.C2] * Cases_sub[, paste0('C2_', date.start + iter + 1)]
    } else {
      link.infec = terms.infec[, 2] * I.new.log + log(S.ratio) + rowSums(terms.infec[, -2]) 
    }
    
    mpred.gsvcm.infec <- exp(link.infec)
    p.mpred.gsvcm.infec <- 1 - exp(-exp(theta.infec[1] + exp(theta.infec[2]) * link.infec ))
    
    # death
    if ('Control2' %in% colnames(terms.death)) {
      index.C2 = which(colnames(terms.death) == 'Control2')
      link.death = terms.death[, 2] * I.new.log + rowSums(terms.death[, - c(2, index.C2)]) +
        terms.death[, index.C2] * Cases_sub[, paste0('C2_', date.start + iter + 1)]
    } else {
      if (ncol(terms.death) == 2) {
        link.death = terms.death[, 2] * I.new.log + terms.death[, 1]
      } else {
        link.death = terms.death[, 2] * I.new.log + rowSums(terms.death[, -2])
      }
    }
    mpred.gsvcm.death <- exp(link.death)
    p.mpred.gsvcm.death <- 1 - exp(-exp(theta.death[1] + exp(theta.death[2]) * link.death ))
    mpred.gsvcm.death [mpred.gsvcm.death < 10e-4] = 10e-4
    p.mpred.gsvcm.death [p.mpred.gsvcm.death < 10e-4] = 10e-4
    
    # recover
    mpred.recover <- R.rate * I.new
    }
  
  # Daily infected
  Daily_Infected <- Cases_sub[, paste0("P_", date.all[-(est.h+1)])] - Cases_sub[, paste0("P_", date.all[-1])]
  Y.infec <- c(as.matrix(Daily_Infected))
  
  # Daily death
  Daily_death <- Cases_sub[, paste0("D_", date.all[-(est.h+1)])] - Cases_sub[, paste0("D_", date.all[-1])]
  Y.death <- c(as.matrix(Daily_death))
  
  # I infected information # I_{t-1}
  I <- c(as.matrix(Cases_sub[, paste0("I_", date.all[-1])]))
  S.ratio <- c(1 - as.matrix(Cases_sub[, paste0("P_", date.all[-1])]) / Cases_sub$population)
  
  BQ2_all.infec <- kronecker(rep(1, est.h), BQ2.infec)
  BQ2_all.X.infec <- kr(as.matrix(log(I + 1)), BQ2_all.infec, byrow = T)
  BQ2_all.death <- kronecker(rep(1, est.h), BQ2.death)
  BQ2_all.X.death <- kr(as.matrix(log(I + 1)), BQ2_all.death, byrow = T)
  
  
  list(y.infec = Y.infec, y.death = Y.death, BQ2_all.X.infec = BQ2_all.X.infec,
       BQ2_all.X.death = BQ2_all.X.death, S.ratio = S.ratio)
}