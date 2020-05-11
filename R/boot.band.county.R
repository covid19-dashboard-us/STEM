bootstrap.band.county <- function (terms.infec, terms.death,
  bootstrap.infec.array, bootstrap.death.array, dat, date.est,
  date.pred, Result.pred, County.ind.all, alpha0 = 0.05) {

  Cases_sub = dat$Cases_sub
  Result.pred.infec <- Result.pred$P.pred
  Result.pred.death <- Result.pred$D.pred

  # obtain path
  Result.boot <- lapply(1:nBoot, FUN = function(iter){
    cat(iter, '\r')
    # beta, alpha, gamma
    terms.infec.correct = 2*terms.infec - bootstrap.infec.array[, , iter]
    terms.death.correct = 2*terms.death - bootstrap.death.array[, , iter]
    terms.infec.correct[terms.infec.correct[,2]>1,2] = 1
    terms.infec.correct[terms.infec.correct[,2]<0,2] = 0
    P.pred <- PluginRandom.Correct( seed.iter = iter,
                                    terms.infec = terms.infec.correct,
                                    terms.death = terms.death.correct,
                                    theta.infec = 2*theta.infec - theta.infec.boot[iter, ],
                                    theta.death = 2*theta.death - theta.death.boot[iter, ],
                                    date = date.est, dat = dat,
                                    R.rate = R.rate, pred.h = pred.h)
    P.pred.infec <- P.pred$P.pred
    P.pred.death <- P.pred$D.pred
    newP.pred.infec <- cbind(P.pred.infec[,c('ID', 'State', 'County')],
                             P.pred.infec[, paste0('P_', date.pred[1])]-Cases_sub[, paste0('P_', date.est)],
                             P.pred.infec[, paste0('P_', date.pred[-1])] - P.pred.infec[, paste0('P_', date.pred[-pred.h])])
    newP.pred.death <- cbind(P.pred.death[,c('ID', 'State', 'County')],
                             P.pred.death[, paste0('D_', date.pred[1])]-Cases_sub[, paste0('D_', date.est)],
                             P.pred.death[, paste0('D_', date.pred[-1])] - P.pred.death[, paste0('D_', date.pred[-pred.h])])

    names(newP.pred.infec)[4] <- paste0('P_', date.pred[1])
    names(newP.pred.death)[4] <- paste0('D_', date.pred[1])
    return(list(
      P.pred.infec = P.pred.infec, newP.pred.infec = newP.pred.infec,
      P.pred.death = P.pred.death, newP.pred.death = newP.pred.death))
  })

  # State-level Band
  # infectious
  P.county.pred.infec <- Result.pred.infec
  P.county.boot.infec <- lapply(Result.boot, FUN = '[[', 1)

  P.county.pred.new.infec <- cbind(Result.pred.infec[,c('ID', 'State', 'County')],
                                   Result.pred.infec[, paste0('P_', date.pred[1])]-Cases_sub[, paste0('P_', date.est)],
                                   Result.pred.infec[, paste0('P_', date.pred[-1])] - Result.pred.infec[, paste0('P_', date.pred[-pred.h])])

  P.county.boot.new.infec <- lapply(Result.boot, FUN = '[[', 2)

  # death
  P.county.pred.death <- Result.pred.death
  P.county.boot.death <- lapply(Result.boot, FUN = '[[', 3)
  P.county.pred.new.death <- cbind(Result.pred.death[,c('ID', 'State', 'County')],
                                   Result.pred.death[, paste0('D_', date.pred[1])]-Cases_sub[, paste0('D_', date.est)],
                                   Result.pred.death[, paste0('D_', date.pred[-1])] - Result.pred.death[, paste0('D_', date.pred[-pred.h])])
  P.county.boot.new.death <- lapply(Result.boot, FUN = '[[', 4)

  # create empty matrices store bands
  # County.ind.all = c(2, 840, 187, 1823, 2932)
  n.county = length(County.ind.all)
  P.county.lower.infec <- data.frame(P.county.pred.infec[County.ind.all, c('ID', 'State', 'County')],
    matrix(NA,nrow = n.county, ncol = pred.h))
  names(P.county.lower.infec) <- c('ID','State','County',paste0('P_', date.pred))
  P.county.upper.infec <- data.frame(P.county.pred.infec[County.ind.all, c('ID','State','County')],
    matrix(NA,nrow = n.county, ncol = pred.h))
  names(P.county.upper.infec) <- c('ID','State','County', paste0('P_', date.pred))

  P.county.lower.new.infec <- data.frame(P.county.pred.new.infec[County.ind.all, c('ID', 'State', 'County')],
    matrix(NA,nrow = n.county, ncol = pred.h))
  names(P.county.lower.new.infec) <- c('ID','State','County',paste0('P_', date.pred))
  P.county.upper.new.infec <- data.frame(P.county.pred.new.infec[County.ind.all, c('ID','State','County')],
    matrix(NA,nrow = n.county, ncol = pred.h))
  names(P.county.upper.new.infec) <- c('ID','State','County', paste0('P_', date.pred))

  P.county.lower.death <- data.frame(P.county.pred.death[County.ind.all, c('ID', 'State', 'County')],
    matrix(NA,nrow = n.county, ncol = pred.h))
  names(P.county.lower.death) <- c('ID','State','County',paste0('D_', date.pred))
  P.county.upper.death <- data.frame(P.county.pred.death[County.ind.all, c('ID','State','County')],
    matrix(NA,nrow = n.county, ncol = pred.h))
  names(P.county.upper.death) <- c('ID','State','County', paste0('D_', date.pred))

  P.county.lower.new.death <- data.frame(P.county.pred.new.death[County.ind.all, c('ID', 'State', 'County')],
    matrix(NA,nrow = n.county, ncol = pred.h))
  names(P.county.lower.new.death) <- c('ID','State','County',paste0('D_', date.pred))
  P.county.upper.new.death <- data.frame(P.county.pred.new.death[County.ind.all, c('ID','State','County')],
    matrix(NA,nrow = n.county, ncol = pred.h))
  names(P.county.upper.new.death) <- c('ID','State','County', paste0('D_', date.pred))

  cores=detectCores()
  cl <- makeCluster(cores[1]-2) #not to overload your computer
  registerDoParallel(cl)

  foreach(i = 1:n.county) %do% {

    ii = County.ind.all[i]
    band.infec.i <- county.band(county.ind=ii, df.pred = P.county.pred.infec,
                                df.boot = P.county.boot.infec, nBoot, pred.h, alpha0)
    P.county.lower.infec[i,paste0('P_', date.pred)] <- band.infec.i$lb
    P.county.upper.infec[i,paste0('P_', date.pred)] <- band.infec.i$ub

    band.new.infec.i <- county.band(county.ind=ii, df.pred = P.county.pred.new.infec,
                                    df.boot = P.county.boot.new.infec, nBoot, pred.h, alpha0)
    P.county.lower.new.infec[i,paste0('P_', date.pred)] <- band.new.infec.i$lb
    P.county.upper.new.infec[i,paste0('P_', date.pred)] <- band.new.infec.i$ub


    band.death.i <- county.band(county.ind=ii, df.pred = P.county.pred.death,
                                df.boot = P.county.boot.death, nBoot, pred.h, alpha0)
    P.county.lower.death[i,paste0('D_', date.pred)] <- band.death.i$lb
    P.county.upper.death[i,paste0('D_', date.pred)] <- band.death.i$ub

    band.new.death.i <- county.band(county.ind=ii, df.pred = P.county.pred.new.death,
                                    df.boot = P.county.boot.new.death, nBoot, pred.h, alpha0)
    P.county.lower.new.death[i,paste0('D_', date.pred)] <- band.new.death.i$lb
    P.county.upper.new.death[i,paste0('D_', date.pred)] <- band.new.death.i$ub
  }

  P.county.pred.infec <- P.county.pred.infec[County.ind.all, ]
  P.county.pred.new.infec <- P.county.pred.new.infec[County.ind.all, ]
  P.county.pred.death <- P.county.pred.death[County.ind.all, ]
  P.county.pred.new.death <- P.county.pred.new.death[County.ind.all, ]

  Result = list(P.county.pred.infec = P.county.pred.infec, P.county.pred.death = P.county.pred.death,
       P.county.lower.infec = P.county.lower.infec, P.county.lower.death = P.county.lower.death,
       P.county.upper.infec = P.county.upper.infec, P.county.upper.death = P.county.upper.death,
       P.county.pred.new.infec = P.county.pred.new.infec, P.county.pred.new.death = P.county.pred.new.death,
       P.county.lower.new.infec = P.county.lower.new.infec, P.county.lower.new.death = P.county.lower.new.death,
       P.county.upper.new.infec = P.county.upper.new.infec, P.county.upper.new.death = P.county.upper.new.death)
}
