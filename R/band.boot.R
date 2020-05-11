band.boot = function (nBoot, terms.infec, terms.death,
  bootstrap.infec.array, bootstrap.death.array,
  theta.infec.boot, theta.death.boot,
  dat, date.est, date.pred, Result.pred, alpha0 = 0.05) {

  pred.h = length(date.pred)
  Cases_sub = dat$Cases_sub
  Result.pred.infec <- Result.pred$P.pred
  Result.pred.death <- Result.pred$D.pred

  # obtain path
  Result.boot <- lapply(1:nBoot, FUN = function(iter){
    cat(iter, '\r')
    # beta, alpha, gamma
    # terms.infec.correct = bootstrap.infec.array[, , iter]
    # terms.death.correct = bootstrap.death.array[, , iter]
    # terms.infec.correct[terms.infec.correct[,2]>1,2] = 1
    # terms.infec.correct[terms.infec.correct[,2]<0,2] = 0
    P.pred <- PluginRandom.Correct(seed.iter = iter,
      terms.infec = bootstrap.infec.array[, , iter],
      terms.death = bootstrap.death.array[, , iter],
      theta.infec = theta.infec.boot[iter, ],
      theta.death = theta.death.boot[iter, ],
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
  diff.infec = Diff.stco(date.est)$diff
  diff.death = Diff.stco(date.est, type = 'death')$diff


  # aggregate state level and add unassgined cases
  # infectious
  P.county.pred.infec <- Result.pred.infec
  P.state.pred.infec = Agg.stco(P.county.pred.infec, diff.infec, date.pred)

  P.county.boot.infec <- lapply(Result.boot, FUN = '[[', 1)
  P.state.boot.infec = lapply(1:nBoot, function(ib){
    Agg.stco(P.county.boot.infec[[ib]], diff = diff.infec,
             date.pred = date.pred)})

  P.county.pred.new.infec <- cbind(Result.pred.infec[,c('ID', 'State', 'County')],
                                   Result.pred.infec[, paste0('P_', date.pred[1])]-Cases_sub[, paste0('P_', date.est)],
                                   Result.pred.infec[, paste0('P_', date.pred[-1])] - Result.pred.infec[, paste0('P_', date.pred[-pred.h])])
  P.state.pred.new.infec <- Agg.stco(P.county.pred.new.infec, diff = rep(0, 49), date.pred)

  P.county.boot.new.infec <- lapply(Result.boot, FUN = '[[', 2)
  P.state.boot.new.infec = lapply(1:nBoot, function(ib){
    Agg.stco(P.county.boot.new.infec[[ib]], diff = rep(0, 49),
             date.pred = date.pred)})

  # death
  P.county.pred.death <- Result.pred.death
  P.state.pred.death = Agg.stco(P.county.pred.death, diff.death, date.pred)

  P.county.boot.death <- lapply(Result.boot, FUN = '[[', 3)
  P.state.boot.death = lapply(1:nBoot, function(ib){
    Agg.stco(P.county.boot.death[[ib]], diff = diff.death,
             date.pred = date.pred)})

  P.county.pred.new.death <- cbind(Result.pred.death[,c('ID', 'State', 'County')],
                                   Result.pred.death[, paste0('D_', date.pred[1])]-Cases_sub[, paste0('D_', date.est)],
                                   Result.pred.death[, paste0('D_', date.pred[-1])] - Result.pred.death[, paste0('D_', date.pred[-pred.h])])
  P.state.pred.new.death <- Agg.stco(P.county.pred.new.death, diff = rep(0, 49), date.pred)

  P.county.boot.new.death <- lapply(Result.boot, FUN = '[[', 4)
  P.state.boot.new.death = lapply(1:nBoot, function(ib){
    Agg.stco(P.county.boot.new.death[[ib]], diff = rep(0, 49),
             date.pred = date.pred)})

  # create empty matrices store bands
  P.state.lower.infec <- data.frame(P.state.pred.infec[,'State'], matrix(NA,nrow = 50, ncol = pred.h))
  names(P.state.lower.infec) <- c('State', paste0('P_', date.pred))
  P.state.upper.infec <- data.frame(P.state.pred.infec[,'State'], matrix(NA,nrow = 50, ncol = pred.h))
  names(P.state.upper.infec) <- c('State', paste0('P_', date.pred))

  P.state.lower.new.infec <- data.frame(P.state.pred.new.infec[,'State'], matrix(NA,nrow = 50, ncol = pred.h))
  names(P.state.lower.new.infec) <- c('State', paste0('P_', date.pred))
  P.state.upper.new.infec <- data.frame(P.state.pred.new.infec[,'State'], matrix(NA,nrow = 50, ncol = pred.h))
  names(P.state.upper.new.infec) <- c('State', paste0('P_', date.pred))

  P.state.lower.death <- data.frame(P.state.pred.death[,'State'], matrix(NA,nrow = 50, ncol = pred.h))
  names(P.state.lower.death) <- c('State', paste0('D_', date.pred))
  P.state.upper.death <- data.frame(P.state.pred.death[,'State'], matrix(NA,nrow = 50, ncol = pred.h))
  names(P.state.upper.death) <- c('State', paste0('D_', date.pred))

  P.state.lower.new.death <- data.frame(P.state.pred.new.death[,'State'], matrix(NA,nrow = 50, ncol = pred.h))
  names(P.state.lower.new.death) <- c('State', paste0('D_', date.pred))
  P.state.upper.new.death <- data.frame(P.state.pred.new.death[,'State'], matrix(NA,nrow = 50, ncol = pred.h))
  names(P.state.upper.new.death) <- c('State', paste0('D_', date.pred))


  for(i in 1:50){
    # Cumulative Prediction
    band.infec.i <- state.band(state.ind=i, df.pred = P.state.pred.infec,
                               df.boot = P.state.boot.infec, nBoot, pred.h, alpha0)
    P.state.lower.infec[i,paste0('P_', date.pred)] <- band.infec.i$lb
    P.state.upper.infec[i,paste0('P_', date.pred)] <- band.infec.i$ub

    band.new.infec.i <- state.band(state.ind=i, df.pred = P.state.pred.new.infec,
                                   df.boot = P.state.boot.new.infec, nBoot, pred.h, alpha0)
    P.state.lower.new.infec[i,paste0('P_', date.pred)] <- band.new.infec.i$lb
    P.state.upper.new.infec[i,paste0('P_', date.pred)] <- band.new.infec.i$ub


    band.death.i <- state.band(state.ind=i, df.pred = P.state.pred.death,
                               df.boot = P.state.boot.death, nBoot, pred.h, alpha0)
    P.state.lower.death[i,paste0('D_', date.pred)] <- band.death.i$lb
    P.state.upper.death[i,paste0('D_', date.pred)] <- band.death.i$ub

    band.new.death.i <- state.band(state.ind=i, df.pred = P.state.pred.new.death,
                                   df.boot = P.state.boot.new.death, nBoot, pred.h, alpha0)
    P.state.lower.new.death[i,paste0('D_', date.pred)] <- band.new.death.i$lb
    P.state.upper.new.death[i,paste0('D_', date.pred)] <- band.new.death.i$ub

  }

  list(P.state.pred.infec = P.state.pred.infec, P.state.pred.death = P.state.pred.death,
       P.state.lower.infec = P.state.lower.infec, P.state.lower.death = P.state.lower.death,
       P.state.upper.infec = P.state.upper.infec, P.state.upper.death = P.state.upper.death,
       P.state.pred.new.infec = P.state.pred.new.infec, P.state.pred.new.death = P.state.pred.new.death,
       P.state.lower.new.infec = P.state.lower.new.infec, P.state.lower.new.death = P.state.lower.new.death,
       P.state.upper.new.infec = P.state.upper.new.infec, P.state.upper.new.death = P.state.upper.new.death)
}
