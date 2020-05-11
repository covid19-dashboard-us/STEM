#' Predict for Spatio-Temporal Epidemic Model (STEM)
#'
#' @import mgcv
#'
#' @return
#'
predict.stem <- function(mfit1.stem, mfit2.stem = NULL,
  date.est = date.est, dat = list(), cov.names, date.update, R.rate = 0.1,
  pred.h = 7, aq = c(0.001,0.999)){

  mpred1.stem = plugin.predict(mfit.stem = mfit1.stem, dat = dat,
    cov.names = cov.names, date.est = date.est, R.rate = R.rate,
    pred.h = pred.h, aq = aq)

  if(!is.null(mfit2.stem)){
    mpred2.stem = plugin.predict(mfit.stem = mfit2.stem, dat = dat,
      cov.names = cov.names, date.est = date.est, R.rate = R.rate,
      pred.h = pred.h, aq = aq)
  }

  P1.pred = mpred1.stem$P.pred
  P1.pred.interval = mpred1.stem$P.pred.iterval
  D1.pred = mpred1.stem$D.pred
  D1.pred.interval = mpred1.stem$D.pred.iterval
  if(!is.null(mfit2.stem)){
    P2.pred = mpred2.stem$P.pred
    P2.pred.interval = mpred2.stem$P.pred.iterval
    D2.pred = mpred2.stem$D.pred
    D2.pred.interval = mpred2.stem$D.pred.iterval
  }

  # Extract the true observation
  if(date.est < (date.update - pred.h)){
    load("data/dat.true.rda")
    P.true = as.matrix(dat.true$P.county.true[,-(1:3)])
    D.true = as.matrix(dat.true$D.county.true[,-(1:3)])
    # prediction error
    P1.spe = (P.true - as.matrix(P1.pred[,-(1:3)]))^2
    D1.spe = (D.true - as.matrix(D1.pred[,-(1:3)]))^2
    if(!is.null(mfit2.stem)){
      P2.spe = (P.true - as.matrix(P2.pred[,-(1:3)]))^2
      D2.spe = (D.true - as.matrix(D2.pred[,-(1:3)]))^2
    }else{
      P2.spe = P1.spe
      D2.spe = D1.spe
    }

    weight.infec = as.numeric(apply(P1.spe, 1, max) <= apply(P2.spe, 1, max))
    weight.death = as.numeric(apply(D1.spe, 1, max) <= apply(D2.spe, 1, max))
    P.pred = P1.pred
    P.pred[weight.infec == 0,] = P2.pred[weight.infec == 0,]
    D.pred = D1.pred
    D.pred[weight.death == 0,] = D2.pred[weight.death == 0,]
    P.pred.interval = P1.pred.interval
    P.pred.interval[weight.infec == 0,] = P2.pred.interval[weight.infec == 0,]
    D.pred.interval = D1.pred.interval
    D.pred.interval[weight.death == 0,] = D2.pred.interval[weight.death == 0,]
    # weight1.infec = as.numeric(rowSums(P1.spe) <= rowSums(P2.spe))
    # weight.mean = cbind(P1.pred[, 2:3], weight1.infec)
    # write.csv(weight.mean, "weight.mean.csv", row.names = FALSE)

    # weight2.infec = as.numeric(apply(P1.spe, 1, max) <= apply(P2.spe, 1, max))
    # weight.max = cbind(P1.pred[, 2:3], weight2.infec)
    # write.csv(weight.max, "weight.max.csv", row.names = FALSE)

    # weight3.infec = as.numeric(apply(P1.spe < P2.spe, 1, sum) > pred.h/2)
    # weight.table = cbind(P1.pred[, 2:3], weight3.infec)
    # write.csv(weight.table, "weight.table.csv", row.names = FALSE)

    # weight.infec = cbind(cbind(cbind(P1.pred[, 1:3], weight1.infec),
    #   weight2.infec), weight3.infec)
    # write.csv(weight.infec, "weight.infec.csv", row.names = FALSE)
  }else{
    cat("Not enough validation dates!")
  }

  list(P.pred = P.pred, D.pred = D.pred,
    P.pred.interval = P.pred.interval,
    D.pred.interval = D.pred.interval,
    weight.infec = weight.infec,
    weight.death = weight.death)
}
