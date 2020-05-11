county.band <- function(county.ind, df.pred, df.boot, nBoot, pred.h, alpha0, plot.tmp =TRUE){
  # Cumulative Prediction
  Pred = as.matrix(df.pred[county.ind, -c(1:3)])
  tmp03 = lapply(1:nBoot, function(ib){
    tmp01 = df.boot[[ib]]
    tmp02 = tmp01[county.ind,]
  })
  P.boot = do.call("rbind", tmp03)
  tmp04 = P.boot
  nrm = 0
  if (alpha0 != 0) {
    while(nrm < alpha0*nBoot){
      ind = lapply(2:(pred.h+1), function(ih){
        tmp05 = tmp04[, ih]
        ind1 = which(tmp05 == max(tmp05))
        ind2 = which(tmp05 == min(tmp05))
        ind = c(ind1, ind2)
        return(ind)
      })
      ind.all = sort(unique(do.call("c",ind)))
      tmp06 = as.matrix(tmp04[ind.all, -c(1:3)])
      tmp07 = sweep(tmp06, 2, Pred, "-")
      mse = apply(tmp07^2, 1, mean)
      ind.max = which(mse == max(mse))[1]
      tmp04 = tmp04[-(ind.max),]
      nrm = nrm + length(ind.max)
    }
    P.boot = tmp04
    P.lb = apply(tmp04[, -c(1:3)], 2, min)
    P.ub = apply(tmp04[, -c(1:3)], 2, max)
  } else {
    P.lb = apply(tmp04[, -c(1:3)], 2, median)
    P.ub = apply(tmp04[, -c(1:3)], 2, median)
  }
  
  if (plot.tmp == TRUE) {
    plot(1:pred.h, c(rep(min(P.lb), pred.h/2), rep(max(P.ub), pred.h/2)), col = "white",
         xlab = "date", ylab = "Cases",
         main = paste(df.pred$State[county.ind], df.pred$County[county.ind]))
    lines(1:pred.h, Pred, col=2)
    lines(1:pred.h, P.lb, col=4)
    lines(1:pred.h, P.ub, col=4)
  }
  return(list(lb = P.lb, ub = P.ub))
}
