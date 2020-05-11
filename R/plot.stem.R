#' Plots for Spatio-Temporal Epidemic Model (STEM)
#'
#' @import plotly
#' @import BPST
#'
#' @return
#'
plot.stem <- function(mfit.stem, VT.infec, VT.death){

  b1.type = mfit.stem$b1.type
  cov.type = mfit.stem$cov.type

  if(cov.type == "vary"){
    obj = plot(mfit.infec, select = 0)
    for(i in 1:length(obj)){
      name = obj[[i]]$xlab
      x = obj[[i]]$x
      fitted = obj[[i]]$fit
      upper = obj[[i]]$fit + 1.96*obj[[i]]$se
      lower = obj[[i]]$fit - 1.96*obj[[i]]$se

      png(file = paste0('CB_infec_', name, '.png'))
      plot.SCB(x, fitted, upper, lower,
               xlab = '', true=NULL)
      dev.off()
    }
  }

  mfit.infec = mfit.stem$mfit.stem.infec
  mfit.death = mfit.stem$mfit.stem.death
  BQ2.infec = mfit.stem$BQ2.infec
  BQ2.death = mfit.stem$BQ2.death
  nq.infec = ncol(BQ2.infec)
  nq.death = ncol(BQ2.death)
  coef0.infec = mfit.infec$coefficients[1:nq.infec]
  fig0.infec = plot.map(coef0.infec, VT.infec, limits = c(-6.5, 1), trunc= c(-6, 1), main="")
  print(fig0.infec)
  if(b1.type == "vary"){
    coef1.infec = mfit.infec$coefficients[(1:nq.infec) + nq.infec]
    fig1.infec = plot.map(coef1.infec, VT.infec, limits = c(-0.1, 2), trunc= c(0, 1.5), main="")
    print(fig1.infec)
  }
}
