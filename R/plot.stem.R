plot.stem <- function(mfit, range = NULL){
  obj = plot(mfit, select = 0)
  for(i in 1:length(obj)){
    name = obj[[i]]$xlab
    x = obj[[i]]$x
    fitted = obj[[i]]$fit
    upper = obj[[i]]$fit + 1.96*obj[[i]]$se
    lower = obj[[i]]$fit - 1.96*obj[[i]]$se

    if(!is.null(range)){
      y.range = range[,i]
      plot.SCB(x, fitted, upper, lower,
        main = paste0("SCB for ", name),
        xlab = '', ylim = y.range, true=NULL)
    }else{
      plot.SCB(x, fitted, upper, lower,
        main = paste0("Simultaneous Confidence Band for ", name),
        xlab = '', true=NULL)
    }
  }
}
