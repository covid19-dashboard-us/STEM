plot.SCB <- function(x0, est, upper, lower, main = '', xlab = '',
    ylim = c(min(lower),max(upper)), true = NULL){
  plot(x0, est, type = 'l', main = main, ylim = ylim, ylab = '',
      xlab = xlab)
  polygon(c(x0, rev(x0)), c(upper, rev(lower)),
      col = "grey", border = NA)
  lines(x0, est, type = 'l')
  if(!is.null(true)){
    lines(x0, true, type = 'l', lty = 2)
  }
}
