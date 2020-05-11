Terms.Fit <- function (mfit, dat, cov.names) {
  Cases_sub = dat$Cases_sub
  BQ2.infec = dat$BQ2.infec
  BQ2.death = dat$BQ2.death
  all.names = names(dat$dat.fit)

  n.ober = nrow(Cases_sub)
  I.new = rep(exp(1) - 1, n.ober)
  S.ratio = rep(1, n.ober)
  new.data <- list(
    I = I.new, lI = log(I.new+1), S.ratio = S.ratio,
    Control2 = rep(1, n.ober),
    Control1 = rep(1, n.ober),
    BQ2_all.infec = as.matrix(BQ2.infec),
    BQ2_all.death = as.matrix(BQ2.death),
    BQ2_all.X.infec = as.matrix(
      kr(log(as.matrix(I.new) + 1), BQ2.infec, byrow = T)),
    BQ2_all.X.death = as.matrix(
      kr(log(as.matrix(I.new) + 1), BQ2.death, byrow = T))
  )
  p <- length(new.data)
  tmp01 = names(new.data)
  for (iter in (p+1):(p+length(cov.names))) {
    new.data[[iter]] <- Cases_sub[,cov.names[iter-p]]
  }
  names(new.data) <- c(tmp01, cov.names)

  terms <- predict(mfit, new.data, type = "terms", se.fit = FALSE)

  intercept = attr(terms,"constant")
  if(!is.null(intercept)) terms = cbind(rep(intercept, n.ober), terms)

  terms
}
