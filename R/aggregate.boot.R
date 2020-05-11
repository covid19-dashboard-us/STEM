#' Aggregate the bootstrap results to construct the SCB for Spatio-Temporal Epidemic Model (STEM)
#'
#' @import mgcv
#'
#' @return
#'
aggregate.boot <- function(mfit.stem, file.path, dat = list(), date.est, date.pred, alpha0 = 0.05){

mfit.infec = mfit.stem$mfit.stem.infec
mfit.death = mfit.stem$mfit.stem.death
theta.infec = mfit.infec$family$getTheta()
theta.death = mfit.death$family$getTheta()
terms.infec = Terms.Fit(mfit = mfit.infec, dat = dat, cov.names)
terms.death = Terms.Fit(mfit = mfit.death, dat = dat, cov.names)

all.files = list.files(file.path)

bootstrap.infec.array = array(dim = c(3104, ncol(terms.infec), length(all.files)))
bootstrap.death.array = array(dim = c(3104, ncol(terms.death), length(all.files)))
theta.infec.boot = c()
theta.death.boot = c()

truncate = function(x, quan = c(0.05, 0.95), type = 'constant'){
  q1 = quantile(x[,1], quan[1])
  q2 = quantile(x[,1], quan[2])
  x1 = x[,1]
  x1[x1 < q1] = q1
  x1[x1 > q2] = q2
  x2 = x[,2]
  if (type == 'varying') {
    x2[x2 < 0] = 0
    x2[x2 > 1] = 1
  }
  x[, 1] = x1
  x[, 2] = x2
  x
}

for (iter in 1:length(all.files)) {
  file.name = paste0(file.path, all.files[iter])
  load(file.name)
  if (length(A) == 4) {
    tmp1 = 2 * terms.infec - A$terms.infec.b
    tmp2 = 2 * terms.death - A$terms.death.b
    # tmp1 = truncate(tmp1)
    # tmp2 = truncate(tmp2)
    bootstrap.infec.array[, , iter] = tmp1
    bootstrap.death.array[, , iter] = tmp2
    theta.infec.boot = rbind(theta.infec.boot, 2 * theta.infec - A$theta.infec.b)
    theta.death.boot = rbind(theta.death.boot, 2 * theta.death - A$theta.death.b)
  } else {
    cat("This path is empty.", iter)
  }
}

mean.theta.infec = colMeans(theta.infec.boot)
mean.theta.death = colMeans(theta.death.boot)
mean.infec.array = apply(bootstrap.infec.array, c(1, 2), mean)
mean.death.array = apply(bootstrap.death.array, c(1, 2), mean)

Result.pred = plugin.correct(
  terms.infec = mean.infec.array, terms.death = mean.death.array ,
  theta.infec = mean.theta.infec,
  theta.death = mean.theta.death, date = date.est,
  dat = dat, R.rate = R.rate, pred.h = pred.h)

nBoot = length(all.files)
results = band.boot(nBoot, mean.infec.array, mean.death.array,
  bootstrap.infec.array, bootstrap.death.array,
  theta.infec.boot, theta.death.boot,
  dat = dat, date.est, date.pred, Result.pred, alpha0 = alpha0)

County.ind.all = 1:3104
results.county  = bootstrap.band.county (mean.infec.array, mean.death.array,
  bootstrap.infec.array, bootstrap.death.array, dat, date.est,
  date.pred, Result.pred, County.ind.all, alpha0 = alpha0)

# save.files(results, results.county, file.path)
list(result = results, result.county = results.county)
}
