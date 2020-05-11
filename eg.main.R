rm(list = ls())

library(dplyr)
library(mgcv)
library(lubridate)
library(MGLM)
library(parallel)
library(BPST)
library(Triangulation)
library(parallel)
library(actuar)
library(doParallel)

# covariates
cov.infec = c('BlackRate', 'HLRate', 'Gini',
              'Affluence', 'Disadvantage', 'UrbanRate',
              'HealCovRate', 'ExpHealth', 'MF', 'dPop_ml2',
              'BED_SUM', 'prop_old')
cov.death=c('BlackRate', 'HLRate', 'Gini',
            'Affluence', 'Disadvantage', 'UrbanRate',
            'HealCovRate', 'ExpHealth', 'dPop_ml2', 'prop_old')
cov.names = union(cov.infec, cov.death)

######################################################################
# STEM model fitting
VT.infec = 20; VT.death = 50;
t0 <- proc.time()
mfit1.stem = fit.stem(cov.infec, cov.death, dat, b1.type = "vary", cov.type = "cons")
t1.fit = proc.time() - t0
plot.stem(mfit1.stem, VT.infec, VT.death)

t0 <- proc.time()
mfit2.stem = fit.stem(cov.infec, cov.death, dat, b1.type = "cons", cov.type = "cons")
t2.fit = proc.time() - t0
plot.stem(mfit2.stem, VT.infec, VT.death)

######################################################################
# Short-Term Prediction
date.est = as.Date("2020-04-25")
date.update = as.Date("2020-05-06")
pred.h = 10; R.rate = 0.1
mpred = predict.stem(mfit1.stem, mfit2.stem,
  date.est, dat, cov.names, date.update, R.rate,
  pred.h, aq = c(0.001,0.999))

######################################################################
# Long-Term Prediction
file.path = "result/boot/"
nboot = 20
est.h = 9; proj.h = 130
date.pred = date.est + 1:proj.h
boot.ests <- mclapply(1:nboot, FUN = fit.boot, mc.cores = 1,
  mfit.stem = mfit2.stem, cov.infec =cov.infec, cov.death = cov.death,
  dat = dat, date.est, est.h = est.h, R.rate = R.rate, file.path = file.path)
boot.res = aggregate.boot(mfit2.stem, file.path, dat = dat, date.est, date.pred, alpha0 = 0.05)
