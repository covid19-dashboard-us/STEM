rm(list = ls())

library(BPST)
library(mgcv)
library(MGLM)

data("county.loc")
data("dat.fit")
offset = log(dat.fit$S.ratio)

data("VT50")
VT = VT50
V = as.matrix(VT$V); Tr = as.matrix(VT$Tr)

cov.names = c('BlackRate', 'HLRate', 'Gini', 'Affluence', 'Disadvantage', 'UrbanRate',
  'HealCovRate', 'ExpHealth', 'MF', 'dPop_ml2', 'BED_SUM', 'prop_old')

tmp = "y.infec ~ Control2 + f(lI)"
formula <- as.formula(paste0(tmp, paste0('+ s(', cov.names,', bs = \'cr\', k = 4)', collapse = '')))
mfit.infec = fit.stem(formula, location = county.loc, V = V, Tr = Tr, d = 2, r = 1,
  family = ziP(), data = dat.fit, offset = offset)
# save(mfit.infec, file = "mfit.infec.rda")

tmp = "y.death ~ lI + Control2"
formula <- as.formula(paste0(tmp, paste0('+ ', cov.names[-c(9,11)], collapse = '')))
mfit.death = fit.stem(formula, location = county.loc, V = V, Tr = Tr, d = 2, r = 1,
  family = ziP(), data = dat.fit)
# save(mfit.death, file = "mfit.death.rda")

# one-day prediction
data("dat.pred.infec")
mpred.infec = predict.stem(mfit.infec, dat.pred.infec, adjust = TRUE)
data("dat.pred.death")
mpred.death = predict.stem(mfit.death, dat.pred.death, adjust = FALSE)

# seven-day rolling prediction
date.est = as.Date("2020-04-14")
pred.h = 7
pred.rolling = predict.rolling.stem(mfit.infec, mfit.death, data.new, cov.names,
  R.rate, pred.h, aq = c(0.001,0.999))
