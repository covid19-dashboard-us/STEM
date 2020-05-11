#' Estimation using Spatio-Temporal Epidemic Model (STEM)
#'
#' This function implement the model fitting of the spatiotemporal epidemic model.
#' @import mgcv
#' @importFrom kr MGLM
#'
#' @param formula A STEM formula which is exactly like the formula for a GAM except that bivariate smooth terms, f(), can be added to the right hand side to specify that the predictor depends on bivariate smooth functions of predictors.
#' @param location Location information used in the bivariate smooth function.
#' @param V Vertices of a triangulation.
#' @param Tr Triangles of a triangulation.
#' @param d Degree of polynomials in bivariate smooth funciton, default is 2.
#' @param r Smoothness parameter in bivariate smooth function, default is 1.
#' @param family This is a family object specifying the distribution and link to use in fitting etc, default is ziP() (the zero-inflated poisson).
#' @param data A data frame or list containing the model response variable and covariates required by the formula.
#' @param offset The offset used to supply a model fitting.
#'
#' @return
fit.stem <- function(cov.infec, cov.death, data = list(), b1.type = "cons", cov.type = "cons"){
  dat.fit = data$dat.fit
  dat.fit$lI = log(dat.fit$I + 1)
  BQ2.infec = data$BQ2.infec
  P.infec = data$P.infec
  BQ2.death = data$BQ2.death
  P.death = data$P.death

  if(b1.type == "vary"){
    tmp01 <- paste0("y.infec ~ 0 + BQ2_all.infec + BQ2_all.X.infec + Control2")
  }else{
    tmp01 <- paste0("y.infec ~ 0 + BQ2_all.infec + lI + Control2")
  }
  if(cov.type == "vary"){
    tmp02 <- paste0(' + s(',cov.infec,', bs = \'cr\', k = 4)', collapse = '')
  }else{
    tmp02 <- paste0(' + ', cov.infec, collapse = '')
  }
  formula.infec = as.formula(paste0(tmp01, tmp02))
  cat("formula.infec:", as.character(formula.infec), "\n")
  if(b1.type == "vary"){
    mfit.stem.infec <- gam(formula.infec, family = ziP(),
      paraPen = list(BQ2_all.infec = list(P.infec), BQ2_all.X.infec = list(P.infec)), data = dat.fit,
      offset = log(dat.fit$S.ratio))
  }else{
    mfit.stem.infec <- gam(formula.infec, family = ziP(),
      paraPen = list(BQ2_all.infec = list(P.infec)), data = dat.fit,
      offset = log(dat.fit$S.ratio))
  }

  tmp01 <- paste0("y.death ~ 0 + BQ2_all.death + lI + Control2")
  tmp02 <- paste0(' + ', cov.death, collapse = '')
  formula.death = as.formula(paste0(tmp01, tmp02))
  cat("formula.death:", as.character(formula.death), "\n")
  mfit.stem.death <- gam(formula.death, family = ziP(),
    paraPen = list(BQ2_all.death = list(P.death)), data = dat.fit,
    offset = log(dat.fit$S.ratio))
  list(mfit.stem.infec = mfit.stem.infec, mfit.stem.death = mfit.stem.death,
    b1.type = b1.type, cov.type = cov.type,
    BQ2.infec = BQ2.infec, BQ2.death = BQ2.death)
}
