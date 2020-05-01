#' Estimation using Spatio-Temporal Epidemic Model (STEM)
#'
#' This function implement the model fitting of the spatiotemporal epidemic model.
#' @import mgcv
#' @importFrom basis BPST
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
#'
#' @examples
#' rm(list = ls())
#' data("county.loc")
#' data("dat.fit")
#' offset = log(dat.fit$S.ratio)
#' data("VT50")
#' VT = VT50
#' V = as.matrix(VT$V)
#' Tr = as.matrix(VT$Tr)
#' cov.names = c('BlackRate', 'HLRate', 'Gini', 'Affluence', 'Disadvantage', 'UrbanRate',
#' 'HealCovRate', 'ExpHealth', 'MF', 'dPop_ml2', 'BED_SUM', 'prop_old')
#'
#' tmp = "y.infec ~ Control2 + f(lI)"
#' formula <- as.formula(paste0(tmp,
#' paste0('+ s(',cov.names,', bs = \'cr\', k = 4)', collapse = '')))
#' mfit.infec = fit.stem(formula, location = county.loc, V = V, Tr = Tr, d = 2, r = 1,
#' family = ziP(), data = dat.fit, offset = offset)
#'
#' tmp = "y.death ~ Control2 + lI"
#' formula <- as.formula(paste0(tmp, paste0('+ ', cov.names, collapse = '')))
#' mfit.death = fit.stem(formula, location = county.loc, V = V, Tr = Tr, d = 2, r = 1,
#' family = ziP(), data = dat.fit)
#' @export

fit.stem <- function(formula, location = NULL, V = NULL, Tr = NULL, d = 2, r = 1,
  family = ziP(), data = list(), offset = NULL){

  if(is.null(V) | is.null(Tr)){
    stop("Triangulation information is required.")
  }
  if(is.null(location)){
    stop("Location information for the bivariate function is required.")
  }

  tf <- terms.formula(formula, specials = c("s", "f")) # specials attribute indicates which terms are non-para
  if (attr(tf, "response") > 0) {  # start the replacement formulae
    response <- as.character(attr(tf,"variables")[2])
  } else {
    response <- NULL
  }

  est.h <- nrow(dat.fit[response])/nrow(location)
  B0 <- basis(V = V, Tr = Tr, d = d, r = r, Z = location)
  B <- B0$B
  Q2 <- B0$Q2
  BQ2 <- B%*%Q2
  K <- B0$K
  P <- as.matrix(t(Q2) %*% K %*% Q2)

  beta0 <- as.matrix(kronecker(rep(1, est.h), BQ2))
  data$beta0 <- beta0
  np <- length(data)
  names.data <- names(data)

  terms <- attr(tf, "term.labels") # labels of the model terms
  ind.f <- attr(tf, "specials")$f # array of indices of bivarite smooth terms
  nf <- length(ind.f)
  names.f <- c()
  paraP = vector(mode = "list", length = (nf+1))
  paraP[[1]] = list(P)
  if(nf > 0){
    for(i in 1:nf){
      term.f <- terms[ind.f[i]-1]
      var.f <- substring(term.f, 3, nchar(term.f)-1)
      # name.f <- paste0("beta.",var.f)
      name.f <- "beta1"
      names.f <- c(names.f, name.f)
      beta.f <- as.matrix(kr(as.matrix(data[var.f]), beta0, byrow = T))
      data[[np+i]] <- beta.f
      paraP[[i]] <- list(P)
    }
    names(data) <- c(names.data, names.f)
    names(paraP) <- c("beta0", names.f)
  }

  tmp01 <- paste0(response, " ~", " 0 + beta0")

  if(nf > 0){
    tmp02 <- paste0(" + ", names.f, collapse = '')
    tmp03 <- paste0(" + ", terms[-(ind.f-1)], collapse = '')
    formula.new <- as.formula(paste0(tmp01, tmp02, tmp03))
  }else{
    tmp03 <- paste0(" + ", terms, collapse = '')
    formula.new <- as.formula(paste0(tmp01, tmp03))
  }
  cat("formula = ", as.character(formula.new), "\n")

  if(nf > 0){
    mfit.stem <- gam(formula.new, family = ziP(),
      paraPen = list(beta0 = list(P), beta1 = list(P)),
      data = data, offset = offset)
  }else{
    mfit.stem <- gam(formula.new, family = ziP(),
      paraPen = list(beta0 = list(P)),
      data = data, offset = offset)
  }
  # mfit.stem <- gam(formula.new, family = family,
  #   paraPen = paraP, data = data, offset = offset)

  mfit.stem$BQ2 = BQ2
  mfit.stem$K = K
  mfit.stem$P = P
  return(mfit.stem)
}
