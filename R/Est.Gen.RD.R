Est.Gen.RD <- function(date.est, est.h, CasesP.state, CasesP.county, CasesD.state, CasesD.county, CasesR.state,
                       R.rate.est = FALSE, R.rate = 0.1, D.rate.est = TRUE, D.rate = NULL){
  date.start = as.Date(date.est - est.h)
  date.all = as.character(seq(date.est, date.start, by=-1)) # all the date in estimation

  if (R.rate.est){
    CasesI.state <- data.frame(State = CasesP.state$State, CasesP.state[,paste0('P_',date.all)] -
                                  CasesR.state[,paste0('R_',date.all)] -
                                  CasesD.state[,paste0('D_',date.all)])
    names(CasesI.state)[-1] <- paste0('I_', as.character(date.all,format = '%Y.%m.%d'))
    # Indices where recovered cases are observed in the states:
    ind.R.obs <- which(CasesR.state[2] > 0)

    diffR.state <- CasesR.state[ind.R.obs,paste0('R_',date.all[-(est.h+1)])] -
      CasesR.state[ind.R.obs, paste0('R_',date.all[-1])]
    ind.R.fit <- ind.R.obs[apply(diffR.state[,-1], 1, FUN = function(x){all(x > 0)})]
    CasesR.state$State[ind.R.fit]
    # ind.R.fit <- ind.R.obs[apply(diffR.state, 1, FUN = function(x){all(x >= 0) & any(x>0)})]
    # CasesR.state$State[ind.R.fit]

    diffR.state <- CasesR.state[ind.R.fit,paste0('R_',date.all[-(est.h+1)])] -
      CasesR.state[ind.R.fit, paste0('R_',date.all[-1])]

    diffR.state.vec <- c(t(as.matrix(diffR.state)))

    I.state <- c(t(as.matrix(CasesI.state[ind.R.fit, paste0('I_',date.all[-1])])))
    fit.df <- data.frame(State = rep(CasesR.state$State[ind.R.fit], each = est.h), diffR = diffR.state.vec, I = I.state)
    lm1.fit <- lm(diffR ~ 0 + I, data = fit.df[fit.df$diffR>0,])

    nu <- lm1.fit$coefficients
    cat('Recovery Rate = ',nu, '\n')
    # if use all 34 states: 0.012, if use only 8 states: 0.0146
  }else{
    nu <- R.rate
  }

  # since R_{i,t}- (1-nu)R_{i,t-1} = nu * (P_{i,t-1} - D_{i,t-1})
  # nu is small
  CasesPmD.county <- CasesP.county[,c('ID', 'County', 'State')]
  CasesPmD.county <- cbind(CasesPmD.county,
                            CasesP.county[,paste0('P_',date.all)] -
                              CasesD.county[,paste0('D_',date.all)])
  names(CasesPmD.county)[-c(1:3)] <- paste0('PmD_',date.all)

  CasesPmD.state <- CasesP.state$State
  CasesPmD.state <- cbind(CasesPmD.state,
                           CasesP.state[,paste0('P_',date.all)] -
                             CasesD.state[,paste0('D_',date.all)])
  names(CasesPmD.state)[1] <- 'State'
  names(CasesPmD.state)[-1] <- paste0('PmD_',date.all)
  CasesPmD.county$State <- as.character(CasesPmD.county$State)
  CasesPmD.state$State <- as.character(CasesPmD.state$State)
  CasesR.state$State <- as.character(CasesR.state$State)
  CasesP.state$State <- as.character(CasesP.state$State)

  CasesR.county <- CasesP.county[,c('ID', 'County', 'State')]
  for (i in 1:(est.h+1)){
    initial_R.df <- CasesP.county[,c('ID', 'County', 'State')]
    initial_R.df <- initial_R.df %>%
      mutate(County_case = CasesPmD.county[,paste0('PmD_',date.all[i])]) %>%
      left_join(CasesPmD.state[,c('State',paste0('PmD_',date.all[i]))],
                by= c('State' = 'State')) %>%
      left_join(CasesR.state[,c('State',paste0('R_',date.all[i]))],
                by= c('State' = 'State'))
    names(initial_R.df)[5] <- 'State_case'
    names(initial_R.df)[6] <- 'State_R'
    R_name <- paste0('R_',date.all[i])
    CasesR.county <- CasesR.county %>%
      mutate(!!R_name := round(initial_R.df$County_case/initial_R.df$State_case*initial_R.df$State_R))
  }

  CasesI.county <- CasesP.county[,c('ID', 'County', 'State')]
  CasesI.county <- cbind(CasesI.county,
                          CasesP.county[,paste0('P_',date.all)] -
                            CasesR.county[,paste0('R_',date.all)] -
                            CasesD.county[,paste0('D_',date.all)])
  names(CasesI.county)[-c(1:3)] <- paste0('I_',date.all)

  correct.R <- round(CasesI.county[,paste0('I_',date.all[1])] * 0.05)
  CasesI.county[,paste0('I_',date.all[1])] <- CasesI.county[,paste0('I_',date.all[1])] - correct.R
  CasesR.county[,paste0('R_',date.all[1])] <- CasesR.county[,paste0('R_',date.all[1])] + correct.R


  if (D.rate.est){
    # Estimate rate of death:
    diffD.county <- CasesD.county[,paste0('D_',date.all[-(est.h+1)])] -
      CasesD.county[, paste0('D_',date.all[-1])]

    diffD.county.vec <- c(as.matrix(diffD.county))
    I.county <- c(as.matrix(CasesI.county[, paste0('I_',date.all[-1])]))
    fit2.df <- data.frame(diffD = diffD.county.vec, I = I.county)
    lm2.fit <- lm(diffD ~ 0 + I, data = fit2.df)

    D.rate <- lm2.fit$coefficients
    cat('Fatal Rate = ',D.rate, '\n')
  }else{
    D.rate <- D.rate
  }
  return(list(recovery_rate = nu, fatal_rate = D.rate, CasesI.county = CasesI.county, CasesR.county = CasesR.county))
}



#
