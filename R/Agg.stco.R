#'
#' @return
Agg.stco <- function(P.county, diff, date.pred){

  P.state = aggregate(P.county[, -c(1:3)], by = list(P.county$State), sum)
  P.state[, -1] = sweep(P.state[, -1], 1, diff, "+")
  P.state[, -1] = round(P.state[, -1])
  P.state[nrow(P.state) + 1, ] = c(0, colSums(P.state[, -1]))
  P.state[nrow(P.state), 1] = "UnitedState"
  names(P.state) = c("State", paste0('X',
    as.character(date.pred, format = '%Y.%m.%d')))
  return(P.state)

}
