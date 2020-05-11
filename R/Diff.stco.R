#'
#' @return
Diff.stco <- function(date.est, type = 'infec'){
  if(type == 'infec') {
    file.name = paste0("data/Cum_Infected_state_", as.character(date.update), "_updated.tsv")
    P.state = read.delim(file.name)
    file.name = paste0("data/Cum_Infected_county_", as.character(date.update), "_updated.tsv")
    P.county = read.delim(file.name)
  }

  if(type == 'death') {
    file.name = paste0("data/Cum_Death_state_", as.character(date.update), "_updated.tsv")
    P.state = read.delim(file.name)
    file.name = paste0("data/Cum_Death_county_", as.character(date.update), "_updated.tsv")
    P.county = read.delim(file.name)
  }

  P.stco = aggregate(P.county[, -(1:3)], by = list(P.county$State), sum)
  var.name = paste0("X", as.character(date.est, format = '%Y.%m.%d'))
  Diff = data.frame(State = P.stco$Group.1,
    diff = P.state[, var.name] - P.stco[, var.name])
  return(Diff)
}
