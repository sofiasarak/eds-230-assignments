# Lot. Voltera Model, with hunting
#' Compute population the rate of change of populations in a predator, prey, and hunter model
#'
#' @param t 
#' @param pop 
#' @param pars 
#'
#' @returns
#' @export
#'
#' @examples
lotvmod_hunt <- function(t, pop, pars){
  with(as.list(c(pars, pop)), {
    dprey <- rprey * (1 - prey / K) * prey - alpha * prey * pred
    dpred <- eff * alpha * prey * pred - pmort * pred
    return(list(c(dprey, dpred)))
  })
}