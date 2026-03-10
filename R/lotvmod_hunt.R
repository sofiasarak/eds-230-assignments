# Lot. Voltera Model, with hunting
#' Compute population the rate of change of populations in a predator, prey, and hunter model
#'
#' @param t 
#' @param pop 
#' @param pars 
#'  \emph{rprey} is growth rate of prey population;
#'  \emph{eff} is the rate of ingestion of prey by predators (or effort)
#'  \emph{alpha} is a interaction coefficient (higher values greater interaction)
#’  \emph{pmort}  mortality rate of predator population
#'  \emph{hunt_eff} hunting effort as percetnage of prey population
#'
#' @returns
#' @export
#'
#' @examples
lotvmod_hunt <- function(t, pop, pars){
  with(as.list(c(pars, pop)), {
    
    # include hunting if prey population is over 100
    if (prey > 100){
      dprey <- rprey * (1 - prey / K) * prey - alpha * prey * pred - prey * hunt_eff
      dpred <- eff * alpha * prey * pred - pmort * pred
      return(list(c(dprey, dpred)))
    }else{
      
      # if prey population is under 100 (hunting not allowed)
      
      dprey <- rprey * (1 - prey / K) * prey - alpha * prey * pred
      dpred <- eff * alpha * prey * pred - pmort * pred
      return(list(c(dprey, dpred)))
    }
    
  })
}