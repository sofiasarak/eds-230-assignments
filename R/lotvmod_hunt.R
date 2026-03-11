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
    
    # make sure you are not hunting more prey than exist
    if(prey * hunt_eff > prey){hunt_eff = 0}
    
    # make sure there are no negative values
    prey <- max(prey, 0)
    pred <- max(pred, 0)
    
    # include hunting if prey population is over threshold
    if (prey > thresh){
      dprey <- rprey * (1 - prey / K) * prey - alpha * prey * pred - prey * hunt_eff
      dpred <- eff * alpha * prey * pred - pmort * pred
      return(list(c(dprey, dpred)))
    }else{
      
      # if prey population is under threshold (hunting not allowed)
      
      dprey <- rprey * (1 - prey / K) * prey - alpha * prey * pred
      dpred <- eff * alpha * prey * pred - pmort * pred
      return(list(c(dprey, dpred)))
    }
    
  })
}