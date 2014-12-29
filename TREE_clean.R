TREE_clean <- function(TREE){
  for(i in 1:N_DENDRITE){
    for(j in 1:length(TREE[[i]])){
      TREE[[i]][[j]][["diam"]] <- 1
      TREE[[i]][[j]][["K_conductance"]] <- 0
      TREE[[i]][[j]][["Ca_conductance"]] <- 0
    }
  }
  return(TREE)
}
