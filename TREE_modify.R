TREE_modify <- function(Params,TREE){
  for(i in 1:N_DENDRITE){
    Stem_diam <- Params[[i]][["Stem_diameter"]]
    K_Conductances <- Params[[i]][["K_Conductances"]]
    Ca_Conductances <- Params[[i]][["Ca_Conductances"]]

    diam <- Stem_diam
    
    for(j in 1:N_Segs){
      if(diam < FOURCE_MIN_DIAM) diam <- FOURCE_MIN_DIAM
      
      TREE[[i]][[j]][["diam"]] <- diam
      TREE[[i]][[j]][["K_conductance"]] <- K_Conductances[j]
      TREE[[i]][[j]][["Ca_conductance"]] <- Ca_Conductances[j]
      diam <- diam*(1 + TAPER_RATE)
    }
  }
  return(TREE)
}
