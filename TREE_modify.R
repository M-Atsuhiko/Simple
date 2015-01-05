TREE_modify <- function(Params,TREE){
  for(i in 1:N_DENDRITE){
    Stem_diam <- Params[[i]][["Stem_diameter"]]
    Taper <- Params[[i]][["Taper"]]
    K_Conductances <- Params[[i]][["K_Conductances"]]
    Ca_Conductances <- Params[[i]][["Ca_Conductances"]]

    diam <- Stem_diam
    
    for(j in 1:N_Segs){
      TREE[[i]][[j]][["diam"]] <- diam
      TREE[[i]][[j]][["K_conductance"]] <- K_Conductances[j]
      TREE[[i]][[j]][["Ca_conductance"]] <- Ca_Conductances[j]
      
      #ここで、木に分岐があったら直径を半分にする
      if(length(TREE[[i]][[j]][["connect"]]) > 1) diam <- diam/2
      else diam <- diam*Taper
    }
  }
  return(TREE)
}
