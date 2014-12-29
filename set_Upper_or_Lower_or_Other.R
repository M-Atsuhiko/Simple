set_Upper_or_Lower_or_Other <- function(TREE){
  Upper_Dend <- as.list(NULL)
  Lower_Dend <- as.list(NULL)
  Other_Dend <- as.list(NULL)
  for(Dend in TREE){
    frag <- 0
    for(Branch in Dend){
      if(is.matrix(Branch[["synapse"]])){
        if(Branch[["synapse"]][1,2] == UPPER_SYNAPTIC_ZONE_INDEX){
          Upper_Dend[[length(Upper_Dend) + 1]] <- Dend
          frag <- 1
        }else{
          Lower_Dend[[length(Lower_Dend) + 1]] <- Dend
          frag <- 2
        }
        break
      }
    }
    if(frag == 0)
      Other_Dend[[length(Other_Dend) + 1]] <- Dend
  }

  TREE <- list(Upper_Dend,Lower_Dend,Other_Dend)
  names(TREE) <- TREE_Labels
  return(TREE)
}

