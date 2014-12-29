source("make_Branched_Dend.R")

branched_TREE <- function(){
  TREE <- list(make_Branched_Dend(90),
               make_Branched_Dend(-90))

  TREE <- lapply(TREE,set_coordinate)
  TREE <- lapply(TREE,set_synapse)
  
  return(TREE)
}


