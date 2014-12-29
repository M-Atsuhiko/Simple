source("make_Liner_Dend.R")

liner_TREE <- function(){
  N_comp <- 40
  TREE <- list(make_Liner_Dend(N_comp,90),
               make_Liner_Dend(N_comp,-90))
  TREE <- lapply(TREE,set_coordinate)
  TREE <- lapply(TREE,set_synapse)
  return(TREE)
}
