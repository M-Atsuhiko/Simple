Simple_Morpho_penalty <- function(TREE){
  #ここにくるTREEは全て、枝の直径が最小のdiamを下回っているもの

  sum_diam <- 0

  for(Dendrite in TREE){
    sum_diam <- sum(sum_diam,Dendrite[[length(Dendrite)]][["diam"]])
  }
  
  return(sum_diam - FOURCE_MIN_DIAM*length(TREE) + MORPHO_PENALTY_MIEW)
}
