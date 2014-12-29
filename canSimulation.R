canSimulation <- function(TREE){

  TREE <- set_Upper_or_Lower_or_Other(TREE)

  #両方のsynaptic zoneにシナプスを形成したかどうかを判定する関数
  Upper_flag <- length(TREE[["Upper_Dend"]]) > 0 #Upper_Synaptic_zoneにシナプスを持っているか
  Lower_flag <- length(TREE[["Lower_Dend"]]) > 0 #Lower_Synaptic_zoneにシナプスを持っているか

  N_comp <- 0
  is_Bif <- FALSE
  Branchs_flag <- FALSE
  
  for(name in TREE_Labels){
    for(Dendrite in TREE[[name]]){
      N_comp <- N_comp + length(Dendrite)
      Branchs_flag <- N_comp > N_MIN_BRANCHS
      is_Bif <- sum(sapply(lapply(Dendrite,"[[","connect"),
                           function(con){
                             if(length(con) >= 2) return(TRUE)#分岐があったら もし複数あったら、is_Bifが1を超えることになる
                             else return(FALSE)
                           }),
                    is_Bif)
    }
    if(is_Bif * Branchs_flag) break
  }
  return(Upper_flag * Lower_flag * is_Bif * Branchs_flag)
}
