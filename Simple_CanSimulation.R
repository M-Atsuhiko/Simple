Simple_CanSimulation <- function(TREE){
  #Simpleの場合は末端の直径が最小値を超えているかどうかで形態エラーかどうかを判断する

  canSimulation <- TRUE

  for(Dendrite in TREE){
    for(Branch in Dendrite){
      if(Branch[["diam"]] < FOURCE_MIN_DIAM){
        canSimulation <- FALSE
      }
    }
  }
  
  return(canSimulation)
}
