Param_init <- function(N_DENDRITE,N_Segs){ #確率的にランダムなパラメータ群を初期Paramとして返す

  INDIVIDUAL <- as.list(NULL)

  for(i in 1:N_DENDRITE){
    Param <- list(  ### Stem parameter ###
                                        #Stem_diameter (Const)
                  runif(1,min = 0.2,max = 10),
#                                        #taper rate
#                  runif(1,min = 0.2,max = 1),
                                        #K_Conductance
                  runif(N_Segs,min = 0,max = MAX_PEAK)*WITH_K,
                                        #Ca_Conductance
                  runif(N_Segs,min = 0,max = Ca_MAX)*WITH_Ca
                  )
    names(Param) <- Param_Labels
    INDIVIDUAL[[length(INDIVIDUAL) + 1]] <- Param
  }
  return(INDIVIDUAL)
}
