Param_init <- function(N_DENDRITE,N_Segs){ #確率的にランダムなパラメータ群を初期Paramとして返す

  INDIVIDUAL <- as.list(NULL)

  for(i in 1:N_DENDRITE){

    Ca_ratio <- dnorm((1:N_Segs)/N_Segs,
                      mean=runif(1,max=1,min=0),
                      sd=runif(1,max=1.5,min=10^(-3)))

    Ca_peak <- runif(1,min = 0,max = Ca_MAX)*WITH_Ca

    Ca_Conductance <- (Ca_ratio/max(Ca_ratio))*Ca_peak

    K_ratio <- dnorm((1:N_Segs)/N_Segs,
                     mean=runif(1,max=1,min=0),
                      sd=runif(1,max=1.5,min=10^(-3)))

    K_peak <- runif(1,min = 0,max = K_MAX)*WITH_K

    K_Conductance <- (K_ratio/max(K_ratio))*K_peak
    
    Param <- list(  ### Stem parameter ###
                                        #Stem_diameter (Const)
                  runif(1,min = 0.2,max = 10),
                                        #taper rate
                  runif(1,min = 10^(-5),max = 1),
                                        #K_Conductance
                  K_Conductance,
                                        #Ca_Conductance
                  Ca_Conductance
                  
                  ##                       #K_Conductance
                  ## runif(N_Segs,min = 0,max = MAX_PEAK)*WITH_K,
                  ##                       #Ca_Conductance
                  ## runif(N_Segs,min = 0,max = Ca_MAX)*WITH_Ca
                  )
    names(Param) <- Param_Labels
    INDIVIDUAL[[length(INDIVIDUAL) + 1]] <- Param
  }
  return(INDIVIDUAL)
}
