Simple_evolution <- function(MULTI_GENERATION,Good_ID,Top_Estimate){
  # 論文によると、すべての個体がcross_overによってのみ作られているわけではないらしい
  # もとの個体のパラメータをそのままコピーする個体もいるようだが、それがどうやって選ばれるのかは謎
  
  GENERATION_Params <- lapply(MULTI_GENERATION,"[[","Params")

  N_Cross_over <- sum(runif(N_INDIVIDUAL) <= GA_CROSS_OVER)#一点交叉を行う個体の総数

  #一点交叉しない個体の番号を成績上位から選ぶ(最も成績の良かった個体はエリート保存を行う)
  Not_Cross_ID <- Good_ID[c(rep(TRUE,(N_INDIVIDUAL - N_Cross_over)),rep(FALSE,(N_Cross_over)))]
  Cross_ID <- setdiff(Good_ID[1:N_INDIVIDUAL],Not_Cross_ID)

  # 1:エリート保存(Good_ID[1]の個体にはなにもしない) シミュレーションの行える個体が発生してから有効にする
  Best_ID <- Good_ID[1]#最も最良の個体
  Worst_ID <- Good_ID[N_INDIVIDUAL]#最も最悪の個体

  #最良個体のIDを取り出す
  Not_Cross_ID <- setdiff(Not_Cross_ID,c(Best_ID,Good_ID[N_INDIVIDUAL]))
  Cross_ID <- setdiff(Cross_ID,c(Best_ID,Good_ID[N_INDIVIDUAL]))

  MULTI_GENERATION[[Best_ID]][["TREE"]] <- TREE_clean(MULTI_GENERATION[[Best_ID]][["TREE"]])
  MULTI_GENERATION[[Best_ID]][["Estimate"]] <- NULL
  MULTI_GENERATION[[Best_ID]][["Parent"]] <- Best_ID

  #最悪の個体に最良の個体のパラメータを突然変異させて残す これはNot_Cross_overの個体と関係なく行う
  MULTI_GENERATION[[Worst_ID]][["Params"]] <- Simple_mutation(MULTI_GENERATION[[Best_ID]][["Params"]])
  MULTI_GENERATION[[Worst_ID]][["TREE"]] <- TREE_clean(MULTI_GENERATION[[Worst_ID]][["TREE"]])
  MULTI_GENERATION[[Worst_ID]][["Estimate"]] <- NULL
  MULTI_GENERATION[[Worst_ID]][["Parent"]] <- Best_ID

  # 2:一点交叉
  for(IND_i in Cross_ID){
    DECENDENT <- list(NULL)# 次世代の個体、以下でそのパラメータを決める
    
    parent_pair <- sample(Good_ID[1:ES_RAMDA],size=2,prob=SELECT_PROB)

    Upper_Param <- as.list(NULL)
    Lower_Param <- as.list(NULL)
    
    for(Dend_i in 1:N_DENDRITE){
      parent_pair <- sample(parent_pair,size=2)

      new_Stem_diam <- GENERATION_Params[[sample(parent_pair,size=1)]][[Dend_i]][["Stem_diameter"]] #片親のdiamを用いる
      new_Taper <- GENERATION_Params[[sample(parent_pair,size=1)]][[Dend_i]][["Taper"]] #片親のtaperを用いる
      
      K_Cross_point <- sample(1:N_Segs,size=1)
      Ca_Cross_point <- sample(1:N_Segs,size=1)
    
      parent1_K <- GENERATION_Params[[parent_pair[1]]][[Dend_i]][["K_Conductances"]]
      parent2_K <- GENERATION_Params[[parent_pair[2]]][[Dend_i]][["K_Conductances"]]
      parent1_Ca <- GENERATION_Params[[parent_pair[1]]][[Dend_i]][["Ca_Conductances"]]
      parent2_Ca <- GENERATION_Params[[parent_pair[2]]][[Dend_i]][["Ca_Conductances"]]

      new_K_Conductances <- c(parent1_K[c(rep(TRUE,K_Cross_point),rep(FALSE,N_Segs - K_Cross_point))],
                              parent2_K[c(rep(FALSE,K_Cross_point),rep(TRUE,N_Segs - K_Cross_point))])
      new_Ca_Conductances <- c(parent1_Ca[c(rep(TRUE,Ca_Cross_point),rep(FALSE,N_Segs - Ca_Cross_point))],
                               parent2_Ca[c(rep(FALSE,Ca_Cross_point),rep(TRUE,N_Segs - Ca_Cross_point))])


      if(Dend_i == 1){
        Upper_Param <- list(new_Stem_diam,
                            new_Taper,
                            new_K_Conductances,
                            new_Ca_Conductances)
        names(Upper_Param) <- Param_Labels
      }
      else{
        Lower_Param <- list(new_Stem_diam,
                            new_Taper,
                            new_K_Conductances,
                            new_Ca_Conductances)
        names(Lower_Param) <- Param_Labels
      }
    }
    
    DECENDENT <- list(Upper_Param,Lower_Param)

    MULTI_GENERATION[[IND_i]][["Params"]] <- DECENDENT
    #前世代で作ったTREEも初期化
    MULTI_GENERATION[[IND_i]][["TREE"]] <- TREE_clean(MULTI_GENERATION[[IND_i]][["TREE"]])
    MULTI_GENERATION[[IND_i]][["Estimate"]] <- NULL
    MULTI_GENERATION[[IND_i]][["Parent"]] <- parent_pair
    
  }#end(交叉する場合)

  # 3:前世代の個体をコピーして突然変異
  for(IND_i in Not_Cross_ID){
    MULTI_GENERATION[[IND_i]][["Params"]] <- Simple_mutation(MULTI_GENERATION[[IND_i]][["Params"]])
    #前世代で作ったTREEも初期化
    MULTI_GENERATION[[IND_i]][["TREE"]] <- TREE_clean(MULTI_GENERATION[[IND_i]][["TREE"]])
    MULTI_GENERATION[[IND_i]][["Estimate"]] <- NULL
    MULTI_GENERATION[[IND_i]][["Parent"]] <- IND_i
  }
  
  return(MULTI_GENERATION)
}

Simple_mutation <- function(individual){
  for(dend_i in 1:N_DENDRITE){
    Mutation_or_Not <- sample(c(TRUE,FALSE),size=MAX_PARAMS,replace=TRUE)

    diam <- individual[[dend_i]][["Stem_diameter"]]
    Taper <- individual[[dend_i]][["Taper"]]
    K_Conductances <- individual[[dend_i]][["K_Conductances"]]
    Ca_Conductances <- individual[[dend_i]][["Ca_Conductances"]]

    new_diam <- diam
    new_Taper <- Taper
    new_K_Conductances <- K_Conductances
    new_Ca_Conductances <- Ca_Conductances
    
    if(Mutation_or_Not[1]){
      new_diam <- max(diam + rnorm(1,mean=0,sd=MUTATION_alfa_taper_diam_SD),
                      FOURCE_MIN_DIAM)
    }
    
    if(Mutation_or_Not[2]){
      new_Taper <- min(max(Taper + rnorm(1,mean=0,sd=MUTATION_taper_rate)
                           ,0),1)
    }
    
    if(Mutation_or_Not[3]){
      new_K_Conductances <- c()
      for(conductance in K_Conductances){
        new_K_Conductances <- c(new_K_Conductances,
                                max(min(conductance + rnorm(1,mean=0,sd=MUTATION_K_peak),K_MAX),0))*WITH_K
      }
    }
    
    if(Mutation_or_Not[4]){
      new_Ca_Conductances <- c()
      for(conductance in Ca_Conductances){
        new_Ca_Conductances <- c(new_Ca_Conductances,
                                 max(min(conductance + rnorm(1,mean=0,sd=MUTATION_Ca_peak),Ca_MAX),0))*WITH_Ca
      }
    }

    new_Param<- list(new_diam,
                     new_Taper,
                     new_K_Conductances,
                     new_Ca_Conductances)
    names(new_Param) <- Param_Labels

    individual[[dend_i]] <- new_Param
  }    
  return(individual)
}
