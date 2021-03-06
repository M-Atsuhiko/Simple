estimate <- function(result_file_ul,result_file_lu,result_file_test_u,result_file_test_l,V_INIT,TREE){
  
  data_ul<- read.table(result_file_ul)
  data_lu<- read.table(result_file_lu)

  data_test_u<- read.table(result_file_test_u)
  data_test_l<- read.table(result_file_test_l)

  isEPSP_SMALL <- FALSE #LOWERのEPSPが2mVを超えているかどうか

  Oscillation <- FALSE
  Over_MAXmv <- FALSE

  Ratio <- -1

  if(as.character(data_test_u[1,1]) == EPSP_ERROR_MESSAGE ||
     as.character(data_test_l[1,1]) == EPSP_ERROR_MESSAGE ||
     as.character(data_ul[1,1]) == EPSP_ERROR_MESSAGE ||
     as.character(data_lu[1,1]) == EPSP_ERROR_MESSAGE){
    isEPSP_SMALL <- TRUE
  }
  
  # 最後の5ステップで膜電位が上昇してはいけない
  # MAXmVを超えてはいけない
  if((WITH_K || WITH_Ca) && !(isEPSP_SMALL)){
    length_data_ul <- nrow(data_ul)
    length_data_lu <- nrow(data_lu)
    length_data_test_u <- nrow(data_test_u)
    length_data_test_l <- nrow(data_test_l)
    
    if(length_data_ul < (NOT_ACTIVATE_STEP + 1) ||#CVodeが作成したファイルデータが非常に少ない場合
       length_data_lu < (NOT_ACTIVATE_STEP + 1) ||
       length_data_test_u< (NOT_ACTIVATE_STEP + 1) ||
       length_data_test_l < (NOT_ACTIVATE_STEP + 1)){
      stop("Error! CVode resulted very small data file with less than",NOT_ACTIVATE_STEP,"rows!\n",)#エラーとして終了する
    }

    #最後のNOT_ACTIVATE_STEP個のデータについて、ひとつ後ろのデータを引いたベクトルを作る
    diff_ul <- data_ul[length_data_ul:(length_data_ul - NOT_ACTIVATE_STEP),2] - data_ul[(length_data_ul - 1):(length_data_ul - (NOT_ACTIVATE_STEP + 1)),2]
    diff_lu <- data_lu[length_data_lu:(length_data_lu - NOT_ACTIVATE_STEP),2] - data_lu[(length_data_lu - 1):(length_data_lu - (NOT_ACTIVATE_STEP + 1)),2]
    diff_u <- data_test_u[length_data_test_u:(length_data_test_u - NOT_ACTIVATE_STEP),2] - data_test_u[(length_data_test_u - 1):(length_data_test_u - (NOT_ACTIVATE_STEP + 1)),2]
    diff_l <- data_test_l[length_data_test_l:(length_data_test_l - NOT_ACTIVATE_STEP),2] - data_test_l[(length_data_test_l - 1):(length_data_test_l - (NOT_ACTIVATE_STEP + 1)),2]
    #作成したベクトルが全て負の値なれば、最後の5ステップで膜電位が上昇していないことが確かめられる

    all_vect <- c(diff_ul,diff_lu,diff_u,diff_l)

    if(length(all_vect[all_vect > 0]) > 0)
      Oscillation <- TRUE

    if(max(data_test_u[,2]) > MAX_MEMBRANE ||
       max(data_test_l[,2]) > MAX_MEMBRANE ||
       max(data_ul[,2]) > MAX_MEMBRANE ||
       max(data_lu[,2]) > MAX_MEMBRANE)
      Over_MAXmv <- TRUE
  }
  
  if(isEPSP_SMALL || Oscillation || Over_MAXmv){ #EPSEが小さすぎる場合 #膜電位が振動する場合 #膜電位が-MAXを超えた場合
#    cat("EPSP is very small\n")
    Estimate_Value <- NA
  }else{                        #EPSPがMIN_EPSP以上の場合
    Mul <- max(data_ul[,2])#Non-preferrd direction
    Mlu <- max(data_lu[,2])#-preferrd direction
    
    Ratio <- abs(V_INIT - Mul)/abs(V_INIT - Mlu) #Non-pref/Pref の比
    
#    cat("Ratio: ",Ratio,"\n")

#    if(Ratio > BAD_RATIO)#負の値を返す
#      Estimate_Value <- -1*Ratio
#    else{#ここが本来の評価式
#      Estimate_Value <- 100 - 10*Ratio - ALPHA*(ALL_BRANCH_LENGTH/L0)
      #上の式だと、サイズによっては0未満の値になる
      #評価関数変更
    if(WITH_K + WITH_Ca) TREE_conductance_Ratio <- calc_Conductance_ratio(TREE,WITH_K,WITH_Ca)
    else TREE_conductance_Ratio <- 0
      
    Func_minus <- Function_ratio*(1 - (F_MAX^(-1))/Ratio)
#    Morpho_minus <- Morphology_ratio*(1 - V0/calc_volume(TREE))
    Conductance_minus <- Conductance_ratio*TREE_conductance_Ratio
      
#    Estimate_Value <- 100 - Func_minus - Morpho_minus - Conductance_minus
    Estimate_Value <- 100 - Func_minus - Conductance_minus
#    }
  }
  return(c(Estimate_Value,Ratio^(-1)))
}
