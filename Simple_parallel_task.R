parallel_task <- function(Individual_Data){
                                        #まず各コアに個体、世代に固有のRand_SEED値を設定する
  set.seed(Individual_Data[["Seed"]])
  
  Id_Ind<- Individual_Data[["ID_IND"]]
  Morpho_file <- Individual_Data[["Morpho_file_name"]]
  Output_upper_lower <- Individual_Data[["Output_upper_lower_file_name"]]
  Output_lower_upper <- Individual_Data[["Output_lower_upper_file_name"]]
  Output_upper_test <- Individual_Data[["Output_upper_test_file_name"]]
  Output_lower_test <- Individual_Data[["Output_lower_test_file_name"]]
  Params <- Individual_Data[["Params"]]
  Rank <- Individual_Data[["Rank"]]
  Estimate <- Individual_Data[["Estimate"]]
  Soma_Conductance <- Individual_Data[["Soma_Conductance"]]
  TREE <- Individual_Data[["TREE"]]
                                        #main関数では世代で最高の個体がEPSP_PENALTY_MIEWを超える評価値を取らないと順位は計算されないようになっている
  
  TREE <- TREE_modify(Params,TREE)

  simulation_or_not <- Simple_CanSimulation(TREE)
    
  if(simulation_or_not){

    Individual_Data[["TREE_Volume"]] <- calc_volume(TREE)
    
    Conductance_amount <- calc_Conductance_amount(TREE)
    Individual_Data[["K_Amount"]] <- Conductance_amount[1]
    Individual_Data[["Ca_Amount"]] <- Conductance_amount[2]
    make_NEURON_morpho_conductance_data(TREE,Morpho_file)

    system(paste(SIMULATION_SCRIPT,
                 Morpho_file,
                 SIMPLE_SYNAPSE_FILE_NAME,
                 Output_upper_lower,
                 Output_lower_upper,
                 Output_upper_test,
                 Output_lower_test,
                 FIRST_ACTIVATE_TIME,
                 DELTA_T,
                 SIM_TIME,
                 V_INIT,
                 Id_Ind,
                 SIMUL_PARAMETER_FILE,
                 Dir_SimHoc,
                 sep=" "))
                                        #      cat("* NEURON SIMULATION END *\n")
                                        # ここで出力ファイルが本当にできたかどうか確認する
    if(!(file.exists(Output_upper_lower)) || 
       !(file.exists(Output_lower_upper)) ||
       !(file.exists(Output_upper_test)) ||
       !(file.exists(Output_lower_test))){
      cat("ERROR: NEURON output has some error!\n")
    }
  }
                                        # 適応度計算
                                        # EPSEが小さくペナルティを与えることになった場合はSimul_EstimateがNAになっている
  Simul_Estimate <- return_result(Output_upper_lower,
                                  Output_lower_upper,
                                  Output_upper_test,
                                  Output_lower_test,
                                  V_INIT,
                                  simulation_or_not)

  Individual_Data[["TREE"]] <- TREE
  Individual_Data[["Ratio"]] <- Simul_Estimate[[1]] #評価値を代入
  Individual_Data[["Result"]] <- Simul_Estimate[[2]]

  return(Individual_Data)
}
