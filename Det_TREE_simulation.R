source("Simple_Dendritic_function_parameter.R")
source("display_conductance_on_morphology.R")
source("TREE_simulation_function.R")
source("./display_morphology.R")
source("./display_synapse.R")

Dir <- "../R_functions/"
source(paste(Dir,"calc_syn_length_diameter.R",sep=""))
source(paste(Dir,"calc_number_synapse.R",sep=""))
source(paste(Dir,"calc_contraction.R",sep=""))
source(paste(Dir,"Stem_diam.R",sep=""))
source(paste(Dir,"calc_Conductance_amount.R",sep=""))
source(paste(Dir,"set_Upper_or_Lower_or_Other.R",sep=""))

WITH_K <- FALSE
WITH_Ca <- TRUE
RAND_SEED <- 1
DELTA_T <- 30
Function_ratio <- 75
Conductance_ratio <- 100 - Function_ratio
Morphology_ratio <- 0
extra_prefix <- paste("determine_liner_",Function_ratio,"_",Conductance_ratio,sep="")

if(WITH_K*WITH_Ca){
  name <- "k_ca"
}else if(WITH_K){
  name <- "k"
}else if(WITH_Ca){
  name <- "ca"
}else name <- "passive"

cat("Delta_T:",DELTA_T,"\n")
cat("SEED:",RAND_SEED,"\n")
cat("inciude conductance:",name,"\n")

#Data_Dir <- paste("~/Datas/Tsuishi/",name,"_Result/",sep="")
#Data_Dir <- "./test/"
Data_Dir <- paste("./",name,"_Result/",sep="")

#load(paste(Data_Dir,"SEED",RAND_SEED,"_dt",DELTA_T,"_",name,"NA_LAST_TREEs.xdr",sep=""))
#load(paste(Data_Dir,"SEED",RAND_SEED,"_dt",DELTA_T,"_",name,"NA_Best_TREEs.xdr",sep=""))
input_filename <- paste(Data_Dir,"SEED",RAND_SEED,"_","dt",DELTA_T,"_",paste(name,collapse="_"),"_",paste("FR",Function_ratio,sep=""),"_",extra_prefix,"_Best_Datas.xdr",sep="")
#input_filename <- paste(Data_Dir,"SEED",RAND_SEED,"_","dt",DELTA_T,"_",paste(name,collapse="_"),"_",paste("FR",Function_ratio,sep=""),"_",extra_prefix,"_Best_TREEs.xdr",sep="")
cat("input file:",input_filename,"\n")
load(input_filename)

GENERATION <- length(Best_Datas)
#GENERATION <- length(Best_TREEs)
                                        #GENERATION <- c(1,seq(0,650,by=50)[-1])
for(i in GENERATION){
  cat("GENERATION:",i,"\n")
  cat("dt:",DELTA_T,"\n")

#  TREE <- Best_TREEs[[i]]
  TREE <- Best_Datas[[i]][["TREE"]]
  Params <- Best_Datas[[i]][["Params"]]
  print(Params)

#  filename <- "Display"
  filename <- paste("~/Desktop/",name,"_EPSP.eps",sep="")

  if(WITH_Ca && !(WITH_K)){
    SIM_TIME                     <- 100 #シミュレーションの長さ
  }else{
    SIM_TIME                     <- 50 #シミュレーションの長さ
  }


  max_conductance <- 0

  max_conductance <- max(Params[[1]][["Ca_Conductances"]],
                         Params[[2]][["Ca_Conductances"]])
  
  par(mfrow=c(1,2))
  for(Param_i in 1:length(Params)){

    if(Param_i == 1) color = "red"
    else color = "blue"
    
    plot(Params[[Param_i]][["Ca_Conductances"]],
         type="l",
         ylim=c(0,max_conductance),
         col=color)
    par(new=TRUE)
  }
  par(new=FALSE)


  named_TREE <- set_Upper_or_Lower_or_Other(TREE)

  K_Ca_Conductace <- calc_Conductance_amount(TREE,WITH_K,WITH_Ca)

  N_Upper_synapse <- calc_number_synapse(named_TREE[["Upper_Dend"]])
  N_Lower_synapse <- calc_number_synapse(named_TREE[["Lower_Dend"]])
  

  cat("lower stem diam:",Stem_Diam(named_TREE[["Lower_Dend"]]),"\n")
  cat("upper stem diam:",Stem_Diam(named_TREE[["Upper_Dend"]]),"\n")
  cat("length-diam",calc_syn_length_diameter(named_TREE[["Upper_Dend"]]),calc_syn_length_diameter(named_TREE[["Lower_Dend"]]),"\n")
  cat("Amount of K,Ca:",K_Ca_Conductace[1],",",K_Ca_Conductace[2],"(S)\n")
  cat("Max Amount of K,Ca:",K_Ca_Conductace[3],",",K_Ca_Conductace[4],"(S)\n")
  cat("Parcent:",round(100*K_Ca_Conductace[1]/K_Ca_Conductace[3],digits=3),",",
      round(100*K_Ca_Conductace[2]/K_Ca_Conductace[4],digits=3),"(%)\n")

  print(TREE_simulation_function(TREE,DELTA_T,filename,WITH_K,WITH_Ca,Params)[1:2])

  if(WITH_K && WITH_Ca){
    display_conductance_on_morphology(TREE,"Ca_conductance")
  }else if(WITH_Ca){
    display_conductance_on_morphology(TREE,"Ca_conductance")
  }else if(WITH_K){
    display_conductance_on_morphology(TREE,"K_conductance")
  }else {
    display_morphology(TREE)
  }
                                        #display_synaptic_zone()
#  readline("next?")
}

