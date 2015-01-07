source("Simple_Dendritic_function_parameter.R")
source("display_conductance_on_morphology.R")
source("TREE_simulation_function.R")
source("./display_morphology.R")
source("./display_synapse.R")
source("./set_Upper_or_Lower_or_Other.R")

Dir <- "./Result_investigation/"
source(paste(Dir,"calc_syn_length_diameter.R",sep=""))
source(paste(Dir,"calc_number_synapse.R",sep=""))
source(paste(Dir,"calc_contraction.R",sep=""))
source(paste(Dir,"Stem_diam.R",sep=""))
source(paste(Dir,"calc_Conductance_amount.R",sep=""))

WITH_K <- FALSE
WITH_Ca <- TRUE
RAND_SEED <- 2
DELTA_T <- 30
Function_ratio <- 75
Conductance_ratio <- 0
Morphology_ratio <- 100 - (Function_ratio + Conductance_ratio*(WITH_K || WITH_Ca))
extra_prefix <- paste("Determine_",Function_ratio,"_",Conductance_ratio,sep="")


if(WITH_K*WITH_Ca){
  name <- "k_ca"
}else if(WITH_K){
  name <- "k"
}else if(WITH_Ca){
  name <- "ca"
}else name <- "passive"

if(WITH_Ca && !(WITH_K)){
  SIM_TIME                     <- 100 #シミュレーションの長さ
}else{
  SIM_TIME                     <- 50 #シミュレーションの長さ
}

cat("Delta_T:",DELTA_T,"\n")
cat("SEED:",RAND_SEED,"\n")
cat("inciude conductance:",name,"\n")

Data_Dir <- paste("./",name,"_Result/",sep="")

input_filename <- paste(Data_Dir,"SEED",RAND_SEED,"_","dt",DELTA_T,"_",paste(name,collapse="_"),"_",paste("FR",Function_ratio,sep=""),"_",extra_prefix,"_Best_Datas.xdr",sep="")
cat("input file:",input_filename,"\n")
load(input_filename)

GENERATION <- length(Best_Datas)
#GENERATION <- length(Best_TREEs)
                                        #GENERATION <- c(1,seq(0,650,by=50)[-1])
for(i in GENERATION){
  cat("GENERATION:",i,"\n")
  cat("dt:",DELTA_T,"\n")

  TREE <- Best_Datas[[i]][["TREE"]]
  Params <- Best_Datas[[i]][["Params"]]

  filename <- paste("~/Desktop/",name,"_EPSP.eps",sep="")

  named_TREE <- set_Upper_or_Lower_or_Other(TREE)
  K_Ca_Conductace <- calc_Conductance_amount(TREE)

  N_Upper_synapse <- calc_number_synapse(named_TREE[["Upper_Dend"]])
  N_Lower_synapse <- calc_number_synapse(named_TREE[["Lower_Dend"]])
  
  cat("lower num syn: ",N_Lower_synapse,"upper num syn:",N_Upper_synapse,"\n")
  cat("lower stem diam:",Stem_Diam(named_TREE[["Lower_Dend"]]),"\n")
  cat("upper stem diam:",Stem_Diam(named_TREE[["Upper_Dend"]]),"\n")
  cat("length-diam",calc_syn_length_diameter(named_TREE[["Upper_Dend"]]),calc_syn_length_diameter(named_TREE[["Lower_Dend"]]),"\n")
  cat("sum length:",sum_length(TREE),"Min length:",L0,"\n")
  cat("sum volume:",calc_volume(TREE),"Min volume:",V0,"\n")
  cat("Amount of K,Ca:",K_Ca_Conductace[1],",",K_Ca_Conductace[2],"(S)\n")
  cat("Max Amount of K,Ca:",K_Ca_Conductace[3],",",K_Ca_Conductace[4],"(S)\n")
  cat("Parcent:",round(100*K_Ca_Conductace[1]/K_Ca_Conductace[3],digits=3),",",
      round(100*K_Ca_Conductace[2]/K_Ca_Conductace[4],digits=3),"(%)\n")

  plot.new()
  par(mfrow=c(1,2),
      cex=1.3,
      lwd=3)

  if(WITH_Ca){
    Upper_Dend_Ca <- sapply(TREE[[1]],"[[","Ca_conductance")
    Lower_Dend_Ca <- sapply(TREE[[2]],"[[","Ca_conductance")

    plot(rbind(c(0,0),c(length(TREE[[1]]),max(Upper_Dend_Ca,Lower_Dend_Ca))),
         type="n",
         xlab=expression(paste("length [",mu,"m]",sep="")),
         ylab=expression(paste("conductance [S/c",m^2,"]",sep="")))

    lines(Upper_Dend_Ca,col="red")
    lines(Lower_Dend_Ca,col="blue")
  }
  
  if(N_Upper_synapse > 0 && N_Lower_synapse > 0){
    ## if(WITH_Ca && !WITH_K){
    ##                                     #postscript("WITH_ca_NO_ca_result.eps",horizontal=FALSE)
    ##   par(mfcol=c(1,2))
    ##   print(TREE_simulation_function(TREE,DELTA_T,filename,WITH_K,WITH_Ca,Params)[1:2])
    ##   WITH_Ca <- FALSE
    ##   print(TREE_simulation_function(TREE,DELTA_T,filename,WITH_K,WITH_Ca,Params)[1:2])
    ##   WITH_Ca <- TRUE
    ##                                     #dev.off()
    ## }else{
    print(TREE_simulation_function(TREE,DELTA_T,filename,WITH_K,WITH_Ca,Params)[1:2])
  }else{
    cat("This neuron can't simulation.\n")
  }

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

