source("Dendritic_function_parameter.R")
source("display_conductance_on_morphology.R")
source("display_conductance_on_morphology.R")

WITH_K <- FALSE
WITH_Ca <- TRUE
SEED <- c(2:7,9,10)
  
if(WITH_K*WITH_Ca){
  name <- "k_ca"
}else if(WITH_K){
  name <- "k"
}else if(WITH_Ca){
  name <- "ca"
}else name <- "passive"

Data_Dir <- "./Morphology/"

for(dt in Delta_T){
}
load(paste(Data_Dir,"SEED",RAND_SEED,"_dt",DELTA_T,"_",name,"NA_Best_TREEs.xdr",sep=""))

TREE <- Best_TREEs[[length(Best_TREEs)]]
