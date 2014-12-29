Data_Dir <- "./test/"
Filename <- "SEED2_dt5_passive75_LAST_GENERATION.xdr"

load(paste(Data_Dir,Filename,sep=""))

Param_Mat <- c()

for(Params in Last_Generation){
  for(Param in Params){
    Param_Mat <- rbind(Param_Mat,unlist(Param))
  }
}

plot(cbind(Param_Mat[,"Stem_rotation_MIEW"],Param_Mat[,"Stem_rotation_SIGMA"]))
