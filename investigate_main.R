make_graphs <- function(All_Lower_data,All_Upper_data,output_file,ylabel){
  mean_Lower_data <- apply(All_Lower_data,1,mean)
  sd_Lower_data <- apply(All_Lower_data,1,sd)
  mean_Upper_data <- apply(All_Upper_data,1,mean)
  sd_Upper_data <- apply(All_Upper_data,1,sd)
  plot(rbind(c(min(Delta_T),min(mean_Lower_data - sd_Lower_data,mean_Upper_data - sd_Upper_data)),
             c(max(Delta_T),max(mean_Lower_data + sd_Lower_data,mean_Upper_data + sd_Upper_data))),
       type="n",
       xlab=expression(paste("Optimized ",Delta,"t [ms]")),
       ylab=ylabel,
       ps=17)

  par(pch=5,cex=1.5)
  
  par(lty="solid",lwd=4)
  lines(cbind(Delta_T,mean_Lower_data),col="blue")

  par(lwd=1)
  arrows(Delta_T,mean_Lower_data,
         Delta_T,mean_Lower_data + sd_Lower_data,
         angle=90,length=0.1,col="blue")
  arrows(Delta_T,mean_Lower_data,
         Delta_T,mean_Lower_data - sd_Lower_data,
         angle=90,length=0.1,col="blue")
  par(lwd=4)
  lines(cbind(Delta_T,mean_Upper_data),col="red")
  par(lwd=1)
  arrows(Delta_T,mean_Upper_data,
         Delta_T,mean_Upper_data + sd_Upper_data,
         angle=90,length=0.1,col="red")
  arrows(Delta_T,mean_Upper_data,
         Delta_T,mean_Upper_data - sd_Upper_data,
         angle=90,length=0.1,col="red")
  legend("topright",legend=c("left","right"),col=c("blue","red"),lty=c("solid","solid"),lwt=c(4,4))
  cat("output:",output_file,"\n")
  dev.copy2eps(file=output_file)
}

Dir <- "./Result_investigation/"

source(paste(Dir,"calc_syn_length_diameter.R",sep=""))
source(paste(Dir,"calc_number_synapse.R",sep=""))
source(paste(Dir,"calc_contraction.R",sep=""))
source(paste(Dir,"Stem_diam.R",sep=""))

source("./Dendritic_function_parameter.R")
source("./TREE_simulation_function.R")

WITH_K <- FALSE
WITH_Ca <- FALSE
SEED <- c(2,3,4,9,10,11)
Function_ratio <- 75
extra_prefix <- "times_Ldet"

Delta_T<- seq(5,30,by=5)
Morphology_ratio <- 100 - Function_ratio

if(WITH_K && !(WITH_Ca)){
  contain_conductance <- "k"
}else if(WITH_K && WITH_Ca){
  contain_conductance <- "k_ca"
}else if(WITH_Ca){
  contain_conductance <- "ca"
}else{
  contain_conductance <- "passive"
}

Data_Directory <- paste("./",contain_conductance,"_Result/",sep="")
output_Directory <- "./Result/"
file_prefix <- paste(output_Directory,contain_conductance,"_FR",Function_ratio,"_result_",extra_prefix,"_",sep="")

Ratios <- c()
All_Ratios <- c()
                                        # synapse_length_diam
Lower_length_diams <- c()
Upper_length_diams <- c()
All_Lower_length_diams <- c()
All_Upper_length_diams <- c()

# synapse nums
Lower_syn_nums <- c()
Upper_syn_nums <- c()
All_Lower_syn_nums <- c()
All_Upper_syn_nums <- c()

# contruction
contructions <- c()
All_contructions <- c()

# Stem diam
Lower_Stem_diams <- c()
All_Lower_Stem_diams <- c()
Upper_Stem_diams <- c()
All_Upper_Stem_diams <- c()

TREE_sizes <- c()
All_TREE_sizes <- c()


for(dt in Delta_T){
  for(seed in SEED){
    cat("SEED: ",seed," dt: ",dt,"\n")
    load(paste(Data_Directory,"SEED",seed,"_dt",dt,"_",contain_conductance,"_FR",Function_ratio,"_",extra_prefix,"_Best_Datas.xdr",sep=""))
    TREE <- Best_Datas[[length(Best_Datas)]][["TREE"]]
    rm("Best_Datas")
    if(WITH_Ca && !(WITH_K)){
      SIM_TIME                     <- 100 #シミュレーションの長さ
    }else{
      SIM_TIME                     <- 50 #シミュレーションの長さ
    }
    Estimate_ratio_length <- TREE_simulation_function(TREE,dt,"Not_display",WITH_K,WITH_Ca)
    if(is.na(Estimate_ratio_length[1])) cat("Data is not optimized!\n")
    Ratios <- c(Ratios,Estimate_ratio_length[2])
    TREE_sizes <- c(TREE_sizes,Estimate_ratio_length[3])
    lower_upper_syn_length_diam <- calc_syn_length_diameter(TREE)
    lower_upper_syn_num <- calc_number_synapse(TREE)
    Lower_length_diams <- c(Lower_length_diams,lower_upper_syn_length_diam[1])
    Upper_length_diams <- c(Upper_length_diams,lower_upper_syn_length_diam[2])
    Lower_syn_nums <- c(Lower_syn_nums,lower_upper_syn_num[1])
    Upper_syn_nums <- c(Upper_syn_nums,lower_upper_syn_num[2])
    Lower_Stem_diams <- c(Lower_Stem_diams,Stem_Diam(TREE,LOWER_SYNAPTIC_ZONE_INDEX))
    Upper_Stem_diams <- c(Upper_Stem_diams,Stem_Diam(TREE,UPPER_SYNAPTIC_ZONE_INDEX))
  }
  
  All_Ratios <- rbind(All_Ratios,Ratios)
  All_TREE_sizes <- rbind(All_TREE_sizes,TREE_sizes)
  
  All_Lower_length_diams <- rbind(All_Lower_length_diams,Lower_length_diams)
  All_Upper_length_diams <- rbind(All_Upper_length_diams,Upper_length_diams)

  All_Lower_syn_nums <- rbind(All_Lower_syn_nums,Lower_syn_nums)
  All_Upper_syn_nums <- rbind(All_Upper_syn_nums,Upper_syn_nums)

  All_Lower_Stem_diams <- rbind(All_Lower_Stem_diams,Lower_Stem_diams)
  All_Upper_Stem_diams <- rbind(All_Upper_Stem_diams,Upper_Stem_diams)
  
  Ratios <- c()
  Lower_length_diams <- c()
  Upper_length_diams <- c()
  Lower_syn_nums <- c()
  Upper_syn_nums <- c()
  Lower_Stem_diams <- c()
  Upper_Stem_diams <- c()
  TREE_sizes <- c()
}


print(All_Ratios)
print(All_TREE_sizes)
print(All_Lower_length_diams)
print(All_Upper_length_diams)
print(All_Lower_syn_nums)
print(All_Upper_syn_nums)
print(All_Lower_Stem_diams)
print(All_Upper_Stem_diams)

#グラフの作成
output_file <- paste(file_prefix,"Ratios.eps",sep="")
mean_Ratios<- apply(All_Ratios,1,mean)
sd_Ratios<- apply(All_Ratios,1,sd)
plot(rbind(c(min(Delta_T),min(mean_Ratios - sd_Ratios)),
           c(max(Delta_T),max(mean_Ratios + sd_Ratios))),
     type="n",
     xlab=expression(paste("Optimized ",Delta,"t [ms]")),
     ylab="F",
     ps=17)
lines(cbind(Delta_T,mean_Ratios),col="red",
      lwd=4)
arrows(Delta_T,mean_Ratios,
       Delta_T,mean_Ratios + sd_Ratios,
       angle=90,length=0.1,lwd=2)
arrows(Delta_T,mean_Ratios,
       Delta_T,mean_Ratios - sd_Ratios,
       angle=90,length=0.1,lwd=2)
cat("output:",output_file,"\n")
dev.copy2eps(file=output_file)


output_file <- paste(file_prefix,"TREE_sizes.eps",sep="")
mean_TREE_sizes<- apply(All_TREE_sizes,1,mean)
sd_TREE_sizes<- apply(All_TREE_sizes,1,sd)
plot(rbind(c(min(Delta_T),min(mean_TREE_sizes - sd_TREE_sizes)),
           c(max(Delta_T),max(mean_TREE_sizes + sd_TREE_sizes))),
     type="n",
     xlab=expression(paste("Optimized ",Delta,"t [ms]")),
     ylab="Size",
     ps=17)
lines(cbind(Delta_T,mean_TREE_sizes),col="red",
      lwd=4)
arrows(Delta_T,mean_TREE_sizes,
       Delta_T,mean_TREE_sizes + sd_TREE_sizes,
       angle=90,length=0.1,lwd=2)
arrows(Delta_T,mean_TREE_sizes,
       Delta_T,mean_TREE_sizes - sd_TREE_sizes,
       angle=90,length=0.1,lwd=2)
cat("output:",output_file,"\n")
dev.copy2eps(file=output_file)

make_graphs(All_Lower_length_diams,All_Upper_length_diams,paste(file_prefix,"syn_diam.eps",sep=""),"Length to synapse/diameter")
make_graphs(All_Lower_syn_nums,All_Upper_syn_nums,paste(file_prefix,"num_syn.eps",sep=""),"number of synapse")
make_graphs(All_Lower_Stem_diams,All_Upper_Stem_diams,paste(file_prefix,"stem_diam.eps",sep=""),"Stem diameter")

save_Ratio <- cbind(Delta_T,All_Ratios)
save(save_Ratio,file=paste(file_prefix,"save_Ratio.xdr",sep=""))
#file_prefix <- paste(output_Directory,contain_conductance,"_result_",sep="")

