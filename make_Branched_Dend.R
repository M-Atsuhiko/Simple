source("make_Liner_Dend.R")

make_Branched_Dend <- function(angle){
  #160μm のところで枝をつける
  
  length <- COMP_MAX_SIZE
  Branched_angle <- 30

  Main_Branch_length <- 130
  N_comp_Main <- ceiling(Main_Branch_length/length)
  
  Sub1_Branch_length <- 37/cos(30*pi/180)
  N_comp_Sub1 <- ceiling(Sub1_Branch_length/length)
  
  Sub2_Branch_length <- 32/cos(30*pi/180)
  N_comp_Sub2 <- ceiling(Sub2_Branch_length/length)

  Main_Branch <- make_Liner_Dend(N_comp_Main,angle)
  Sub1_Branch <- make_Liner_Dend(N_comp_Sub1,30)
  Sub2_Branch <- make_Liner_Dend(N_comp_Sub2,-30)

  Main_Branch[[N_comp_Main]][["connect"]] <- c(N_comp_Main + 1,
                                               N_comp_Main + N_comp_Sub1 + 1)

  parent_max_i <- N_comp_Main
  parent_path <- Main_Branch_length
  for(i in 1:N_comp_Sub1){

    Sub1_Branch[[i]][["No"]] <- parent_max_i + Sub1_Branch[[i]][["No"]]
    path_leng <- parent_path + Sub1_Branch[[i]][["length"]]
    Sub1_Branch[[i]][["path_leng"]] <- path_leng
    
    if(i == 1){
      Sub1_Branch[[i]][["parent"]] <- parent_max_i
    }else{
      Sub1_Branch[[i]][["parent"]] <- parent_max_i + Sub1_Branch[[i]][["parent"]]
    }
    if(i != N_comp_Sub1){
      Sub1_Branch[[i]][["connect"]] <- parent_max_i + Sub1_Branch[[i]][["connect"]]
    }
  }

  parent_max_i <- N_comp_Main + N_comp_Sub1
  for(i in 1:N_comp_Sub2){

    Sub2_Branch[[i]][["No"]] <- parent_max_i + Sub2_Branch[[i]][["No"]]
    path_leng <- parent_path + Sub2_Branch[[i]][["length"]]
    Sub2_Branch[[i]][["path_leng"]] <- path_leng
    
    if(i == 1){
      Sub2_Branch[[i]][["parent"]] <- parent_max_i
    }else{
      Sub2_Branch[[i]][["parent"]] <- parent_max_i + Sub2_Branch[[i]][["parent"]]
    }
    if(i != N_comp_Sub2){
      Sub2_Branch[[i]][["connect"]] <- parent_max_i + Sub2_Branch[[i]][["connect"]]
    }
  }

  Dendrite <- c(Main_Branch,Sub1_Branch,Sub2_Branch)
  return(Dendrite)
}
