display_dendrite <- function(Dendrite){
  Color <- "green"
#  rgl.spheres(c(0,0,0),radius = SOMA_DIAM/2,color = "green",sides = 10)#somaの描画
  #somaとdendriteの間に隙間があるように見えるが、これはRGLの球の描画が荒いせい？
  cat("========== CREATING NEURON ==========\n")

  # RGLの描画を行う
  library("rgl")
#  rgl.clear()

    #3次元軸の生成
  rgl.lines(c(-300,300),0,0,color="red")
  rgl.lines(0,c(170,-170),0,color="red")
  rgl.lines(0,0,c(-300,300),color="red")
  rgl.texts(c(300,0,0),c(0,170,0),c(0,0,300),text=c("x","y","z"),color="blue")

  for(i in 1:length(Dendrite)){
    Branch_coordinate <- Dendrite[[i]][["coordi"]]
    diam <- Dendrite[[i]][["diam"]]
    Dendrite_cyl <- cylinder3d(Branch_coordinate,closed = -2,radius = diam,sides = 100)
    shade3d(Dendrite_cyl,color = Color,override=TRUE)
  }

  display_synapse(Dendrite)
}

display_synapse <- function(Dendrite){

  UPPER_SYNAPTIC_ZONE_INDEX      <- 1
  LOWER_SYNAPTIC_ZONE_INDEX      <- 0
  
  UPPER_SYNAPTIC_ZONE_COLOR      <- "red"
  LOWER_SYNAPTIC_ZONE_COLOR      <- "blue"
  SYNAPSE_RADIUS                 <- 2.5
  
  for(Branch in Dendrite){
    synapse_matrix <- Branch[["synapse"]]
    if(is.matrix(synapse_matrix)){
      Branch_coordi <- Branch[["coordi"]]
      for(rn in 1:nrow(synapse_matrix)){
        synapse_position_ratio <- synapse_matrix[rn,1]
        synaptic_zone<- synapse_matrix[rn,2]

        synapse_coordi <-(Branch_coordi[2,] - Branch_coordi[1,])*synapse_position_ratio + Branch_coordi[1,]
          
        if(synaptic_zone == UPPER_SYNAPTIC_ZONE_INDEX){        #UPPER SYNAPTIC ZONE
          rgl.spheres(synapse_coordi,radius = SYNAPSE_RADIUS,sides = 10,color = UPPER_SYNAPTIC_ZONE_COLOR)
        }else if(synaptic_zone == LOWER_SYNAPTIC_ZONE_INDEX){  #LOWER SYNAPTIC ZONE
          rgl.spheres(synapse_coordi,radius = SYNAPSE_RADIUS,sides = 10,color = LOWER_SYNAPTIC_ZONE_COLOR)
        }
      }
    }
  }
}
