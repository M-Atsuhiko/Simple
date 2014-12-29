display_dendrite <- function(Dendrite,Color){
#  rgl.spheres(c(0,0,0),radius = SOMA_DIAM/2,color = "green",sides = 10)#somaの描画
  #somaとdendriteの間に隙間があるように見えるが、これはRGLの球の描画が荒いせい？

  for(i in 1:length(Dendrite)){
    Branch_coordinate <- Dendrite[[i]][["coordi"]]
    diam <- Dendrite[[i]][["diam"]]
    path_length <- Dendrite[[i]][["path_leng"]]
    Branch_length <- Dendrite[[i]][["length"]]
    Dendrite_cyl <- cylinder3d(Branch_coordinate,closed = -2,radius = diam,sides = 100)
    shade3d(Dendrite_cyl,color = Color,override=TRUE)
#    rgl.texts(Branch_coordinate[2,],text=paste(path_length)) #path lengthが正しいかテスト
#    rgl.texts((Branch_coordinate[2,] - Branch_coordinate[1,])/2 + Branch_coordinate[1,],text=paste(Branch_length),color="blue")
  }
}

display_morphology<- function(TREE){
  #--- シミュレーション全体の状態を3次元空間状に図示する --#

  cat("========== CREATING NEURON ==========\n")

  # RGLの描画を行う際
  library("rgl")
  rgl.clear()

  #3次元軸の生成
  rgl.lines(c(-300,300),0,0,color="red")
  rgl.lines(0,c(UPPER_SYNAPTIC_ZONE_Y,LOWER_SYNAPTIC_ZONE_Y),0,color="red")
  rgl.lines(0,0,c(-300,300),color="red")
  rgl.texts(c(300,0,0),c(0,UPPER_SYNAPTIC_ZONE_Y,0),c(0,0,300),text=c("x","y","z"),color="blue")

  #somaの描画
  rgl.spheres(c(0,0,0),radius = SOMA_DIAM/2,sides = 10,color="green")#,texture = SOMA_TEXTURE)
  
  #synaptic_zoneの描画
#  display_synaptic_zone()

  #Dendriteの描画
  for(i in 1:length(TREE)){
    display_dendrite(TREE[[i]],BRANCH_COLOR)
  }
  
  #synapseの描画
  for(i in 1:length(TREE)){
    display_synapse(TREE[[i]])
  }

  # Branchの番号を描画する
#  for(i in 1:length(TREE)){
#    display_branch_No(TREE[[i]],Params[[i]][["Dendrite_Color"]])
#  }
}
