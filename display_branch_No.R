display_branch_No <- function(Dendrite,No_col){
  for(Branch in Dendrite){
    No <- Branch[["No"]]
    coordi <- Branch[["coordi"]]

    Branch_mid_coordi <- (coordi[2,] - coordi[1,])/2 + coordi[1,]

    No_coordi <- Branch_mid_coordi + c(15,0,0)

    rgl.texts(No_coordi,text=paste(No),color=No_col)
  }
}
