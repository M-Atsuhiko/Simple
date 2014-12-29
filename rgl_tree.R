source("Dendritic_function_parameter.R")

rgl_tree <- function(filename){
  load(filename)
  print(TREE)
  display_morphology(TREE)
}

rgl_tree("/Users/Atsuhiko/Dropbox/workspace/Function_Morphology/stochastic_morphology/MorDataR/TREE_G10_ID10.xdr")
