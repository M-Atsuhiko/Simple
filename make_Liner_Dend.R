make_Liner_Dend <- function(N_comp,angle){

  Dendrite <- as.list(NULL)
  length <- COMP_MAX_SIZE
  
  for(i in 1:N_comp){
    No <- i

    rotation <- 0
    diam <- 1
    path_leng <- i*length
    
    if(i == 1){
      elevation <- angle
      parent <- -1
    }
    else{
      parent <- i - 1
      elevation <- 0
    }

    if(i != N_comp) connect <- i + 1
    else connect <- 0

    coordi <- c()
    nseg <- 1
    synapse <- -1

    K_conductance <- 0
    Ca_conductance <- 0
    
    new_Segment <- list(
      i,
      length,
      elevation,
      rotation,
      diam,
      path_leng,
      parent,
      connect,
      coordi,
      nseg,
      synapse,
      K_conductance,
      Ca_conductance
      )

    names(new_Segment) <- Segment_Labels
    Dendrite[[i]] <- new_Segment
  }
  return(Dendrite)
}

