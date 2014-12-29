take_movie <- function(){
#  if(!rgl.useNULL()) play3d(spin3d(axis = c(0,1,0),rpm = 8),duration = 20)
  Sys.setenv(PATH=paste("/opt/local/bin",Sys.getenv("PATH"),sep=":"))
  movie3d(spin3d(axis = c(0,1,0,rpm = 12)),,fps = 16,duration = 20,movie = OUTPUT_MOVIE)
}
