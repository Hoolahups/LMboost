parallelsetup<- function(){
  registerDoParallel(cores = (detectCores()-5))
}
