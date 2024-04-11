parallelsetup<- function(){
  registerDoParallel(cores = (detectCores()-1))
}
