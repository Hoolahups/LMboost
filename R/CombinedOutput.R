combined_output <- function(data, datatest, response){
  truestart <- Sys.time()
  totalOutput <- list()
  lms <- c(5,25,50,75,100,150,200)
  for(i in 1:length(lms)){
    lmamount<- lms[i]
    dataList <- lmSetup(data, response, lmPerCycle = lmamount)
    votes <- list(vote_all(dataList, datatest, response))
    sums <- list(sum_all(dataList, datatest, response))
    wvotes <- list(weighted_vote_all(dataList, datatest, response))
    combo <- list(combined_LM_predictor(dataList, datatest, response))
    output <- list(lms = lmamount, votes = votes, sums = sums, wvotes = wvotes, combo = combo)
    totalOutput[[i]] <- output
  }
  iteration_end <- Sys.time() # Capture end time of the iteration
  iteration_time <- round((iteration_end - truestart)*60) # Calculate duration
  print(paste("Overall", response ," took", iteration_time, "seconds"))
  totalOutput
}

saveRDS(LMboost:::combined_output(data, datatest, response), file = "LAQI.rds")
