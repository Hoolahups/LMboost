lmSetup <- function(data, response = "", lmPerCycle = 100, dataPercent = 1/3){
  lmPerCycle <- as.integer(lmPerCycle)
  lmChecks(data, response, lmPerCycle)

  #set up the response variable
  if(response == ""){
    response <- names(data)[1]
  }

  #initialize the final df
  bigList <- list()

  #start the loopin
  for(j in 2:(ncol(data)-2)){
    iteration_start <- Sys.time()
    coefficients_list <- list()

    # Loop through each model
    coefficients_list <- foreach(i = 1:lmPerCycle) %dopar% {
      # Sample j predictor variables randomly
      predictors <- sample(setdiff(names(data), response), j)

      #bootstrap the data
      numRows <- nrow(data) * dataPercent
      subset_indices <- sample(1:nrow(data), size = numRows, replace = TRUE)
      bootstrap <- data[subset_indices, ]

      # Fit a linear model
      lm_model <- lm(bootstrap[[response]] ~ ., data = bootstrap[, c(predictors), drop = FALSE])

      # Extract coefficients
      coefs <- coef(lm_model)
      coefs <- coefs[!sapply(coefs, is.na)]
      return(coefs)
    }

    # Add the coefficients to the Big List

    bigList[[j-1]] <- coefficients_list

    iteration_end <- Sys.time() # Capture end time of the iteration
    iteration_time <- round((iteration_end - iteration_start) * 1000) # Calculate duration
    gc()
    print(paste("LM Generation Round", j-1, "/", ncol(data)-3, "took", iteration_time, "milliseconds."))
  }
  bigList <- setNames(bigList, seq_along(bigList))
  return(bigList)
}

lmChecks <- function(data, response, lmPerCycle){
  if(!is.data.frame(data)){
    stop("The data must be a dataframe")
  }
  if(!is.character(response)){
    stop("Response must be blank or a string")
  }
  if(!is.integer(lmPerCycle)){
    stop("Number of LMs per Cycle must be an integer")
  }
  if(lmPerCycle * ncol(data) >= 100000){
    warning("Thats a lot of data to process, consider lowering your LMs per cycle.")
    proceed <- readline(prompt = "Do you want to proceed? (y/n): ")
    if (tolower(proceed) != "y") {
      stop("User chose not to proceed.")
    } else {
      print("Continuing...")
    }
  }
}
