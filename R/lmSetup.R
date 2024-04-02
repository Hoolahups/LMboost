lmSetup <- function(data, response = "", lmPerCycle = 500 ){
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
    for (i in 1:lmPerCycle) {
      # Sample j predictor variables randomly
      predictors <- sample(setdiff(names(data), response), j)

      # Fit a linear model
      lm_model <- lm(data[[response]] ~ ., data = data[, c(predictors, response), drop = FALSE])

      # Extract coefficients
      coefficients_list[[i]] <- coef(lm_model)
    }

    # Add the coefficients to the Big List
    bigList[[j-1]] <- coefficients_list

    iteration_end <- Sys.time() # Capture end time of the iteration
    iteration_time <- round((iteration_end - iteration_start) * 1000) # Calculate duration

    print(paste("Round", j-1, "/", ncol(data)-3, "took", iteration_time, "milliseconds."))
  }
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
