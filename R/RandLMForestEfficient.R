VoteCasterEff <- function(modelList, testData, response, name){
  iteration_start <- Sys.time()
  coefficients_list <- list()
  predictions <- foreach(i = 1:nrow(testData), .combine = 'c', .noexport = "VoteSingle") %dopar% {
    sourceCpp("src/VoteSingle.cpp")
    row <- testData[i, ]
    VoteSingle(modelList, row)
  }
  conf_matrix <- table(Predicted = predictions, Actual = testData[,response])
  accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
  iteration_end <- Sys.time() # Capture end time of the iteration
  iteration_time <- round((iteration_end - iteration_start) * 1000) # Calculate duration
  print(paste("Vote Round #", name ," took", iteration_time, "milliseconds, with an accuracy of", accuracy))
  return(accuracy)
}

vote_all <- function(bigdata, testData, response) {
  voteout <- rep(0,length(bigdata))
  for(i in 1:length(bigdata)){
    name <- names(bigdata)[i]
    voteout[i] <- VoteCasterEff(bigdata[[i]], testData, response, name)
  }
  voteout
}

SumEff <- function(modelList, testData, response, name){
  iteration_start <- Sys.time()
  coefficients_list <- list()
  predictions <- foreach(i = 1:nrow(testData), .combine = 'c', .noexport = "VoteSingle") %dopar% {
    sourceCpp("src/SumSingle.cpp")
    row <- testData[i, ]
    SumSingle(modelList, row)
  }
  conf_matrix <- table(Predicted = predictions, Actual = testData[,response])
  accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
  iteration_end <- Sys.time() # Capture end time of the iteration
  iteration_time <- round((iteration_end - iteration_start) * 1000) # Calculate duration
  print(paste("Sum Round #", name ," took", iteration_time, "milliseconds, with an accuracy of", accuracy))
  return(accuracy)
}

sum_all <- function(bigdata, testData, response) {
  sumout <- rep(0,length(bigdata))
  for(i in 1:length(bigdata)){
    name <- names(bigdata)[i]
    sumout[i] <- SumEff(bigdata[[i]], testData, response, name)
  }
  sumout
}

single_accuracy <- function(model, testData, response){
  predictor_names <- names(model)[-1]
  predictors_matrix <- testData[, predictor_names, drop = FALSE]
  model_matrix <- as.matrix(cbind(1, predictors_matrix))
  z <- model_matrix %*% unlist(model)
  predicted_probabilities <- z
  predicted_labels <- ifelse(predicted_probabilities > 0.5, 1, 0)
  conf_matrix <- table(Predicted = predicted_labels, Actual = testData[,response])
  if(any(dim(conf_matrix) != c(2,2))){
    zero_matrix <- matrix(0, nrow = 2, ncol = 2,
                          dimnames = list(c("0", "1"), c("0", "1")))
    small_row_name <- rownames(conf_matrix)[1]
    # Check if the small matrix row name exists in the large matrix
    if (small_row_name %in% rownames(zero_matrix)) {
      # If yes, create a compatible matrix filled with zeros

      # Assign the small matrix to the corresponding row(s) and column(s)
      zero_matrix[small_row_name, colnames(conf_matrix)] <- conf_matrix

      # Add the temporary matrix to the large matrix
      conf_matrix <- zero_matrix
    }
  }
  accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
  accuracy
}

ensemble_accuracy <- function(modelList, testData, response){
  sapply(modelList, function(elem) single_accuracy(elem, datatest, response))
}

WeightedVoteCasterEff <- function(modelList, testData, response, name){
  iteration_start <- Sys.time()
  coefficients_list <- list()
  weights <- ensemble_accuracy(modelList, testData, response)
  totalweight <- mean(weights)
  predictions <- foreach(i = 1:nrow(testData), .combine = 'c', .noexport = "VoteSingle") %dopar% {
    sourceCpp("src/WeightedVotesSingle.cpp")
    row <- testData[i, ]
    WeightedVotesSingle(modelList, row, weights, totalweight)
  }
  conf_matrix <- table(Predicted = predictions, Actual = testData[,response])
  accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
  iteration_end <- Sys.time() # Capture end time of the iteration
  iteration_time <- round((iteration_end - iteration_start) * 1000) # Calculate duration
  print(paste("Weighted Vote Round #", name ," took", iteration_time, "milliseconds, with an accuracy of", accuracy))
  return(accuracy)
}

weighted_vote_all <- function(bigdata, testData, response) {
  wvoteout <- rep(0,length(bigdata))
  for(i in 1:length(bigdata)){
    name <- names(bigdata)[i]
    wvoteout[i] <- WeightedVoteCasterEff(bigdata[[i]], testData, response, name)
  }
  wvoteout
}

combined_LM <- function(modelList){
  all_coefficients <- list()
  # Combine all coefficients, handling missing variables by assuming their coefficients are 0
  for (coeffs in modelList) {
    # Iterate through each named coefficient in this model's output
    for (var_name in names(coeffs)) {
      # If the variable isn't in the all_coefficients list yet, initialize it
      if (!var_name %in% names(all_coefficients)) {
        all_coefficients[[var_name]] <- vector("numeric")
      }
      # Append the coefficient value to the appropriate list
      all_coefficients[[var_name]] <- c(all_coefficients[[var_name]], coeffs[var_name])
    }
  }

  # Calculate the mean of coefficients for each variable
  average_coefficients <- sapply(all_coefficients, mean)
  return(average_coefficients)
}

combined_LM_predictor <- function(bigdata, testData, response){
  combinedLMout <- rep(0,length(bigdata))
  for(i in 1:length(bigdata)){
    iteration_start <- Sys.time()
    name <- names(bigdata)[i]
    combinedLMout[i] <- single_accuracy(combined_LM(bigdata[[i]]), testData, response)
    iteration_end <- Sys.time() # Capture end time of the iteration
    iteration_time <- round((iteration_end - iteration_start) * 1000) # Calculate duration
    print(paste("Combined Linear Model Round #", name ," took", iteration_time, "milliseconds, with an accuracy of", combinedLMout[i]))
  }
  combinedLMout
}
