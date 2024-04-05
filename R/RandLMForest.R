single_accuracy <- function(model, testData, response){
  predictor_names <- names(model)[-1]
  predictors_matrix <- testData[, predictor_names, drop = FALSE]
  model_matrix <- cbind(1, predictors_matrix)
  z <- as.matrix(model_matrix) %*% model
  predicted_probabilities <- z
  predicted_labels <- ifelse(predicted_probabilities > 0.5, 1, 0)
  conf_matrix <- table(Predicted = predicted_labels, Actual = testData[[response]])
  accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
  accuracy
}
ensemble_accuracy <- function(modelList, testData, response){
  mean(sapply(modelList, function(elem) single_accuracy(elem, datatest, response)))
}

ensemble_accuracy(dataList[[1]], data2, "Diabetes")

vote_caster <- function(modelList, testData, response){
  iteration_start <- Sys.time()
  coefficients_list <- list()
  predictions <- rep(0,nrow(testData))
  for(i in 1:nrow(testData)){
    datum <- testData[i,]
    pred <- 0
    for(j in 1:length(modelList)){
      model <- modelList[[j]]
      predictor_names <- names(model)[-1]
      predictors_matrix <- datum[, predictor_names, drop = FALSE]
      model_matrix <- cbind(1, predictors_matrix)
      z <- as.matrix(model_matrix) %*% model
      predicted_probabilities <- z
      predicted_labels <- ifelse(predicted_probabilities > 0.5, 1, 0)
      pred <- pred + predicted_labels
    }
    if(pred > length(modelList)/2){
      predictions[i] <- 1
    }
  }
  conf_matrix <- table(Predicted = predictions, Actual = testData[[response]])
  accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)

  iteration_end <- Sys.time() # Capture end time of the iteration
  iteration_time <- round((iteration_end - iteration_start)) # Calculate duration
  gc()
  print(paste("Vote Round took", iteration_time, "seconds, with an accuracy of", accuracy))
  accuracy
}

vote_all <- function(bigdata, testData, response){
  voteout <- c()
  for(i in 1:length(bigdata)){
    accuracy <- vote_caster(bigdata[[i]], testData, response)
    voteout[i] <- accuracy
  }
  voteout
}

vote_all(dataList, data2, "Diabetes")

sum_caster <- function(modelList, testData, response){
  iteration_start <- Sys.time()
  coefficients_list <- list()
  predictions <- rep(0,nrow(testData))
  for(i in 1:nrow(testData)){
    datum <- testData[i,]
    pred <- 0
    for(j in 1:length(modelList)){
      model <- modelList[[j]]
      predictor_names <- names(model)[-1]
      predictors_matrix <- datum[, predictor_names, drop = FALSE]
      model_matrix <- cbind(1, predictors_matrix)
      z <- as.matrix(model_matrix) %*% model
      pred<- z
    }
    if(pred > length(modelList)/2){
      predictions[i] <- 1
    }
  }
  conf_matrix <- table(Predicted = predictions, Actual = testData[[response]])
  accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)

  iteration_end <- Sys.time() # Capture end time of the iteration
  iteration_time <- round((iteration_end - iteration_start)) # Calculate duration
  gc()
  print(paste("Sum Round took", iteration_time, "seconds, with an accuracy of", accuracy))
  accuracy
}

sum_all <- function(bigdata, testData, response){
  sumout <- c()
  for(i in 1:length(bigdata)){
    accuracy <- sum_caster(bigdata[[i]], testData, response)
    sumout[i] <- accuracy
  }
  sumout
}
sum_all(dataList, datatest, "VETH")
