data <- read.csv("mull2.csv")
data <- data[, !(names(data) == "LABEGRD_ID")]
data <- data[, !(names(data) == "aspd")]
data <- data[, !(names(data) == "UTMX")]
data <- data[, !(names(data) == "UTMY")]
data <- data.frame(data)
datatest <- read.csv("mull2test.csv")
datatest <- datatest[, !(names(datatest) == "LABEGRD_ID")]
datatest <- datatest[, !(names(datatest) == "aspd")]
datatest <- datatest[, !(names(datatest) == "UTMX")]
datatest <- datatest[, !(names(datatest) == "UTMY")]
datatest <- as.matrix(datatest)
response <- "VETH"

data <- read.csv("Pima Diabetes 3.csv")
datatest <- as.matrix(data)
response <- "Diabetes"

data <- read.csv("trainplane.csv")
datatest <-read.csv("testplane.csv")
data$satisfaction <- ifelse(data$satisfaction == "satisfied", 1, 0)
datatest$satisfaction <- ifelse(datatest$satisfaction == "satisfied", 1, 0)
datatest <- as.matrix(datatest)
response <- "satisfaction"

data <-read.csv("LoL_15_Diamond.csv")
data <- data[, !(names(data) == "match_id")]
data <- data[, !(names(data) == "red_Win")]
n <- nrow(data)
test_size <- floor(0.20 * n)
test_indices <- sample(1:n, test_size)
datatest <- as.matrix(data[test_indices, ])
data <- data[-test_indices, ]
response <- "blue_Win"

data <-read.csv("LAQI.csv")
n <- nrow(data)
test_size <- floor(0.20 * n)
test_indices <- sample(1:n, test_size)
datatest <- as.matrix(data[test_indices, ])
data <- data[-test_indices, ]
response <- "LobaOreg"

out <- LMboost:::combined_output(data, datatest, response)

LMboost:::parallelsetup()

dataList <- LMboost:::lmSetup(data, response)

plot(LMboost:::vote_all(dataList, datatest, response))

plot(LMboost:::sum_all(dataList, datatest, response))

plot(LMboost:::weighted_vote_all(dataList, datatest, response))

plot(LMboost:::combined_LM_predictor(dataList, datatest, response))

coefs <- coef(lm(LobaOreg ~ ., data = data))
coefs <- coefs[!sapply(coefs, is.na)]
single_accuracy(coefs, data, response)
