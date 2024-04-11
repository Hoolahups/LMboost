#importing the data (note that read.csv() automatically creates dataframe objects)
data <- read.csv("mull2.csv")
#subsetting data to exclude unnessecary columns
data <- subset(data, select = -c(LABEGRD_ID, aspd, UTMX, UTMY))
#prepping mullein data
data_dataList <- lmSetup(data, response = "VETH", dataPercent = 1)
data_coefs <- coef(lm(VETH ~ ., data = data))
data_coefs <- data_coefs[!sapply(coefs, is.na)]
single_accuracy(data_coefs, data, "VETH")

#same thing here for mullein test data
datatest <- read.csv("newvalid.csv")
datatest <- subset(datatest, select = -c(LABEGRD_ID, aspd, UTMX, UTMY))
test_dataList <- lmSetup(datatest, response = "VETH", dataPercent = 1)
test_coefs <- coef(lm(VETH ~ ., data = datatest))
test_coefs <- test_coefs[!sapply(coefs, is.na)]
single_accuracy(test_coefs, datatest, "VETH")

#no major edits made here
data2 <- read.csv("Pima Diabetes 3.csv")
dataList <- lmSetup(data2, response = "Diabetes", dataPercent = 1)
coefs <- coef(lm(Diabetes ~ ., data = data2))
coefs <- coefs[!sapply(coefs, is.na)]
single_accuracy(coefs, data2, "Diabetes")
