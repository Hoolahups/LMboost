data <- read.csv("mull2.csv")
data <- data[, !(names(data) == "LABEGRD_ID")]
data <- data[, !(names(data) == "aspd")]
data <- data[, !(names(data) == "UTMX")]
data <- data[, !(names(data) == "UTMY")]
data <- data.frame(data)

datatest <- read.csv("newvalid.csv")
datatest <- datatest[, !(names(datatest) == "LABEGRD_ID")]
datatest <- datatest[, !(names(datatest) == "aspd")]
datatest <- datatest[, !(names(datatest) == "UTMX")]
datatest <- datatest[, !(names(datatest) == "UTMY")]
datatest <- data.frame(datatest)

data2 <- read.csv("Pima Diabetes 3.csv")


dataList <- lmSetup(data2, response = "Diabetes", dataPercent = 1)

coefs <- coef(lm(Diabetes ~ ., data = data2))
coefs <- coefs[!sapply(coefs, is.na)]
single_accuracy(coefs, data2, 'Diabetes')
