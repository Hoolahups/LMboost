data <- read.csv("newvalid.csv")
data <- data[, !(names(data) == "LABEGRD_ID")]
data <- data[, !(names(data) == "aspd")]
data <- data[, !(names(data) == "UTMX")]
data <- data[, !(names(data) == "UTMY")]
data <- data.frame(data)

dataList <- lmSetup(data, response = "VETH")
