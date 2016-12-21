setwd("C:/Users/Aashna/Documents/R/win-library/3.3/BMI_PROJECT")
library("AUC");
library("randomForest")
library(caret)

data <- read.table("Problem2_expressionMatrix.txt", header=TRUE)
patient.data <- read.table("Problem2_patientData.txt", header=TRUE)

data <- t(data)

patient_id <- rownames(data)
rownames(data) <- NULL
data <- cbind(data, patient_id)
data <- merge(data, patient.data, by = "patient_id")

colnames(data) <- make.names(colnames(data))

data <-  data[, -which(names(data) %in% c("patient_id"))]
data$relapse[grep("True", data$relapse)] <- 1
data$relapse[grep("False", data$relapse)] <- 0

indx <- sapply(data, is.factor)
data[indx] <- lapply(data[indx], function(x) as.numeric(as.character(x)))
data$relapse <- as.factor(data$relapse)

log.reg.names <- c("SMC2","ACKR3","RSL1D1","TMPRSS15", "RALGPS1","AKAP12","PTGER2","UBL3","SFXN3","CDC25C",
"SLC35D2","PBX3","TM2D1","PSPH","KCNA5","CHML","SMARCD3","ANXA4","DHRS3","HTR3A", "relapse")

data <- data[, which(names(data) %in% log.reg.names)]
sample.size <- round(0.75 *nrow(data))
train.indx <- sample(seq_len(nrow(data)), size=sample.size)

train <- data[train.indx, ]
test.labels <- data[-train.indx, which(names(data) %in% c("relapse"))]
test <- data[-train.indx, -which(names(data) %in% c("relapse"))]

rf <- randomForest(relapse ~ ., data = train, importance=TRUE)
plot(rf)


