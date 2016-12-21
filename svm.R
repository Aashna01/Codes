setwd("C:/Users/Aashna/Documents/R/win-library/3.3/BMI_PROJECT")
install.packages("mlr")
install.packages("ROCR")
install.packages("kernlab")
library("mlr")
library("ROCR")
library("kernlab")
library(dplyr)
library(ggplot2)
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


#ps = makeParamSet(
#   makeDiscreteParam("C", values = 2^(-12:12))#,
#makeDiscreteParam("sigma", values = 2^(-2:2))#,
#makeDiscreteParam("offset", values = 2^(-2:2))
#)
ctrl = makeTuneControlGrid()
#inner = makeResampleDesc("Subsample", iters = 2)
ps = makeParamSet(
  makeNumericParam("C", lower = 1, upper = 12),
  makeDiscreteParam("kernel", values = c("vanilladot", "polydot", "rbfdot")),
  makeNumericParam("sigma", lower = 1, upper = 12,
                   requires = quote(kernel == "rbfdot")),
  makeIntegerParam("degree", lower = 2L, upper = 5L,
                   requires = quote(kernel == "polydot"))
)
outer = makeResampleDesc("RepCV", reps = 100, folds = 10)
#outer = makeResampleDesc("CV", iters=3)

classif.task = makeClassifTask(data = train, target = "relapse")
lrn = makeTuneWrapper("classif.ksvm",resampling=inner, par.set = ps, control = ctrl, show.info = FALSE)
#
## Outer resampling loop
#r = resample(lrn, classif.task , resampling = outer, extract = getTuneResult, show.info = FALSE)
r = tuneParams("classif.ksvm", task = classif.task, resampling = outer, par.set = ps, control = ctrl,  measures = list(acc, setAggregation(acc, test.sd)))
summary(r);
print(r$extract);

saveRDS(r,"r");
lrn = makeLearner("classif.ksvm", predict.type='prob')

mod <- train(lrn, classif.task)
pred <- predict(mod, newdata = test)

pred$data$truth <- test.labels
saveRDS(pred, "pred")

opt.grid <- as.data.frame(r$opt.path);
vanilla.opt.grid <- opt.grid[opt.grid$kernel=='vanilladot',];
g = ggplot(vanilla.opt.grid, aes(x = C, y = acc.test.mean, fill = acc.test.mean, label = round(acc.test.mean ,3)))
g + geom_tile() + geom_text(color = "white")
poly.opt.grid <- opt.grid[opt.grid$kernel=='polydot',];
g = ggplot(poly.opt.grid, aes(x = C, y = degree, fill = acc.test.mean, label = acc.test.mean))
g + geom_tile() + geom_text(color = "white")
rbf.opt.grid <- opt.grid[opt.grid$kernel=='rbfdot',];
g = ggplot(poly.opt.grid, aes(x = C, y = sigma, fill = acc.test.mean, label = acc.test.mean))
g = ggplot(rbf.opt.grid, aes(x = C, y = sigma, fill = acc.test.mean, label = acc.test.mean))
g + geom_tile() + geom_text(color = "white")


