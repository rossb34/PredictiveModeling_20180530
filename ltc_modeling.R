library(MASS)
library(caret)
library(pROC)
library(kernlab)
library(e1071)
source("data_prep.R")

# Modeling

# Define the train and test subset
train.subset <- "/2018-04-20"
test.subset <- "2018-04-22/"

# Get and assign the data
sym <- "LTCUSD"
x.md <- md[[sym]]
x <- volume.bars[[sym]]

# Add labels... keep it simple
x$Next.Change <- lag(diff(x$VWAP), k = -1)
x$Next.Change.Sign <- ifelse(x$Next.Change > 1e-8, 1, 0)

# Add some features based on trade imbalance
x$Trade.Imbalance.Ratio <- x$Trade.Imbalance / x$Volume
x$Trade.Imbalance.Diff <- diff(x$Trade.Imbalance)
x <- na.omit(x)

x.train <- x[train.subset]
x.test <- x[test.subset]

x.train.features <- x.train[, c("Trade.Imbalance", "Trade.Imbalance.Ratio", 
                                "Trade.Imbalance.Diff")]
cor(na.omit(x.train.features))

feature.cols <- c("Trade.Imbalance", "Trade.Imbalance.Diff")
target.col <- "Next.Change.Sign"

# Scale the features
x.train.features <- scale(x.train[, feature.cols])

# scale the features of the test data using the statistics from the train set
x.test.features <- scale(x.test[, feature.cols], 
                         center = attr(x.train.features, "scaled:center"), 
                         scale = attr(x.train.features, "scaled:scale"))

# Prep the training data
data.train <- as.data.frame(cbind(x.train[,target.col], x.train.features))
data.train[,target.col] <- factor(data.train[,target.col], levels = c("0", "1"))

# Prep the test data
data.test <- as.data.frame(cbind(x.test[,target.col], x.test.features))
data.test[,target.col] <- factor(data.test[,target.col], levels = c("0", "1"))

m1 <- glm(Next.Change.Sign ~ ., data = data.train, 
          family = binomial(link = "logit"))
summary(m1)

# predict on out of sample data
test.probs <- predict(m1, newdata = data.test, type = "response")
test.pred <- ifelse(test.probs >= 0.5, "1", "0")
lr.opt.cm <- confusionMatrix(factor(test.pred, levels = c("0", "1")),
                             data.test$Next.Change.Sign, positive = "1")
m1.roc.curve <- roc(response = data.test$Next.Change.Sign, predictor = test.probs)

# LDA Model
lda.fit <- lda(Next.Change.Sign ~ ., data = data.train)
lda.predictions <- predict(lda.fit, data.test)
lda.opt.cm <- confusionMatrix(lda.predictions$class, data.test$Next.Change.Sign, positive = "1")
lda.roc.curve <- roc(response = data.test$Next.Change.Sign, predictor = lda.predictions$posterior[,"1"])

# QDA Model
qda.fit <- qda(Next.Change.Sign ~ ., data = data.train)
qda.predictions <- predict(qda.fit, data.test)
qda.opt.cm <- confusionMatrix(qda.predictions$class, data.test$Next.Change.Sign, positive = "1")
qda.roc.curve <- roc(response = data.test$Next.Change.Sign, predictor = qda.predictions$posterior[,"1"])

plot(m1.roc.curve)
lines(x = lda.roc.curve$specificities, y = lda.roc.curve$sensitivities, lty = 2)
lines(x = qda.roc.curve$specificities, y = qda.roc.curve$sensitivities, lty = 3)
legend("topleft", legend = c("LR", "LDA", "QDA"), lty = 1:3, bty = "n")


# SVM
svm.model <- svm(Next.Change.Sign ~ ., data = data.train, scale = FALSE, 
                 type = "C-classification")
svm.predictions <- predict(svm.model, data.test)
confusionMatrix(svm.predictions, data.test$Next.Change.Sign, positive = "1")


ep.d <- endpoints(x.train, on = "days")
index.cv <- list()
index.cv$index.in <- list()
index.cv$index.out <- list()

j <- 1
for(i in 6:length(ep.d)-1){
  index.cv$index.in[[j]] <- 1:ep.d[i]
  index.cv$index.out[[j]] <- (ep.d[i] + 1):ep.d[i+1]
  j <- j + 1
}

for(i in seq_along(index.cv$index.in)){
  idx.in <- index.cv$index.in[[i]]
  idx.out <- index.cv$index.out[[i]]
  if(i == 1){
    plot(x = 1:nrow(x.train), y = rep(1, nrow(x.train)), xlab = "Index",
         ylab = "Iteration", ylim = c(1, length(index.cv$index.in)), type = "n")
  }
  points(x = idx.in, y = rep(i, length(idx.in)), col = "blue")
  points(x = idx.out, y = rep(i, length(idx.out)), col = "red")
}
legend("bottomright", legend = c("Train", "Test"), col = c("blue", "red"), pch = c(1,1))

par(mfrow = c(2, 3))
for(i in seq_along(index.cv$index.in)){
  idx.in <- index.cv$index.in[[i]]
  idx.out <- index.cv$index.out[[i]]
  p <- plot(x.train$VWAP, main = paste("TSCV", i), major.ticks = "weeks", 
            col = "gray", lwd = 1)
  x.in <- x.train[idx.in]
  x.out <- x.train[idx.out]
  p <- lines(x.in$VWAP, col = "blue", lwd = 2)
  p <- lines(x.out$VWAP, col = "red", lwd = 2)
  # p <- addEventLines(xts(c("Train End", "Test End"), 
  #                        c(index(x.train[last(idx.in)]), 
  #                          index(x.train[last(idx.out)]))),
  #                  on = 1, srt = 90, pos = 2)
  p <- addLegend(legend.loc = "topleft", legend.names = c("Train", "Test"), 
                 col = c("blue", "red"), lty = c(1,1))
  print(p)
}
par(mfrow = c(1, 1))

tc <- trainControl(method = "timeslice", index = index.cv$index.in, 
                          indexOut = index.cv$index.out, classProbs = TRUE)
data.train.label <- data.train
levels(data.train.label$Next.Change.Sign) <- c("dn", "up")
data.test.label <- data.test
levels(data.test.label$Next.Change.Sign) <- c("dn", "up")

sigma <- sigest(Next.Change.Sign ~ ., data = data.train.label)
sigma <- seq(from = 0.01, to = 0.2, length.out = 10)
svm.grid <- expand.grid(.sigma = sigma,
                        .C = 2^(seq(-4,4)))
svm.tune <- train(Next.Change.Sign ~ Trade.Imbalance + Trade.Imbalance.Diff, 
                  data = data.train.label,
                  method = "svmRadial", tuneGrid = svm.grid, 
                  metric = "Accuracy", trControl = tc)
svm.tune$method

svm.predictions <- predict(svm.tune$finalModel, data.test.label[, c("Trade.Imbalance", "Trade.Imbalance.Diff")])
svm.opt.cm <- confusionMatrix(svm.predictions, data.test.label$Next.Change.Sign, positive = "up")

# KNN
knn.grid <- data.frame(.k = c(seq(from = 5, to = 25, by = 5),
                              seq(from = 50, to = 200, by = 50)))
knn.tune <- train(Next.Change.Sign ~ Trade.Imbalance + Trade.Imbalance.Diff, 
                  data = data.train.label,
                  method = "knn", tuneGrid = knn.grid, 
                  metric = "Accuracy", trControl = tc)
knn.tune
knn.predictions <- predict(knn.tune$finalModel, data.test.label[, c("Trade.Imbalance", "Trade.Imbalance.Diff")], type = "class")
knn.opt.cm <- confusionMatrix(knn.predictions, data.test.label$Next.Change.Sign, positive = "up")

# Naive Bayes
nb.model <- naiveBayes(Next.Change.Sign ~ ., data = data.train, laplace = 1)
nb.predictions <- predict(nb.model, data.test, type = "class")#type = "raw")
confusionMatrix(nb.predictions, data.test$Next.Change.Sign, positive = "1")

nb.grid <- expand.grid(.usekernel = c(TRUE, FALSE),
                       .laplace = 0:10,
                       .adjust = seq(from = 0.25, to = 2, by = 0.25))
nb.tune <- train(Next.Change.Sign ~ ., data = data.train.label,
                 method = "naive_bayes", tuneGrid = nb.grid, 
                 metric = "Accuracy", trControl = tc)
nb.tune
nb.predictions <- predict(nb.tune$finalModel, data.test.label[, c("Trade.Imbalance", "Trade.Imbalance.Diff")], type = "class")
nb.opt.cm <- confusionMatrix(nb.predictions, data.test.label$Next.Change.Sign, positive = "up")

overall.df <- as.data.frame(do.call(rbind, 
                                    list("GLM"=lr.opt.cm$overall,
                                         "LDA"=lda.opt.cm$overall,
                                         "QDA"=qda.opt.cm$overall,
                                         "SVM"=svm.opt.cm$overall,
                                         "KNN"=knn.opt.cm$overall,
                                         "NaiveBayes"=nb.opt.cm$overall)))

plot(overall.df$Accuracy, ylim = c(0.66, 0.72), main = "Test Set Accuracy", 
     ylab = "Accuracy", xaxt = "n", xlab = "")
axis(1, at = 1:nrow(overall.df), labels = rownames(overall.df))
points(overall.df$AccuracyLower, pch = 3)
points(overall.df$AccuracyUpper, pch = 3)
legend("topright", legend = c("Accuracy", "95% CI "), pch = c(1, 3), bty = "n")
