library(caret)
source("data_prep.R")

# Modeling

# Define the train and test subset
train.subset <- "/2018-04-20"
test.subset <- "2018-04-22/"

models <- list()
for(sym in symbols){
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
  
  # predict on out of sample data
  test.probs <- predict(m1, newdata = data.test, type = "response")
  test.pred <- ifelse(test.probs >= 0.5, "1", "0")
  cm <- confusionMatrix(factor(test.pred, levels = c("0", "1")),
                        data.test$Next.Change.Sign, positive = "1")
  out <- list()
  out$model <- m1
  out$test.probabilities <- test.probs
  out$test.class <- test.pred
  out$data.train <- data.train
  out$data.test <- data.test
  out$confusion.matrix <- cm
  models[[sym]] <- out
}

overall.df <- as.data.frame(do.call(rbind, lapply(models, function(x) x$confusion.matrix$overall)))

plot(overall.df$Accuracy, ylim = c(0.65, 0.72), main = "Test Set Accuracy", 
     ylab = "Accuracy", xaxt = "n", xlab = "")
axis(1, at = 1:nrow(overall.df), labels = rownames(overall.df))
points(overall.df$AccuracyLower, pch = 3)
points(overall.df$AccuracyUpper, pch = 3)
legend("topright", legend = c("Accuracy", "95% CI "), pch = c(1, 3), bty = "n")
