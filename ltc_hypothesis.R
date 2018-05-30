library(caret)
source("data_prep.R")

# Modeling

# Define the train and test subset
train.subset <- "/2018-04-20"
test.subset <- "2018-04-22/"

# Get and assign the data
sym <- "LTCUSD"
x.md <- md[[sym]]
x <- volume.bars[[sym]]

# Visualization

# Plot the L1 tick data
p <- plot(x.md$WMP, major.ticks = "weeks")
p <- addSeries(x.md$BAS, on = NA, type = "h")
p <- addSeries(x.md$Volume, on = NA, type = "h")
print(p)

# Plot the L1 tick and volume bar data for a short time period
ss <- "2018-04-12 07:59:00/2018-04-12 08:05:59"
xs <- x.md[ss]
xs$Trade.On <- ifelse(xs$Price <= xs$Bid.Price, -1, ifelse(xs$Price >= xs$Ask.Price, 1, 0))
p <- plot(xs$WMP, grid.ticks.on = "hours", main = "Price")
p <- addSeries(xs$Volume, on = NA, type = "h", main = "Volume")
# Add volume bar series
p <- points(x[ss, "VWAP"], pch = 23, bg = "black", on = 1)
p <- addSeries(x[ss, "Trade.Imbalance"], on = NA, type = "h", main = "Trade Imbalance")
print(p)

# Add the trades
p <- points(xs[xs$Trade.On > 0, "Price"], on = 1, bg = "green", pch = 24)
p <- points(xs[xs$Trade.On < 0, "Price"], on = 1, bg = "red", pch = 25)
print(p)

# Plot the volume bar data
p <- plot(x$VWAP, major.ticks = "weeks", observation.based = TRUE)
p <- addSeries(x$Volume, on = NA, type = "h", main = "Volume")
p <- addSeries(x$Trade.Imbalance, on = NA, type = "h", main = "Trade Imbalance")
p <- addEventLines(xts(matrix(c("Train", "Test"), ncol = 1),
                        c(index(xts::last(x[train.subset])), 
                          index(xts::first(x[test.subset])))),
                   on = 1, srt = 90, pos = 2)
print(p)

# Add labels... keep it simple
x$Next.Change <- lag(diff(x$VWAP), k = -1)
x$Next.Change.Sign <- ifelse(x$Next.Change > 1e-8, 1, 0)
x <- x[-nrow(x),]

# Subset the train and test data
x.train <- x[train.subset]
x.test <- x[test.subset]

nrow(x.train)
nrow(x.test)

# Prevalance of target class
table(x.train$Next.Change.Sign) / sum(table(x.train$Next.Change.Sign))

# Histogram of trade imbalance feature
hist(x.train$Trade.Imbalance)
cor(sign(x.train$Trade.Imbalance), sign(x.train$Next.Change.Sign))

forecast::Acf(as.vector(x.train$Trade.Imbalance), 
              main = "ACF of Trade Imbalance")
forecast::Pacf(as.vector(x.train$Trade.Imbalance),
               main = "Partial ACF of Trade Imbalance")

# Fit a simple model using logistic regression
data.train <- as.data.frame(na.omit(x.train[,c("Trade.Imbalance", "Next.Change.Sign")]))
data.train$Next.Change.Sign <- factor(data.train$Next.Change.Sign, levels = c("0", "1"))

# Fit the model
# glm treats the second factor as the event of interest
m1 <- glm(Next.Change.Sign ~ Trade.Imbalance, 
          data = data.train, family = binomial(link = "logit"))
summary(m1)

# In sample predictions
train.probs <- predict(m1, type = "response")
train.pred <- ifelse(train.probs >= 0.5, "1", "0")
table(train.pred, data.train$Next.Change.Sign)

no.information <- function(x){
  binom.test(sum(diag(x)), sum(x), p = max(apply(x, 
             2, sum)/sum(x)), alternative = "greater")$null.value
}
no.information(table(train.pred, data.train$Next.Change.Sign))
confusionMatrix(factor(train.pred, levels = c("0", "1")),
                       data.train$Next.Change.Sign, positive = "1")

data.test <- as.data.frame(na.omit(x.test[,c("Trade.Imbalance", "Next.Change.Sign")]))
data.test$Next.Change.Sign <- factor(data.test$Next.Change.Sign, levels = c("0", "1"))
# predict on out of sample data
test.probs <- predict(m1, newdata = data.test, type = "response")
test.pred <- ifelse(test.probs >= 0.5, "1", "0")

confusionMatrix(factor(test.pred, levels = c("0", "1")),
                data.test$Next.Change.Sign, positive = "1")

roc.curve <- roc(response = data.test$Next.Change.Sign, predictor = test.probs)
auc(roc.curve)
ci.roc(roc.curve)
plot(roc.curve)


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
range(index.cv$index.in[[1]])
range(index.cv$index.out[[1]])
range(index.cv$index.in[[2]])
range(index.cv$index.out[[2]])

tc <- caret::trainControl(method = "timeslice", index = index.cv$index.in, 
                          indexOut = index.cv$index.out)
mc <- caret::train(Next.Change.Sign ~ Trade.Imbalance, data = data.train,
                   method = "glm", family = binomial(link = "logit"),
                   trControl = tc)
mc
mc$results

for(i in seq_along(index.cv)){
  idx.in <- index.cv$index.in[[i]]
  idx.out <- index.cv$index.out[[i]]
  d.train <- data.train[idx.in,]
  d.test <- data.train[idx.out,]
  m.fit <- glm(Next.Change.Sign ~ Trade.Imbalance, 
               data = d.train, family = binomial(link = "logit"))
  test.probs <- predict(m.fit, newdata = d.test, type = "response")
  test.pred <- ifelse(test.probs >= 0.5, "1", "0")
  print(caret::confusionMatrix(factor(test.pred, levels = c("0", "1")),
                         d.test$Next.Change.Sign, positive = "1"))
}

# scratch
# acf(x$Trade.Imbalance)
# plot(x = coredata(x$Trade.Imbalance), y = coredata(x$vwap.next.change))
# plot(x = coredata(abs(x$Trade.Imbalance)), y = coredata(abs(x$vwap.next.change)))
# x$Trade.Imbalance.Sign <- sign(x$Trade.Imbalance)
# X <- sign(x$Trade.Imbalance)
# y <- x$vwap.next.change
# summary(lm(y ~ X))
# cor(sign(x$Trade.Imbalance), sign(x$vwap.next.change))
# cor(x$Trade.Imbalance, x$vwap.next.change)
# forecast::Acf(as.vector(x$Trade.Imbalance))
# No Information Rate
# from caret:::confusionMatrix.table to get the no information rate
# x is the confusion matrix table
# binom.test(sum(diag(x)), sum(x), p = max(apply(x, 
#             2, sum)/sum(x)), alternative = "greater")
#
# Distribution of accuracy of a random trader
# I could make a positive bet every single observation and be right 52.44% of
# the time
# This is just 50%
# n.sim <- 10000
# acc.out <- vector("numeric", length = n.sim)
# for(i in 1:n.sim){
#   sig <- ifelse(runif(nrow(x.test)) >= 0.5, 1, 0)
#   acc.out[i] <- sum(sig == as.vector(x.test$Next.Change.Sign)) / nrow(x.test)
# }
# hist(acc.out, main = "OOS Accuracy for Random Signal")
# mean(acc.out)