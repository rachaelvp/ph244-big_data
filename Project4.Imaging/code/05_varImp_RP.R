library(randomForest)
library(dplyr)
# Sampling without replacement is important here, as otherwise samples from the
# smaller classes will contain many more repetitions, and the class will still be
# underrepresented
train_summ$cancer <- droplevels.factor(train_summ$cancer)
train1 <- train_summ %>%
filter(cancer == 1)
train0 <- train_summ %>%
filter(cancer == 0)
train0 <- data.frame(sample_n(train0, 361))
train <- rbind(train1, train0)

cols <- seq(1:66)
train[,cols] <- apply(train[,cols], 2, function(x) as.numeric(as.character(x)))
train$cancer <- as.factor(train$cancer)
rf <- randomForest(x=train[,-c(67:68)], y =train$cancer, ntree = 1000,
importance = TRUE, strata = train$cancer)
rf
# randomForest(x = train[, -c(67:68)], y = train$cancer, ntree = 1000,      strata = train$cancer, importance = TRUE)
#               Type of random forest: classification
#                     Number of trees: 1000
# No. of variables tried at each split: 8
#
#        OOB estimate of  error rate: 48.06%
# Confusion matrix:
#    0   1 class.error
# 0 192 169   0.4681440
# 1 178 183   0.4930748

varImpPlot(rf)
