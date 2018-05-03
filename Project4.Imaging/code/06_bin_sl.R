library("SuperLearner")
train_data <- read.csv("train_summ.csv")

actual_train_data <- head(train_data, 1000)
actual_test_data <- tail(train_data, 397)

drops <- c("Kurtosis","Skew")
actual_train_data <- actual_train_data[ , !(names(actual_train_data) %in% drops)]
actual_test_data <- actual_test_data[ , !(names(actual_test_data) %in% drops)]

extra_covariates <- c("id", "cancer")
training_data_x <- actual_train_data[ , !(names(actual_train_data) %in% extra_covariates)]
training_data_y <- actual_train_data$cancer

test_data_x <- actual_test_data[ , !(names(actual_test_data) %in% extra_covariates)]

# Replace NAs with 0
training_data_x[is.na(training_data_x)] <- 0
test_data_x[is.na(test_data_x)] <- 0

library <- c("SL.glmnet", "SL.glm", "SL.knn", "SL.gam", "SL.mean", "SL.ranger")
bin_sl <- SuperLearner(Y = training_data_y, X = training_data_x, newX = test_data_x, SL.library = library, family = binomial(), method = "method.NNLS"	)

save(bin_sl, file = "bin_sl.rData")
