load("actual_training_data.Rdata")
new_train <- head(training_data, 1000)
new_test <- tail(training_data, 397)

# Subset the x covariates
extra_covariates <- c("id", "cancer")
training_data_x <- new_train[ , !(names(new_train) %in% extra_covariates)]
training_data_y <- new_train$cancer

# Subset the test data
test_data_x <- new_test[ , !(names(new_test) %in% extra_covariates)]

# Replace NAs with 0
training_data_x[is.na(training_data_x)] <- 0
test_data_x[is.na(test_data_x)] <- 0

# Some packages don't like the column names to be numeric. 
colnames(training_data_x) <- paste("HU",colnames(training_data_x), sep = "_")
colnames(test_data_x) <- paste("HU",colnames(test_data_x), sep = "_")
colnames(training_data_x) <- gsub("-", "n", colnames(training_data_x))
colnames(test_data_x) <- gsub("-", "n", colnames(test_data_x))

# Run the superlearner algorithm
library("SuperLearner")
library <- c("SL.glmnet", "SL.glm", "SL.knn", "SL.gam", "SL.mean", "SL.ranger")
raw_sl <- SuperLearner(Y = training_data_y, X = training_data_x, newX = test_data_x, SL.library = library, family = binomial(), method = "method.NNLS"	)

save(raw_sl, file = "bin_sl.rData")
