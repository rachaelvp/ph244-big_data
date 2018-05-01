load("actual_test_data.Rdata")
load("actual_training_data.Rdata")

# The following code is only a first pass at the superlearner.

# Subset the x covariates
extra_covariates <- c("patientid","id", "cancer")
training_data_x <- training_data[ , !(names(training_data) %in% extra_covariates)]
training_data_y <- training_data$cancer

# Subset the test data
test_data_x <- test_data[ , !(names(test_data) %in% extra_covariates)]

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

save(raw_sl, file = "raw_sl.rData")
