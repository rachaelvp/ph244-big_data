library("SuperLearner")
train_data <- read.csv2("train_summ.csv")

train_data <- train_data[complete.cases(train_data), ]

drops <- c("Kurtosis","Skew")
train_data <- train_data[ , !(names(train_data) %in% drops)]


extra_covariates <- c("id", "cancer")
training_data_x <- train_data[ , !(names(train_data) %in% extra_covariates)]
train_data <- train_data$cancer

library <- c("SL.glmnet", "SL.glm", "SL.knn", "SL.gam", "SL.mean", "SL.ranger")
bin_cv_sl <- CV.SuperLearner(Y = train_data, X = training_data_x, SL.library = library, family = binomial(), method = "method.NNLS"	)

save(bin_cv_sl, file = "bin_cv_sl.rData")