library("SuperLearner")
train_data <- read.csv("train_summ.csv")
extra_covariates <- c("cancer")
training_data_x <- train_data[ , !(names(train_data) %in% extra_covariates)]
training_data_y <- train_data$cancer

library <- c("SL.glmnet", "SL.glm", "SL.knn", "SL.gam", "SL.mean", "SL.ranger")
bin_sl <- CV.SuperLearner(Y=training_data_y, X=training_data_x, family = 'binomial',
                           SL.library = library,
                           method = "method.AUC",
                           cvControl = SuperLearner.CV.control(V = 10, stratifyCV = TRUE),
                           innerCvControl = rep(list(SuperLearner.CV.control(V = 10,
                                                                             stratifyCV = TRUE)), 10))

save(bin_sl, file = "bin_sl.rData")
