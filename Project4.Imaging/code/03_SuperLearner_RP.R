library(SuperLearner)

folds <- 10
# set up Super Learner library
lib <- list(c("SL.glm"),
            c("SL.bayesglm"),
            c("SL.mean"),
            c("SL.qda"),
            c("SL.gam"),
            c("SL.svm"),
            c("SL.ranger"),
            c("SL.glmnet"))

# run cross-validated Super Learner? If possible
cvSL <- CV.SuperLearner(Y=dat[,28], X=dat[,-c(28,29)],
  method = "method.AUC", family = "binomial", SL.library = lib,
  cvControl = SuperLearner.CV.control(V = folds,
  stratifyCV = TRUE), innerCvControl = rep(list(SuperLearner.CV.control(V =
  folds, stratifyCV = TRUE)), folds))
summary_cvSL <- summary.CV.SuperLearner(cvSL)
cvSL_results <- summary_cvSL$Table
