library(SuperLearner)

folds <- 10
# set up Super Learner library
lib <-

# run cross-validated Super Learner? If possible
cvSL <- CV.SuperLearner(Y=dat[,28], X=dat[,-c(28,29)],
  method = "method.NNloglik", family = "binomial", SL.library = lib,
  parallel = 'multicore', cvControl = SuperLearner.CV.control(V = folds,
  stratifyCV = TRUE), innerCvControl = rep(list(SuperLearner.CV.control(V =
  folds, stratifyCV = TRUE)), folds))
summary_cvSL <- summary.CV.SuperLearner(cvSL)
cvSL_results <- summary_cvSL$Table
