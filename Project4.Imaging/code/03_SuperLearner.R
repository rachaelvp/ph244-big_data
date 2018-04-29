library(SuperLearner)

# set up Super Learner library
lib <-

# run cross-validated Super Learner? If possible
cvSL <- CV.SuperLearner(Y=dat[,1], X=dat[,-1], newX= ,
  method = "method.NNloglik", family = "binomial", SL.library = lib,
  parallel = 'multicore', cvControl = SuperLearner.CV.control(V = folds,
  stratifyCV = TRUE), innerCvControl = rep(list(SuperLearner.CV.control(V =
  folds, stratifyCV = TRUE)), folds))
summary_cvSL <- summary.CV.SuperLearner(cvSL)
rm(cvSL)
cvSL_results <- summary_cvSL$Table
