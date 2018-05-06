library(SuperLearner)
library(here)

# load and prep data
dat <- read.csv(file=here::here("Project4.Imaging","data","train_summ.csv"))
cols <- seq(1:67)
dat[,cols] <- apply(dat[,cols], 2, function(x) as.numeric(as.character(x)))

# set up Super Learner library
folds <- 100
lib <- list(c("SL.glm"),
            c("SL.mean"),
            c("SL.qda"),
            c("SL.gam"),
            c("SL.svm"),
            c("SL.ranger"),
            c("SL.glmnet"),
            c("SL.nnet"))

# run cross-validated Super Learner
cvSL <- CV.SuperLearner(Y=dat[,67], X=dat[,-c(67)],
  method = "method.AUC", family = "binomial", parallel = 'multicore',
  SL.library = lib, cvControl = SuperLearner.CV.control(V = folds,
  stratifyCV = TRUE), innerCvControl = rep(list(SuperLearner.CV.control(V =
  folds, stratifyCV = TRUE)), folds))
summary_cvSL <- summary.CV.SuperLearner(cvSL)
summary_cvSL$Table

# Risk is based on: Area under ROC curve (AUC)
#
# All risk estimates are based on V =  100
#
#      Algorithm     Ave se     Min     Max
#  Super Learner 0.51559 NA 0.10000 0.90000
#    Discrete SL 0.52392 NA 0.10000 0.90000
#     SL.glm_All 0.52661 NA 0.13333 0.92500
#    SL.mean_All 0.50000 NA 0.50000 0.50000
#     SL.qda_All 0.51478 NA 0.12500 0.96667
#     SL.gam_All 0.52830 NA 0.10000 0.90000
#     SL.svm_All 0.50247 NA 0.10000 0.77273
#  SL.ranger_All 0.48166 NA 0.05000 0.92045
#  SL.glmnet_All 0.49492 NA 0.10000 0.66667
#    SL.nnet_All 0.49593 NA 0.12500 0.85000
