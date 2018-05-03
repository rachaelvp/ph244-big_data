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
cvSL_results <- summary_cvSL$Table
cvSL_results
