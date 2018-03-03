library(here)
library(SuperLearner)
library(ck37r)
library(readr)

load(here("Project2.Accelerometer_Biometric_Competition","data", "train.RData"))

SL.library <- list(c("SL.ranger"))

folds <- 10

Seq_auc <- CV.SuperLearner(Y=train[,1], X=train[,-1], family = 'binomial',
           SL.library = SL.library, parallel = 'multicore',
           method = "method.AUC",
           cvControl = SuperLearner.CV.control(V = folds, stratifyCV = TRUE),
           innerCvControl = rep(list(SuperLearner.CV.control(V = folds,
           stratifyCV = TRUE)), folds))
summary(Seq_auc)
