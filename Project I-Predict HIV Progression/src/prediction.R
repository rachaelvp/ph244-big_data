library(here)
library(SuperLearner)
library(ck37r)

Seq <- read_csv(table_path = here("data", "Seq.csv"))
AA <- read_csv(table_path = here("data", "AA.csv"))

#############################################################
# predict with CV.SuperLearner
#############################################################

SL.library <- list(c("SL.glm"),
                   c("SL.bayesglm"),
                   c("SL.mean"),
                   c("SL.qda"),
                   c("SL.gam"),
                   c("SL.svm"),
                   c("SL.ranger"),
                   c("SL.xgboost"))

folds <- 10

Seq_auc <- CV.SuperLearner(Y=Seq[,1], X=Seq[,-1], family = 'binomial',
           SL.library = SL.library, parallel = 'multicore',
           method = "method.AUC",
           cvControl = SuperLearner.CV.control(V = folds, stratifyCV = TRUE),
           innerCvControl = rep(list(SuperLearner.CV.control(V = folds,
           stratifyCV = TRUE)), folds))

ck37r::cvsl_plot_roc(Seq_auc, Y = Seq_auc$Y,
                     title = "Cross Validated AUC for
                     Prediction of Short-term HIV Improvement
                     with DNA Sequences, CD4 count, and Viral Load",
                     digits = 4)
