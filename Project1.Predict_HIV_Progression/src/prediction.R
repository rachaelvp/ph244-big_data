library(here)
library(SuperLearner)
library(ck37r)
library(readr)

Seq <- data.frame(read_csv(here::here("Project1.Predict_HIV_Progression",
"data", "Seq.csv"))[1:920,])
Seq$Resp <- as.numeric(Seq$Resp)

AA <- data.frame(read_csv(here::here("Project1.Predict_HIV_Progression",
"data", "AA.csv"))[1:920,])
AA$Resp <- as.numeric(AA$Resp)



#############################################################
# predict with CV.SuperLearner
#############################################################

SL.library <- list(c("SL.ranger"))

folds <- 10

Seq_auc <- CV.SuperLearner(Y=Seq[,1], X=Seq[,-1], family = 'binomial',
           SL.library = SL.library, parallel = 'multicore',
           method = "method.AUC",
           cvControl = SuperLearner.CV.control(V = folds, stratifyCV = TRUE),
           innerCvControl = rep(list(SuperLearner.CV.control(V = folds,
           stratifyCV = TRUE)), folds))
summary(Seq_auc)
# Risk is based on: Area under ROC curve (AUC)
#
# All risk estimates are based on V =  10
#
#      Algorithm     Ave se     Min     Max
#  Super Learner 0.84958 NA 0.79668 0.89403
#    Discrete SL 0.84958 NA 0.79668 0.89403
#  SL.ranger_All 0.84958 NA 0.79668 0.89403

pdf(file = here::here("Project1.Predict_HIV_Progression",
           "graphs", "Seq_auc.pdf"))
ck37r::cvsl_plot_roc(Seq_auc, Y = Seq_auc$Y,
                     title = "Cross Validated AUC for
                     Prediction of Short-term HIV Improvement
                     using Top 10 DNA Positions from both PR and RT,
                     CD4 count, and Viral Load",
                     digits = 4)
dev.off()

AA_auc <- CV.SuperLearner(Y=AA[,1], X=AA[,-1], family = 'binomial',
           SL.library = SL.library, parallel = 'multicore',
           method = "method.AUC",
           cvControl = SuperLearner.CV.control(V = folds, stratifyCV = TRUE),
           innerCvControl = rep(list(SuperLearner.CV.control(V = folds,
           stratifyCV = TRUE)), folds))
summary(AA_auc)
# Risk is based on: Area under ROC curve (AUC)
#
# All risk estimates are based on V =  10
#
#      Algorithm     Ave se     Min     Max
#  Super Learner 0.83252 NA 0.75198 0.90267
#    Discrete SL 0.83252 NA 0.75198 0.90267
#  SL.ranger_All 0.83252 NA 0.75198 0.90267

pdf(file = here::here("Project1.Predict_HIV_Progression",
           "graphs", "AA_auc.pdf"))
ck37r::cvsl_plot_roc(AA_auc, Y = AA_auc$Y,
                     title = "Cross Validated AUC for
                     Prediction of Short-term HIV Improvement
                     using Top 10 AA Positions from both PR and RT,
                     CD4 count, and Viral Load",
                     digits = 4)
dev.off()

# try without CD4 counts
Seq1 <- Seq[,-c(22,23)]
AA1 <- AA[,-c(22,23)]

Seq1_auc <- CV.SuperLearner(Y=Seq1[,1], X=Seq1[,-1], family = 'binomial',
           SL.library = SL.library, parallel = 'multicore',
           method = "method.AUC",
           cvControl = SuperLearner.CV.control(V = folds, stratifyCV = TRUE),
           innerCvControl = rep(list(SuperLearner.CV.control(V = folds,
           stratifyCV = TRUE)), folds))

# Risk is based on: Area under ROC curve (AUC)
#
# All risk estimates are based on V =  10
#
#     Algorithm     Ave se     Min     Max
# Super Learner 0.76821 NA 0.63584 0.83001
# Discrete SL 0.76821 NA 0.63584 0.83001
# SL.ranger_All 0.76821 NA 0.63584 0.83001

pdf(file = here::here("Project1.Predict_HIV_Progression",
           "graphs", "Seq_auc_onlyDNA.pdf"))
ck37r::cvsl_plot_roc(Seq1_auc, Y = Seq1_auc$Y,
                     title = "Cross Validated AUC for
                     Prediction of Short-term HIV Improvement
                     using Top 10 DNA Positions from both PR and RT",
                     digits = 4)
dev.off()

AA1_auc <- CV.SuperLearner(Y=AA1[,1], X=AA1[,-1], family = 'binomial',
           SL.library = SL.library, parallel = 'multicore',
           method = "method.AUC",
           cvControl = SuperLearner.CV.control(V = folds, stratifyCV = TRUE),
           innerCvControl = rep(list(SuperLearner.CV.control(V = folds,
           stratifyCV = TRUE)), folds))

# Risk is based on: Area under ROC curve (AUC)
#
# All risk estimates are based on V =  10
#
#   Algorithm     Ave se     Min     Max
# Super Learner 0.69333 NA 0.58499 0.75284
# Discrete SL 0.69333 NA 0.58499 0.75284
# SL.ranger_All 0.69333 NA 0.58499 0.75284

pdf(file = here::here("Project1.Predict_HIV_Progression",
           "graphs", "AA_auc_onlyAA.pdf"))
ck37r::cvsl_plot_roc(AA1_auc, Y = AA1_auc$Y,
                     title = "Cross Validated AUC for
                     Prediction of Short-term HIV Improvement
                     using Top 10 AA Positions from both PR and RT",
                     digits = 4)
dev.off()

Seq1 <- Seq[,-c(22,23)]
AA2 <- AA1[,-c(1)]
AA2 <- data.frame(Seq1, AA2)

AA2_auc <- CV.SuperLearner(Y=AA2[,1], X=AA2[,-1], family = 'binomial',
           SL.library = SL.library, parallel = 'multicore',
           method = "method.AUC",
           cvControl = SuperLearner.CV.control(V = folds, stratifyCV = TRUE),
           innerCvControl = rep(list(SuperLearner.CV.control(V = folds,
           stratifyCV = TRUE)), folds))

           # Risk is based on: Area under ROC curve (AUC)
           #
           # All risk estimates are based on V =  10
           #
           #      Algorithm     Ave se     Min     Max
           #  Super Learner 0.79011 NA 0.71337 0.84566
           #    Discrete SL 0.79011 NA 0.71337 0.84566
           #  SL.ranger_All 0.79011 NA 0.71337 0.84566
pdf(file = here::here("Project1.Predict_HIV_Progression",
                      "graphs", "auc_onlyAAandDNA.pdf"))
ck37r::cvsl_plot_roc(AA2_auc, Y = AA2_auc$Y,
    title = "Cross Validated AUC for Prediction of Short-term HIV Improvement
              using Top 10 AA Positions and Top 10 DNA Positions
              from both PR and RT",
                                digits = 4)
           dev.off()

Seq1 <- Seq[,-c(22,23)]
AA3 <- AA[,-c(1)]
AA3 <- data.frame(Seq1, AA3)

AA3_auc <- CV.SuperLearner(Y=AA3[,1], X=AA3[,-1], family = 'binomial',
           SL.library = SL.library, parallel = 'multicore',
           method = "method.AUC",
           cvControl = SuperLearner.CV.control(V = folds, stratifyCV = TRUE),
           innerCvControl = rep(list(SuperLearner.CV.control(V = folds,
           stratifyCV = TRUE)), folds))

           # Risk is based on: Area under ROC curve (AUC)
           #
           # All risk estimates are based on V =  10
           #
           #      Algorithm     Ave se     Min     Max
           #  Super Learner 0.85826 NA 0.79224 0.93075
           #    Discrete SL 0.85826 NA 0.79224 0.93075
           #  SL.ranger_All 0.85826 NA 0.79224 0.93075

pdf(file = here::here("Project1.Predict_HIV_Progression",
                                 "graphs", "auc_All.pdf"))
ck37r::cvsl_plot_roc(AA3_auc, Y = AA3_auc$Y,
  title = "Cross Validated AUC for Prediction of Short-term HIV Improvement
        using Top 10 AA Positions and Top 10 DNA Positions
        from both PR and RT, CD4 count, and Viral Load",
        digits = 4)
                      dev.off()
