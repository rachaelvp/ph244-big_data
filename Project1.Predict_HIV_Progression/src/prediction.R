library(here)
library(SuperLearner)
library(ck37r)
library(readr)

Seq <- data.frame(read_csv(here::here("Project1.Predict_HIV_Progression",
"data", "Seq.csv"))[1:920,])
Seq$Resp <- as.numeric(Seq$Resp)
Seq[,2] <- as.factor(as.character(Seq[,2]))
Seq[,3] <- as.factor(as.character(Seq[,3]))
Seq[,4] <- as.factor(as.character(Seq[,4]))
Seq[,5] <- as.factor(as.character(Seq[,5]))
Seq[,6] <- as.factor(as.character(Seq[,6]))
Seq[,7] <- as.factor(as.character(Seq[,7]))
Seq[,8] <- as.factor(as.character(Seq[,8]))
Seq[,9] <- as.factor(as.character(Seq[,9]))
Seq[,10] <- as.factor(as.character(Seq[,10]))
Seq[,11] <- as.factor(as.character(Seq[,11]))
Seq[,12] <- as.factor(as.character(Seq[,12]))
Seq[,13] <- as.factor(as.character(Seq[,13]))
Seq[,14] <- as.factor(as.character(Seq[,14]))
Seq[,15] <- as.factor(as.character(Seq[,15]))
Seq[,16] <- as.factor(as.character(Seq[,16]))
Seq[,17] <- as.factor(as.character(Seq[,17]))
Seq[,18] <- as.factor(as.character(Seq[,18]))
Seq[,19] <- as.factor(as.character(Seq[,19]))
Seq[,20] <- as.factor(as.character(Seq[,20]))
Seq[,21] <- as.factor(as.character(Seq[,21]))
Seq[,22] <- as.numeric(as.character(Seq[,22]))
Seq[,23] <- as.numeric(as.character(Seq[,23]))

AA <- data.frame(read_csv(here::here("Project1.Predict_HIV_Progression",
"data", "AA.csv"))[1:920,])
AA$Resp <- as.numeric(AA$Resp)
AA[,2] <- as.factor(as.character(AA[,2]))
AA[,3] <- as.factor(as.character(AA[,3]))
AA[,4] <- as.factor(as.character(AA[,4]))
AA[,5] <- as.factor(as.character(AA[,5]))
AA[,6] <- as.factor(as.character(AA[,6]))
AA[,7] <- as.factor(as.character(AA[,7]))
AA[,8] <- as.factor(as.character(AA[,8]))
AA[,9] <- as.factor(as.character(AA[,9]))
AA[,10] <- as.factor(as.character(AA[,10]))
AA[,11] <- as.factor(as.character(AA[,11]))
AA[,12] <- as.factor(as.character(AA[,12]))
AA[,13] <- as.factor(as.character(AA[,13]))
AA[,14] <- as.factor(as.character(AA[,14]))
AA[,15] <- as.factor(as.character(AA[,15]))
AA[,16] <- as.factor(as.character(AA[,16]))
AA[,17] <- as.factor(as.character(AA[,17]))
AA[,18] <- as.factor(as.character(AA[,18]))
AA[,19] <- as.factor(as.character(AA[,19]))
AA[,20] <- as.factor(as.character(AA[,20]))
AA[,21] <- as.factor(as.character(AA[,21]))
AA[,22] <- as.numeric(as.character(AA[,22]))
AA[,23] <- as.numeric(as.character(AA[,23]))


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
