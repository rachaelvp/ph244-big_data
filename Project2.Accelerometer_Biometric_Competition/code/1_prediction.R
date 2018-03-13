library(here)
library(SuperLearner)
library(randomForest)

load(here("Project2.Accelerometer_Biometric_Competition","data",
"summary.RData"))
summary_table$Device <- as.numeric(summary_table$Device)

screen.corRank_5 <- function(...)screen.corRank(..., rank=5)
SL.library <- list(c("SL.nnet", "screen.corRank_5"),
                   c("SL.rpart", "screen.corRank_5"),
                   c("SL.ranger", "screen.corRank_5"))
folds <- 10

Y <- summary_table$Device

cvSL <- CV.SuperLearner(Y, X=summary_table[,-1], family = 'gaussian',
method = "method.NNloglik", SL.library = SL.library,
cvControl = SuperLearner.CV.control(V = folds),
innerCvControl = rep(list(SuperLearner.CV.control(V = folds)), folds))

Risk is based on: Negative Log Likelihood (-2*log(L))

All risk estimates are based on V =  10

                  Algorithm        Ave se        Min        Max
              Super Learner  0.0010005 NA  0.0010005  0.0010005
                Discrete SL -5.2684607 NA -5.2839619 -5.2448135
   SL.nnet_screen.corRank_5 -5.2684607 NA -5.2839619 -5.2448135
  SL.rpart_screen.corRank_5 -5.1165588 NA -5.3019911 -4.9484550
 SL.ranger_screen.corRank_5 -5.1660978 NA -5.3676121 -5.0386694

 fit <- randomForest(Device ~ mean_X+median_X+mean_Y+median_Y+mean_Z+median_Z
   +Corr_YZ+Corr_XZ+mean_P_X+median_P_X+mean_P_Y+median_P_Y+mean_P_Z+median_P_Z
 + Corr_P_YZ+Corr_P_XZ+mean_M_X+median_M_X+mean_M_Y+median_M_Y+mean_M_Z+
 median_M_Z+Corr_M_YZ+Corr_M_XZ,data = summary_table)
 varImpPlot(fit)
