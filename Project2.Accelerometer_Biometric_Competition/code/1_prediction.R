library(here)
library(sl3)
library(readr)

load(here("Project2.Accelerometer_Biometric_Competition","data", "train.RData"))

# here are the covariates we are interested in and, of course, the outcome
covars <- c("mean_X", "median_X", "mean_Y", "median_Y",
"mean_Z", "median_Z","mean_A", "median_A","Corr_YZ", "Corr_XZ",
"mean_P_X", "median_P_X", "mean_P_Y", "median_P_Y", "mean_P_Z", "median_P_Z",
"mean_P_A", "median_P_A", "Corr_P_YZ", "Corr_P_XZ", "mean_M_X","median_M_X",
"mean_M_Y","median_M_Y","mean_M_Z","median_M_Z","mean_M_A","median_M_A",
"Corr_M_YZ","Corr_M_XZ")

outcome <- "Device"

# create the sl3 task and take a look at it
task <- make_sl3_Task(data = summ, covariates = covars,
                            outcome = outcome, outcome_type = "categorical")

sl3_list_learners(c("categorical"))
# [1] "Lrnr_glmnet"               "Lrnr_h2o_glm"
# [3] "Lrnr_h2o_grid"             "Lrnr_independent_binomial"
# [5] "Lrnr_mean"                 "Lrnr_optim"
# [7] "Lrnr_randomForest"         "Lrnr_solnp"
# [9] "Lrnr_xgboost"

# make learner objects
lrnr_glmnet <- make_learner(Lrnr_glmnet)
lrnr_mean <- make_learner(Lrnr_mean)
lrnr_rf <- make_learner(Lrnr_randomForest)
lrnr_ib <- make_learner(Lrnr_independent_binomial)
lrnr_xg <- make_learner(Lrnr_xgboost)
lrnr_sol <- make_learner(Lrnr_solnp)
lrnr_opt <- make_learner(Lrnr_optim)
lrnr_glm <- make_learner(Lrnr_h2o_glm)

stack <- make_learner(Stack, lrnr_mean,lrnr_rf,lrnr_ib,
  lrnr_sol,lrnr_opt, lrnr_glm, lrnr_xg, lrnr_glmnet)

cv_stack <- Lrnr_cv$new(stack)
cv_fit <- cv_stack$train(task)
cv_preds <- cv_fit$predict()
risks <- cv_fit$cv_risk(loss_loglik_multinomial)
print(risks)

stack_fit <- stack$train(task)
sl <- Lrnr_sl$new(learners = stack,
                  metalearner = metalearner)
sl_fit <- sl$train(task)
lrnr_sl_preds <- sl_fit$predict()
head(lrnr_sl_preds)
