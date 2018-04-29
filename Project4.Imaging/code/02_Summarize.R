library(dplyr)
library(e1071)

# make sure the Var and finalFreq_scaled are numeric
dat$Var1 <- as.numeric(dat$Var1)
dat$finalFreq_scaled <- as.numeric(dat$finalFreq_scaled)

run_summ <- function(dat){

HUsect <- ifelse(dat$Var1 <= -1000, "HU_A",
          ifelse(dat$Var1 > -1000 & dat$Var1 <= -900, "HU_B",
          ifelse(dat$Var1 > -900 & dat$Var1 <= -800, "HU_C",
          ifelse(dat$Var1 > -800 & dat$Var1 < -700, "HU_D",
          ifelse(dat$Var1 >= -700 & dat$Var1 <= -600, "HU_E",
          ifelse(dat$Var1 > -600 & dat$Var1 <= -500, "HU_F",
          ifelse(dat$Var1 > -500 & dat$Var1 <= -400, "HU_G",
          ifelse(dat$Var1 > -400 & dat$Var1 <= -300, "HU_H",
          ifelse(dat$Var1 > -300 & dat$Var1 <= -200, "HU_I",
          ifelse(dat$Var1 > -200 & dat$Var1 <= -100, "HU_J",
          ifelse(dat$Var1 > -100 & dat$Var1 <= 0, "HU_K",
          ifelse(dat$Var1 > 0 & dat$Var1 <= 100, "HU_L",
          ifelse(dat$Var1 > 100 & dat$Var1 <= 200, "HU_M",
          ifelse(dat$Var1 > 200 & dat$Var1 <= 300, "HU_N",
          ifelse(dat$Var1 > 300 & dat$Var1 <= 400, "HU_O",
          ifelse(dat$Var1 > 400 & dat$Var1 <= 500, "HU_P",
          ifelse(dat$Var1 > 500 & dat$Var1 <= 600, "HU_Q",
          ifelse(dat$Var1 > 600 & dat$Var1 <= 700, "HU_R",
          ifelse(dat$Var1 > 700 & dat$Var1 <= 800, "HU_S",
          ifelse(dat$Var1 > 800 & dat$Var1 <= 900, "HU_T",
          ifelse(dat$Var1 > 900 & dat$Var1 <= 1000, "HU_U",
          ifelse(dat$Var1 > 1000, "HU_V", NA))))))))))))))))))))))

dat1 <- data.frame(dat, HUsect = as.factor(HUsect))
dat_summ <- dat1 %>%
  group_by(HUsect.2) %>%
  summarise(Frequency = sum(finalFreq_scaled))
colnames(dat_summ) <- c("covariate", "value")

Kurtosis <- kurtosis(dat_summ$value)
Variance <- var(dat_summ$value)
Skew <- skewness(dat_summ$value)
Mean <- mean(dat_summ$value)
Median <- median(dat_summ$value)
PatientID <- unique(dat$patientid)
df <- t(data.table(Kurtosis, Variance, Skew, Mean, Median, PatientID))
df1 <- data.frame(names = row.names(df), df)
colnames(df1) <- c("covariate", "value")

result <- t(rbind(dat_summ, df1))
colnames(result) <- result[1,]
result <- t(data.frame(result[-1,]))

return(result)
}
