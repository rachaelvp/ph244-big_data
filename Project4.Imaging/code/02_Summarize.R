library(dplyr)
library(e1071)

load("~/Downloads/training_data-4.Rdata")
df2 <- data.frame(t(training_data))
df2$Var1 <- as.numeric(as.character(rownames(df2)))
cp <- data.frame(df2[c(4072, 4071),])
dat <- df2[-c(1, 4072, 4071),]

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
dat2 <- data.frame(dat1[,-1398])

cols = seq(1:1397)    
dat2[,cols] = apply(dat2[,cols], 2, function(x) as.numeric(as.character(x)))

Kurtosis <- dat2 %>%
  dplyr::group_by(HUsect) %>%
  dplyr::summarise_all(kurtosis)
colnames(Kurtosis)[1] <- "covariate"

Skew <- dat2 %>%
  dplyr::group_by(HUsect) %>%
  dplyr::summarise_all(skewness)
Skew[,1] <- c("HU_A_S", "HU_B_S", "HU_C_S", "HU_D_S", "HU_E_S", "HU_F_S", "HU_G_S",
                  "HU_H_S", "HU_I_S", "HU_J_S", "HU_K_S", "HU_L_S", "HU_M_S", "HU_N_S", 
                  "HU_O_S", "HU_P_S", "HU_Q_S", "HU_R_S", "HU_S_S", "HU_T_S", "HU_U_S", "HU_V_S")
colnames(Skew)[1] <- "covariate"

res <- rbind(data.frame(Skew), data.frame(Kurtosis))

dat_summ <- dat2 %>%
  dplyr::group_by(HUsect) %>%
  dplyr::summarise_all(sum)
colnames(dat_summ)[1] <- "covariate"

result <- rbind(res, dat_summ)

cp1 <- data.frame(names = row.names(cp), cp)
colnames(cp1)[1] <- "covariate"
cp2 <- cp1[,-1399]

result1 <- rbind(result, cp2)
result1[,1] <- c("HU_A_S", "HU_B_S", "HU_C_S", "HU_D_S", "HU_E_S", "HU_F_S", "HU_G_S",
              "HU_H_S", "HU_I_S", "HU_J_S", "HU_K_S", "HU_L_S", "HU_M_S", "HU_N_S", 
              "HU_O_S", "HU_P_S", "HU_Q_S", "HU_R_S", "HU_S_S", "HU_T_S", "HU_U_S", "HU_V_S", 
              "HU_A_K", "HU_B_K", "HU_C_K", "HU_D_K", "HU_E_K", "HU_F_K", "HU_G_K",
              "HU_H_K", "HU_I_K", "HU_J_K", "HU_K_K", "HU_L_K", "HU_M_K", "HU_N_K", 
              "HU_O_K", "HU_P_K", "HU_Q_K", "HU_R_K", "HU_S_K", "HU_T_K", "HU_U_K", "HU_V_K", 
              "HU_A", "HU_B", "HU_C", "HU_D", "HU_E", "HU_F", "HU_G",
              "HU_H", "HU_I", "HU_J", "HU_K", "HU_L", "HU_M", "HU_N", 
              "HU_O", "HU_P", "HU_Q", "HU_R", "HU_S", "HU_T", "HU_U", "HU_V", 
              "cancer", "id")
result2 <- data.frame(t(result1))
train_summ <- result2
colnames(train_summ) <- lapply(train_summ[1, ], as.character)
train_summ <- train_summ[-1,]
train_summ <- train_summ[-374,]
save(train_summ, file = "~/Desktop/train_summ.RData")
write.csv(train_summ, "~/Desktop/train_summ.csv")

##########################################################################################
# test data
##########################################################################################
load("~/Downloads/test_data-2.Rdata")
df6 <- data.frame(t(test_data))
df6$Var1 <- as.numeric(as.character(rownames(df6)))
cp6 <- data.frame(df6[c(4072, 1),])
dat <- df6[-c(1, 4072, 4071),]

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
dat2 <- data.frame(dat1[,-199])

cols = seq(1:198)    
dat2[,cols] = apply(dat2[,cols], 2, function(x) as.numeric(as.character(x)))

dat_summ <- dat2 %>%
  dplyr::group_by(HUsect) %>%
  dplyr::summarise_all(sum)


Kurtosis <- apply(dat_summ[2:199],2, kurtosis)  
Variance <- apply(dat_summ[2:199],2, var)
Skew <- apply(dat_summ[2:199],2, skewness)
Mean <- apply(dat_summ[2:199],2, mean)
Median <- apply(dat_summ[2:199],2, median)
df <- t(data.frame(Kurtosis, Variance, Skew, Mean, Median))
df1 <- data.frame(names = row.names(df), df)

colnames(df1)[1] <- "covariate"
colnames(dat_summ)[1] <- "covariate"

result <- rbind(dat_summ, df1)

cp7 <- data.frame(names = row.names(cp6), cp6)
colnames(cp7)[1] <- "covariate"
cp2 <- cp7[,-200]

result1 <- rbind(result, cp2)
result2 <- data.frame(t(result1))
colnames(result2) <- lapply(result2[1, ], as.character)
result2 <- result2[-1,]
test_summ <- result2
save(test_summ, file = "~/Desktop/test_summ.RData")
write.csv(test_summ, file = "~/Desktop/test_summ.csv")


