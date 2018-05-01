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

dat_summ <- dat2 %>%
  dplyr::group_by(HUsect) %>%
  dplyr::summarise_all(sum)


Kurtosis <- apply(dat_summ[2:1398],2, kurtosis)  
Variance <- apply(dat_summ[2:1398],2, var)
Skew <- apply(dat_summ[2:1398],2, skewness)
Mean <- apply(dat_summ[2:1398],2, mean)
Median <- apply(dat_summ[2:1398],2, median)
df <- t(data.frame(Kurtosis, Variance, Skew, Mean, Median))
df1 <- data.frame(names = row.names(df), df)

colnames(df1)[1] <- "covariate"
colnames(dat_summ)[1] <- "covariate"
  
result <- rbind(dat_summ, df1)

cp1 <- data.frame(names = row.names(cp), cp)
colnames(cp1)[1] <- "covariate"
cp2 <- cp1[,-1399]

result1 <- rbind(result, cp2)
result2 <- data.frame(t(result1))
train_summ <- result2
colnames(train_summ) <- lapply(train_summ[1, ], as.character)
train_summ <- train_summ[-1,]
save(train_summ, file = "~/Desktop/train_summ.RData")

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



