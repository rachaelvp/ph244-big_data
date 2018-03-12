library(here)
library(dplyr)
library(plyr)

# load the data
train <- read.csv(here("..", "train.csv"))
test <- read.csv(here("..", "test.csv"))

# calculate the acceleration across all rows
# A <- apply(train[ ,c("X","Y","Z")], 1,
# function(x) sqrt(x[1]^2+x[2]^2+x[3]^2))
# train$A <- A

###############################################################################
# calculate the Fast Fourier Transform (FFT) of the axial data
###############################################################################

FFT_X <- fft(train$X)
train$FFT_X <- FFT_X
FFT_Y <- fft(train$Y)
train$FFT_Y <- FFT_Y
FFT_Z <- fft(train$Z)
train$FFT_Z <- FFT_Z

# calculate the Fast Fourier Transform (FFT) of the acceleration data
# FFT_A <- apply(train[ ,c("FFT_X","FFT_Y","FFT_Z")], 1,
# function(x) sqrt(x[1]^2+x[2]^2+x[3]^2))
# train$FFT_A <- FFT_A

###############################################################################
# calculate summary measures for each device id
###############################################################################

mean_X <- train %>%
group_by(Device) %>%
summarize(mean(X))

mean_Y <- train %>%
group_by(Device) %>%
summarize(mean(Y))

mean_Z <- train %>%
group_by(Device) %>%
summarize(mean(Z))

# mean_A <- train %>%
# dplyr::group_by(Device) %>%
# dplyr::summarize(mean(A))

mean_FFT_X <- train %>%
group_by(Device) %>%
summarize(mean(FFT_X))

mean_FFT_Y <- train %>%
group_by(Device) %>%
summarize(mean(FFT_Y))

mean_FFT_Z <- train %>%
group_by(Device) %>%
summarize(mean(FFT_Z))

# mean_FFT_A <- train %>%
# dplyr::group_by(Device) %>%
# dplyr::summarize(mean(FFT_A))

median_X <- train %>%
group_by(Device) %>%
summarize(median(X))

median_Y <- train %>%
group_by(Device) %>%
summarize(median(Y))

median_Z <- train %>%
group_by(Device) %>%
summarize(median(Z))

# median_A <- train %>%
# dplyr::group_by(Device) %>%
# dplyr::summarize(median(A))

median_FFT_X <- train %>%
group_by(Device) %>%
summarize(median(FFT_X))

median_FFT_Y <- train %>%
group_by(Device) %>%
summarize(median(FFT_Y))

median_FFT_Z <- train %>%
group_by(Device) %>%
summarize(median(FFT_Z))

# median_FFT_A <- train %>%
# dplyr::group_by(Device) %>%
# dplyr::summarize(median(FFT_A))

corr_XZ <- train %>%
group_by(Device) %>%
summarise(mean(X)/mean(Z))

corr_YZ <- train %>%
group_by(Device) %>%
summarise(mean(Y)/mean(Z))

corr_FFT_XZ <- train %>%
group_by(Device) %>%
summarise(mean(FFT_X)/mean(FFT_Z))

corr_FFT_YZ <- train %>%
group_by(Device) %>%
summarise(mean(FFT_Y)/mean(FFT_Z))

###############################################################################
# merge all of this together to make a summary table w rows the device id
###############################################################################

corr_FFT <- join(corr_FFT_YZ, corr_FFT_XZ, by = "Device")
corr <- join(corr_YZ, corr_XZ, by = "Device")

all_X <- join(mean_X, median_X, by="Device")
all_Y <- join(mean_Y, median_Y, by="Device")
all_Z <- join(mean_Z, median_Z, by="Device")
all_A <- join(mean_A, median_A, by="Device")
all_XY <- join(all_X, all_Y, by="Device")
all_XYZ <- join(all_XY, all_Z, by="Device")
all_XYZA <- join(all_XYZ, all_A, by="Device")
XYZA <- join(all_XYZA, corr, by = "Device")

all_FFT_X <- join(mean_FFT_X, median_FFT_X, by="Device")
all_FFT_Y <- join(mean_FFT_Y, median_FFT_Y, by="Device")
all_FFT_Z <- join(mean_FFT_Z, median_FFT_Z, by="Device")
all_FFT_A <- join(mean_FFT_A, median_FFT_A, by="Device")
all_FFT_XY <- join(all_FFT_X, all_FFT_Y, by="Device")
all_FFT_XYZ <- join(all_FFT_XY, all_FFT_Z, by="Device")
all_FFT_XYZA <- join(all_FFT_XYZ, all_FFT_A, by="Device")
FFT_XYZA <- join(all_FFT_XYZA, corr_FFT, by = "Device")
M_XYZA <- data.frame(sapply(FFT_XYZA, function(x) Mod(x)))
M_XYZA$Device <- FFT_XYZA$Device
colnames(M_XYZA) <- c("Device","mean_M_X", "median_M_X", "mean_M_Y",
"median_M_Y", "mean_M_Z", "median_M_Z", "mean_M_A", "median_M_A" ,
"Corr_M_YZ","Corr_M_XZ")
P_XYZA <- data.frame(sapply(FFT_XYZA, function(x) Arg(x)))
P_XYZA$Device <- FFT_XYZA$Device
colnames(P_XYZA) <- c("Device","mean_P_X", "median_P_X", "mean_P_Y",
"median_P_Y", "mean_P_Z", "median_P_Z", "mean_P_A", "median_P_A" ,
"Corr_P_YZ","Corr_P_XZ")
MP_XYZA <- join(P_XYZA, M_XYZA, by = "Device")
summ <- join(XYZA, MP_XYZA, by = "Device")
# > colnames(summ)
# [1] "Device"          "mean(X)"         "median(X)"       "mean(Y)"
# [5] "median(Y)"       "mean(Z)"         "median(Z)"       "mean(A)"
# [9] "median(A)"       "mean(Y)/mean(Z)" "mean(X)/mean(Z)" "mean_P_X"
# [13] "median_P_X"      "mean_P_Y"        "median_P_Y"      "mean_P_Z"
# [17] "median_P_Z"      "mean_P_A"        "median_P_A"      "Corr_P_YZ"
# [21] "Corr_P_XZ"       "mean_M_X"        "median_M_X"      "mean_M_Y"
# [25] "median_M_Y"      "mean_M_Z"        "median_M_Z"      "mean_M_A"
# [29] "median_M_A"      "Corr_M_YZ"       "Corr_M_XZ"
colnames(summ) <- c("Device", "mean_X", "median_X", "mean_Y", "median_Y",
"mean_Z", "median_Z","mean_A", "median_A","Corr_YZ", "Corr_XZ",
"mean_P_X", "median_P_X", "mean_P_Y", "median_P_Y", "mean_P_Z", "median_P_Z",
"mean_P_A", "median_P_A", "Corr_P_YZ", "Corr_P_XZ", "mean_M_X","median_M_X",
"mean_M_Y","median_M_Y","mean_M_Z","median_M_Z","mean_M_A","median_M_A",
"Corr_M_YZ","Corr_M_XZ")
summary_table <- summ[,-c(8,9,18,19,28,29)]
write.csv(summary_table,
  file = here::here("Project2.Accelerometer_Biometric_Competition",
"data", "summary_table.csv"))
# train_summ <- join(train, summ, by = "Device")

###############################################################################
#Additional Question Specific Processing
###############################################################################

# obtain the time from the Unix timestamp
T <- as.POSIXct(train$T, origin="1970-01-01")
t <- strftime(T, format="%H:%M:%S")
# create a new time object with a constant date
ct <- as.POSIXct(strptime(t, format="%H:%M:%S"))
ctt <- as.POSIXct(t, format="%H:%M:%S")
train$ct <- ct
train$ctt <- ctt
train$t <- t

lt <- as.POSIXlt(t, format="%H:%M:%S")
train$lt <- lt
sec <- x$sec + x$min*60 + x$hour*3600
train$sec <- sec

# You may also try ggplot:

library(ggplot2)
df$time <- as.POSIXct(strptime(df$Time, format="%H:%M:%S"))

# Automatic scale selection
ggplot(data = df, aes(x = time, y = EcNo)) + geom_point()

# scale_x_datetime is a ggplot function, but for the nice arguments date_breaks,
# and date_format you need package scales:

library(scales)

ggplot(data = df, aes(x = time, y = EcNo)) + geom_point() +
  scale_x_datetime(breaks = date_breaks("1 sec"), labels = date_format("%S"))

ggplot(data = df, aes(x = time, y = EcNo)) + geom_point() +
  scale_x_datetime(breaks = date_breaks("1 sec"), labels = date_format("%OS3"))

ggplot(data = df, aes(x = time, y = EcNo)) + geom_point() +
  scale_x_datetime(breaks = date_breaks("4 sec"), labels = date_format("%M:%S"))


###############################################################################
# save it
###############################################################################

save(train, file = here::here("Project2.Accelerometer_Biometric_Competition",
"data", "train.RData"))
save(summ, file = here::here("Project2.Accelerometer_Biometric_Competition",
"data", "summary.RData"))
save(train_summ,file=here::here("Project2.Accelerometer_Biometric_Competition",
"data", "train_summary.RData"))
