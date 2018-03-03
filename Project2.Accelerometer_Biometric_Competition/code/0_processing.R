library(here)

# load the data
train <- read.csv(here("..", "train.csv"))
test <- read.csv(here("..", "test.csv"))

# calculate the acceleration across all rows
train$A <- apply(train[ ,c("X","Y","Z")], 1,
function(x) sqrt(x[1]^2+x[2]^2+x[3]^2))

#### Additional Question Specific Preprocessing
# obtain the time from the Unix timestamp
T <- as.POSIXct(train$T, origin="1970-01-01")
t <- strftime(T, format="%H:%M:%S")
# create a new time object with a constant date
xx <- as.POSIXct(t, format="%H:%M:%S")
train$xx <- xx
train$t <- t

# save it
save(train, file = here("Project2.Accelerometer_Biometric_Competition",
"data", "train.RData")))
save(test, file = here("Project2.Accelerometer_Biometric_Competition",
"data", "test.RData")))
