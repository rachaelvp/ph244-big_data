# Set the working directory
setwd("/Volumes/HUGE storage drive/Lexin/")

# Load the required packages to interpret DICOM
library(oro.dicom)
library(oro.nifti)
library(readr)
library(imager)
library(fslr)
library(plyr)
library(ggplot2)

# # Obtain records from all patients
# patientDir <- sort(list.dirs(path = "sample_images", full.names = TRUE, recursive = FALSE))
# 
# # Actually convert the DICOM for each patient
# dicom <- readDICOM(patientDir[17])
# dicomdf <- dicomTable(dicom$hdr)
# 
# # Extract the dicom headers
# ImagePositionPatient <- extractHeader(dicom$hdr, "ImagePositionPatient", numeric=FALSE)
# ImagePosPatient <- Reduce(c,strsplit(ImagePositionPatient," "))
# head(ImagePosPatient)
# 
# # Convert dicom to nifti
# dicom2nifti <- dicom2nifti(dicom)
# 
# hist(dicom2nifti)
# image(dicom2nifti, plot.type = "single")
# 
# # Removing the air
# thresh.img <- dicom2nifti
# #thresh.img <- (thresh.img - mean(thresh.img))/sd(thresh.img)
# thresh.img[thresh.img <= -2000] <- NA
# thresh.img <- cal_img(thresh.img)
# image(thresh.img)
#########################





# Second approach: using coreCT
library("coreCT")

# Obtain records from all patients
patientDir <- sort(list.dirs(path = "stage1", full.names = TRUE, recursive = FALSE))

dataframes <- list()
i = 1
while(i<1596){
  # Strip the patient out
  patient <- coreHist(patientDir[i])
  
  print("1")
  setwd("/Volumes/HUGE storage drive/Lexin/")
  exists<- file.exists(patientDir[i])
  
  print(exists)
  
  # Extract the relevant information from the patient
  # Set a loop to try the function if it fails
  attempt <- 1
  while(is.null(dicom) && attempt <= 3){
    attempt <- attempt + 1
    try(
      dicom <- readDICOM(patientDir[i])
    )  
  }
  
  dicomdf <- dicomTable(dicom$hdr)  
  patient_id <- dicomdf$`0010-0020-PatientID`[1] 
  
  # Normalize their VX's
  sum<- sum(patient$histData$finalFreq)

  # Create the new VX's
  patient$histData$finalFreq_scaled <- (patient$histData$finalFreq/sum)

  # Add their ID
  patient$histData$patientid <- patient_id
  
  # Keep only the important columns
  patient$histData <- patient$histData[c("patientid", "Var1", "finalFreq_scaled")]
  
  # Add these dataframes to a list for better recall afterwards
  dataframes[[i]] <- patient$histData
  
  # Additional code to transpose and merge dataframes
  if(i == 1){
    long_df <- patient$histData
  }else{
    long_df <- rbind(long_df,patient$histData )
  }
  
  print(paste(c("Patient", i), sep ="", collapse = "-"))

  i = i+1
  dicom <- NULL
}

# Save giant df
save(long_df, file = "actual_long_df.rData")

# Reshape the data from long to wide
library(reshape2)
wide_df <- dcast(long_df, patientid ~ Var1, value.var = "finalFreq_scaled", fun.aggregate = mean)
save(wide_df, file = "actual_wide_df.rData")

# Read in the file containing patient disease status
labels <- read.csv("stage1_labels.csv")
labels$patientid <- labels$id

# Merge these results with those obtained 
training_test_data <- merge(wide_df, labels, by="patientid", all = TRUE)
save(training_test_data, file = "actual_training_test.rData")

# We want the actual training and test data
test_data <- training_data[as.logical(sapply(training_data$cancer, function(x) sum(is.na(x)))),]
training_data <- training_data[!as.logical(sapply(training_data$cancer, function(x) sum(is.na(x)))),]
save(test_data, file = "actual_test_data.Rdata")
save(training_data, file = "actual_training_data.Rdata")


# Subset the x covariates
x_covariates <- c("patientid","id", "cancer")
training_data_x <- training_data[ , !(names(training_data) %in% x_covariates)]
training_data_y <- training_data$cancer

# Run the superlearner algorithm
library("SuperLearner")
library <- c("SL.mean", "SL.glm")
SuperLearner(Y = training_data_y, X = training_data_x, SL.library = library, family = binomial())

