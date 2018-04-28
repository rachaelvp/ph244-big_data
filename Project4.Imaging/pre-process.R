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

# Obtain records from all patients
patientDir <- sort(list.dirs(path = "sample_images", full.names = TRUE, recursive = FALSE))

# Actually convert the DICOM for each patient
dicom <- readDICOM(patientDir[17])
dicomdf <- dicomTable(dicom$hdr)

# Extract the dicom headers
ImagePositionPatient <- extractHeader(dicom$hdr, "ImagePositionPatient", numeric=FALSE)
ImagePosPatient <- Reduce(c,strsplit(ImagePositionPatient," "))
head(ImagePosPatient)

# Convert dicom to nifti
dicom2nifti <- dicom2nifti(dicom)

hist(dicom2nifti)
image(dicom2nifti, plot.type = "single")

# Removing the air
thresh.img <- dicom2nifti
#thresh.img <- (thresh.img - mean(thresh.img))/sd(thresh.img)
thresh.img[thresh.img <= -2000] <- NA
thresh.img <- cal_img(thresh.img)
image(thresh.img)
#########################



# # Obtain records from all patients
# patientDir <- sort(list.dirs(path = "sample_images", full.names = TRUE, recursive = FALSE))
# 
# # Convert to haunsfield units for a person
# HUfreq <- coreHist(patientDir[17], lowerLim = -700, upperLim = -600)
# 
# # Normalize frequency range
# sum<- sum(HUfreq$histData$finalFreq)
# HUfreq$histData$finalFreq_scaled <- (HUfreq$histData$finalFreq/sum)
# plot(x=HUfreq$histData$Var1, y=HUfreq$histData$finalFreq_scaled)

# Second approach: using coreCT
library("coreCT")

# Obtain records from all patients
patientDir <- sort(list.dirs(path = "sample_images", full.names = TRUE, recursive = FALSE))

dataframes <- list()
i = 1
while(i<=2){
  # Strip the patient out
  patient <- coreHist(patientDir[i])
  
  # Extract the relevant information from the patient
  dicom <- readDICOM(patientDir[i])
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
  
  i = i+1
}

