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
dicom <- readDICOM(patientDir[10])
dicomdf <- dicomTable(dicom$hdr)

# Extract the dicom headers
ImagePositionPatient <- extractHeader(dicom$hdr, "ImagePositionPatient", numeric=FALSE)
ImagePosPatient <- Reduce(c,strsplit(ImagePositionPatient," "))
head(ImagePosPatient)

# Convert dicom to nifti
dicom2nifti <- dicom2nifti(dicom)

hist(dicom2nifti)
image(dicom2nifti, plot.type = "single")