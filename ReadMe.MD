---
title: "ReadMe.MD"
author: "Rod Maclean"
date: "January 27, 2016"

---

## REPOSITORY CONTENTS

This repository contains an R script designed to cleanse and summarize data for the Getting and Cleaning Data Course Project

The output files are 

1. ReadMe.MD. - This contains as summary of the repository purpose and the original data used..
2. run_analysis.R - This script performs the analysis required for the project
3. CodeBook.MD - This describes the functioning of the script in detail
4. summary_by_subject_activity.txt - This file has the output data set as required for the project

##Script Functionality

The full functionality of the script is describe in CodeBook.MD

In summary it loads all the data sets, cleanses ans attaches the column headings.
It then conmbines the TEST and TRAINING data sets and merges in the Activity and Subject data into the data set.

The required columns contaning mean and std deviation data are then selected out and these are melted into Key:Value pairs.

For the final output the variables are summarized with the average of each variable for each activity and each subject.

##Project Requirement

Getting and Cleaning Data Course Projectless 
The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. 
The goal is to prepare tidy data that can be used for later analysis. 
You will be graded by your peers on a series of yes/no questions related to the project. 
You will be required to submit: 
1) a tidy data set as described below, 
2) a link to a Github repository with your script for performing the analysis, and 
3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. 
You should also include a README.md in the repo with your scripts. 
This repo explains how all of the scripts work and how they are connected.

## Original Data sources
The explanation of the original data is at:

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

The original data for the project can be found at:

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
You can also embed plots, for example:

Attribute Information:

For each record in the dataset there is provided: 
- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration. 
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

