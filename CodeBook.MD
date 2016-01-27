---
title: "CodeBook.MD"
author: "Rod Maclean"
date: "January 27, 2016"

---

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

### Data Summary

The data sets are split into a TEST and TRAINING data set with 2947 records in the TEST and 7352 records in the TRAINING set.

Each measurment record contains 561-feature vector with time and frequency domain variables

The test and training result data sets and in 
X_test.txt and
X_train.txt


### Activity Data

The labels are stored in the file activity_labels.txt

The activity type id's that match the result data set are stored in 
y_test.txt and
y_train.txt

### Subject Data

The subject ids for the test and training data are stored in the filed :
subject_test.txt
subject_train.txt

### Measurement Labels

These are stored in the file features.txt
Further information on the measurments are available in features_info.txt

##Data Types

The data has the following types:

 $ subject_id   : int 
 $ activity_id  : int  
 $ set_type     : chr 
 $ activity_type: chr  
 $ variable     : Factor w/ 66 levels "tBodyAcc-mean()-X",..:
 $ value        : num

##Analysis Processing

The data sets above were loaded into R without filtering.
```{r}
##read data files
test_set <- fread("./UCI HAR Dataset/test/X_test.txt", header = FALSE,  stringsAsFactors =FALSE)
training_set <- fread("./UCI HAR Dataset/train/X_train.txt", header = FALSE,  stringsAsFactors =FALSE)
```
Column Headers are assigned
```{r}
##read and assign col headers
header_names <-fread("./UCI HAR Dataset/features.txt", header = FALSE,  stringsAsFactors =FALSE)
name_v <- header_names$V2
```

An Anomoly in the variable naming is corrected and colnm names are set
```{r}
##Clean Up Names
name_v <-gsub("BodyBody","Body",name_v)


##Set clean Names
names (test_set)<- name_v
names (training_set)<- name_v
```

Subject data is loaded
```{r}
##read subjects
subject_train <- fread("./UCI HAR Dataset/train/subject_train.txt", header = FALSE,  stringsAsFactors =FALSE)
subject_test <- fread("./UCI HAR Dataset/test/subject_test.txt", header = FALSE,  stringsAsFactors =FALSE)

names(subject_train) <- ("subject_id")
names(subject_test) <- ("subject_id")
```


Activity Data is loaded
```{r}
##Read Activity data
activity_train <- fread("./UCI HAR Dataset/train/y_train.txt", header = FALSE,  stringsAsFactors =FALSE)
activity_test <- fread("./UCI HAR Dataset/test/y_test.txt", header = FALSE,  stringsAsFactors =FALSE)
activity_labels <- fread("./UCI HAR Dataset/activity_labels.txt", header = FALSE,  stringsAsFactors =FALSE)
names(activity_train) <- ("activity_id")
names(activity_test) <- ("activity_id")
names(activity_labels) <- c("activity_id", "activity_type")
```

Subject and Activity data are merged with the measurement data set
```{r}
##Set activity and subject data in data sets
test_set2 <- cbind(subject_test, activity_test , test_set)
training_set2 <- cbind(subject_train, activity_train , training_set)
```

A column is added to preseve data set origin before merging
```{r}
##Add column to identify original set
test_set2$set_type = "TEST"
training_set2$set_type = "TRAINING"    
```

The Test and Traing sets are merged      
```{r}      

##combine into single data.table
full_set <- rbind(test_set2, training_set2)
``` 

The Mean and STD data is extracted and the activity key is translated to the activity name.
```{r} 
##extract required data
##get only mean and sd 
name_v <- names(full_set)
final_set <- subset(full_set, select=grepl("mean\\(\\)", name_v)|grepl("std\\(\\)", name_v)|grepl("set_type", name_v)|grepl("subject_id", name_v)|grepl("activity_id", name_v)  )

##Translate key to activity type
final_set_merged<-merge(final_set,activity_labels,by='activity_id',all.x=TRUE)
```

The data set is melted into Key : Value pairs 
```{r}
##Melt into key:value pairs
##completes
##TIDY DATA SET
final_melted <-melt(final_set_merged,id.vars=c("subject_id", "activity_id","set_type", "activity_type"))


```
 
The data could be tidied further by splitting out the measurement components into their own columns. This doesnt appear to be required for this assignment, but an example is below
```{r}
## We could take this further by parsing out variables from variable name
## t=time,f=freq
##source = "Body',"Gravity"
##sensor = "Acc","Gyro"
##coordinate = "X","Y","Z"
##measure_type = "MEAN","STD"

##final_melted %>% separate(variable, into=c("measure", "coordinate"),"\\(\\)-")%>% separate(measure, into=c("variable", "measure_type"),"-")
```

The final step creates a second, independent tidy data set with the average of each variable for each activity and each subject.
```{r}
##5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
summary_by_subject_activity <-summarize(group_by(final_melted,subject_id, activity_type, variable), mean = mean(value), st_deviation = sd(value))
```
