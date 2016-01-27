##load libraries
library (data.table)
library ("tidyr")
library("dplyr")
## Set fixed width vector
a<- rep(16,561)

##read data files
test_set <- fread("./UCI HAR Dataset/test/X_test.txt", header = FALSE,  stringsAsFactors =FALSE)
training_set <- fread("./UCI HAR Dataset/train/X_train.txt", header = FALSE,  stringsAsFactors =FALSE)




##read and assign col headers
header_names <-fread("./UCI HAR Dataset/features.txt", header = FALSE,  stringsAsFactors =FALSE)
name_v <- header_names$V2
##Clean Up Names
name_v <-gsub("BodyBody","Body",name_v)

##Set clean Names
names (test_set)<- name_v
names (training_set)<- name_v

##read subjects
subject_train <- fread("./UCI HAR Dataset/train/subject_train.txt", header = FALSE,  stringsAsFactors =FALSE)
subject_test <- fread("./UCI HAR Dataset/test/subject_test.txt", header = FALSE,  stringsAsFactors =FALSE)

names(subject_train) <- ("subject_id")
names(subject_test) <- ("subject_id")

##Read Activity data
activity_train <- fread("./UCI HAR Dataset/train/y_train.txt", header = FALSE,  stringsAsFactors =FALSE)
activity_test <- fread("./UCI HAR Dataset/test/y_test.txt", header = FALSE,  stringsAsFactors =FALSE)
activity_labels <- fread("./UCI HAR Dataset/activity_labels.txt", header = FALSE,  stringsAsFactors =FALSE)
names(activity_train) <- ("activity_id")
names(activity_test) <- ("activity_id")
names(activity_labels) <- c("activity_id", "activity_type")

##Set activity and subject data in data sets
test_set2 <- cbind(subject_test, activity_test , test_set)
training_set2 <- cbind(subject_train, activity_train , training_set)

##Add column to identify original set
test_set2$set_type = "TEST"
training_set2$set_type = "TRAINING"      
      
      

##combine into single data.table
full_set <- rbind(test_set2, training_set2)

##extract required data
##get only mean and sd 
name_v <- names(full_set)
final_set <- subset(full_set, select=grepl("mean\\(\\)", name_v)|grepl("std\\(\\)", name_v)|grepl("set_type", name_v)|grepl("subject_id", name_v)|grepl("activity_id", name_v)  )

##Translate key to activity type
final_set_merged<-merge(final_set,activity_labels,by='activity_id',all.x=TRUE)

##Melt into key:value pairs
##completes
##TIDY DATA SET
final_melted <-melt(final_set_merged,id.vars=c("subject_id", "activity_id","set_type", "activity_type"))

## We could take this further by parsing out variables from variable name
## t=time,f=freq
##source = "Body',"Gravity"
##sensor = "Acc","Gyro"
##coordinate = "X","Y","Z"
##measure_type = "MEAN","STD"

##final_melted %>% separate(variable, into=c("measure", "coordinate"),"\\(\\)-")%>% separate(measure, into=c("variable", "measure_type"),"-")

##5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
summary_by_subject_activity <-summarize(group_by(final_melted,subject_id, activity_type, variable), mean = mean(value), st_deviation = sd(value))
write.table(summary_by_subject_activity,"summary_by_subject_activity.txt", row.names = FALSE)