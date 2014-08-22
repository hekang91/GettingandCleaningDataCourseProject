## Getting and Cleaning Data Course Project

## Merges the training and the test sets 
## to create one data set. (step 1)
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")

X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")

X_merged <- rbind(X_test,X_train)
y_merged <- rbind(y_test,y_train)
subject_merged <- rbind(subject_test,subject_train)

## for debugging
#X_merged <- X_test
#y_merged <- y_test
#subject_merged <- subject_test


## Extracts only the measurements 
## on the mean and standard deviation 
## for each measurement. (step 2)
features <- read.table("./UCI HAR Dataset/features.txt")
names(features) <- c("id_feature","name_feature")
id_mean_or_std <- c(agrep("mean",features[,"name_feature"]), agrep("std",features[,"name_feature"]))
id_mean_or_std <- sort(id_mean_or_std)
X_merged <- X_merged[,id_mean_or_std]


## Prepares to Use descriptive activity names 
## to name the activities in the data set.
## (Part of step 3)
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
names(activity_labels) <- c("id_activity","name_activity")


## Appropriately labels the data set 
## with descriptive variable names. (step 4) 
names(X_merged) <- as.character(features[id_mean_or_std,"name_feature"])


## Creates a second, independent tidy data set 
## with the average of each variable 
## for each activity and each subject. (step 5)

n_subject <- max(subject_merged)
n_activity <- max(y_merged)
n_variable <- length(id_mean_or_std)
n_row <- n_subject*n_activity
n_column <- 2 + n_variable # add columns indicating the subject and activity index

tidy_vector <- rep(NA,n_row*n_column)
tidy_matrix <- matrix(tidy_vector,n_row,n_column)
tidy_data <- data.frame(tidy_matrix)
names(tidy_data) <- c("subject","activity",as.character(features[id_mean_or_std,"name_feature"]))

i_row <- 0
for (i_subject in 1:n_subject) {
  for (i_activity in 1:n_activity) {
    i_row <- i_row + 1
    tidy_data[i_row,"subject"] <- i_subject    
    
    ## actually this is step 3
    tidy_data[i_row,"activity"] <- as.character(activity_labels[i_activity,"name_activity"]) 
    
    index_row <- subject_merged==i_subject & y_merged==i_activity   
    tidy_data[i_row,-1:-2] <- colMeans(X_merged[index_row,])
  }
}

write.table(tidy_data,file = "./tidy_data.txt",row.name=FALSE)