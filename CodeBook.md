The tidy data set includes 180 rows with 68 columns.


Each row corresponds to the average of each variable for each activity and each subject.


Each coloum:
- subject:
  An integer within the range from 1 to 30, indicating the subject's ID.
- activity:
  A character vector, indicating one of the six activities below:
  walking, walking upstairs, walking downstairs, sitting, standing, laying
- 66 variables:
  All of these variables are float, indicating the mean (with the suffix "mean()") or standard deviation (with the suffix "std()") for each measurement below ("-XYZ" is used to denote 3-axial signals in the X, Y and Z directions.):
  tBodyAcc-XYZ
  tGravityAcc-XYZ
  tBodyAccJerk-XYZ
  tBodyGyro-XYZ
  tBodyGyroJerk-XYZ
  tBodyAccMag
  tGravityAccMag
  tBodyAccJerkMag
  tBodyGyroMag
  tBodyGyroJerkMag
  fBodyAcc-XYZ
  fBodyAccJerk-XYZ
  fBodyGyro-XYZ
  fBodyAccMag
  fBodyAccJerkMag
  fBodyGyroMag
  fBodyGyroJerkMag
  