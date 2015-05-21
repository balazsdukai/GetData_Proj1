# You should create one R script called run_analysis.R that does the following.
# 1) Merges the training and the test sets to create one data set.
# 2) Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3) Uses descriptive activity names to name the activities in the data set
# 4) Appropriately labels the data set with descriptive variable names. 
# 5) From the data set in step 4, creates a second, independent tidy data set 
#     with the average of each variable for each activity and each subject.

# Load the packages
library(dplyr)

# Read in the data
con <- "UCI HAR Dataset/train/X_train.txt"
train_x <- as.tbl(read.table(con, header = F))
format(object.size(train_x), units = "Mb")

con <- "UCI HAR Dataset/train/y_train.txt"
train_y <- as.tbl(read.table(con, header = F))

con <- "UCI HAR Dataset/train/subject_train.txt"
train_subject <- as.tbl(read.table(con, header = F))

con <- "UCI HAR Dataset/test/X_test.txt"
test_x <- as.tbl(read.table(con, header = F))

con <- "UCI HAR Dataset/test/y_test.txt"
test_y <- as.tbl(read.table(con, header = F))

con <- "UCI HAR Dataset/test/subject_test.txt"
test_subject <- as.tbl(read.table(con, header = F))

con <- "UCI HAR Dataset/features.txt"
features <- as.tbl(read.table(con, header = F))

# con <- "UCI HAR Dataset/activity_labels.txt"
# labels <- as.tbl(read.table(con, header = F))
# I don't really need this, as I've assigned the activity labels manually.

# Reshape the data
train <- as.tbl(bind_cols(train_subject, train_y, train_x))
names(train)[1:2] <- c("id", "activity")
test <- as.tbl(bind_cols(test_subject, test_y, test_x))
names(test)[1:2] <- c("id", "activity")

# 1) Merges the training and the test sets to create one data set.
meas <- bind_rows(train, test)

# 4) Appropriately labels the data set with descriptive variable names.
valid_column_names <- make.names(names=as.character(features$V2), unique=TRUE, allow_ = TRUE)
names(meas)[3:563] <- valid_column_names
# for explanation check:
# http://stackoverflow.com/questions/28549045/dplyr-select-error-found-duplicated-column-name

# clean up
rm(list=setdiff(ls(), c("meas")))

# 2) Extracts only the measurements on the mean and standard deviation for each measurement.
meas_subset <- select(meas, id, activity, contains("mean"), contains("std"))

# 3) Uses descriptive activity names to name the activities in the data set
meas_subset$activity[meas_subset$activity == 1] <- "WALKING"
meas_subset$activity[meas_subset$activity == 2] <- "WALKING_UPSTAIRS"
meas_subset$activity[meas_subset$activity == 3] <- "WALKING_DOWNSTAIRS"
meas_subset$activity[meas_subset$activity == 4] <- "SITTING"
meas_subset$activity[meas_subset$activity == 5] <- "STANDING"
meas_subset$activity[meas_subset$activity == 6] <- "LAYING"
# I couldn't find a more elegant solution to this...

# 5) From the data set in step 4, creates a second, independent tidy data set 
#     with the average of each variable for each activity and each subject.
human_activity <- 
    meas_subset %>%
    group_by(id, activity) %>%
    summarise_each(funs(mean))
print(human_activity)

# export the data
write.table(x = human_activity, file = "human_activity.txt", row.names = FALSE)
