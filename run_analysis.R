library(tidyverse)

#### Getting the data ####

      #general
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")
features <- read.table("UCI HAR Dataset/features.txt")

      # train data
train_x <- read.table("UCI HAR Dataset/train/x_train.txt")
train_y <- read.table("UCI HAR Dataset/train/y_train.txt")
train_subject <- read.table("UCI HAR Dataset/train/subject_train.txt")

      # test data
test_x <- read.table("UCI HAR Dataset/test/x_test.txt")
test_y <- read.table("UCI HAR Dataset/test/y_test.txt")
test_subject <- read.table("UCI HAR Dataset/test/subject_test.txt")



#### 1. Merging data ####

all_train <- cbind(train_subject,train_x,train_y)
all_test <- cbind(test_subject,test_x,test_y)
 
all_data <- rbind(all_train,all_test)


#### 2. Extracts only the measurements on the mean and standard deviation for each measurement #####
feat <- features$V2
colnames(all_data) <- c("Subjects",feat,"Activity")


      # Select only the mean and std measurements for each measure 
all_data_mean_std <- all_data %>% select(c("Subjects","Activity",feat[grepl(feat,pattern = "mean|std")]))
      # Avoid the meanFreq() measure
all_data_mean_std <- all_data_mean_std %>% select(-(grep(names(all_data_mean_std),pattern = "meanFreq")))



##### 3. Uses descriptive activity names to name the activities in the data set #####
all_data_mean_std$Activity<- as.factor(all_data_mean_std$Activity)
all_data_mean_std$Activity <- fct_collapse(all_data_mean_std$Activity,"Walking"="1","Walking_upstairs"="2",
                           "Walking_downstairs"="3","Sitting"="4",
                           "Standing"="5","Laying"="6")


##### 4.Appropriately labels the data set with descriptive variable names #####
names_modify <- sub(names(all_data_mean_std),pattern = "Acc",replacement = "_Acceleration_")
names_modify <- sub(names_modify,pattern = "Gyro",replacement = "_AngVelocity_")
names_modify <- sub(names_modify,pattern = "^t",replacement = "TimeDomain_")
names_modify <- sub(names_modify,pattern = "^f",replacement = "FrequencyDomain_")
names_modify <- sub(names_modify,pattern = "BodyBody",replacement = "Body")
names_modify <- sub(names_modify,pattern = "-X",replacement = "_Axis_X")
names_modify <- sub(names_modify,pattern = "-Y",replacement = "_Axis_Y")
names_modify <- sub(names_modify,pattern = "-Z",replacement = "_Axis_Z")


colnames(all_data_mean_std) <- names_modify

##### 5. creates a second, independent tidy data set #####
### with the average of each variable for each activity and each subject. 

all_data_mean_std <- all_data_mean_std %>% arrange("Subjects")
New_data_set <- all_data_mean_std %>% group_by(Subjects,Activity) %>% summarise_all(mean)
view(New_data_set)


write.table(New_data_set, row.names = FALSE,file = "submission_dataSet.txt") 

#
table <- read.table("submission_dataSet.txt",header = T)
