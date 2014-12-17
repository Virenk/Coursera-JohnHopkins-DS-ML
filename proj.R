#set working directory
setwd("H:/Coursera/jh/proj-ml")

#read the csv file
dat <- read.csv("pml-training.csv")


# count the no of NA's present in every column
#stored the counts of na entries for every variable in data frame
count.na <- data.frame()
cols <- colnames(dat)
for (i in 1:length(cols)) 
{
  count.na[i, 1] = cols[i]
  count.na[i, 2] = sum(is.na(dat[,i]))
}
colnames(count.na) <- c("Var", "nacount")
######################

# will ignore those variables which have more than 10% missing data
count.na <- count.na[count.na$nacount < 0.1*length(cols),]

# get the subset for onyl those who have very less missing na's
dat <- subset(dat, select = c(count.na$Var))

# drop the variables which doesn't make any sense for our analysis
dat <- subset(dat, select= -c(X, user_name, raw_timestamp_part_1,raw_timestamp_part_2, 
                                          cvtd_timestamp, new_window, num_window, kurtosis_yaw_belt, skewness_yaw_belt, 
                                          amplitude_yaw_belt, kurtosis_yaw_forearm, skewness_yaw_forearm, amplitude_yaw_forearm, 
                                          kurtosis_yaw_dumbbell, skewness_yaw_dumbbell, amplitude_yaw_dumbbell,
                                          kurtosis_roll_belt, kurtosis_picth_belt, skewness_roll_belt, skewness_roll_belt.1,
                                          max_yaw_belt, min_yaw_belt, kurtosis_roll_arm, kurtosis_picth_arm, kurtosis_yaw_arm,
                                          skewness_roll_arm, skewness_pitch_arm, skewness_yaw_arm, kurtosis_roll_dumbbell, kurtosis_picth_dumbbell,
                                          skewness_roll_dumbbell, skewness_pitch_dumbbell, max_yaw_dumbbell, min_yaw_dumbbell,
                                          kurtosis_roll_forearm, kurtosis_picth_forearm, skewness_roll_forearm, skewness_pitch_forearm,
                                          max_yaw_forearm, min_yaw_forearm))

##############

# train random forest model classe as predicted variable

inTrain <- createDataPartition(y = dat$classe, p = 0.8, list=FALSE)
training <- dat[inTrain,]
testing <- dat[-inTrain,]

dim(training)
dim(testing)
rfFit <- randomForest(classe ~ .,data = training)

pred <- predict(rfFit, testing)
testing$predictRight <- pred ==testing$classe
confMatrix <- table(pred, testing$classe)
class.error <- diag(apply(confMatrix, 1, function(x) { (sum(x) - x)/sum(x)}))
confMatrix <- cbind(confMatrix, class.error)

##############
rfFit$confusion # prints the insample error
confMatrix      # prints the error for test data


### out of sample test data
testData <- read.csv("pml-testing.csv")

test <- testData
# count the no of NA's present in every column
#stored the counts of na entries for every variable in data frame
count.na.t <- data.frame()
cols.t <- colnames(testData)
for (i in 1:length(cols.t)) 
{
  count.na.t[i, 1] = cols.t[i]
  count.na.t[i, 2] = sum(is.na(testData[,i]))
}
colnames(count.na.t) <- c("Var", "nacount")
######################

# will ignore those variables which have more than 10% missing data
count.na.t <- count.na.t[count.na.t$nacount < 0.1*length(cols.t),]

# get the subset for onyl those who have very less missing na's
testData <- subset(testData, select = c(count.na.t$Var))

# drop the variables which doesn't make any sense for our analysis
testData <- subset(testData, select= -c(X, user_name, raw_timestamp_part_1,raw_timestamp_part_2, 
                              cvtd_timestamp, new_window, num_window))
          
testPredict <- predict(rfFit, testData)

testData <- cbind(test, testPredict)

write.csv(testData, "output.csv")