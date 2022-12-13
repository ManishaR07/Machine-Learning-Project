#installing packages

install.packages("randomForest")
install.packages("rpart")
install.packages("e1071")

library(caret)
library(randomForest)
library(rpart)
library(e1071)
library(rattle)

link_train <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
link_test <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

download.file(link_train,destfile = "C:/Users/drpra/Desktop/Home Stuff/Coursera_Data_Science/Machine-Learning-Project/train.csv", method = "curl")
download.file(link_test,destfile =  "C:/Users/drpra/Desktop/Home Stuff/Coursera_Data_Science/Machine-Learning-Project/test.csv", method = "curl")

train_df <- read.csv("train.csv")
test_df <- read.csv("test.csv")
colSums(is.na(train_df))

train_df[train_df == ""] <- NA
test_df[test_df == ""] <- NA

work_traindf <- train_df

#Removing columns with more than 50% of missing data

for(i in 1:length(train_df)){
  if((sum(is.na(train_df[,i])))/nrow(train_df) >= .5){
    for(j in 1:length(train_df)){
      if(length( grep(names(train_df)[i], names(work_traindf)[j])) == 1)
        work_traindf[,j] <- NULL
    }
    
  }
}

train_df <- work_traindf

work_testdf <-test_df

for(i in 1:length(test_df)){
  if((sum(is.na(test_df[,i])))/nrow(test_df) >= .5){
    for(j in 1:length(test_df)){
      if(length( grep(names(test_df)[i], names(work_testdf)[j])) == 1)
        work_testdf[,j] <- NULL
    }
    
  }
}

test_df <- work_testdf

##Check if any null values
colSums(is.na(train_df))
colSums(is.na(test_df))

##Preprocessing data
preprocess_traindf <- preProcess(train_df,method= c("center","scale"))
predData <- predict(preprocess_traindf,newdata =train_df)

##Partitioning the training set into train and test
inTrain <- createDataPartition(y=train_df$classe, p=0.6, list=FALSE)
Train_traindf <- train_df[inTrain, ]; Test_traindf <- train_df[-inTrain, ]
dim(Train_traindf); dim(Test_traindf)

#Removing ID column of dataset
Train_traindf <- Train_traindf[c(-1)]
Test_traindf <- Test_traindf[c(-1)]

Train_traindf$classe <- as.factor(Train_traindf$classe)
Test_traindf$classe <- as.factor(Test_traindf$classe)

#Using Decision Trees for prediction
model1 <- rpart(classe~.,data=Train_traindf,method="class")
fancyRpartPlot(model1)

#Predicting
predData1 <- predict(model1,Test_traindf,type="class")

#confusion Matrix
confusionMatrix(predData1,Test_traindf$classe)

##Using Random Forest for prediction

model2<- randomForest(classe~.,data=Train_traindf)

#Predicting in sample error
predData2 <- predict(model2,Test_traindf,type="class")

#confusion Matrix
confusionMatrix(predData2,Test_traindf$classe)


#Cleaning test data and train data similar to Train data
test_df <- test_df[c(-1)]

##Testing on Test dataset
predDataFinal <- predict(model2,test_df,type="class")
 
predDataFinal