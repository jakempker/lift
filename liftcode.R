setwd("C:/Users/jkempke/Box Sync/Coursera/lift")

download.file(url="https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",
	destfile = "./pml-training.csv")

download.file(url="https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",
	destfile = "./pml-testing.csv")

library(caret)
library(dplyr)

training<-read.csv("./pml-training.csv")
testing<-read.csv("./pml-testing.csv")


#One of the first things I noticed about the test set is that there are many columns
#with all data missing.  There seems to be no point building a model to apply to 
#no data so I am going to remove these varaibles.

#create a logical which =TRUE if data is missing for all observations of variable
missing<-sapply(testing, function(x)all(is.na(x)))
#get the column names where all data are missing, using above logical and save to 
#character vector
MissingVariables <- colnames(testing[missing==TRUE])
#select only variables that have data using the "one_of" helper of dplyr::select
testing <- select(testing, -one_of(MissingVariables))
#apply this to training set so that we are only working with variables that we will
#encounter in the true test set.
training <- select(training,-one_of(MissingVariables))

#I am dropping time and identifiers since I am not sure how to use them inteligently
#to incorporate them as predictors.
training <- select(training, 
	-X,
	- user_name, 
	-raw_timestamp_part_1, 
	-raw_timestamp_part_2,
	-cvtd_timestamp, 
	-new_window,
	-num_window)
testing <- select(testing,
	-X,
	- user_name, 
	-raw_timestamp_part_1, 
	-raw_timestamp_part_2,
	-cvtd_timestamp, 
	-new_window,
	-num_window)

#Create a data partition of our set for training and testing.
inTrain <- createDataPartition(y=training$classe, p=0.75, list=FALSE)
train <- training[inTrain,]
test <- training[-inTrain,]

rpart.model <- train(classe ~ ., data=train, method='rpart')
rf.model <- train(classe ~ ., data=train, method='rf')  #random forest
gbm.model <- train(classe ~ ., data=train, method='gbm')#boosting
lda.model <- train(classe ~ ., data=train, method='lda')#linear discriminant analysis


rpart.predict <- predict(rpart.model, test)
#rf.predict    <- predict(rf.model, test) #tried this but took too long to run!
gbm.predict   <- predict(gbm.model, test)
lda.predict   <- predict(lda.model, test)


confusionMatrix(rpart.predict, test$classe)
confusionMatrix(gbm.predict, test$classe)
confusionMatrix(lda.predict, test$classe)


confusionMatrix(rpart.predict, test$classe)$overall["Accuracy"]
#confusionMatrix(rf.predict, test$classe)$overall["Accuracy"]
confusionMatrix(gbm.predict, test$classe)$overall["Accuracy"]
confusionMatrix(lda.predict, test$classe)$overall["Accuracy"]

library(knitr)

knit2html("lift.Rmd")
