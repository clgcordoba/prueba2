---
title: "PRACTICAL MACHINE LEARNING-ASSIGNMENT"
author: "CARMEN LUQUE"
date: "Saturday, October 18, 2014"
output:
  html_document:
    fig_caption: yes
    keep_md: yes
    theme: journal
    toc: yes
---

##**ABSTRACT**

The goal of this assignment is to predict the manner in which six participants did the exercise. The data for this project come from the source  <http://groupware.les.inf.puc-rio.br/har> (Weight Lifting Exercises Dataset). The activity-quality classes are categorized in 5 different ways: A, B, C, D y E. we will have to build a model using the random forest method and cross validation. Then apply the final model to predict the 20 test cases that not classified.


##**LIBRARIES**

First we load the necessary libraries to run our task. 
```{r}
library(corrplot)
library(caret)
library(randomForest)
library(kernlab)
library(e1071)
```
##**DOWNLOAD FILES**
<br/>
We download the files in the selected directory. For it will indicate the url addresses of the files and the directory on the local computer where you want to save. Once the download of the files we will proceed to read the csv file for training
```{r}
download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv","/users/xxx/downloads/pml-testing.csv")
download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv","/users/xxx/downloads/pml-training.csv")
data_training <- read.csv ("/Users/xxx/Downloads/pml-training.csv",header=TRUE,na.strings=c("NA",""," "))
```
##**DATA: CLEANING AND PROCESSING**
<br/>

1.- CLEANING DATA
<br/>
Before processing the information is necessary to clean the data. To do this we will remove the columns that may generate noise in the output, such as columns with data columns blank or with unnecessary information such as name, timestamp, etc.
```{r}
data_training_NAs <- apply(data_training, 2, function(x) {sum(is.na(x))})
data_training_clean <- subset(data_training[,which(data_training_NAs == 0)],select=-c(X,user_name, raw_timestamp_part_1, raw_timestamp_part_2, cvtd_timestamp , new_window, num_window))
```
<br/>
2.- SPLIT DATA
<br/>
Now we split the clean data into a training dataset ("training") and a validation dataset ("testing"). The particion is 70/30.

```{r}
inTrain <- createDataPartition(y = data_training_clean$classe, p = 0.7, list = FALSE)
training <- data_training_clean[inTrain, ]
testing <- data_training_clean[-inTrain, ]
```
<br/>
3.- CORRELATION MATRIX
To study the correlation between variables we perform the representation of the correlation matrix
<br/>
```{r}
correlMatrix <- cor(training[, -length(training)])
corrplot(correlMatrix, order = "FPC", method = "square", type = "full", tl.cex = 0.7)
```
<br/>
4.- RANDOM FOREST and CROSS-VALIDATION. CONFUSION MATRIX
<br/>
Now we will build the training model using the random forest method and applying a cross-validation (n = 4).
We then generate the confusion matrix to determine the effectiveness of the generated model.Each column of the matrix represents the instances in a predicted class, while each row represents the instances in an actual class.
<br/>

```{r}
ctrl <-trainControl(method="cv",number=4)
model <- train(classe ~.,data=training, model="rf",trControl=ctrl)
predictCrossVal <- predict(model,newdata=testing)
mconfusion <- confusionMatrix(testing$classe,predictCrossVal)
mconfusion$table
salida_accu <-print(mconfusion$overall)
```
<br/>
5.- ACCURACY
<br/>
The accuracy is the proportion of true results (both true positives and true negatives). The estimated accuracy of this model is 99.25%.
<br/>
```{r}
accu <- salida_accu[[1]]
accu
```
<br/>
6.- OUT-OF-SAMPLE ERROR
<br/>
For out-of-sample errors subtract 1 minus the value of the accuracy. The estimated out-of-sample error obtained is 0.74%.
<br/>

```{r}
out_of_sample <- 1-accu
out_of_sample
```
##**PREDICTED RESULTS**
<br/>
Having overcome the previous phases, will now apply the same initial steps of data cleaning to the DataTest. After cleaning unnecessary data, we applied our model execute against testing dataset and obtain results of prediction for the 20 proposed cases.
<br/>

```{r}
data_test <- read.csv("/users/xxx/downloads/pml-testing.csv",header=TRUE,na.strings=c("NA",""," "))
data_test_NAs <- apply(data_test, 2, function(x) {sum(is.na(x))})
data_test_clean <- data_test[,which(data_test_NAs == 0)]
data_test_clean <- subset(data_test[,which(data_test_NAs == 0)],select=-c(X,user_name, raw_timestamp_part_1, raw_timestamp_part_2, cvtd_timestamp , new_window, num_window))
predictTest <- predict(model, data_test_clean)
predictTest
```
<br/>
Through this function we will obtain 20 text file with the contents of the prediction for each case.

```{r}
pml_write_files = function(x){  
  n = length(x)
  for(i in 1:n){	
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(predictTest)

```


