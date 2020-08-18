---
title: "Prediction Assignment Writeup"
author: "Silvio J de Souza Jr"
date: "17/08/2020"
output: html_document
---
## Overview

The goal of this project is to predict the manner in which they did the exercise. This is the "classe" variable in the training set. We may use any of the other variables to predict with. We will describe how the model was built, how we used cross validation and what is the expected out of sample error. We will also use the prediction model to predict 20 different test cases. 

## Data Processing 

```{r, include=F}
library(knitr)
library(caret)
library(rpart)
library(rpart.plot)
library(rattle)
library(randomForest)
set.seed(1309)
```

### Download Data

```{r}
training_url <- 'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv'
testing_url <- 'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv'

if (!file.exists('pml-training.csv')){
      download.file(url = training_url, destfile = 'pml-training.csv')
}

if (!file.exists('pml-testing.csv')){
      download.file(url = testing_url, destfile = 'pml-testing.csv')
}
```

### Cleaning Data

First we need to remove predictors that have one unique value (i.e. are zero variance predictors) or predictors that are have both of the following characteristics: they have very few unique values relative to the number of samples and the ratio of the frequency of the most common value to the frequency of the second most common value is large.  
Second, the NAs variables was removed and the first identification variables.

```{r}
training <- read.csv('pml-training.csv')
testing <- read.csv('pml-testing.csv')

training <- training[,-nearZeroVar(training)]
testing <- testing[,-nearZeroVar(testing)]

# Remove NAs
training <- training[,colSums(is.na(training)) == 0]
testing <- testing[,colSums(is.na(testing)) == 0]

# Remove identification variables
training <- training[,-(1:5)]
testing <- testing[,-(1:5)]
```

### Dataset Partitioning

We will split the training data into training and validation partitions and use the  as a validation sample. I’ll use cross validation within the training partition to improve the model fit and then do an out-of-sample test with the testing partition.

```{r}
inTrain <- createDataPartition(training$classe, p=0.7, list=F)

training_sets <- training[inTrain,]
testing_sets <- training[-inTrain,]
```

## Prediction Model

Three methods will be applied to model the regressions and the model with higher accuracy will be used for the quiz predictions.   
The methods are: **Random Forests**, **Decision Tree** and **Generalized Boosted Model**. A Confusion Matrix is plotted at the end of each analysis to better visualize the accuracy of the models.

### Random Forest
```{r, cache=T}
model_rf <- randomForest(classe ~., data=training_sets, method='class')
```

```{r}
predict_rf <- predict(model_rf, testing_sets)
confusionMatrix(predict_rf, testing_sets$classe)
```

### Generalized Boosted Model (GBM)

```{r, cache=T}
fitControl <- trainControl(method="repeatedcv", number=5, repeats=1)
model_gbm <- train(classe ~., method='gbm', data=training_sets, verbose=F, trControl=fitControl)
```

```{r}
predict_gbm <- predict(model_gbm, testing_sets)
confusionMatrix(predict_gbm, testing_sets$classe)
```

### Decision Tree
```{r, cache=T}
model_dt <- rpart(classe ~., method='class', data=training_sets)
```

```{r}
predict_dt <- predict(model_dt, testing_sets, type='class')
confusionMatrix(predict_dt, testing_sets$classe)
```
## Testing the Selected Model

The accuracy of the 3 models are:

* Random Forest: 0.9975
* GBM: 0.989
* Decision Tree: 0.7327

So we'll use the random forest model to predict de final quiz test:
```{r}
predict_test <- predict(model_rf, testing)
predict_test
```