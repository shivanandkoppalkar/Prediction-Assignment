# Given the CV tests on the training set, we conclude that the
# best model from the ones compared is random forest, fit in the
# following way:
# * Throw away all variables with _any_ missing values.
# * Do not do any rescaling, or rotation.
# * Fit randomForest() on the remaining variables, with default parameters.

# In this script we fit the model to the entire training set, and get a final
# estimate on its performance, by testing on the held out (40%) validation set.
library(caret)
library(randomForest)
library(dplyr)

# Load the data
source('0_get_data.R')
source('1_load_and_clean.R')

set.seed(23756)

# Remove missing variables
missing              <- is.na(train_set)
keep_columns         <- names(which(colSums(missing) == 0))
train_processed      <- train_set[, keep_columns]
validation_processed <- validation_set[, keep_columns]

# Fit the model
# =============
model <- randomForest(classe ~ ., data = train_processed)

# Record which guesses are correct
# ================================
predictions <- predict(model, newdata = validation_processed)
cm <- confusionMatrix(data = predictions, reference = validation_processed$classe)

# cm
# Confusion Matrix and Statistics
#
# Reference
# Prediction    A    B    C    D    E
# A 2232    0    0    0    0
# B    0 1518    3    0    0
# C    0    0 1361    1    0
# D    0    0    4 1285    0
# E    0    0    0    0 1442
#
# Overall Statistics
#
# Accuracy : 0.999
# 95% CI : (0.998, 0.9996)
# No Information Rate : 0.2845
# P-Value [Acc > NIR] : < 2.2e-16
#
# Kappa : 0.9987
# Mcnemar's Test P-Value : NA
#
# Statistics by Class:
#
#                      Class: A Class: B Class: C Class: D Class: E
# Sensitivity            1.0000   1.0000   0.9949   0.9992   1.0000
# Specificity            1.0000   0.9995   0.9998   0.9994   1.0000
# Pos Pred Value         1.0000   0.9980   0.9993   0.9969   1.0000
# Neg Pred Value         1.0000   1.0000   0.9989   0.9998   1.0000
# Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
# Detection Rate         0.2845   0.1935   0.1735   0.1638   0.1838
# Detection Prevalence   0.2845   0.1939   0.1736   0.1643   0.1838
# Balanced Accuracy      1.0000   0.9998   0.9974   0.9993   1.0000

accuracy <- mean(predictions == validation_processed$classe)

# accuracy
# [1] 0.9989804
