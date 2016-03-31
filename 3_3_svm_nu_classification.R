# Evaluation of algorithm number 3
# SVM (nu-classification)
# Preprocessing = throw away features with missing values
#                 + use svm()'s built-in scaling
library(caret)
library(e1071)
library(dplyr)

source('0_get_data.R')
source('1_load_and_clean.R')

set.seed(3756)

accuracy <- numeric(5)
for (j in 1:5) {
  print(paste0("Repetition ", j))
  # Splits into 10 folds.
  folds <- createFolds(y = train_set$classe, k = 10, list = FALSE)
  correct <- logical(nrow(train_set))
  for (i in 1:10) {
    # Split into training and validation sets
    # =======================================
    train_subset <- train_set[folds != i, ]
    test_subset  <- train_set[folds == i, ]

    # Pre-processing
    # ==============

    # Remove features with missing values
    missing <- is.na(train_subset)
    keep_columns <- names(which(colSums(missing) == 0))
    train_subset <- train_subset[, keep_columns]
    test_subset  <- test_subset[, keep_columns]

    # Fit the model
    # =============
    model <- svm(classe ~ ., data = train_subset, type = "nu-classification")

    # Record which guesses are correct
    # ================================
    predictions <- predict(model, newdata = test_subset)
    correct[folds == i] <- (predictions == test_subset$classe)
  }
  # Accuracy for the j-th iteration
  accuracy[j] <- mean(correct)
}


# accuracy
# [1] 0.8385700 0.8376359 0.8378057 0.8355978 0.8378057
# mean(accuracy)
# [1] 0.837483
# sd(accuracy)
# [1] 0.001114343
#
