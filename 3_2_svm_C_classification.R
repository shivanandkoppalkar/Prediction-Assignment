# Evaluation of algorithm number 2
# SVM (with default tuning parameters)
# Preprocessing = throw away features with missing values
#                 + use svm()'s built-in scaling
library(caret)
library(e1071)
library(dplyr)

source('0_get_data.R')
source('1_load_and_clean.R')

set.seed(411)

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
    model <- svm(classe ~ ., data = train_subset)

    # Record which guesses are correct
    # ================================
    predictions <- predict(model, newdata = test_subset)
    correct[folds == i] <- (predictions == test_subset$classe)
  }
  # Accuracy for the j-th iteration
  accuracy[j] <- mean(correct)
}


# accuracy:
# [1] 0.9311311 0.9313010 0.9300272 0.9298573 0.9314708
# mean(accuracy)
# [1] 0.9307575
# sd(accuracy)
# [1] 0.0007562037
