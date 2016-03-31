# We have chosen and assessed the accuracy of the final model.
# We will now train it on the entire data set, and use it to
# predict the outcome for the 20 homework examples.

library(caret)
library(randomForest)
library(dplyr)

# Load the data
source('0_get_data.R')
source('1_load_and_clean.R')

set.seed(1468)

# Remove missing variables
missing        <- is.na(all_data)
keep_columns   <- names(which(colSums(missing) == 0))
all_processed  <- all_data[, keep_columns]
test_processed <- test_data[, setdiff(keep_columns, "classe")]

# Fit the model
# =============
model <- randomForest(classe ~ ., data = all_processed)

# Make predictions
# ================================
predictions <- predict(model, newdata = test_processed)

# Write the predictions to the output files
# =========================================
answers <- as.character(predictions)

# Provided code from the instructions (slightly modified to write to folder)
pml_write_files = function(x, folder_name = "answers"){
  if(!file.exists(folder_name)) {
    dir.create(folder_name)
  }

  n = length(x)
  for(i in 1:n){
    filename = paste0(folder_name, "/problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(answers)
