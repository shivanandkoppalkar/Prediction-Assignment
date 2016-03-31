# Libraries
library(caret)
library(dplyr)
library(lubridate)

# First we visually inspect the file, and notice it has a header line
# specifying variable names. In addition the first (unnamed) column is
# simply the row number.

# Load all data, naively (producing many factors) to capture
# all possible missing/NA values. We will only study the
# test set, so as not to let any of the training data leak in.
all_data_pre <- read.csv(training_data_file, row.names = 1)

# Separate into training and validation sets.
train_percentage <- .6

set.seed(3251)
train_index <- createDataPartition(y = all_data_pre$classe,
                                   p = train_percentage,
                                   list = FALSE)
train_set_pre <- all_data_pre[train_index, ]

# Explore the types and values of variables, for cleaning
# =======================================================
# Get variable types
all_variables <- vapply(train_set_pre, class, character(1))

# All types
# =========
unique(all_variables)

# We see that only numeric(some integer) and factor variables were detected
# We now try to discover which of these are actually numeric with missing
# values, and what are the actual types of the others.
factor_variables <- names(all_variables[all_variables == "factor"])
na_analysis <- lapply(factor_variables,
                      function(var_name) {
                        var_values  <- levels(train_set_pre[[var_name]])
                        na_index    <- is.na(as.numeric(var_values))
                        # Which factor levels can't convert to numeric?
                        na_values   <- var_values[na_index]
                        # Were there any levels that DID convert to numeric?
                        any_numeric <- !all(na_index)
                        list(na_values   = na_values,
                             any_numeric = any_numeric)
                      })
names(na_analysis) <- factor_variables
# Unpack the results
any_numeric <- vapply(na_analysis, function(x) x$any_numeric, logical(1))
na_values   <- lapply(na_analysis, function(x) x$na_values)

# =============================================
# NA levels for the secretely numeric variables
numeric_na_values <- unique(unlist(na_values[any_numeric]))

# print(numeric_na_values)
# We get "" and "#DIV/0!"

# =============================================
# Which variables had no numeric values at all?
non_numeric_variables <- factor_variables[!any_numeric]
# print(na_values[non_numeric_variables])

# We see that for several of these the values are
# precisely the numeric NA values. Therefore these are actually numeric
# variables where all data is missing.
numeric_missing_index <- vapply(non_numeric_variables,
                                function(var_name) {
                                  all(na_values[[var_name]] %in% numeric_na_values)
                                },
                                logical(1))
numeric_missing_variables <- non_numeric_variables[numeric_missing_index]

# Actual not-numeric variables
non_numeric_variables <- non_numeric_variables[!numeric_missing_index]

# ==========================================================================
# What are the values for these variables
# print(na_values[non_numeric_variables])

# We see the following:
# * user_name contains a string (6 possible values)
# * cvtd_timestamp contains date/time in format "DD/MM/YYYY HH:MM"
#   (24 hour time)
# * new_window contains has two factors "yes" and "no"
# * classe (the outcome variable) has five values "A", "B", "C", "D", "E"

# ========================================================================
# With the above information in hand we can reload the data with the
# correct variable types

# We assemble everything into a function, so that we can treat both
# the testing and training files exactly the same.
load_data_func <-
  function(filename) {
    # Read the file with correct variable types. We don use colClasses,
    # since apparently it gets really confused by quoted numbers.
    # Instead, we explicitly specify all possible NA values, so that
    # the read.csv will figure out that those are actually numbers.
    # In addition we treat all other variables as strings for now
    # and convert them later.
    res <- read.csv(file             = filename,
                    row.names        = 1,
                    na.strings       = c("NA", numeric_na_values),
                    stringsAsFactors = FALSE)

    # Conert the timestamp to the correct format, and
    # explicitly define the levels (and order) for the factors, so that
    # we get the exact same results on the training and test set.
    classe_levels     <- levels(train_set_pre$classe)
    new_window_levels <- levels(train_set_pre$new_window)
    user_name_levels  <- levels(train_set_pre$user_name)

    res <- mutate(res,
                  cvtd_timestamp = mdy_hm(cvtd_timestamp),
                  new_window     = factor(new_window, levels = new_window_levels),
                  user_name      = factor(user_name, levels = user_name_levels))
    # The testing set has no outcome labels.
    if ("classe" %in% names(res)) {
      res <- mutate(res,
                    classe = factor(classe, levels = classe_levels))
    }
    
    # Note that any variables with only missing values will now be set
    # as type 'logical'. Convert them to 'numeric'.
    for (varname in names(res)) {
      if (class(res[[varname]]) == "logical") {
        res[[varname]] <- as.numeric(res[[varname]])
      }
    }
    res
  }

all_data  <- load_data_func(training_data_file)
test_data <- load_data_func(testing_data_file)

# Make sure to use the same training/validation set division
train_set      <- all_data[train_index, ]
validation_set <- all_data[-train_index, ]


