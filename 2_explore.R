library(dplyr)
library(tidyr)
library(GGally)

# Get some statistics on the missing values: how many for each column,
# do they occur together, etc.

missing <- is.na(train_set)
missing_rates <- colMeans(missing)

# See the stats
#    print(unique(missing_rates))
# We see that there are very few different values. Meaning, that there seems
# to be a few different conditions causing multiple variables to be missing
# In addition the missing rates are either 0, or very high (over 90%).

any_missing <- names(which(missing_rates > 0))
no_missing  <- names(which(missing_rates == 0))
partially_missing <- names(which(missing_rates > 0 & missing_rates < 1))

# Check if any of the non-numeric variables are among the missing:
#    print(non_numeric_variables %in% any_missing)
# We se that none are.

# Check how many variables are (mostly) missing, and
# how many are left (with no missing values)
#    print(length(any_missing))
#    print(length(no_missing))
# We see that about a third of the variables are left.

# ===========================================================================

# Next, we look at the ranges for the numeric variables, to see if they
# should be rescaled
ranges <- vapply(train_set[sapply(train_set, class) != "factor"], range, numeric(2), na.rm = TRUE)
dimnames(ranges)[[1]] <- c("min", "max")
sds <- vapply(train_set[sapply(train_set, class) != "factor"], sd, numeric(1), na.rm = TRUE)
summary_stats <- cbind(data.frame(t(ranges)),
                       data.frame(sd = sds,
                                  missing_rate = missing_rates[sapply(train_set, class) != "factor"]))
summary_stats$variable <- row.names(summary_stats)
summary_stats <- mutate(summary_stats,
                        range = max - min)

# We see that the ranges vary a lot, so the variables should probably be
# rescaled and centered.

# In addition several variables are angles in degrees with ranges
# either 0 to 360, or -180 to 180. This implies there is an artificial
# discontinuity coming from converting an angle to a real number.

# ===========================================================================

# Explore principal factors.
numeric_factors <- train_set[, no_missing]
numeric_factors <- numeric_factors[sapply(numeric_factors, class) != "factor"]
# Convert timestamp to numeric
numeric_factors$cvtd_timestamp <- as.numeric(numeric_factors$cvtd_timestamp)

# Create principal components
transform <- preProcess(x = numeric_factors, method = c("center", "scale", "pca"))
transformed <- predict(transform, numeric_factors)
numeric_factors <- cbind(numeric_factors, transformed)
numeric_factors$classe <- train_set$classe
numeric_factors$user_name <- train_set$user_name

# Note: there is one extreme outlier that skews the plots extremeley
# so we get rid of it first.
# Do a scatter plot of the first 6 principal componets pairwise,
# coloring by the outcome and by user.
#    ggpairs(data = numeric_factors[-3234, ],
#            columns = c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6"),
#            colour = "user_name")
#    ggpairs(data = numeric_factors[-3234, ],
#            columns = c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6"),
#            colour = "classe")

# We see very little separation between the outcome variables using the
# first six principal componentes, but very large separation between the
# users. Therefore it seems like the strongest effect on the features is
# the user.

# ===========================================================================
# We also plot the timestamp variables versus the user.
#    ggpairs(data = numeric_factors,
#            columns = c("raw_timestamp_part_1",
#                        "raw_timestamp_part_2",
#                        "cvtd_timestamp"),
#            colour = "user_name")

# We see that two of these take only a very small set of values that can
# be completed determined by the user_name variable.

# ============================================================================
# In light of the above two observations, we will remove the timestamp
# variables, and will then subtract the per-user means of each variable to
# remove the excessive effect the user has on all features.
numeric_factors <- train_set[, no_missing]
numeric_factors <- numeric_factors[sapply(numeric_factors, class) != "factor"]
numeric_factors <- select(numeric_factors, -contains("timestamp"))

numeric_factors$user_name <- train_set$user_name
numeric_factors$classe    <- train_set$classe
numeric_factors$obs <- 1:nrow(numeric_factors)

outcomes_only <-
  numeric_factors %>%
  select(obs, classe)

numeric_factors_tall <-
  numeric_factors %>%
  select(-classe) %>%
  gather(key = "variable", value = "true_value", -user_name, -obs)

numeric_factors_means <-
  numeric_factors %>%
  select(-obs) %>%
  group_by(user_name) %>%
  summarise_each(funs(mean)) %>%
  gather(key = "variable", value = "mean_value", -user_name)

numeric_factors_recentered <-
  numeric_factors_tall %>%
  merge(numeric_factors_means) %>%
  mutate(new_value = true_value - mean_value) %>%
  select(-true_value, -mean_value) %>%
  spread(key = variable, value = new_value) %>%
  merge(outcomes_only) %>%
  arrange(obs)

recentered_numbers_only <-
  numeric_factors_recentered %>%
  select(-obs, -user_name, -classe)

transform <- preProcess(x = recentered_numbers_only, method = "pca")
transformed <- predict(transform, recentered_numbers_only)
numeric_factors_recentered <- cbind(numeric_factors_recentered, transformed)




