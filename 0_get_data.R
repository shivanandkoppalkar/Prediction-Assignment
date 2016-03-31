# Filenames
training_data_file <- "pml-training.csv"
testing_data_file <- "pml-testing.csv"

# Download the data
download_data <-
  function () {
    training_data_location <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
    testing_data_location  <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
    download.file(url = training_data_location, destfile = training_data_file,
                  method = "wget")
    download.file(url = testing_data_location, destfile = testing_data_file,
                  method = "wget")
  }

if (!file.exists(training_data_file) || !file.exists(testing_data_file)) {
  download_data()
}
