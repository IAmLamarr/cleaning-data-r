library(dplyr)
library(rlist)


folder <- "UCI HAR Dataset"
if (!file.exists(folder)) {
  archive_name = "data.zip"
  download.file(
    "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",
    archive_name,
    method = "curl",
  )
  unzip(archive_name)
  file.remove(archive_name)
}


process_file <- function(filename, func) {
  con <- file(filename, "r")
  while (TRUE) {
    line <- readLines(con, n = 1)
    if (length(line) == 0) {
      break
    }
    func(line)
  }
  
  close(con)
}

process_dataset <- function(line) {
  line_vals <- strsplit(line, " ")
  vals <<- append(vals, line_vals)
}

process_feature <- function(line) {
  line_features <- unlist(strsplit(line, " "))
  features <<- append(features, line_features[2])
}

process_target <- function(line) {
  targets <<- append(targets, as.numeric(line))
}

process_activity <- function(line) {
  activity <<- append(activity, strsplit(line, " "))
}

test_dataset_filepath <- paste(folder, "test", "X_test.txt", sep = "/")
train_dataset_filepath <- paste(folder, "train", "X_train.txt", sep = "/")

test_target_filepath <- paste(folder, "test", "y_test.txt", sep = "/")
train_target_filepath <- paste(folder, "train", "y_train.txt", sep = "/")

features_filepath <- paste(folder, "features.txt", sep = "/")
activity_filepath <- paste(folder, "activity_labels.txt", sep = "/")

if (!exists("df")) {
  features <- list()
  vals <- list()
  targets <- list()
  activity <- list()

  process_file(train_dataset_filepath, process_dataset)
  process_file(test_dataset_filepath, process_dataset)
  
  process_file(train_target_filepath, process_target)
  process_file(test_target_filepath, process_target)
  
  process_file(features_filepath, process_feature)
  process_file(activity_filepath, process_activity)
  
  vals <- unlist(vals)
  features <- unlist(features)
  
  features <- append(features, "target")
  
  vals <- vals[lapply(vals, nchar) > 0]
  
  m <- matrix(vals, ncol = 561, byrow = TRUE)
  
  m <- cbind(m, targets)
  
  df <- as.data.frame(
    m
  )
  
  names(df) <- features
}

activity_df <- data.frame(
  matrix(unlist(activity), ncol=2, byrow=TRUE)
)

names(activity_df) <- c("target", "activity")

rel_features <- features[grep("(mean)|(std)|(target)", features)]

rel_df <- df[, rel_features]

merged_df <- merge(rel_df, activity_df, by="target", sort=FALSE)