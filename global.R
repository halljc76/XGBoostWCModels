##### PACKAGES #####
library(dplyr)
library(caTools)
library(caret)
library(e1071)
library(Boruta)
library(rfUtilities)
library(aws.s3)
library(fst)
library(xgboost)

##### AWS INFORMATION #####
readRenviron("./.Renviron")
Sys.getenv("AWS_ACCESS_KEY_ID")
Sys.getenv("AWS_SECRET_ACCESS_KEY")
Sys.getenv("AWS_DEFAULT_REGION")

#' @title Get Training Data for XGBoost Modeling
#' @description Function to get Training Data from our AWS S3 Bullpen Bucket
#' @return A large dataset of training data.
getModelTrain <- function() {
  ret <- data.frame()
  directories <- c("ModelTrain/")
  for (direc in directories) {
    keys <- unique(data.table::rbindlist(get_bucket(bucket = "uncbullpen", prefix = direc))$Key)
    for (key in keys) {
      if (gregexpr(".fst", key)[[1]][1] != -1) {
        temp <- s3read_using(FUN = read.fst, bucket = "uncbullpen", object = key)
        ret <- rbind(ret, temp)
      }
    }
  }
  return(ret)
}
