#' @title Data Cleaning Function
#' @description This function is used by the remaining functions in this script
#'              to clean our TM data before using the XGBoost model.
#' @returns A dataframe of cleaned, but NOT YET ENCODED, training data.
prep <- function(data,
                 response = "StrikeSwinging",
                 forWhiff = T) {

  # Define mock strike zone and filter out data not w/in or around it
  sz <- data.frame(xmin = -0.95,
                   xmax = 0.95,
                   ymin = 1.525,
                   ymax = 3.316)

  data <- data %>% filter(PlateLocSide > sz$xmin - 0.25) %>%
    filter(PlateLocSide < sz$xmax + 0.25) %>%
    filter(PlateLocHeight > sz$ymin - 0.25) %>%
    filter(PlateLocHeight < sz$ymax + 0.25)

  # If this is being used for Swing and Miss (Whiff) model
  if (forWhiff) {
    # Condition on swing -- filter data to only deal with swings (no bunts)
    data <- data %>% filter(PitchCall %in% c("FoulBall",
                                             "StrikeSwinging",
                                             "InPlay")) %>%
      filter(gregexpr("Bunt", Pitch_Result)[[1]][1] == -1)
    data$response <- data$PitchCall == "StrikeSwinging"
  }

  # If this is for Called Strike model, ensure that pitches in 'middle' are
  # tagged as strikes
  if (response == "StrikeCalled") {
    data$response <- c()
    for (i in 1:nrow(data)) {
      if (data$PlateLocSide[i] >= sz$xmin + 0.3 &
          data$PlateLocSide[i] <= sz$xmax - 0.3 &
          data$PlateLocHeight[i] >= sz$ymin + 0.3 &
          data$PlateLocHeight[i] <= sz$ymax - 0.3) { # Ensuring that all pitches
        data$response[i] <- T                        # 'down-the-middle' = strikes
      } else {
        # Check if the Pitch is in response(s) encoding a swing-and-miss
        data$response[i] <- data$PitchCall[i] == response
      }
    }
  }
  # Cleaning the dataset
  data <- data %>% filter(BatterSide != "Undefined")
  data$BatterSide <- ifelse(data$BatterSide == "RIght", "Right",
                            ifelse(data$BatterSide == "Right", "Right",
                                   "Left")) # Fixing typos

  # Updating the pitch type definitions/cleaning data
  data <- data %>% filter(!(TaggedPitchType %in% c("Undefined", "", NA)))
  data$updPT <- ifelse(data$TaggedPitchType == "Fastball", "FB",
                       ifelse(data$TaggedPitchType == "ChangeUp", "CH",
                              ifelse(data$TaggedPitchType == "Curveball", "BR",
                                     ifelse(data$TaggedPitchType == "Slider", "BR",
                                            ifelse(data$TaggedPitchType == "Splitter", "CH",
                                                   ifelse(data$TaggedPitchType == "Cutter", "BR",
                                                          ifelse(data$TaggedPitchType == "Sinker", "BR",
                                                                 "NA")))))))

  # 'Bagging' the observations into groups -- DEPRECATED
  # data$updPT <- ifelse(data$TaggedPitchType == "FB", "FB",
  #                      ifelse(data$TaggedPitchType == "CH", "CH",
  #                             ifelse(data$TaggedPitchType == "CU", "BR",
  #                                    ifelse(data$TaggedPitchType == "SL", "BR",
  #                                           ifelse(data$TaggedPitchType == "SP", "CH",
  #                                                  ifelse(data$TaggedPitchType == "CT", "BR",
  #                                                         ifelse(data$TaggedPitchType == "SI", "BR",
  #                                                                "NA")))))))

  # Get rid of untagged pitches -- safeguard
  data <- data %>% filter(data$updPT != "NA")

  return(data)
}

#' @title Get Variables Of-Interest
#' @description Given training data that has been cleaned by the prep function
#' @returns Filtered training data -- to consider more variables, add them!
getVariables <- function(data) {
  if (!("updPT" %in% colnames(data))) {
    errorCondition("Data has not been cleaned!")
  }

  # These are the features! (Response is whatever binary response desired/made)
  cols <- c("updPT", "Extension", "InducedVertBreak", "HorzBreak",
            "PlateLocHeight", "PlateLocSide", "response")
  data <- data[,cols]
  return(data)
}

#' @title Partition Data into Train/Test
#' @description Given prepared and subset training data (from above functions),
#'              return a train or test set.
#' @returns Subset of training data.
partitionData <- function(data, test_size = 0.8, returnTrain = T) {
  if (test_size < 0 || test_size > 1) {
    errorCondition("Invalid proportion value given to split dataset -- 0 < test_size < 1")
    return()
  }

  set.seed(27514) # For reproducibility
  split <- sample.split(data, SplitRatio = test_size)

  data <- getVariables(data)

  if (returnTrain) {
    train <- data[split,]
    return(train)
  } else {
    test <- data[!split,]
    return(test)
  }
}

#' @title In-House One-Hot Encoder
#' @description This exists in an R package... but I hated the implementation.
#'              This function creates binary columns for each level of a categorical
#'              variable.
#' @return A binary of dataframe, size m x n -- m := number of obs., n := number of factors
oneHotEncoder <- function(v) {
  levels <- unique(v)

  ret <- data.frame()
  for (i in 1:length(v)) {
    temp <- c()
    for (j in 1:length(levels)) {
      temp <- append(temp,
                     ifelse(v[i] == levels[j],1,0))
    }
    ret <- rbind(ret,temp)
  }

  names <- c()
  for (j in 1:length(levels)) {
    names <- append(names,levels[j])
  }
  colnames(ret) <- names
  return(ret)
}

#' @title Make the XGBoost Model!
#' @description Perform the data analytics process to make our XGBoost model.
#' @returns Trained XGBoost Model
makeXGBMod <- function(data,rounds) {

  # Get the train/test datasets
  train <- partitionData(data,returnTrain = T)
  test <- partitionData(data, returnTrain = F)

  # Appropriate the features and targets to their own objects
  trainX <- train %>% select(-c("response"))
  trainX <- cbind(trainX %>% select(-c("updPT")), oneHotEncoder(trainX$updPT))
  trainY <- train$response

  # Appropriate the features and targets to their own objects
  testX <- test %>% select(-c("response"))
  testX <- cbind(testX %>% select(-c("updPT")), oneHotEncoder(testX$updPT))
  testY <- test$response

  # Create Data Matrices for the xgboost model (taken from online tutorial)
  xgb_train = xgb.DMatrix(data = as.matrix(trainX), label = trainY)
  xgb_test = xgb.DMatrix(data = as.matrix(testX), label = testY)
  watchlist = list(train=xgb_train, test=xgb_test)

  # Fit XGBoost model and display training and testing data at each round
  mod = xgb.train(data = xgb_train, max.depth = 3, watchlist=watchlist,
                  nrounds = rounds,verbose = 1,objective = "binary:logistic",
                  eval_metric = "auc")
  return(mod)
}
