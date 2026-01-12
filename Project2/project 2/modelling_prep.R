library(caret)
library(dplyr)

prep_data <- function(data, target) {
  
  # Encode categorical variables (except target)
  dummies <- dummyVars(as.formula(paste("~ . -", target)), data = data)
  data_encoded <- data.frame(predict(dummies, newdata = data))
  data_encoded[[target]] <- data[[target]]
  
  # Scale numeric variables (except target)
  num_vars <- sapply(data_encoded, is.numeric)
  if (target %in% names(num_vars)) {
    num_vars[target] <- FALSE
  }
  data_encoded[num_vars] <- scale(data_encoded[num_vars])
  
  # Train/Test Split
  set.seed(123)
  trainIndex <- createDataPartition(data_encoded[[target]], p = 0.7, list = FALSE)
  train <- data_encoded[trainIndex, ]
  test  <- data_encoded[-trainIndex, ]
  
  return(list(train = train, test = test))
}


data_dir <- "C:/Users/Dell/Downloads/Milestone2/Milestone2/project 2/Datasets"

cmr <- read.csv(file.path(data_dir, "child-mortality-rates_national_zaf.csv"), stringsAsFactors = FALSE)
cmr_split <- prep_data(cmr, target = "MortalityRate")

mm <- read.csv(file.path(data_dir, "maternal-mortality_national_zaf.csv"), stringsAsFactors = FALSE)
mm_split <- prep_data(mm, target = "MaternalDeaths")

ath <- read.csv(file.path(data_dir, "access-to-health-care_national_zaf.csv"), stringsAsFactors = FALSE)
ath_split <- prep_data(ath, target = "LevelRank")

cat("✅ Data prepared and split successfully!\n")
