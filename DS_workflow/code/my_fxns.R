# ----------------------------------------------------------------------------------------
# Load dependencies
# ----------------------------------------------------------------------------------------
library(tidyverse)
library(stringr)
library(rpart)
library(caret)
library(pROC)
library(dummy)
library(data.table)
library(randomForest)

# ----------------------------------------------------------------------------------------
# Preprocess raw dataset
# ----------------------------------------------------------------------------------------
cleanse <- function(data) {
  
  # 1. Remove non-informative and mostly missing variables
  rm <- c()
  for(i in names(data)) {
    if(length(unique(data[[i]])) == 1 | # non-informative variables OR
       sum(is.na(data[[i]]))/nrow(data) > 0.95) # more than 95% missing
    {
      rm <- append(rm, i)
    }
    also_rm <- c("row_id", "first_name", "last_name", "address1")
    rm_fin <- append(rm, also_rm)
  }
  rm_col <- which(names(data) %in% rm_fin)
  cleaner_temp <- subset(data, select = -rm_col)
  
  # 2. Remove misplaced observations. E.g. Albuquerque isn't a state
  rm_rows <- which(nchar(cleaner_temp$state) > 2) # all state abbr. have 2 characters
  cleaner_temp2 <- cleaner_temp[-rm_rows, ]
  
  # 3. Treat NAs in character and integer variables as separate category
  for(i in names(cleaner_temp2)) {
    if(class(cleaner_temp2[[i]]) %in% c("character", "integer"))
    {
      cleaner_temp2[[i]] <- factor(cleaner_temp2[[i]], exclude = NULL)
    }
  }
  
  # 4. Remove name & DOB columns
  col_names <- names(cleaner_temp2)
  name_8610_cols <- col_names[which(!is.na(str_match(names(cleaner_temp2), "8610")))]
  name_8612_cols <- col_names[which(!is.na(str_match(names(cleaner_temp2), "8612")))]
  DOB_8624_cols <- col_names[which(!is.na(str_match(names(cleaner_temp2), "8624")))]
  
  rm_cols <- c(name_8610_cols, name_8612_cols, DOB_8624_cols)
  rm_ix <- which(names(cleaner_temp2) %in% rm_cols)
  
  cleaner_temp3 <- cleaner_temp2[, -rm_ix]
  
  # 5. Convert data types using manually-made dict.csv, which contains
  # names of ordinal factor variables and other variables that'd be
  # better encoded as integers or numerics.
  
  dict <- read_csv("data/dict.csv")
  dict_col <- dict$column_name
  data_type <- dict$data_type
  
  for(i in 1:nrow(dict)) {
    if (data_type[i] == "character") {
      cleaner_temp3[[dict_col[i]]] <- as.character(cleaner_temp3[[dict_col[i]]])
    }
    else if (data_type[i] == "integer") {
      cleaner_temp3[[dict_col[i]]] <- as.integer(cleaner_temp3[[dict_col[i]]])
    }
    else if (data_type[i] == "factor") {
      cleaner_temp3[[dict_col[i]]] <- factor(cleaner_temp3[[dict_col[i]]],
                                             exclude = NULL)
    }
    else
      cleaner_temp3[[dict_col[i]]] <- as.numeric(cleaner_temp3[[dict_col[i]]])
  }
  
  # 6. Convert all "rank" columns to integers
  rank_columns <- names(cleaner_temp3)[str_which(names(cleaner_temp3), "rank")]
  for(col in rank_columns) {
    cleaner_temp3[[col]] <- as.integer(cleaner_temp3[[col]])
  }
  
  # 7. Remove the city column
  clean_data <- cleaner_temp3[, -1]
  
  # Return clean dataset
  return(clean_data)
}

# ----------------------------------------------------------------------------------------
# Check data type of each column in a dataset
# ----------------------------------------------------------------------------------------
check_cols <- function(data) {
  vec <- c()
  index <- 1
  for(name in names(data)) {
    vec[index] <- class(data[[name]])
    index <- index + 1
  }
  fac_vars <- names(data)[which(vec == "factor")]
  int_vars <- names(data)[which(vec == "integer")]
  num_vars <- names(data)[which(vec == "numeric")]
  type <- unique(vec)
  return(list(unique = type,
              factors = list(fac_vars, which(vec == "factor")),
              integers = int_vars, numerical = num_vars))
}

# ----------------------------------------------------------------------------------------
# Get numerical and integer data type in dataset
# ----------------------------------------------------------------------------------------
extractNum <- function(data) {
  num_vars <- c()
  j <- 1
  for(name in names(data)) {
    if (class(data[[name]]) %in% c("integer", "numeric")) {
      num_vars[j] <- name
      j <- j + 1
    }
  }
  num_data <- data[, c(num_vars)]
  return(num_data)
}

# ----------------------------------------------------------------------------------------
# Extract factor data types from dataset
# ----------------------------------------------------------------------------------------
extractFac <- function(data) {
  fac_vars <- c()
  j <- 1
  for(name in names(data)) {
    if (class(data[[name]]) == "factor") {
      fac_vars[j] <- name
      j <- j + 1
    }
  }
  fac_data <- data[, c(fac_vars)]
  return(fac_data)
}

# ----------------------------------------------------------------------------------------
# Binarize factor variables in clean_data
# ----------------------------------------------------------------------------------------
binarize <- function(data) {
  # Drop zip, label
  dummy <- dummy(x = data[, -c(2, ncol(data))]) # checked
  
  # Impute NAs
  NA_cols <- names(dummy)[which(!is.na(str_match(names(dummy), "NA")))]
  na_ix <- which(names(dummy) %in% NA_cols)
  
  dummy2 <- dummy[, -na_ix]
  
  # Remove columns with only 1 level, i.e. columns with levels
  # that only exist in either test set or train set but not both
  ind <- 1
  save <- c()
  for(col_name in names(dummy2)) {
    if(length(unique(dummy2[[col_name]])) == 1) {
      save[ind] <- col_name
      ind <- ind + 1
    }
  }
  one_level_ix <- which(names(dummy2) %in% save)
  dummy3 <- dummy2[, -one_level_ix]
  
  # Convert factors to ints
  binarized_data <- as.data.frame(sapply(dummy3, as.integer))
  
  return(binarized_data)
}

# ----------------------------------------------------------------------------------------
# Given model, output AUC
# ----------------------------------------------------------------------------------------
get_auc <- function(model) {
  test <- fread("../data/test.csv")
  if("glm" %in% class(model) | "lm" %in% class(model)) {
    model_type <- "response"
  }
  # else if(class(model) == "rpart") {
  #   test <- fread("../data/tree_test.csv")
  #   model_type <- "class"
  # }
  # else if("randomForest.formula" %in% class(model) | "randomForest" %in% class(model)) {
  #   test <- fread("../data/tree_test.csv")
  #   model_type <- "response"
  # }
  set.seed(1)
  preds <- predict(model, newdata = test, type = model_type)
  roc <- roc(as.numeric(test$label), as.numeric(preds))
  return(auc(roc))
}

get_last_auc <- function(model) {
  test <- fread("../data/last_test.csv")
  model_type <- "response"
  preds <- predict(model, newdata = test, type = model_type)
  roc <- roc(as.numeric(test$label), as.numeric(preds))
  return(auc(roc))
}

# ----------------------------------------------------------------------------------------
# Given linear model, get predictor variables
# ----------------------------------------------------------------------------------------
get_predictors <- function(model) {
  pred_vars <- as.character(formula(model))[3] # extracts the RHS of equation
  temp <- unlist(str_split(pred_vars, pattern = " +"))
  vars <- unique(temp)[which(unique(temp) != "+" &
                             unique(temp) != "\n")]
  return(vars)
}
