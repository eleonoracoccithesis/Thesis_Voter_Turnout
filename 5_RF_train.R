# 1. LOAD LIBRARIES ------------------------------------------------------------
library(caret)
library(dplyr)
library(MLmetrics)
library(randomForest)

# 2. PREPARE BINARY TRAINING SET -----------------------------------------------
train_df_bin <- train_final %>%
  filter(vote_2023 %in% c(1, 2)) %>%
  select(-participant, -year, -split)

# Convert to factor
train_df_bin$vote_2023 <- factor(ifelse(train_df_bin$vote_2023 == 1, "Yes", "No"), levels = c("No", "Yes"))

# 3. COMPUTE CLASS WEIGHTS -----------------------------------------------------
class_counts <- table(train_df_bin$vote_2023)
class_weights <- 1 / class_counts
class_weights <- class_weights / sum(class_weights)
row_weights <- class_weights[as.character(train_df_bin$vote_2023)]

# 4. CUSTOM F1 METRIC FUNCTION -------------------------------------------------
f1Summary <- function(data, lev = NULL, model = NULL) {
  precision <- Precision(y_pred = data$pred, y_true = data$obs, positive = "No")
  recall <- Recall(y_pred = data$pred, y_true = data$obs, positive = "No")
  f1 <- F1_Score(y_pred = data$pred, y_true = data$obs, positive = "No")
  c(Precision = precision, Recall = recall, F1 = f1)
}

# 5. CROSS-VALIDATION CONTROL --------------------------------------------------
cv_control <- trainControl(
  method = "cv",
  number = 5,
  classProbs = FALSE,
  savePredictions = TRUE,
  summaryFunction = f1Summary
)

# 6. HYPERPARAMETER GRID -------------------------------------------------------
tune_grid <- expand.grid(mtry = c(3, 4, 5))

# 7. TRAIN RANDOM FOREST -------------------------------------------------------
set.seed(42)
rf_model <- train(
  vote_2023 ~ .,
  data = train_df_bin,
  method = "rf",
  weights = row_weights,
  trControl = cv_control,
  tuneGrid = tune_grid,
  metric = "F1",
  ntree = 100
)

# 8. REVIEW RESULTS ------------------------------------------------------------
print(rf_model)
cv_results <- rf_model$resample
print(cv_results)
t.test(cv_results$F1)
