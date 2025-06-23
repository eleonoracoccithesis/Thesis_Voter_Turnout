# 1. LOAD LIBRARIES -----------------------------------------------------------
library(caret)
library(dplyr)
library(MLmetrics)
library(e1071)
library(kernlab)

# 2. DEFINE CUSTOM F1 SUMMARY FUNCTION ----------------------------------------
f1Summary <- function(data, lev = NULL, model = NULL) {
  # Ensure both levels exist to avoid NaN
  if (!all(c("Yes", "No") %in% levels(data$obs))) return(c(Precision = NA, Recall = NA, F1 = NA))
  
  precision <- tryCatch(Precision(y_pred = data$pred, y_true = data$obs, positive = "No"), error = function(e) NA)
  recall    <- tryCatch(Recall(y_pred = data$pred, y_true = data$obs, positive = "No"), error = function(e) NA)
  f1        <- tryCatch(F1_Score(y_pred = data$pred, y_true = data$obs, positive = "No"), error = function(e) NA)
  
  return(c(Precision = precision, Recall = recall, F1 = f1))
}

# 3. PREPARE BINARY TRAINING DATA ---------------------------------------------
train_df_bin <- train_final %>%
  filter(vote_2023 %in% c(1, 2)) %>%
  mutate(vote_2023 = factor(ifelse(vote_2023 == 1, "Yes", "No"), levels = c("No", "Yes"))) %>%
  select(-participant, -year, -split)

# 4. COMPUTE CLASS WEIGHTS ----------------------------------------------------
class_counts <- table(train_df_bin$vote_2023)
class_weights <- 1 / class_counts
class_weights <- class_weights / sum(class_weights)
row_weights <- class_weights[as.character(train_df_bin$vote_2023)]

# 5. DEFINE CV CONTROL ---------------------------------------------------------
cv_control <- trainControl(
  method = "cv",
  number = 5,
  classProbs = FALSE,
  savePredictions = TRUE,
  summaryFunction = f1Summary
)

# 6. DEFINE HYPERPARAMETER GRID -----------------------------------------------
tune_grid_svm <- expand.grid(cost = 2^(-5:5))

# 7. TRAIN SVM MODEL -----------------------------------------------------------
set.seed(42)
model_svm_f1 <- train(
  vote_2023 ~ .,
  data = train_df_bin,
  method = "svmLinear2",
  weights = row_weights,
  trControl = cv_control,
  tuneGrid = tune_grid_svm,
  metric = "F1",
  preProcess = c("center", "scale")
)

# 8. REVIEW RESULTS ------------------------------------------------------------
print(model_svm_f1)

best_svm_params <- model_svm_f1$bestTune
cat("Best SVM Cost:\n")
print(best_svm_params)

cv_results_svm <- model_svm_f1$resample
cat("\nCross-validation F1 Scores:\n")
print(cv_results_svm)

# Optional: Statistical test on F1 scores
cat("\nT-test of F1 scores across folds:\n")
print(t.test(cv_results_svm$F1))
