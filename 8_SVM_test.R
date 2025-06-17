#LOAD LIBRARIES_________________________________________________________________
library(MLmetrics)
library(caret)
library(dplyr)

#1 DEFINE EVALUATION FUNCTION WITH EXPLICIT METRICS_____________________________
evaluate_test_set <- function(data, model, name) {
  test_bin <- data %>%
    filter(vote_2023 %in% c(1, 2)) %>%
    mutate(vote_2023 = factor(ifelse(vote_2023 == 1, "Yes", "No"),
                              levels = c("No", "Yes"))) %>%
    drop_na()
  
  X_test <- test_bin %>% select(-vote_2023, -participant, -year, -split)
  y_test <- test_bin$vote_2023
  
  pred <- predict(model, newdata = X_test)
  
  # Print confusion matrix
  cat("\n=== Confusion Matrix for", name, "===\n")
  cm <- confusionMatrix(pred, y_test, positive = "Yes")
  print(cm)
  
  # Extract metrics
  precision <- Precision(y_pred = pred, y_true = y_test, positive = "Yes")
  recall <- Recall(y_pred = pred, y_true = y_test, positive = "Yes")
  f1 <- F1_Score(y_pred = pred, y_true = y_test, positive = "Yes")
  
  # Print metrics
  cat("\nMetrics for", name, ":\n")
  cat("Precision:", round(precision, 4), "\n")
  cat("Recall   :", round(recall, 4), "\n")
  cat("F1 Score :", round(f1, 4), "\n")
}

# Evaluate both sets
evaluate_test_set(test_before, model_svm_f1, "Test Set (2021–2022)")
evaluate_test_set(final_test, model_svm_f1, "Final Test Set (2023)")
