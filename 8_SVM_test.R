evaluate_test_set <- function(data, model, name) {
  test_bin <- data %>%
    filter(vote_2023 %in% c(1, 2)) %>%
    mutate(vote_2023 = factor(ifelse(vote_2023 == 1, "Yes", "No"),
                              levels = c("No", "Yes"))) %>%
    drop_na()
  
  X_test <- test_bin %>% select(-vote_2023, -participant, -year, -split)
  y_test <- test_bin$vote_2023
  
  pred <- predict(model, newdata = X_test)
  
  # Confusion matrix
  cat("\n=== Confusion Matrix for", name, "===\n")
  print(confusionMatrix(pred, y_test))
  
  # Class-specific metrics
  precision_no  <- Precision(y_pred = pred, y_true = y_test, positive = "No")
  recall_no     <- Recall(y_pred = pred, y_true = y_test, positive = "No")
  f1_no         <- F1_Score(y_pred = pred, y_true = y_test, positive = "No")
  
  precision_yes <- Precision(y_pred = pred, y_true = y_test, positive = "Yes")
  recall_yes    <- Recall(y_pred = pred, y_true = y_test, positive = "Yes")
  f1_yes        <- F1_Score(y_pred = pred, y_true = y_test, positive = "Yes")
  
  macro_f1 <- mean(c(f1_no, f1_yes))
  
  # Print metrics
  cat("\n=== Metrics for", name, "===\n")
  cat("Class: No  | Precision:", round(precision_no, 4),
      " Recall:", round(recall_no, 4),
      " F1:", round(f1_no, 4), "\n")
  cat("Class: Yes | Precision:", round(precision_yes, 4),
      " Recall:", round(recall_yes, 4),
      " F1:", round(f1_yes, 4), "\n")
  cat("Macro F1 Score:", round(macro_f1, 4), "\n")
}


# Evaluate both sets
evaluate_test_set(test_before, model_svm_f1, "Test Set (2021–2022)")
evaluate_test_set(final_test, model_svm_f1, "Final Test Set (2023)")
