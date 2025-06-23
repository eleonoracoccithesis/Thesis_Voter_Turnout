evaluate_test_set <- function(data, model, name) { 
  # Prepare test data
  test_bin <- data %>%
    filter(vote_2023 %in% c(1, 2)) %>%
    mutate(vote_2023 = factor(ifelse(vote_2023 == 1, "Yes", "No"),
                              levels = c("No", "Yes"))) %>%
    drop_na()
  
  X_test <- test_bin %>% select(-vote_2023, -participant, -year, -split)
  y_test <- test_bin$vote_2023

  # Predict
  pred <- predict(model, newdata = X_test)

  # Confusion matrix
  cat("\n=== Confusion Matrix for", name, "===\n")
  cm <- confusionMatrix(pred, y_test)
  print(cm$table)

  # Class-specific metrics
  precision_no  <- Precision(y_pred = pred, y_true = y_test, positive = "No")
  recall_no     <- Recall(y_pred = pred, y_true = y_test, positive = "No")
  f1_no         <- F1_Score(y_pred = pred, y_true = y_test, positive = "No")

  precision_yes <- Precision(y_pred = pred, y_true = y_test, positive = "Yes")
  recall_yes    <- Recall(y_pred = pred, y_true = y_test, positive = "Yes")
  f1_yes        <- F1_Score(y_pred = pred, y_true = y_test, positive = "Yes")

  macro_f1 <- mean(c(f1_no, f1_yes))
  accuracy <- cm$overall["Accuracy"]

  # Print results
  cat("\n=== Metrics for", name, "===\n")
  cat("Class: No  | Precision:", round(precision_no, 4),
      " Recall:", round(recall_no, 4),
      " F1:", round(f1_no, 4), "\n")
  cat("Class: Yes | Precision:", round(precision_yes, 4),
      " Recall:", round(recall_yes, 4),
      " F1:", round(f1_yes, 4), "\n")
  cat("Macro F1 Score:", round(macro_f1, 4), "\n")
  cat("Overall Accuracy:", round(accuracy, 4), "\n")

  # Optional: return as list for export
  return(list(
    ConfusionMatrix = cm$table,
    Accuracy = accuracy,
    Class_Metrics = list(
      No  = c(Precision = precision_no, Recall = recall_no, F1 = f1_no),
      Yes = c(Precision = precision_yes, Recall = recall_yes, F1 = f1_yes)
    ),
    MacroF1 = macro_f1
  ))
}
