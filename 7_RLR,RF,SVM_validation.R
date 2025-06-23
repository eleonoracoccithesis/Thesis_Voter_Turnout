# 3. DEFINE EVALUATION FUNCTION ------------------------------------------------
evaluate_model <- function(predictions, truth) {
  cm <- confusionMatrix(predictions, truth, positive = "No")
  
  precision_no <- Precision(y_pred = predictions, y_true = truth, positive = "No")
  recall_no <- Recall(y_pred = predictions, y_true = truth, positive = "No")
  f1_no <- F1_Score(y_pred = predictions, y_true = truth, positive = "No")
  
  precision_yes <- Precision(y_pred = predictions, y_true = truth, positive = "Yes")
  recall_yes <- Recall(y_pred = predictions, y_true = truth, positive = "Yes")
  f1_yes <- F1_Score(y_pred = predictions, y_true = truth, positive = "Yes")
  
  macro_f1 <- mean(c(f1_no, f1_yes))
  
  list(
    ConfusionMatrix = cm$table,
    Precision_No = precision_no,
    Recall_No = recall_no,
    F1_No = f1_no,
    Precision_Yes = precision_yes,
    Recall_Yes = recall_yes,
    F1_Yes = f1_yes,
    Macro_F1 = macro_f1
  )
}

# 4. PREDICT & EVALUATE (Logistic Regression) ----------------------------------
val_pred_rll <- predict(model_rll_f1, newdata = X_val)
results_rll <- evaluate_model(val_pred_rll, y_val)

# 5. PREDICT & EVALUATE (Random Forest) ----------------------------------------
val_pred_rf <- predict(rf_model, newdata = X_val)
results_rf <- evaluate_model(val_pred_rf, y_val)

# 6. PREDICT & EVALUATE (SVM) --------------------------------------------------
val_pred_svm <- predict(model_svm_f1, newdata = X_val)
results_svm <- evaluate_model(val_pred_svm, y_val)

# Optional: Print results
print(results_rll)
print(results_rf)
print(results_svm)
