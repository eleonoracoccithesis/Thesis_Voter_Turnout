#1 LOAD LIBRARIES_______________________________________________________________
library(caret)
library(dplyr)
library(MLmetrics)


#2 DEFINE F1-BASED SUMMARY FUNCTION_____________________________________________
f1Summary <- function(data, lev = NULL, model = NULL) {
  precision <- Precision(y_pred = data$pred, y_true = data$obs, positive = "Yes")
  recall <- Recall(y_pred = data$pred, y_true = data$obs, positive = "Yes")
  f1 <- F1_Score(y_pred = data$pred, y_true = data$obs, positive = "Yes")
  out <- c(Precision = precision, Recall = recall, F1 = f1)
  return(out)
}


#3 PREPARE BINARY TRAINING SET__________________________________________________
train_df_bin <- train_final %>%
  filter(vote_2023 %in% c(1, 2)) %>%
  mutate(vote_2023 = factor(ifelse(vote_2023 == 1, "Yes", "No"),
                            levels = c("No", "Yes"))) %>%
  select(-participant, -year, -split)


#4 SET CV CONTROL WITH DOWNSAMPLING_____________________________________________
cv_control_svm <- trainControl(
  method = "cv",
  number = 5,
  classProbs = FALSE,
  savePredictions = TRUE,
  summaryFunction = f1Summary,
  sampling = "down"
)


#5 TRAIN SVM (LINEAR KERNEL)____________________________________________________
set.seed(42)
model_svm_f1 <- train(
  vote_2023 ~ .,
  data = train_df_bin,
  method = "svmLinear",
  trControl = cv_control_svm,
  metric = "F1",
  preProcess = c("center", "scale")
)


#6 REVIEW RESULTS_______________________________________________________________
print(model_svm_f1)
cv_results_svm <- model_svm_f1$resample
print(cv_results_svm)
t.test(cv_results_svm$F1)
