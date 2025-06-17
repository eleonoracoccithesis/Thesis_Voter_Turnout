#1 LOAD LIBRARIES_______________________________________________________________
library(caret)
library(dplyr)
library(MLmetrics)


#1 PREPARE BINARY TRAINING SET__________________________________________________
train_df_bin <- train_final %>%
  filter(vote_2023 %in% c(1, 2)) %>%
  select(-participant, -year, -split)

train_df_bin$vote_2023 <- factor(ifelse(train_df_bin$vote_2023 == 1, "Yes", "No"), levels = c("No", "Yes"))


#2 DEFINE CUSTOM F1 FUNCTION____________________________________________________
f1Summary <- function(data, lev = NULL, model = NULL) {
  precision <- Precision(y_pred = data$pred, y_true = data$obs, positive = "Yes")
  recall <- Recall(y_pred = data$pred, y_true = data$obs, positive = "Yes")
  f1 <- F1_Score(y_pred = data$pred, y_true = data$obs, positive = "Yes")
  c(Precision = precision, Recall = recall, F1 = f1)
}


#3 CONTROL SETTIGNS WITH BAGGIN + DOWNSAMPLING__________________________________
cv_control_ensemble <- trainControl(
  method = "cv",
  number = 5,
  sampling = "down",        # automatic per-fold balancing
  classProbs = TRUE,
  savePredictions = TRUE,
  summaryFunction = f1Summary
)


#4 TRAIN ENSEMBLE RANDOM FOREST_________________________________________________
set.seed(42)
ensemble_rf <- train(
  vote_2023 ~ .,
  data = train_df_bin,
  method = "rf",
  ntree = 100,
  trControl = cv_control_ensemble,
  metric = "F1"
)


#5 REVIEW PERFORMANCE___________________________________________________________
print(ensemble_rf)
cv_results <- ensemble_rf$resample
print(cv_results)
t.test(cv_results$F1)
