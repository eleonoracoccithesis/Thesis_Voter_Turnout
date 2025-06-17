#1 LOAD LIBRARIES_______________________________________________________________
library(caret)
library(dplyr)
library(MLmetrics)  


#2 CUSTOM SUMMARY FUNCTION______________________________________________________
f1Summary <- function(data, lev = NULL, model = NULL) {
  precision <- Precision(y_pred = data$pred, y_true = data$obs, positive = "Yes")
  recall <- Recall(y_pred = data$pred, y_true = data$obs, positive = "Yes")
  f1 <- F1_Score(y_pred = data$pred, y_true = data$obs, positive = "Yes")
  out <- c(Precision = precision, Recall = recall, F1 = f1)
  return(out)
}


#3 PREPARE BINARY TRAINING DATA_________________________________________________
train_df_bin <- train_final %>%
  filter(vote_2023 %in% c(1, 2)) %>%
  select(-participant, -year, -split)

# Recode to binary factor
train_df_bin$vote_2023 <- ifelse(train_df_bin$vote_2023 == 1, 1, 0)
train_df_bin$vote_2023 <- factor(train_df_bin$vote_2023, levels = c(0, 1), labels = c("No", "Yes"))


#4 COMPUTE CLASS WEIGHTS________________________________________________________
class_counts <- table(train_df_bin$vote_2023)
class_weights <- 1 / class_counts
class_weights <- class_weights / sum(class_weights)
row_weights <- class_weights[as.character(train_df_bin$vote_2023)]


#5 SET UP 5-FOLD CV WITH F1 METRICS_____________________________________________
cv_control <- trainControl(
  method = "cv",
  number = 5,
  classProbs = FALSE,
  savePredictions = TRUE,
  summaryFunction = f1Summary
)


#6 TRAIN LOGISTIC REGRESSION____________________________________________________
set.seed(42)
model_rll_f1 <- train(
  vote_2023 ~ .,
  data = train_df_bin,
  method = "glm",
  family = "binomial",
  weights = row_weights,
  trControl = cv_control,
  metric = "F1"
)


#7 REVIEW RESULTS_______________________________________________________________
print(model_rll_f1)
cv_results_f1 <- model_rll_f1$resample
print(cv_results_f1)
t.test(cv_results_f1$F1)



