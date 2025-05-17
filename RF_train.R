# === 1. Load Required Libraries ===
library(caret)
library(dplyr)
library(MLmetrics)
library(randomForest)

# === 2. Custom F1 scoring function ===
f1Summary <- function(data, lev = NULL, model = NULL) {
  precision <- Precision(y_pred = data$pred, y_true = data$obs, positive = "Yes")
  recall <- Recall(y_pred = data$pred, y_true = data$obs, positive = "Yes")
  f1 <- F1_Score(y_pred = data$pred, y_true = data$obs, positive = "Yes")
  out <- c(Precision = precision, Recall = recall, F1 = f1)
  return(out)
}

# === 3. Prepare Binary Training Data ===
train_df_bin <- train_final %>%
  filter(vote_2023 %in% c(1, 2)) %>%
  select(-participant, -year, -split)

# Recode target to factor
train_df_bin$vote_2023 <- factor(ifelse(train_df_bin$vote_2023 == 1, 1, 0),
                                 levels = c(0, 1),
                                 labels = c("No", "Yes"))

# === 4. Compute Class Weights (inversely proportional) ===
class_counts <- table(train_df_bin$vote_2023)
class_weights <- 1 / class_counts
class_weights <- class_weights / sum(class_weights)
row_weights <- class_weights[as.character(train_df_bin$vote_2023)]

# === 5. Set up 5-fold CV ===
cv_control <- trainControl(
  method = "cv",
  number = 5,
  classProbs = FALSE,
  savePredictions = TRUE,
  summaryFunction = f1Summary
)

# === 6. Train Random Forest (weighted via caret sample weights) ===
set.seed(42)
model_rf_f1 <- train(
  vote_2023 ~ .,
  data = train_df_bin,
  method = "rf",
  weights = row_weights,
  trControl = cv_control,
  metric = "F1",
  tuneLength = 3  # basic tuning, or use tuneGrid for advanced
)

# === 7. Review Results ===
print(model_rf_f1)
cv_results_rf <- model_rf_f1$resample
print(cv_results_rf)
t.test(cv_results_rf$F1)
