# === 1. Load Required Libraries ===
library(caret)
library(dplyr)
library(MLmetrics)  # for F1, Precision, Recall

# === 2. Custom summary function ===
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

# Recode to binary factor
train_df_bin$vote_2023 <- ifelse(train_df_bin$vote_2023 == 1, 1, 0)
train_df_bin$vote_2023 <- factor(train_df_bin$vote_2023, levels = c(0, 1), labels = c("No", "Yes"))

# === 4. Compute Class Weights ===
class_counts <- table(train_df_bin$vote_2023)
class_weights <- 1 / class_counts
class_weights <- class_weights / sum(class_weights)
row_weights <- class_weights[as.character(train_df_bin$vote_2023)]

# === 5. Set Up 5-Fold CV With F1 Metrics ===
cv_control <- trainControl(
  method = "cv",
  number = 5,
  classProbs = FALSE,
  savePredictions = TRUE,
  summaryFunction = f1Summary
)

# === 6. Train Logistic Regression ===
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

# === 7. Review Results ===
print(model_rll_f1)
cv_results_f1 <- model_rll_f1$resample
print(cv_results_f1)
t.test(cv_results_f1$F1)



