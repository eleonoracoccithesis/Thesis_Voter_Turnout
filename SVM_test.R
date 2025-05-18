#1 LOAD DATASETS________________________________________________________________
final_test <- read.csv("final_test_2023.csv")
test_before <- read.csv("test_final.csv")

#2 DEFINE A FUNCTION TO CLEAN AND EVALUATE______________________________________
evaluate_test_set <- function(data, model, name) {
  test_bin <- data %>%
    filter(vote_2023 %in% c(1, 2)) %>%
    mutate(vote_2023 = factor(ifelse(vote_2023 == 1, "Yes", "No"),
                              levels = c("No", "Yes"))) %>%
    drop_na()
  
  X_test <- test_bin %>% select(-vote_2023, -participant, -year, -split)
  y_test <- test_bin$vote_2023
  
  pred <- predict(model, newdata = X_test)
  cat("\n=== Confusion Matrix for", name, "===\n")
  print(confusionMatrix(pred, y_test, positive = "Yes"))
}

#3 EVALUATE ON BOTH SETS________________________________________________________
evaluate_test_set(test_before, model_svm_f1, "Test Set (2021–2022)")
evaluate_test_set(final_test, model_svm_f1, "Final Test Set (2023)")
