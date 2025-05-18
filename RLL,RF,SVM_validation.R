#1 FILTER + CLEAN VALIDATION DATA (only once)___________________________________
val_bin <- val_final %>%
  filter(vote_2023 %in% c(1, 2)) %>%
  drop_na() %>%
  mutate(vote_2023 = factor(ifelse(vote_2023 == 1, "Yes", "No"),
                            levels = c("No", "Yes")))


#2 FEATURES AND LABELS__________________________________________________________
X_val <- val_bin %>% select(-vote_2023, -participant, -year, -split)
y_val <- val_bin$vote_2023


#3 PREDICT & EVALUATE (Logistic Regression)_____________________________________
val_pred_rll <- predict(model_rll_f1, newdata = X_val)
confusionMatrix(val_pred_rll, y_val, positive = "Yes")


#4 PREDICT & EVALUATE (Random Forest)___________________________________________
val_pred_rf <- predict(ensemble_rf, newdata = X_val)
confusionMatrix(val_pred_rf, y_val, positive = "Yes")


#5 PREDICT & EVALUATE (SVM)_____________________________________________________
val_pred_svm <- predict(model_svm_f1, newdata = X_val)
confusionMatrix(val_pred_svm, y_val, positive = "Yes")
