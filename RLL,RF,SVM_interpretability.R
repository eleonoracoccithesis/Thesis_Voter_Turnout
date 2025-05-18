#1 INTERPRETABILITY RLL_________________________________________________________
# Load libraries
library(broom)
library(ggplot2)

# Extract and plot top 10 coefficients
tidy(model_rll_f1$finalModel) %>%
  filter(term != "(Intercept)") %>%
  arrange(desc(abs(estimate))) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder(term, estimate), y = estimate)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    x = "Feature", y = "Log-Odds"
  ) +
  theme_minimal()


#2 INTERPRETABILITY RF__________________________________________________________
# Load PDP library
library(pdp)
library(patchwork)  # for wrap_plots

# Get feature names from RF model
features_rf <- ensemble_rf$finalModel$xNames

# Create a list to store plots
pdp_plots <- list()

# Loop over features and store ggplot objects without titles
for (feature in features_rf) {
  pd <- partial(ensemble_rf, pred.var = feature, prob = TRUE)
  p <- autoplot(pd) +
    theme_minimal() +
    theme(
      plot.title = element_blank(),  # remove title
      axis.title = element_text(size = 8),
      axis.text = element_text(size = 7)
    )
  pdp_plots[[feature]] <- p
}

# Combine plots into one page
combined_plot <- wrap_plots(pdp_plots) + plot_layout(ncol = 3)
print(combined_plot)


#3 INTERPRETABILITY SVM_________________________________________________________
# Retrain SVM with class probabilities
set.seed(42)

model_svm_f1 <- train(
  vote_2023 ~ .,
  data = train_df_bin,
  method = "svmLinear",
  trControl = trainControl(
    method = "cv",
    number = 5,
    sampling = "down",
    classProbs = TRUE,              # <--- required
    savePredictions = TRUE,
    summaryFunction = f1Summary
  ),
  metric = "F1",
  preProcess = c("center", "scale")
)

# Rebuild explainer and run LIME
library(lime)
explainer_svm <- lime(X_val, model = model_svm_f1)

lime_explanation <- explain(
  X_val[1:3, ],
  explainer = explainer_svm,
  n_features = 5,
  labels = "Yes"
)

plot_features(lime_explanation)



