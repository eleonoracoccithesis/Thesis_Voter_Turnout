# Download packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(forcats)
library(ggthemes)
library(gridExtra) 


# Define colors, including one for NA (e.g., gray)
vote_colors <- c("1" = "#2ECC71",  # green
                 "2" = "#E74C3C",  # red
                 "3" = "#3498DB",  # blue
                 "NA" = "gray70")  # color for missing

# Define labels, including for NA
vote_labels <- c("1" = "1 = Yes", "2" = "2 = No", "3" = "3 = Not eligible", "NA" = "Missing")

# Don't filter out NA
plot_data <- final_data %>%
  mutate(vote_2023 = factor(vote_2023, levels = c(1, 2, 3)))  # NA will stay NA

# Plot
ggplot(plot_data, aes(x = vote_2023, fill = vote_2023)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  scale_fill_manual(values = vote_colors,
                    labels = vote_labels,
                    na.translate = TRUE,  # <- include NA in fill
                    na.value = "gray70") +
  labs(x = "Vote Code", y = "Count", fill = "vote_2023") +
  theme_minimal() +
  theme(panel.grid = element_blank())


#2 MISSING VALUES_______________________________________________________________
# Count missing values per column
missing_df <- final_data %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), 
               names_to = "variable", 
               values_to = "missing_count") %>%
  arrange(desc(missing_count))  # optional: sort from most to least missing

# View result
print(missing_df)

# Add missing percentages
missing_df <- missing_df %>%
  mutate(missing_percent = round(100 * missing_count / nrow(final_data), 1))

# Save missing_df to CSV file
write.csv(missing_df, "missing_df.csv", row.names = FALSE)


#3 IMPUTATION___________________________________________________________________
# Drop rows with >60% missing values
threshold <- 0.6 * ncol(final_data)
rows_to_drop <- which(rowSums(is.na(final_data)) > threshold)
cleaned_data <- final_data[-rows_to_drop, ]

# Identify variable types
numeric_vars <- grep("^age_", names(cleaned_data), value = TRUE)

ordinal_vars <- grep("^(satisfaction|confidence)_(work_government|government|parliament|politicians|political_parties|financial_situation|economy)_\\d{4}", 
                     names(cleaned_data), value = TRUE)

vote_vars <- grep("^vote_\\d{4}$", names(cleaned_data), value = TRUE)
target_var <- "vote_2023"
vote_vars <- setdiff(vote_vars, target_var)  # Exclude target

categorical_vars <- setdiff(names(cleaned_data),
                            c(numeric_vars, ordinal_vars, vote_vars, target_var))

# Define mode function
get_mode <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}

# Impute numerical variables using random sampling from observed values
for (col in numeric_vars) {
  missing_idx <- which(is.na(cleaned_data[[col]]))
  sampled_vals <- sample(cleaned_data[[col]][!is.na(cleaned_data[[col]])],
                         length(missing_idx), replace = TRUE)
  cleaned_data[[col]][missing_idx] <- sampled_vals
}

# Impute ordinal variables (median of numeric conversion)
for (col in ordinal_vars) {
  cleaned_data[[col]][is.na(cleaned_data[[col]])] <- median(as.numeric(cleaned_data[[col]]), na.rm = TRUE)
}

# Impute categorical + past voting vars (mode)
for (col in c(categorical_vars, vote_vars)) {
  cleaned_data[[col]][is.na(cleaned_data[[col]])] <- get_mode(cleaned_data[[col]])
}

# Check result
cat("Final row count:", nrow(cleaned_data), "\n")
cat("Total missing values remaining:", sum(is.na(cleaned_data)), "\n")

# Save cleaned_data to CSV
write.csv(cleaned_data, "cleaned_data.csv", row.names = FALSE)


#4 AGE BEFORE AND AFTER IMPUTATION______________________________________________
# Identify all age columns
age_vars <- grep("^age_\\d{4}$", names(final_data), value = TRUE)

# Stack long format for pre-imputation
age_long_before <- final_data %>%
  select(all_of(age_vars)) %>%
  pivot_longer(cols = everything(), names_to = "year", values_to = "age") %>%
  mutate(year = gsub("age_", "", year),
         imputed = "Before")

# Stack long format for post-imputation
age_long_after <- cleaned_data %>%
  select(all_of(age_vars)) %>%
  pivot_longer(cols = everything(), names_to = "year", values_to = "age") %>%
  mutate(year = gsub("age_", "", year),
         imputed = "After")

# Combine
age_long_combined <- bind_rows(age_long_before, age_long_after)

# Plot: Faceted Histograms
ggplot(age_long_combined, aes(x = age, fill = imputed)) +
  geom_histogram(position = "identity", bins = 40, alpha = 0.6) +
  facet_wrap(~year, scales = "free_y") +
  scale_fill_manual(values = c("Before" = "#8888FF", "After" = "#4CAF50")) +
  labs(title = "Age Distribution by Year: Before vs After Imputation",
       x = "Age", y = "Count") +
  theme_minimal()


# 5 AGE AND VOTE 2023: DENSITY PLOT_____________________________________________
# Filter out Not Eligible (3)
filtered_data <- cleaned_data[cleaned_data$vote_2023 %in% c(1, 2), ]

ggplot(filtered_data, aes(x = age_2023, fill = as.factor(vote_2023))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("1" = "#2ECC71", "2" = "#3498DB"),
                    labels = c("1" = "Yes", "2" = "No")) +
  labs(x = "Age 2023", y = "Density", fill = "vote_2023") +
  theme_minimal()


#6 CRAMER'S V: CATEGORICAL VARIABLES___________________________________________
# Packages
library(reshape2)
library(DescTools)

# Select categorical vars
cat_vars_2023 <- c("vote_2023", "interest_news_2023", "interest_politics_2023",
                   "follow_news_television_2023", "follow_news_internet_2023",
                   "follow_news_newspaper_2023", "follow_news_none_2023")

# Subset + convert to factor + drop NA
cat_data <- cleaned_data %>%
  select(all_of(cat_vars_2023)) %>%
  mutate(across(everything(), as.factor)) %>%
  drop_na()

# Cramér’s V matrix
get_cramers_v <- function(x, y) {
  tbl <- table(x, y)
  suppressWarnings(DescTools::CramerV(tbl))
}
cramer_matrix <- outer(cat_vars_2023, cat_vars_2023,
                       Vectorize(function(x, y) get_cramers_v(cat_data[[x]], cat_data[[y]])))
dimnames(cramer_matrix) <- list(cat_vars_2023, cat_vars_2023)

# Keep lower triangle + diagonal
cramer_df <- melt(cramer_matrix)
cramer_df <- cramer_df %>%
  filter(as.numeric(Var1) >= as.numeric(Var2))  # include diagonal

# Plot with diagonal
ggplot(cramer_df, aes(x = Var2, y = Var1, fill = value, label = round(value, 2))) +
  geom_tile(color = "white") +
  geom_text(color = "white", size = 4) +
  scale_fill_gradient2(low = "blue", mid = "purple", high = "red", midpoint = 0.5,
                       limits = c(0, 1), name = "Cramér's V") +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#7 SPEARMAN CORRELATION: ORDINAL VARIABLES_____________________________________
# Packages
library(ggcorrplot)

# Filter 2023 ordinal variables once
ordinal_2023_vars <- grep("_2023$", ordinal_vars, value = TRUE)

# Prepare long-format data for stacked bar plot (drop NAs)
ordinal_long <- cleaned_data %>%
  select(vote_2023, all_of(ordinal_2023_vars)) %>%
  drop_na(vote_2023) %>%
  pivot_longer(cols = -vote_2023, names_to = "variable", values_to = "value") %>%
  drop_na(value)

# Plot: stacked bar for each ordinal var by vote
ggplot(ordinal_long, aes(x = factor(value), fill = factor(vote_2023))) +
  geom_bar(position = "fill") +
  facet_wrap(~ variable, scales = "free_x") +
  scale_fill_manual(values = c("1" = "#E74C3C", "2" = "#2ECC71", "3" = "#3498DB"),
                    labels = c("1" = "Yes", "2" = "No", "3" = "Not Eligible")) +
  labs(
    title = "Distribution of Ordinal Variables by Vote 2023 (2023 only)",
    x = "Response Level", y = "Proportion", fill = "Vote 2023"
  ) +
  theme_minimal()

# Spearman Correlation: 2023 ordinal only
ordinal_data <- cleaned_data %>%
  select(all_of(ordinal_2023_vars)) %>%
  mutate(across(everything(), as.numeric)) %>%
  drop_na()  # ensure correlation uses complete rows

# Compute matrix
spearman_matrix <- cor(ordinal_data, method = "spearman")

# Correlation heatmap
ggcorrplot(spearman_matrix, 
           method = "circle", 
           type = "lower", 
           lab = TRUE, 
           lab_size = 5,                         # larger label text
           colors = c("blue", "white", "red"),
           tl.cex = 10) +                        # only one tl.cex
  theme(
    panel.grid = element_blank(),              # removes inner grid lines
    axis.text = element_text(size = 12)        # increases axis label size
  )


#8 VIF: MULTICOLLINEARITY CHECK________________________________________________
# Packages
library(car)
library(dplyr)

# Identify 2023 numeric and ordinal predictors
numeric_vars_2023 <- grep("^age_2023$", names(cleaned_data), value = TRUE)
ordinal_vars_2023 <- grep("^(satisfaction|confidence)_(work_government|government|parliament|politicians|political_parties|financial_situation|economy)_2023$",
                          names(cleaned_data), value = TRUE)
predictors_2023 <- c(numeric_vars_2023, ordinal_vars_2023)

# Subset data and convert to numeric
vif_data <- cleaned_data %>%
  select(all_of(predictors_2023)) %>%
  mutate(across(everything(), as.numeric)) %>%
  drop_na()  # VIF requires complete cases

# Fit dummy linear model
dummy_target <- rnorm(nrow(vif_data))  # arbitrary continuous target
vif_model <- lm(dummy_target ~ ., data = vif_data)

# Compute and show VIF values
vif_values <- vif(vif_model)
vif_df <- data.frame(variable = names(vif_values), VIF = round(vif_values, 2)) %>%
  arrange(desc(VIF))

print(vif_df)
