#LOAD PACKAGES _________________________________________________________________
library(dplyr)
library(ggplot2)
library(tidyr)
library(forcats)
library(ggthemes)
library(gridExtra)
library(DescTools)
library(reshape2)
library(ggcorrplot)
library(car)


# 1. VOTE_2023 DISTRIBUTION BAR PLOT ___________________________________________
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
                    na.translate = TRUE,  # include NA in fill
                    na.value = "gray70") +
  labs(x = "Vote Code", y = "Count", fill = "vote_2023") +
  theme_minimal() +
  theme(panel.grid = element_blank())



# 2. MISSING VALUES _______________________________________________________________________
# Count missing values per column
missing_df <- final_data %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "missing_count") %>%
  arrange(desc(missing_count))  # optional: sort from most to least missing

# Add missing percentages
missing_df <- missing_df %>%
  mutate(missing_percent = round(100 * missing_count / nrow(final_data), 1))

# Save
write.csv(missing_df, "missing_df.csv", row.names = FALSE)


# 3. IMPUTATION ___________________________________________________________________________

# Exclude vote variables and is_first_timer from missingness filtering
excluded_from_check <- c("vote_2012", "vote_2017", "vote_2021", "vote_2023", "is_first_timer")
missing_check_vars <- setdiff(names(final_data), excluded_from_check)

# Drop rows with >60% missing values (among remaining variables)
threshold <- 0.6 * length(missing_check_vars)
rows_to_drop <- which(rowSums(is.na(final_data[, missing_check_vars])) > threshold)

# Apply filtering
cleaned_data <- final_data[-rows_to_drop, ]

# Preserve is_first_timer explicitly (in case of reordering or drop)
cleaned_data$is_first_timer <- final_data$is_first_timer[-rows_to_drop]

# Confirm preservation
stopifnot("is_first_timer" %in% names(cleaned_data))

# Identify variable types
numeric_vars <- grep("^age_", names(cleaned_data), value = TRUE)
ordinal_vars <- grep("^(satisfaction|confidence)_(work_government|government|parliament|politicians|political_parties|financial_situation|economy)_\\d{4}",
                     names(cleaned_data), value = TRUE)
vote_vars <- grep("^vote_\\d{4}$", names(cleaned_data), value = TRUE)
target_var <- "vote_2023"
vote_vars <- setdiff(vote_vars, target_var)
categorical_vars <- setdiff(names(cleaned_data), c(numeric_vars, ordinal_vars, vote_vars, target_var, "is_first_timer"))

# Mode function
get_mode <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}

# Numeric imputation (random sampling)
for (col in numeric_vars) {
  missing_idx <- which(is.na(cleaned_data[[col]]))
  sampled_vals <- sample(cleaned_data[[col]][!is.na(cleaned_data[[col]])],
                         length(missing_idx), replace = TRUE)
  cleaned_data[[col]][missing_idx] <- sampled_vals
}

# Ordinal imputation (median)
for (col in ordinal_vars) {
  cleaned_data[[col]][is.na(cleaned_data[[col]])] <- median(as.numeric(cleaned_data[[col]]), na.rm = TRUE)
}

# Categorical and vote imputation (mode)
for (col in c(categorical_vars, vote_vars)) {
  cleaned_data[[col]][is.na(cleaned_data[[col]])] <- get_mode(cleaned_data[[col]])
}

# Final check
cat("Final row count:", nrow(cleaned_data), "\n")
cat("Remaining missing values:", sum(is.na(cleaned_data)), "\n")

# Optional: sanity check for first-time voters
cat("is_first_timer distribution in cleaned_data:\n")
print(table(cleaned_data$is_first_timer))

# Save to file
write.csv(cleaned_data, "cleaned_data.csv", row.names = FALSE)



# 4. AGE DISTRIBUTION BEFORE AND AFTER IMPUTATION _________________________________________
age_vars <- grep("^age_\\d{4}$", names(final_data), value = TRUE)

age_long_before <- final_data %>%
  select(all_of(age_vars)) %>%
  pivot_longer(cols = everything(), names_to = "year", values_to = "age") %>%
  mutate(year = gsub("age_", "", year), imputed = "Before")

age_long_after <- cleaned_data %>%
  select(all_of(age_vars)) %>%
  pivot_longer(cols = everything(), names_to = "year", values_to = "age") %>%
  mutate(year = gsub("age_", "", year), imputed = "After")

age_long_combined <- bind_rows(age_long_before, age_long_after)

ggplot(age_long_combined, aes(x = age, fill = imputed)) +
  geom_histogram(position = "identity", bins = 40, alpha = 0.6) +
  facet_wrap(~year, scales = "free_y") +
  scale_fill_manual(values = c("Before" = "#8888FF", "After" = "#4CAF50")) +
  labs(title = "Age Distribution: Before vs After Imputation", x = "Age", y = "Count") +
  theme_minimal()


# 5. AGE AND VOTE 2023: DENSITY PLOT ______________________________________________________
filtered_data <- cleaned_data[cleaned_data$vote_2023 %in% c(1, 2), ]

ggplot(filtered_data, aes(x = age_2023, fill = as.factor(vote_2023))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("1" = "#2ECC71", "2" = "#3498DB"),
                    labels = c("1" = "Yes", "2" = "No")) +
  labs(x = "Age 2023", y = "Density", fill = "vote_2023") +
  theme_minimal()


# 6. CRAMER’S V FOR CATEGORICAL VARIABLES ________________________________________________
cat_vars_2023 <- c("vote_2023", "interest_news_2023", "interest_politics_2023",
                   "follow_news_television_2023", "follow_news_internet_2023",
                   "follow_news_newspaper_2023", "follow_news_none_2023")

cat_data <- cleaned_data %>%
  select(all_of(cat_vars_2023)) %>%
  mutate(across(everything(), as.factor)) %>%
  drop_na()

get_cramers_v <- function(x, y) {
  tbl <- table(x, y)
  suppressWarnings(DescTools::CramerV(tbl))
}

cramer_matrix <- outer(cat_vars_2023, cat_vars_2023,
                       Vectorize(function(x, y) get_cramers_v(cat_data[[x]], cat_data[[y]])))
dimnames(cramer_matrix) <- list(cat_vars_2023, cat_vars_2023)

cramer_df <- melt(cramer_matrix) %>%
  filter(as.numeric(Var1) >= as.numeric(Var2))

ggplot(cramer_df, aes(x = Var2, y = Var1, fill = value, label = round(value, 2))) +
  geom_tile(color = "white") +
  geom_text(color = "white", size = 4) +
  scale_fill_gradient2(low = "blue", mid = "purple", high = "red", midpoint = 0.5,
                       limits = c(0, 1), name = "Cramér's V") +
  theme_minimal() +
  labs(x = NULL, y = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 7. SPEARMAN CORRELATION FOR ORDINAL VARIABLES ___________________________________________
ordinal_2023_vars <- grep("_2023$", ordinal_vars, value = TRUE)

ordinal_data <- cleaned_data %>%
  select(all_of(ordinal_2023_vars)) %>%
  mutate(across(everything(), as.numeric)) %>%
  drop_na()

spearman_matrix <- cor(ordinal_data, method = "spearman")

ggcorrplot(spearman_matrix,
           method = "circle",
           type = "lower",
           lab = TRUE,
           lab_size = 5,
           colors = c("blue", "white", "red"),
           tl.cex = 10) +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 12))


# 8. VIF: MULTICOLLINEARITY CHECK _________________________________________________________
numeric_vars_2023 <- grep("^age_2023$", names(cleaned_data), value = TRUE)
ordinal_vars_2023 <- grep("^(satisfaction|confidence)_(work_government|government|parliament|politicians|political_parties|financial_situation|economy)_2023$",
                          names(cleaned_data), value = TRUE)
predictors_2023 <- c(numeric_vars_2023, ordinal_vars_2023)

vif_data <- cleaned_data %>%
  select(all_of(predictors_2023)) %>%
  mutate(across(everything(), as.numeric)) %>%
  drop_na()

dummy_target <- rnorm(nrow(vif_data))
vif_model <- lm(dummy_target ~ ., data = vif_data)

vif_values <- vif(vif_model)
vif_df <- data.frame(variable = names(vif_values), VIF = round(vif_values, 2)) %>%
  arrange(desc(VIF))
print(vif_df)

