# Download packages
library(tidymodels)
library(readr)
library(dplyr)
library(stringr)

#1 LOAD CLEANED DATA AND PREPARE TARGET_______________________________________
data <- read_csv("cleaned_data.csv")

# Optional — only if vote_2023 was not prefiltered
data <- data %>% filter(vote_2023 %in% c(1, 2))
data <- data %>% mutate(vote_2023 = factor(vote_2023, levels = c(1, 2), labels = c("Yes", "No")))



#2 IDENTIFY VARIABLE GROUPS____________________________________________________
numeric_vars <- grep("^age_\\d{4}$", names(data), value = TRUE)

ordinal_vars <- grep("^(satisfaction|confidence)_(work_government|government|parliament|politicians|political_parties|financial_situation|economy)_\\d{4}$",
                     names(data), value = TRUE)

vote_vars <- grep("^vote_\\d{4}$", names(data), value = TRUE)
vote_vars <- setdiff(vote_vars, "vote_2023")

all_predictors <- setdiff(names(data), c("participant", "vote_2023"))
categorical_vars <- setdiff(all_predictors, c(numeric_vars, ordinal_vars, vote_vars))



#3 TIMELINE SPLIT: train (2012–2015), dev (2016–2020), test (2021–2023)_________
get_split_year <- function(row) {
  years <- str_extract(names(row), "_(\\d{4})$") %>%
    na.omit() %>%
    str_remove("_") %>%
    as.numeric()
  year_votes <- sapply(unique(years), function(y) {
    sum(!is.na(row[grepl(paste0("_", y, "$"), names(row))]))
  })
  if (length(year_votes) == 0) return(NA)
  best_year <- as.numeric(names(which.max(year_votes)))
  case_when(
    best_year %in% 2012:2015 ~ "train",
    best_year %in% 2016:2020 ~ "dev",
    best_year %in% 2021:2023 ~ "test",
    TRUE ~ NA_character_
  )
}
data$split <- apply(data, 1, get_split_year)


#4 CREATE TRAIN/DEV/TEST SETS___________________________________________________
train_set <- data %>% filter(split == "train") %>% select(-split)
dev_set   <- data %>% filter(split == "dev") %>% select(-split)
test_set  <- data %>% filter(split == "test") %>% select(-split)

# 💾 (Optional) Save raw split sets
write_csv(train_set, "train_data.csv")
write_csv(dev_set,   "dev_data.csv")
write_csv(test_set,  "test_data.csv")


#5 DEFINE RECIPE FUNCTION FOR PREPROCESSING_____________________________________
create_recipe <- function(df) {
  recipe(vote_2023 ~ ., data = df) %>%
    update_role(participant, new_role = "ID") %>%
    step_mutate_at(all_of(ordinal_vars), fn = as.numeric) %>%
    step_mutate_at(all_of(categorical_vars), fn = ~as.factor(.)) %>%
    step_zv(all_predictors()) %>%
    step_normalize(all_numeric_predictors()) %>%
    step_dummy(all_nominal_predictors(), one_hot = TRUE)
}


#6 PREP + BAKE__________________________________________________________________
prep_and_bake <- function(df) {
  recipe_obj <- create_recipe(df)
  recipe_prep <- prep(recipe_obj)
  bake(recipe_prep, new_data = NULL)
}

baked_train <- prep_and_bake(train_set)
baked_dev   <- prep_and_bake(dev_set)
baked_test  <- prep_and_bake(test_set)


#7 SAVE FINAL MODEL-READY DATASETS______________________________________________
write_csv(baked_train, "preprocessed_train.csv")
write_csv(baked_dev,   "preprocessed_dev.csv")
write_csv(baked_test,  "preprocessed_test.csv")
