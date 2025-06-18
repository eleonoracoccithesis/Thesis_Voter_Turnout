# Load required libraries
library(tidyverse)
library(caret)

#1 LOAD DATA____________________________________________________________________
cleaned_data <- read_csv("cleaned_data.csv")


#2 KEEP ALL VOTING HISTORY, ESCLUDE FROM PIVOT__________________________________
vote_static <- cleaned_data %>%
  select(participant, vote_2012, vote_2017, vote_2021, vote_2023)


#3 PIVOT YEAR-SPECITIFIC FEATURES INTO LONG FORMAT______________________________
long_data <- cleaned_data %>%
  select(-vote_2012, -vote_2017, -vote_2021, -vote_2023) %>%  # exclude vote
  #columns before pivot
  pivot_longer(
    cols = matches("_20(1[3-9]|2[0-3])$"),  # match all year-suffixed features
    names_to = c("feature", "year"),
    names_pattern = "^(.*)_(\\d{4})$"
  ) %>%
  pivot_wider(names_from = feature, values_from = value) %>%
  mutate(year = as.integer(year)) %>%
  left_join(vote_static, by = "participant")  # add back static voting vars


#4 FILTER ROWS WHERE VOTE_2023 EXISTS___________________________________________
long_data <- long_data %>%
  filter(!is.na(vote_2023))


#5 ASSIGN TEMPORAL SPLIT________________________________________________________
long_data <- long_data %>%
  mutate(split = case_when(
    year %in% 2013:2016 ~ "train",
    year %in% 2017:2020 ~ "val",
    year %in% 2021:2022 ~ "test",
    year == 2023        ~ "final_test"
  ))


#6 DROP FUTURE VOTE VARIABLES PER SPLIT_________________________________________
long_data <- long_data %>%
  mutate(
    vote_2017 = ifelse(split %in% c("train"), NA, vote_2017),
    vote_2021 = ifelse(split %in% c("train", "val"), NA, vote_2021)
  )


#7 STANDARDIZE ALL NUMERIC FEATURES (EXCLUDE IDs AN TARGETS)____________________
exclude_cols <- c("participant", "year", "split", "vote_2012", "vote_2017", 
                  "vote_2021", "vote_2023")
feature_cols <- setdiff(names(long_data), exclude_cols)

# Fit scaler only on training features
preproc <- preProcess(long_data %>% filter(split == "train") %>% 
                        select(all_of(feature_cols)),
                      method = c("center", "scale"))

# Apply to all features
scaled_features <- predict(preproc, long_data[, feature_cols])


#8 FINAL DATASET WITH UNSCALED COLUMNS PRESERVED________________________________
preprocessed_data <- bind_cols(long_data[, exclude_cols], scaled_features)


#9 GENERATE SPLIT SET___________________________________________________________
train_final <- preprocessed_data %>% filter(split == "train") %>% select(-vote_2017, 
                                                                  -vote_2021)
val_final   <- preprocessed_data %>% filter(split == "val")   %>% select(-vote_2021)
test_final  <- preprocessed_data %>% filter(split == "test")
final_test_2023 <- preprocessed_data %>% filter(split == "final_test")


# Save data to CSV file
write_csv(train_final, "train_final.csv")
write_csv(val_final, "val_final.csv")
write_csv(test_final, "test_final.csv")
write_csv(final_test_2023, "final_test_2023.csv")
