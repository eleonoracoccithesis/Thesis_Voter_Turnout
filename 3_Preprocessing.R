# Load required libraries
library(tidyverse)
library(caret)

#1 LOAD DATA____________________________________________________________________
cleaned_data <- read_csv("cleaned_data.csv")


#2 KEEP ALL VOTING HISTORY, EXCLUDE FROM PIVOT__________________________________
vote_static <- cleaned_data %>%
  select(participant, vote_2012, vote_2017, vote_2021, vote_2023)


#3 PIVOT YEAR-SPECIFIC FEATURES INTO LONG FORMAT________________________________
long_data <- cleaned_data %>%
  select(-vote_2012, -vote_2017, -vote_2021, -vote_2023) %>%
  pivot_longer(
    cols = matches("_20(1[3-9]|2[0-3])$"),
    names_to = c("feature", "year"),
    names_pattern = "^(.*)_(\\d{4})$"
  ) %>%
  pivot_wider(names_from = feature, values_from = value) %>%
  mutate(year = as.integer(year)) %>%
  left_join(vote_static, by = "participant")


#4 FIX PRIOR-VOTE DATA LEAKAGE AND ADD is_first_timer BEFORE FILTERING__________
long_data <- long_data %>%
  mutate(
    vote_2012 = ifelse(is.na(vote_2012), "Unknown", as.character(vote_2012)),
    vote_2017 = ifelse(is.na(vote_2017), "Unknown", as.character(vote_2017)),
    vote_2021 = ifelse(is.na(vote_2021), "Unknown", as.character(vote_2021)),
    is_first_timer = case_when(
      year == 2013 ~ 1,
      year == 2014 & vote_2012 == "Unknown" ~ 1,
      year == 2015 & vote_2012 == "Unknown" ~ 1,
      year == 2016 & vote_2012 == "Unknown" ~ 1,
      year == 2017 & vote_2012 == "Unknown" ~ 1,
      year %in% 2018:2020 & vote_2012 == "Unknown" & vote_2017 == "Unknown" ~ 1,
      year %in% 2021:2022 & vote_2012 == "Unknown" & vote_2017 == "Unknown" & vote_2021 == "Unknown" ~ 1,
      year == 2023 & vote_2012 == "Unknown" & vote_2017 == "Unknown" & vote_2021 == "Unknown" ~ 1,
      TRUE ~ 0
    )
  )

#5 NOW FILTER OUT vote_2023 MISSING
long_data <- long_data %>%
  filter(vote_2023 != "Unknown")


#6 ASSIGN TEMPORAL SPLIT________________________________________________________
long_data <- long_data %>%
  mutate(split = case_when(
    year %in% 2013:2016 ~ "train",
    year %in% 2017:2020 ~ "val",
    year %in% 2021:2022 ~ "test",
    year == 2023        ~ "final_test"
  ))


#7 DROP FUTURE VOTE VARIABLES PER SPLIT_________________________________________
long_data <- long_data %>%
  mutate(
    vote_2017 = ifelse(split %in% c("train"), NA, vote_2017),
    vote_2021 = ifelse(split %in% c("train", "val"), NA, vote_2021)
  )


#8 STANDARDIZE ALL NUMERIC FEATURES_____________________________________________
exclude_cols <- c("participant", "year", "split", "vote_2012", "vote_2017", 
                  "vote_2021", "vote_2023", "is_first_timer")
feature_cols <- setdiff(names(long_data), exclude_cols)

# Fit scaler on training
preproc <- preProcess(long_data %>% filter(split == "train") %>% 
                        select(all_of(feature_cols)),
                      method = c("center", "scale"))

# Apply scaling
scaled_features <- predict(preproc, long_data[, feature_cols])


#9 FINAL DATASET MERGED_________________________________________________________
preprocessed_data <- bind_cols(long_data[, exclude_cols], scaled_features)


#10 CREATE SPLIT FILES__________________________________________________________
train_final <- preprocessed_data %>% filter(split == "train") %>%
  select(-vote_2017, -vote_2021)
val_final   <- preprocessed_data %>% filter(split == "val") %>%
  select(-vote_2021)
test_final  <- preprocessed_data %>% filter(split == "test")
final_test_2023 <- preprocessed_data %>% filter(split == "final_test")


#11 SAVE FINAL FILES____________________________________________________________
write_csv(train_final, "train_final.csv")
write_csv(val_final, "val_final.csv")
write_csv(test_final, "test_final.csv")
write_csv(final_test_2023, "final_test_2023.csv")

