#1. MERGE VARIABLES INTO ONE DATASET ___________________________________________
# Download packages
library(haven)
library(dplyr)
library(purrr)

# Set working directory 
setwd("C:/Users/cocci/Downloads/Study material/DSS_thesis_2025/Thesis")

# List all .sav files
sav_files <- list.files(pattern = "\\.sav$")

# Read each file and ensure nomem_encr exists
read_file_safe <- function(file) {
  df <- read_sav(file)
  if (!"nomem_encr" %in% names(df)) {
    stop(paste("Missing 'nomem_encr' in:", file))
  }
  return(df)
}

# Read and merge all files by nomem_encr
merged_data <- sav_files %>%
  map(read_file_safe) %>%
  reduce(full_join, by = "nomem_encr")

# Preview 
print(dim(merged_data))
head(merged_data)

# Save merged_data to CSV file
write.csv(merged_data, "merged_data.csv", row.names = FALSE)


#2. SELECT VARIABLES ___________________________________________________________
# Create variable list 

vars_to_keep <- c(
  # 2013
  "nomem_encr", "cv13f160", "cv13f001", "cv13f008", "cv13f002", "cv13f003", "cv13f166", "cv13f006",
  "cv13f012", "cv13f013", "cv13f014", "cv13f017", "cv13f018", "cv13f030", "cv13f031", "cv13f034",
  "cv13f035", "cv13f053", "ci13f006", "ci13f007",
  # 2014
  "cv14g160", "cv14g001", "cv14g008", "cv14g002", "cv14g003", "cv14g166", "cv14g006", "cv14g012",
  "cv14g013", "cv14g014", "cv14g017", "cv14g018", "cv14g030", "cv14g031", "cv14g034", "cv14g035",
  "cv14g053", "ci14g006", "ci14g007",
  # 2016
  "cv16h160", "cv16h001", "cv16h008", "cv16h002", "cv16h003", "cv16h166", "cv16h006", "cv16h012",
  "cv16h013", "cv16h014", "cv16h017", "cv16h018", "cv16h030", "cv16h031", "cv16h034", "cv16h035",
  "cv16h053", "ci15h006", "ci15h007",
  # 2017
  "cv17i160", "cv17i001", "cv17i008", "cv17i002", "cv17i003", "cv17i166", "cv17i006", "cv17i012",
  "cv17i013", "cv17i014", "cv17i017", "cv17i018", "cv17i030", "cv17i031", "cv17i034", "cv17i035",
  "cv17i053", "ci16i006", "ci16i007",
  # 2018
  "cv18j160", "cv18j001", "cv18j008", "cv18j002", "cv18j003", "cv18j166", "cv18j006", "cv18j013",
  "cv18j014", "cv18j017", "cv18j018", "cv18j030", "cv18j031", "cv18j034", "cv18j035", "cv18j053",
  "ci17j006", "ci17j007",
  # 2019
  "cv19k160", "cv19k001", "cv19k008", "cv19k002", "cv19k003", "cv19k166", "cv19k006", "cv19k012",
  "cv19k013", "cv19k014", "cv19k017", "cv19k018", "cv19k030", "cv19k031", "cv19k034", "cv19k035",
  "cv19k053", "ci18k006", "ci18k007",
  # 2020
  "cv20l160", "cv20l001", "cv20l008", "cv20l002", "cv20l003", "cv20l166", "cv20l006", "cv20l012",
  "cv20l013", "cv20l014", "cv20l017", "cv20l018", "cv20l030", "cv20l031", "cv20l034", "cv20l035",
  "cv20l053", "ci19l006", "ci19l007",
  # 2021
  "cv21m160", "cv21m001", "cv21m008", "cv21m002", "cv21m003", "cv21m166", "cv21m006", "cv21m012",
  "cv21m013", "cv21m014", "cv21m017", "cv21m018", "cv21m030", "cv21m031", "cv21m034", "cv21m035",
  "cv21m053", "ci20m006", "ci20m007",
  # 2022
  "cv22n160", "cv22n001", "cv22n008", "cv22n002", "cv22n003", "cv22n166", "cv22n006", "cv22n012",
  "cv22n013", "cv22n014", "cv22n017", "cv22n018", "cv22n030", "cv22n031", "cv22n034", "cv22n035",
  "cv22n053", "ci21n006", "ci21n007",
  # 2023
  "cv23o160", "cv23o001", "cv23o008", "cv23o002", "cv23o003", "cv23o166", "cv23o006", "cv23o012",
  "cv23o013", "cv23o014", "cv23o017", "cv23o018", "cv23o030", "cv23o031", "cv23o034", "cv23o035",
  "cv23o053", "ci22o006", "ci22o007",
  # 2024
  "cv24p160", "cv24p001", "cv24p008", "cv24p002", "cv24p003", "cv24p166", "cv24p006", "cv24p012",
  "cv24p013", "cv24p014", "cv24p017", "cv24p018", "cv24p030", "cv24p031", "cv24p034", "cv24p035",
  "cv24p053", "ci23p006", "ci23p007"
)

# Filter the merged data
filtered_data <- merged_data %>%
  select(any_of(vars_to_keep))  # Safely select only these columns

# Preview
print(dim(filtered_data))
head(filtered_data)

# Save filtered_data to CSV file
write.csv(filtered_data, "filtered_data.csv", row.names = FALSE)


#3 AVOID DUPLICATE VOTERS_______________________________________________________
# Mapping of voting variables by election year
voting_vars_by_year <- list(
  "2012" = c("cv13f053", "cv14g053", "cv16h053", "cv17i053", "cv18j053"),
  "2017" = c("cv19k053", "cv20l053", "cv21m053"),
  "2021" = c("cv22n053", "cv23o053"),
  "2023" = c("cv24p053")
)

# Create composite vote columns per year
for (year in names(voting_vars_by_year)) {
  vote_vars <- voting_vars_by_year[[year]]
  filtered_data[[paste0("vote_", year)]] <- apply(
    filtered_data[, vote_vars],
    1,
    function(x) x[!is.na(x)][1]  # first non-NA
  )
}

# Identify first year a participant answered a vote question
filtered_data$first_voting_year <- NA
for (year in names(voting_vars_by_year)) {
  vote_col <- paste0("vote_", year)
  has_answer <- !is.na(filtered_data[[vote_col]])
  filtered_data$first_voting_year[is.na(filtered_data$first_voting_year) & has_answer] <- year
}


# Keep first observation per participant, even if no voting history
final_data <- filtered_data %>%
  arrange(nomem_encr, first_voting_year) %>%
  distinct(nomem_encr, .keep_all = TRUE)


# Drop raw vote variables and first_voting_year
vars_to_remove <- unlist(voting_vars_by_year)
final_data <- final_data %>% select(-all_of(vars_to_remove), -first_voting_year)

# Preview result
head(final_data)

# Save final_data to CSV file
write.csv(final_data, "final_data.csv", row.names = FALSE)


#4 CHANGE VARIABLES NAMES FOR CLARITY___________________________________________
# Rename vector (corrected: new names as keys)
rename_vector <- c(
  "participant" = "nomem_encr",
  
  # 2013
  "age_2013" = "cv13f160",
  "satisfaction_work_government_2013" = "cv13f001",
  "interest_news_2013" = "cv13f008",
  "follow_news_television_2013" = "cv13f002",
  "follow_news_internet_2013" = "cv13f003",
  "follow_news_newspaper_2013" = "cv13f166",
  "follow_news_none_2013" = "cv13f006",
  "interest_politics_2013" = "cv13f012",
  "confidence_government_2013" = "cv13f013",
  "confidence_parliament_2013" = "cv13f014",
  "confidence_politicians_2013" = "cv13f017",
  "confidence_political_parties_2013" = "cv13f018",
  "satisfaction_government_2013" = "cv13f030",
  "satisfaction_parliament_2013" = "cv13f031",
  "satisfaction_politicians_2013" = "cv13f034",
  "satisfaction_political_parties_2013" = "cv13f035",
  "satisfaction_financial_situation_2013" = "ci13f006",
  "satisfaction_economy_2013" = "ci13f007",
  
  # 2014
  "age_2014" = "cv14g160",
  "satisfaction_work_government_2014" = "cv14g001",
  "interest_news_2014" = "cv14g008",
  "follow_news_television_2014" = "cv14g002",
  "follow_news_internet_2014" = "cv14g003",
  "follow_news_newspaper_2014" = "cv14g166",
  "follow_news_none_2014" = "cv14g006",
  "interest_politics_2014" = "cv14g012",
  "confidence_government_2014" = "cv14g013",
  "confidence_parliament_2014" = "cv14g014",
  "confidence_politicians_2014" = "cv14g017",
  "confidence_political_parties_2014" = "cv14g018",
  "satisfaction_government_2014" = "cv14g030",
  "satisfaction_parliament_2014" = "cv14g031",
  "satisfaction_politicians_2014" = "cv14g034",
  "satisfaction_political_parties_2014" = "cv14g035",
  "satisfaction_financial_situation_2014" = "ci14g006",
  "satisfaction_economy_2014" = "ci14g007",
  
  # 2015
  "age_2015" = "cv16h160",
  "satisfaction_work_government_2015" = "cv16h001",
  "interest_news_2015" = "cv16h008",
  "follow_news_television_2015" = "cv16h002",
  "follow_news_internet_2015" = "cv16h003",
  "follow_news_newspaper_2015" = "cv16h166",
  "follow_news_none_2015" = "cv16h006",
  "interest_politics_2015" = "cv16h012",
  "confidence_government_2015" = "cv16h013",
  "confidence_parliament_2015" = "cv16h014",
  "confidence_politicians_2015" = "cv16h017",
  "confidence_political_parties_2015" = "cv16h018",
  "satisfaction_government_2015" = "cv16h030",
  "satisfaction_parliament_2015" = "cv16h031",
  "satisfaction_politicians_2015" = "cv16h034",
  "satisfaction_political_parties_2015" = "cv16h035",
  "satisfaction_financial_situation_2015" = "ci15h006",
  "satisfaction_economy_2015" = "ci15h007",
  
  # 2016
  "age_2016" = "cv17i160",
  "satisfaction_work_government_2016" = "cv17i001",
  "interest_news_2016" = "cv17i008",
  "follow_news_television_2016" = "cv17i002",
  "follow_news_internet_2016" = "cv17i003",
  "follow_news_newspaper_2016" = "cv17i166",
  "follow_news_none_2016" = "cv17i006",
  "interest_politics_2016" = "cv17i012",
  "confidence_government_2016" = "cv17i013",
  "confidence_parliament_2016" = "cv17i014",
  "confidence_politicians_2016" = "cv17i017",
  "confidence_political_parties_2016" = "cv17i018",
  "satisfaction_government_2016" = "cv17i030",
  "satisfaction_parliament_2016" = "cv17i031",
  "satisfaction_politicians_2016" = "cv17i034",
  "satisfaction_political_parties_2016" = "cv17i035",
  "satisfaction_financial_situation_2016" = "ci16i006",
  "satisfaction_economy_2016" = "ci16i007",
  
  # 2017
  "age_2017" = "cv18j160",
  "satisfaction_work_government_2017" = "cv18j001",
  "interest_news_2017" = "cv18j008",
  "follow_news_television_2017" = "cv18j002",
  "follow_news_internet_2017" = "cv18j003",
  "follow_news_newspaper_2017" = "cv18j166",
  "follow_news_none_2017" = "cv18j006",
  "interest_politics_2017" = "cv18j008",
  "confidence_government_2017" = "cv18j013",
  "confidence_parliament_2017" = "cv18j014",
  "confidence_politicians_2017" = "cv18j017",
  "confidence_political_parties_2017" = "cv18j018",
  "satisfaction_government_2017" = "cv18j030",
  "satisfaction_parliament_2017" = "cv18j031",
  "satisfaction_politicians_2017" = "cv18j034",
  "satisfaction_political_parties_2017" = "cv18j035",
  "satisfaction_financial_situation_2017" = "ci17j006",
  "satisfaction_economy_2017" = "ci17j007",
  
  # 2018
  "age_2018" = "cv19k160",
  "satisfaction_work_government_2018" = "cv19k001",
  "interest_news_2018" = "cv19k008",
  "follow_news_television_2018" = "cv19k002",
  "follow_news_internet_2018" = "cv19k003",
  "follow_news_newspaper_2018" = "cv19k166",
  "follow_news_none_2018" = "cv19k006",
  "interest_politics_2018" = "cv19k012",
  "confidence_government_2018" = "cv19k013",
  "confidence_parliament_2018" = "cv19k014",
  "confidence_politicians_2018" = "cv19k017",
  "confidence_political_parties_2018" = "cv19k018",
  "satisfaction_government_2018" = "cv19k030",
  "satisfaction_parliament_2018" = "cv19k031",
  "satisfaction_politicians_2018" = "cv19k034",
  "satisfaction_political_parties_2018" = "cv19k035",
  "satisfaction_financial_situation_2018" = "ci18k006",
  "satisfaction_economy_2018" = "ci18k007",
  
  # 2019
  "age_2019" = "cv20l160",
  "satisfaction_work_government_2019" = "cv20l001",
  "interest_news_2019" = "cv20l008",
  "follow_news_television_2019" = "cv20l002",
  "follow_news_internet_2019" = "cv20l003",
  "follow_news_newspaper_2019" = "cv20l166",
  "follow_news_none_2019" = "cv20l006",
  "interest_politics_2019" = "cv20l012",
  "confidence_government_2019" = "cv20l013",
  "confidence_parliament_2019" = "cv20l014",
  "confidence_politicians_2019" = "cv20l017",
  "confidence_political_parties_2019" = "cv20l018",
  "satisfaction_government_2019" = "cv20l030",
  "satisfaction_parliament_2019" = "cv20l031",
  "satisfaction_politicians_2019" = "cv20l034",
  "satisfaction_political_parties_2019" = "cv20l035",
  "satisfaction_financial_situation_2019" = "ci19l006",
  "satisfaction_economy_2019" = "ci19l007",
  
  # 2020
  "age_2020" = "cv21m160",
  "satisfaction_work_government_2020" = "cv21m001",
  "interest_news_2020" = "cv21m008",
  "follow_news_television_2020" = "cv21m002",
  "follow_news_internet_2020" = "cv21m003",
  "follow_news_newspaper_2020" = "cv21m166",
  "follow_news_none_2020" = "cv21m006",
  "interest_politics_2020" = "cv21m012",
  "confidence_government_2020" = "cv21m013",
  "confidence_parliament_2020" = "cv21m014",
  "confidence_politicians_2020" = "cv21m017",
  "confidence_political_parties_2020" = "cv21m018",
  "satisfaction_government_2020" = "cv21m030",
  "satisfaction_parliament_2020" = "cv21m031",
  "satisfaction_politicians_2020" = "cv21m034",
  "satisfaction_political_parties_2020" = "cv21m035",
  "satisfaction_financial_situation_2020" = "ci20m006",
  "satisfaction_economy_2020" = "ci20m007",
  
  # 2021
  "age_2021" = "cv22n160",
  "satisfaction_work_government_2021" = "cv22n001",
  "interest_news_2021" = "cv22n008",
  "follow_news_television_2021" = "cv22n002",
  "follow_news_internet_2021" = "cv22n003",
  "follow_news_newspaper_2021" = "cv22n166",
  "follow_news_none_2021" = "cv22n006",
  "interest_politics_2021" = "cv22n012",
  "confidence_government_2021" = "cv22n013",
  "confidence_parliament_2021" = "cv22n014",
  "confidence_politicians_2021" = "cv22n017",
  "confidence_political_parties_2021" = "cv22n018",
  "satisfaction_government_2021" = "cv22n030",
  "satisfaction_parliament_2021" = "cv22n031",
  "satisfaction_politicians_2021" = "cv22n034",
  "satisfaction_political_parties_2021" = "cv22n035",
  "satisfaction_financial_situation_2021" = "ci21n006",
  "satisfaction_economy_2021" = "ci21n007",
  
  # 2022
  "age_2022" = "cv23o160",
  "satisfaction_work_government_2022" = "cv23o001",
  "interest_news_2022" = "cv23o008",
  "follow_news_television_2022" = "cv23o002",
  "follow_news_internet_2022" = "cv23o003",
  "follow_news_newspaper_2022" = "cv23o166",
  "follow_news_none_2022" = "cv23o006",
  "interest_politics_2022" = "cv23o012",
  "confidence_government_2022" = "cv23o013",
  "confidence_parliament_2022" = "cv23o014",
  "confidence_politicians_2022" = "cv23o017",
  "confidence_political_parties_2022" = "cv23o018",
  "satisfaction_government_2022" = "cv23o030",
  "satisfaction_parliament_2022" = "cv23o031",
  "satisfaction_politicians_2022" = "cv23o034",
  "satisfaction_political_parties_2022" = "cv23o035",
  "satisfaction_financial_situation_2022" = "ci22o006",
  "satisfaction_economy_2022" = "ci22o007",
  
  # 2023
  "age_2023" = "cv24p160",
  "satisfaction_work_government_2023" = "cv24p001",
  "interest_news_2023" = "cv24p008",
  "follow_news_television_2023" = "cv24p002",
  "follow_news_internet_2023" = "cv24p003",
  "follow_news_newspaper_2023" = "cv24p166",
  "follow_news_none_2023" = "cv24p006",
  "interest_politics_2023" = "cv24p012",
  "confidence_government_2023" = "cv24p013",
  "confidence_parliament_2023" = "cv24p014",
  "confidence_politicians_2023" = "cv24p017",
  "confidence_political_parties_2023" = "cv24p018",
  "satisfaction_government_2023" = "cv24p030",
  "satisfaction_parliament_2023" = "cv24p031",
  "satisfaction_politicians_2023" = "cv24p034",
  "satisfaction_political_parties_2023" = "cv24p035",
  "satisfaction_financial_situation_2023" = "ci23p006",
  "satisfaction_economy_2023" = "ci23p007"
)

# Apply rename
final_data <- final_data %>% rename(any_of(rename_vector))

# Preview
head(final_data)

# Save final_data to CSV file
write.csv(final_data, "final_data.csv", row.names = FALSE)
