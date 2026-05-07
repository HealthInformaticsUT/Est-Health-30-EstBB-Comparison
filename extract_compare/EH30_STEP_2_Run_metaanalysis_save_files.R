########## STEP 2: Meta-Analysis two-fold ########

# --- STEP 0: Read in Source Data ---
### Compare EH30 and EstBB
upload1 <- read_csv("source_data/EH30_diagnosis_gender_year_age_group.csv", col_types = readr::cols())
upload2 <- read_csv("source_data/GI_diagnosis_gender_year_age_group.csv", col_types = readr::cols())
output_name_base <- "EH30d1_EstBBd2"

### Compare EH30 and EstBB1
upload1 <- read_csv("source_data/EH30_diagnosis_gender_year_age_group.csv", col_types = readr::cols())
upload2 <- read_csv("source_data/GI_diagnosis_gender_year_age_group_first.csv", col_types = readr::cols())
output_name_base <- "EH30d1_EstBB1d2"

## Compare EH30 and EstBB2
upload1 <- read_csv("source_data/EH30_diagnosis_gender_year_age_group.csv", col_types = readr::cols())
upload2 <- read_csv("source_data/GI_diagnosis_gender_year_age_group_second.csv", col_types = readr::cols())
output_name_base <- "EH30d1_EstBB2d2"

### Compare EstBB1 and EstBB2
upload1 <- read_csv("source_data/GI_diagnosis_gender_year_age_group_first.csv", col_types = readr::cols())
upload2 <- read_csv("source_data/GI_diagnosis_gender_year_age_group_second.csv", col_types = readr::cols())
output_name_base <- "EstBB1d1_EstBB2d2"

# --- STEP 1: Run Meta-Analysis 1 ---
# This function now runs AND saves its own results.
dataMeta_All <- fileGen_data1_data2_diff_meta_Log_pval(
  upload1,
  upload2,
  output_name_base,
  output_dir = "comp_data"
)
# unique(dataMeta_All$year)
# unique(dataMeta_All$gender_EN)
# unique(dataMeta_All$parent2_code)
# unique(dataMeta_All$age_group)

# --- STEP 2: Run Meta-Analysis 2 ---
# This function takes the *entire dataframe* from step 1,
# automatically filters for "Meta" rows, runs, and saves its results.
results_parent2 <- perform_meta_analysis2_genders(
  meta_1_output_df = dataMeta_All,
  output_name_base, # Base name for the output file
  output_dir       = "comp_data"
)
