
## OverWrite the source datasets and add parent2_name_EN and gender N->F. DO JUST ONCE! #####
# EH30 <- read_csv("source_data/EH30_diagnosis_gender_year_age_group.csv", col_types = readr::cols())
# BB <- read_csv("source_data/GI_diagnosis_gender_year_age_group.csv", col_types = readr::cols())
# BB1 <- read_csv("source_data/GI_diagnosis_gender_year_age_group_first.csv", col_types = readr::cols())
# BB2 <- read_csv("source_data/GI_diagnosis_gender_year_age_group_second.csv", col_types = readr::cols())
# icd10_lookup <- readr::read_csv("icd10_category_EN.csv", col_types = readr::cols())
# icd10_chapter_lookup <- readr::read_csv("ICD10_chapters.csv", col_types = readr::cols())
# process_and_save <- function(filepath, icd10_chapter_lookup, icd10_lookup) {
#   data <- readr::read_csv(filepath, col_types = readr::cols())
#
#   # Apply all transformations in a single pipeline
#   transformed_data <- data %>%
#     dplyr::mutate(
#       gender_EN = dplyr::recode(gender, "N" = "F", "M" = "M")
#     ) %>%
#     # 3. Join with ICD-10 chapter lookup table
#     dplyr::left_join(icd10_chapter_lookup, by = c("parent0_code" = "parent0_code")) %>%
#
#     # 4. Join with specific ICD-10 diagnosis lookup table
#     dplyr::left_join(icd10_lookup, by = c("parent2_code" = "parent2_code"))
#
#   # Write the transformed data back to the original file path,
#   # effectively overwriting the original source file.
#   readr::write_csv(transformed_data, filepath)
#
#   cat(sprintf("Successfully processed and overwrote: %s\n", filepath))
# }
#
#
# # --- 4. Execute the Transformations ---
############
############
# Workflow STEP 1
# Source Data update with gender_EN, parent2_name_EN, parent0_name_EN, short_name
############
############

# # Define the list of files to process
# file_paths <- c(
#   "source_data/EH30_diagnosis_gender_year_age_group.csv",
#   "source_data/GI_diagnosis_gender_year_age_group.csv",
#   "source_data/GI_diagnosis_gender_year_age_group_first.csv",
#   "source_data/GI_diagnosis_gender_year_age_group_second.csv"
# )
# # Use a loop to apply the function to every file
# for (path in file_paths) {
#   tryCatch({
#     process_and_save(path, icd10_chapter_lookup, icd10_lookup)
#   }, error = function(e) {
#     # Print an error message if a file fails to process
#     cat(sprintf("--- ERROR processing %s: %s ---\n", path, e$message))
#   })
# }
#
# # (Optional: Clean up dummy files/directory after successful run)
# # unlink("source_data", recursive = TRUE)
#
# # Verification check (optional)
# # View a sample of one of the newly written files to confirm the new columns
# cat("\n--- Sample of the first processed file ---\n")
# read_csv(file_paths[1], col_types = readr::cols()) %>%
#   arrange(parent2_name) %>%
#   filter(gender_EN == "F", year=="2013", age_group=="10-19") %>%
#   head(3) %>%
#   print()
#
#

