# build_demo_source_data.R
#
# Produce small demo source CSVs that
# preserve the exact schema of source_data/ but cover only the top
# 15 F-chapter + top 15 I-chapter parent2_codes shared across all
# four diagnosis files.
#
# Output: demo_comparison_script/demo_source_data/
#   - EH30_diagnosis_gender_year_age_group.csv          (filtered)
#   - GI_diagnosis_gender_year_age_group.csv            (filtered)
#   - GI_diagnosis_gender_year_age_group_first.csv      (filtered)
#   - GI_diagnosis_gender_year_age_group_second.csv     (filtered)
#   - EH30_patient_birthyear_gender.csv                 (copied as-is)
#   - GI_patient_birthyear_gender.csv                   (copied as-is)
#   - GI_patient_birthyear_gender_first.csv             (copied as-is)
#   - GI_patient_birthyear_gender_second.csv            (copied as-is)
#
# The filter keeps:
#   - chapter rollup rows (parent0 set, parent1 / parent2 NA) in F + I
#   - block rollup rows   (parent0 + parent1 set, parent2 NA)   in F + I
#   - category rows where parent2_code IN the 30 selected codes
#
# Rollup row values are kept as in the originals (not recomputed) — the
# downstream meta-analysis pipeline reads them straight through.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

# ---- paths -------------------------------------------------------------

# Set REPO to the absolute path of your local clone of EH30-EstBB-Clean.
# Example: REPO <- "/Users/yourname/projects/EH30-EstBB-Clean"
REPO    <- ""  # <-- set this before running
SRC_DIR <- file.path(REPO, "source_data")
OUT_DIR <- file.path(REPO, "demo_comparison_script", "demo_source_data")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

diag_files <- c(
  "EH30_diagnosis_gender_year_age_group.csv",
  "GI_diagnosis_gender_year_age_group.csv",
  "GI_diagnosis_gender_year_age_group_first.csv",
  "GI_diagnosis_gender_year_age_group_second.csv"
)
patient_files <- c(
  "EH30_patient_birthyear_gender.csv",
  "GI_patient_birthyear_gender.csv",
  "GI_patient_birthyear_gender_first.csv",
  "GI_patient_birthyear_gender_second.csv"
)

# ---- 1. pick the 30 demo parent2_codes ---------------------------------

cat("--- selecting demo codes ---\n")

codes_per_ds <- lapply(diag_files, function(f) {
  read_csv(file.path(SRC_DIR, f), col_types = cols()) |>
    filter(parent0_code %in% c("F00-F99", "I00-I99"),
           !is.na(parent2_code)) |>
    distinct(parent0_code, parent2_code)
})
in_all <- Reduce(function(a, b)
  inner_join(a, b, by = c("parent0_code", "parent2_code")),
  codes_per_ds)

eh30_rank <- read_csv(file.path(SRC_DIR, diag_files[1]), col_types = cols()) |>
  filter(parent0_code %in% c("F00-F99", "I00-I99"),
         !is.na(parent2_code)) |>
  group_by(parent0_code, parent2_code) |>
  summarise(total_pc = sum(patient_count, na.rm = TRUE), .groups = "drop") |>
  semi_join(in_all, by = c("parent0_code", "parent2_code")) |>
  arrange(parent0_code, desc(total_pc))

top_f <- eh30_rank |> filter(parent0_code == "F00-F99") |> slice_head(n = 15)
top_i <- eh30_rank |> filter(parent0_code == "I00-I99") |> slice_head(n = 15)
KEEP_CODES <- c(top_f$parent2_code, top_i$parent2_code)
stopifnot(length(KEEP_CODES) == 30)

cat(sprintf("  F codes (%d): %s\n", nrow(top_f),
            paste(top_f$parent2_code, collapse = ", ")))
cat(sprintf("  I codes (%d): %s\n", nrow(top_i),
            paste(top_i$parent2_code, collapse = ", ")))

# ---- 2. filter the 4 diagnosis CSVs -----------------------------------

cat("\n--- writing filtered diagnosis CSVs ---\n")

filter_demo <- function(df) {
  df |> filter(
    parent0_code %in% c("F00-F99", "I00-I99"),
    is.na(parent2_code) | parent2_code %in% KEEP_CODES
  )
}

for (f in diag_files) {
  src <- file.path(SRC_DIR, f)
  out <- file.path(OUT_DIR, f)
  d   <- read_csv(src, col_types = cols())
  fd  <- filter_demo(d)
  write_csv(fd, out)

  lvls <- fd |>
    mutate(level = case_when(
      is.na(parent2_code) & is.na(parent1_code) ~ "chapter",
      is.na(parent2_code)                       ~ "block",
      TRUE                                      ~ "category"
    )) |>
    count(level)
  cat(sprintf("  %-50s  total=%d  chapter=%d  block=%d  category=%d\n",
              f, nrow(fd),
              sum(lvls$n[lvls$level == "chapter"]),
              sum(lvls$n[lvls$level == "block"]),
              sum(lvls$n[lvls$level == "category"])))

  # Schema sanity: column names and order match the source
  stopifnot(identical(names(d), names(fd)))
}

# ---- 3. copy the 4 patient_birthyear CSVs as-is -----------------------

cat("\n--- copying patient_birthyear CSVs ---\n")
for (f in patient_files) {
  file.copy(file.path(SRC_DIR, f), file.path(OUT_DIR, f), overwrite = TRUE)
  cat(sprintf("  %s\n", f))
}

cat(sprintf("\nDone. Output: %s\n", OUT_DIR))
