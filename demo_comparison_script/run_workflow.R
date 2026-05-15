# run_workflow.R
#
# Driver for the EH30 vs EstBB meta-analysis pipeline.
# Runs all 4 comparison pairs end-to-end and writes:
#   - {pair_base}_diff_meta.csv                  (meta1, base write.csv)
#   - {pair_base}_diff_meta_genders_parent2.csv  (meta2, readr::write_csv)
#   - {pair_base}_heterogeneity.csv              (sidecar with I2, tau2, Q, pval_Q, H)
#
# To rerun on demo data instead of the real source files, change SOURCE_DIR
# and COMP_DIR in the config block below.

# Set WORKDIR to the absolute path of your local clone of EH30-EstBB-Clean.
# Example: WORKDIR <- "/Users/yourname/projects/EH30-EstBB-Clean"
WORKDIR <- ""  # <-- set this before running
source(file.path(WORKDIR, "demo_comparison_script", "R", "workflow_functions.R"))

# ── Config: edit these two paths to switch between real and demo runs ──

SOURCE_DIR <- file.path(WORKDIR, "source_data")
COMP_DIR   <- file.path(WORKDIR, "comp_data")

# Demo run:
# SOURCE_DIR <- file.path(WORKDIR, "demo_comparison_script", "demo_source_data")
# COMP_DIR   <- file.path(WORKDIR, "demo_comparison_script", "demo_comp_data")

LOOKUPS_DIR <- file.path(WORKDIR, "demo_comparison_script", "lookups")

# ── Comparison pairs (4 total) ─────────────────────────────────────────

PAIRS <- list(
  list(d1_file = "EH30_diagnosis_gender_year_age_group.csv",
       d2_file = "GI_diagnosis_gender_year_age_group.csv",
       base    = "EH30d1_EstBBd2"),
  list(d1_file = "EH30_diagnosis_gender_year_age_group.csv",
       d2_file = "GI_diagnosis_gender_year_age_group_first.csv",
       base    = "EH30d1_EstBB1d2"),
  list(d1_file = "EH30_diagnosis_gender_year_age_group.csv",
       d2_file = "GI_diagnosis_gender_year_age_group_second.csv",
       base    = "EH30d1_EstBB2d2"),
  list(d1_file = "GI_diagnosis_gender_year_age_group_first.csv",
       d2_file = "GI_diagnosis_gender_year_age_group_second.csv",
       base    = "EstBB1d1_EstBB2d2")
)

# ── Run ────────────────────────────────────────────────────────────────

run_workflow <- function(source_dir, comp_dir, lookups_dir, pairs) {

  chapters_lookup <- readr::read_csv(
    file.path(lookups_dir, "ICD10_chapters.csv"), col_types = readr::cols()
  )
  categories_lookup <- readr::read_csv(
    file.path(lookups_dir, "icd10_category_EN.csv"), col_types = readr::cols()
  )

  unique_files <- unique(c(sapply(pairs, `[[`, "d1_file"),
                           sapply(pairs, `[[`, "d2_file")))

  inputs <- list()
  for (f in unique_files) {
    message("Loading + enriching: ", f)
    raw <- readr::read_csv(file.path(source_dir, f), col_types = readr::cols())
    inputs[[f]] <- enrich_source(raw, chapters_lookup, categories_lookup)
  }

  for (pair in pairs) {
    message("\n=== Running pair: ", pair$base, " ===")
    m1 <- run_meta1(inputs[[pair$d1_file]], inputs[[pair$d2_file]],
                    pair$base, comp_dir)
    m2 <- run_meta2(m1$data, pair$base, comp_dir)
    write_heterogeneity(m1$heterogeneity, m2$heterogeneity,
                        pair$base, comp_dir)
  }

  message("\nAll ", length(pairs), " pairs complete.")
}

if (sys.nframe() == 0) {
  # Only run when this file is the top-level script, not when sourced
  run_workflow(SOURCE_DIR, COMP_DIR, LOOKUPS_DIR, PAIRS)
}
