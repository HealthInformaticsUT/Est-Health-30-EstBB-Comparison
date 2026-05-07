# scripts/regenerate_data.R
#
# Provenance script for the data files in source_data/ and comp_data/.
# Documents how those CSV/parquet files were produced from the underlying
# OMOP CDM databases via the syrona R package.
#
# This script is NOT runnable end-to-end without:
#   1. Access to the EH30 + EstBB OMOP CDM instances (Estonian Genome
#      Centre / University of Tartu - access requires data application
#      to the relevant data controllers).
#   2. The syrona R package, available at
#      https://github.com/MaarjaPajusalu/Syrona
#      install via:
#        remotes::install_github("MaarjaPajusalu/Syrona")
#
# It exists as a paper trail for the published preprint (medRxiv
# 10.64898/2026.02.05.26345634) and the manuscript under revision at
# Nature Communications.

# ============================================================================
# Step 1 - extract per-dataset prevalence using syrona
# ============================================================================

# Each call extracts ~19 tables per dataset (demographics, denominators,
# condition/procedure/drug prevalence, info, chapters, attributes, etc.).
# Output lands in `data/sources/<dataset_name>/` under the syrona project
# root.

# library(syrona)
#
# extract_all("EH30",   db = <DBI-connection-or-CDMConnector-cdm>)
# extract_all("EstBB",  db = <...>)
# extract_all("EstBB1", db = <...>, cohort_id = <sub_cohort_1_id>)
# extract_all("EstBB2", db = <...>, cohort_id = <sub_cohort_2_id>)

# ============================================================================
# Step 2 - pairwise comparisons + meta-analysis
# ============================================================================

# Four pairs are produced. For each pair, syrona computes per-year
# prevalence ratios across age x sex strata, then meta-analyses across
# years to produce summary effect sizes with heterogeneity (tau2, I2, Q,
# pval_Q) and prediction intervals.

# d_eh30  <- syrona::load_dataset("EH30")
# d_estbb <- syrona::load_dataset("EstBB")
# d_e1    <- syrona::load_dataset("EstBB1")
# d_e2    <- syrona::load_dataset("EstBB2")

# pair_eh30_estbb  <- syrona::compare_yearly(d_eh30, d_estbb)  |>
#   syrona::compare_meta_agegroups() |>
#   syrona::compare_meta_by_sex()    |>
#   syrona::compare_meta_summary()

# (...repeat for the other three pairs...)

# ============================================================================
# Step 3 - export to the dashboard's expected filenames
# ============================================================================

# The dashboard reads CSVs with specific names from source_data/ and
# comp_data/. The syrona output filenames differ - rename on copy.
#
# source_data/ expects:
#   EH30_diagnosis_gender_year_age_group.csv
#   EH30_patient_birthyear_gender.csv
#   GI_diagnosis_gender_year_age_group.csv          (= EstBB whole-cohort)
#   GI_diagnosis_gender_year_age_group_first.csv    (= EstBB1)
#   GI_diagnosis_gender_year_age_group_second.csv   (= EstBB2)
#   GI_patient_birthyear_gender.csv                 (= EstBB whole-cohort)
#   GI_patient_birthyear_gender_first.csv           (= EstBB1)
#   GI_patient_birthyear_gender_second.csv          (= EstBB2)
#
# comp_data/ expects 4 pair x 2 = 8 CSVs:
#   <pair>_diff_meta.csv                   (heatmap-level summary)
#   <pair>_diff_meta_genders_parent2.csv   (forest-plot per-stratum view)
# where <pair> is one of:
#   EH30d1_EstBBd2     (Pair 1: EH30 vs EstBB)
#   EH30d1_EstBB1d2    (Pair 2: EH30 vs EstBB1)
#   EH30d1_EstBB2d2    (Pair 3: EH30 vs EstBB2)
#   EstBB1d1_EstBB2d2  (Pair 4: EstBB1 vs EstBB2)

# ============================================================================
# Step 4 - rebuild parquet acceleration cache
# ============================================================================

# After CSVs are placed in source_data/ and comp_data/:
#
# source("scripts/build_parquet_cache.R")
#
# This writes `.parquet` siblings next to each `.csv`, enabling the
# 10x dashboard cold-start speedup measured during development.
