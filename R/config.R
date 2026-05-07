# config.R — Centralized constants for EH30-EstBB dashboard

# Dataset names
DATASET_NAMES <- list(
  d1 = "Est-Health-30",
  d2 = "EstBB",
  d3 = "EstBB1",
  d4 = "EstBB2"
)

# Comparison configurations
# Each comparison: which datasets are compared, file paths, display labels
COMPARISONS <- list(
  list(
    label     = "Est-Health-30 vs EstBB",
    name1     = "Est-Health-30",
    name2     = "EstBB",
    meta_file = "comp_data/EH30d1_EstBBd2_diff_meta.csv",
    gp_file   = "comp_data/EH30d1_EstBBd2_diff_meta_genders_parent2.csv"
  ),
  list(
    label     = "Est-Health-30 vs EstBB1",
    name1     = "Est-Health-30",
    name2     = "EstBB1",
    meta_file = "comp_data/EH30d1_EstBB1d2_diff_meta.csv",
    gp_file   = "comp_data/EH30d1_EstBB1d2_diff_meta_genders_parent2.csv"
  ),
  list(
    label     = "Est-Health-30 vs EstBB2",
    name1     = "Est-Health-30",
    name2     = "EstBB2",
    meta_file = "comp_data/EH30d1_EstBB2d2_diff_meta.csv",
    gp_file   = "comp_data/EH30d1_EstBB2d2_diff_meta_genders_parent2.csv"
  ),
  list(
    label     = "EstBB1 vs EstBB2",
    name1     = "EstBB1",
    name2     = "EstBB2",
    meta_file = "comp_data/EstBB1d1_EstBB2d2_diff_meta.csv",
    gp_file   = "comp_data/EstBB1d1_EstBB2d2_diff_meta_genders_parent2.csv"
  )
)

# Named vector for comparison label → index lookup
COMPARISON_LABELS <- setNames(seq_along(COMPARISONS), sapply(COMPARISONS, `[[`, "label"))

# Age group levels (ordered factor)
AGE_LEVELS <- c("10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

# ICD chapters to exclude from gender/parent2 analyses
EXCLUDED_CHAPTERS <- c("V01-Y98")

# --- Colors ---

# Dataset colors
COLOR_DATA1   <- "#005Fc8"
COLOR_DATA2   <- "#ff6600"
COLOR_DATA3   <- "#009E73"
COLOR_DATA4   <- "#F0D442"

# Gender colors
COLOR_MALE    <- "#9990FF"
COLOR_FEMALE  <- "#FF69B4"
COLOR_ALL     <- "navy"

# Reference/axis colors
COLOR_DOTLINE <- "orangered"
COLOR_LINES   <- "#333333"

# RGBA fills for GT table conditional formatting
BLUE_ALPHA    <- "rgba(0, 95, 200, 0.2)"
ORANGE_ALPHA  <- "rgba(255, 102, 0, 0.2)"
GENDER_BLUE   <- "rgba(153, 144, 255, 0.2)"
GENDER_PINK   <- "rgba(255, 105, 180, 0.2)"

# --- Column visibility defaults ---

# Columns hidden by default in Gender data tables
GENDER_HIDDEN_COLS <- c(
  "parent2_name", "parent1_code", "parent1_name",
  "parent0_code", "parent0_name", "parent0_name_EN",
  "gender", "prevalence_diff", "ci_low", "ci_high",
  "se", "z", "fold_ci_low_nat", "fold_ci_high_nat",
  "ci_width_nat", "meta_model_type"
)

# Columns hidden by default in Age data tables
AGE_HIDDEN_COLS <- c(
  "parent2_name", "parent1_code", "parent1_name",
  "parent0_code", "parent0_name", "meta_model_type"
)

# --- Scale mode helpers ---

#' Format fold difference for tooltips (vectorized)
#' For underrepresented (fold < 1): "Fold diff: 0.25 (1/4)"
#' For overrepresented (fold >= 1): "Fold diff: 2"
format_fold_tooltip <- function(log2_val) {
  fold <- round(2^log2_val, 2)
  ifelse(fold < 1,
    paste0("Fold diff: ", fold, " (", round(1/fold, 1), ")"),
    paste0("Fold diff: ", fold)
  )
}

# Standard fold-diff axis breaks/labels for log2-positioned data
FOLD_BREAKS <- c(-3, -2, -1, 0, 1, 2, 3)
FOLD_LABELS <- c("0.12", "0.25", "0.5", "1", "2", "4", "8")
LOG2_BREAKS <- c(-3, -2, -1, 0, 1, 2, 3)
LOG2_LABELS <- c("-3", "-2", "-1", "0", "1", "2", "3")

# Source data file paths
SOURCE_FILES <- list(
  d1 = "source_data/EH30_diagnosis_gender_year_age_group.csv",
  d2 = "source_data/GI_diagnosis_gender_year_age_group.csv",
  d3 = "source_data/GI_diagnosis_gender_year_age_group_first.csv",
  d4 = "source_data/GI_diagnosis_gender_year_age_group_second.csv"
)
