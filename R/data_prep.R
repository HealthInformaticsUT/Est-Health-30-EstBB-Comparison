# data_prep.R — Data cleaning and preparation functions

#' Remove rows with NA/missing values in key columns
#' @param inputData Source dataframe
#' @return Cleaned dataframe
cleanData <- function(inputData) {
  inputData %>%
    dplyr::filter(!(gender_EN %in% c(NA, "-"))) %>%
    dplyr::filter(!(year %in% c(NA, "-"))) %>%
    dplyr::filter(!(age_group %in% c(NA, "-"))) %>%
    dplyr::filter(!is.na(denominator)) %>%
    dplyr::filter(!is.na(patient_count))
}

#' Filter to parent0 (chapter) level only
#' @param inputData Cleaned source dataframe
#' @return Dataframe with only top-level rows
topData <- function(inputData) {
  inputData %>%
    dplyr::filter(!is.na(parent0_code)) %>%
    dplyr::filter(is.na(parent2_code)) %>%
    dplyr::filter(is.na(parent1_code))
}

#' Prepare histogram data by joining 3 comparisons + DALY
#' Produces the wide-format data needed for the 3x3 histogram grid.
#'
#' @param gp_meta1 Gender/parent2 data for comparison 1 (EH30 vs EstBB)
#' @param gp_meta2 Gender/parent2 data for comparison 2 (EH30 vs EstBB1)
#' @param gp_meta3 Gender/parent2 data for comparison 3 (EH30 vs EstBB2)
#' @param DALY DALY_ICD dataframe
#' @return Wide dataframe with p_vs_bb, p_vs_bb1, p_vs_bb2, and DALY columns
prepare_histogram_data <- function(gp_meta1, gp_meta2, gp_meta3, DALY) {
  join_cols <- c("parent0_code", "parent2_code", "parent2_name_EN", "gender_EN")

  d <- gp_meta2 %>%
    dplyr::select(dplyr::all_of(join_cols), p_vs_bb1 = prevalence_diff, p_vs_bb1_p = p_value) %>%
    dplyr::left_join(
      gp_meta3 %>%
        dplyr::select(dplyr::all_of(join_cols), p_vs_bb2 = prevalence_diff, p_vs_bb2_p = p_value),
      by = join_cols
    ) %>%
    dplyr::left_join(
      gp_meta1 %>%
        dplyr::select(dplyr::all_of(join_cols), p_vs_bb = prevalence_diff, p_vs_bb_p = p_value),
      by = join_cols
    )

  # Join DALY and filter to "Both" gender, exclude V01-Y98
  d %>%
    dplyr::inner_join(
      DALY %>% dplyr::select(parent2_code = ICD10, YLL2021, YLL2021norm, YLD2021, YLD2021norm),
      by = "parent2_code"
    ) %>%
    dplyr::filter(gender_EN == "Both") %>%
    dplyr::filter(!(parent0_code %in% EXCLUDED_CHAPTERS))
}

#' Prepare gender-stratified histogram data
#' Same join but keeps M/F genders (no DALY needed).
#'
#' @param gp_meta1 Gender/parent2 data for comparison 1
#' @param gp_meta2 Gender/parent2 data for comparison 2
#' @param gp_meta3 Gender/parent2 data for comparison 3
#' @return Wide dataframe with p_vs_bb, p_vs_bb1, p_vs_bb2 for M/F
prepare_histogram_data_gender <- function(gp_meta1, gp_meta2, gp_meta3) {
  join_cols <- c("parent0_code", "parent2_code", "parent2_name_EN", "gender_EN")

  gp_meta2 %>%
    dplyr::filter(gender_EN %in% c("M", "F")) %>%
    dplyr::select(dplyr::all_of(join_cols), p_vs_bb1 = prevalence_diff) %>%
    dplyr::left_join(
      gp_meta3 %>%
        dplyr::filter(gender_EN %in% c("M", "F")) %>%
        dplyr::select(dplyr::all_of(join_cols), p_vs_bb2 = prevalence_diff),
      by = join_cols
    ) %>%
    dplyr::left_join(
      gp_meta1 %>%
        dplyr::filter(gender_EN %in% c("M", "F")) %>%
        dplyr::select(dplyr::all_of(join_cols), p_vs_bb = prevalence_diff),
      by = join_cols
    ) %>%
    dplyr::filter(!(parent0_code %in% EXCLUDED_CHAPTERS))
}
