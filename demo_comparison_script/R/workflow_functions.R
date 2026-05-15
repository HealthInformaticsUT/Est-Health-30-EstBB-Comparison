# workflow_functions.R
#
# Functions for the EH30 vs EstBB meta-analysis pipeline.
# Sourced by run_workflow.R.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(meta)
  library(tibble)
  library(readr)
})

# Age groups used by the meta-analysis. "0-9" is intentionally excluded.
age_levels <- c("10-19", "20-29", "30-39", "40-49",
                "50-59", "60-69", "70-79", "80+")


# ── Step 1: source enrichment ───────────────────────────────────────────

#' Add gender_EN, parent0_name_EN, short_name, parent2_name_EN to a source
#' diagnosis CSV. Idempotent — if those columns already exist (the current
#' source_data/ files are pre-enriched) the input is returned unchanged.
#'
#' @param df              data.frame read from a *_diagnosis_gender_year_age_group.csv
#' @param chapters_lookup data.frame with columns parent0_code, parent0_name_EN, short_name
#' @param categories_lookup data.frame with columns parent2_code, parent2_name_EN
enrich_source <- function(df, chapters_lookup, categories_lookup) {
  needed <- c("gender_EN", "parent0_name_EN", "short_name", "parent2_name_EN")
  if (all(needed %in% names(df))) return(df)

  df %>%
    dplyr::mutate(
      gender_EN = dplyr::recode(gender, "N" = "F", "M" = "M")
    ) %>%
    dplyr::left_join(chapters_lookup,   by = c("parent0_code" = "parent0_code")) %>%
    dplyr::left_join(categories_lookup, by = c("parent2_code" = "parent2_code"))
}


# ── Helpers used by both meta-analysis steps ────────────────────────────

#' Drop rows with NA or "-" in key strata columns.
#'
#' This is the offline workflow's equivalent of the dashboard's
#' R/data_prep.R::cleanData() helper.
clean_data <- function(input_data) {
  input_data %>%
    dplyr::filter(!(gender_EN %in% c(NA, "-"))) %>%
    dplyr::filter(!(year      %in% c(NA, "-"))) %>%
    dplyr::filter(!(age_group %in% c(NA, "-"))) %>%
    dplyr::filter(!is.na(denominator)) %>%
    dplyr::filter(!is.na(patient_count))
}

#' Keep only category-level rows (parent2_code populated). The legacy code
#' calls this par2Data(); the source files contain three rollup levels and
#' the meta-analysis works only on the most granular one.
par2_data <- function(input_data) {
  input_data %>% dplyr::filter(!(parent2_code %in% c(NA)))
}

#' Log2 prevalence ratio with confidence interval.
#'
#'   point estimate: log2(p2 / p1)
#'   SE (natural log scale): sqrt((1-p1)/(p1*n1) + (1-p2)/(p2*n2))
#'   SE (log2 scale): SE_ln / ln(2)
#'   CI: log2(p2/p1) +/- qnorm(0.975) * SE_log2
#'
#' Returns c(se_log2, ci_low, ci_high). Returns NAs if either n is <= 0.
confidence_interval <- function(p1, n1, p2, n2) {
  if (n1 <= 0 || n2 <= 0) {
    return(c(se = NA, ci_low = NA, ci_high = NA))
  }
  diff     <- log((p2 / p1), base = 2)
  se_ln    <- sqrt((1 - p1) / (p1 * n1) + (1 - p2) / (p2 * n2))
  se_log2  <- se_ln / log(2)
  ci_low   <- diff - qnorm(0.975) * se_log2
  ci_high  <- diff + qnorm(0.975) * se_log2
  c(se_log2, ci_low, ci_high)
}

#' Run a meta::metagen call with Random-Effects first, falling back to
#' Fixed-Effect if RE fails (e.g. zero between-study variance). Returns the
#' meta_result object or NULL if both attempts fail.
#'
#' Per the published article: this fallback is the way we avoid silently
#' dropping groups where the RE estimator can't converge.
run_metagen_with_fallback <- function(te, se_te, studlab, context = "") {
  tryCatch({
    meta::metagen(
      TE         = te,
      seTE       = se_te,
      studlab    = studlab,
      sm         = "MD",
      method.tau = "PM"
    )
  }, error = function(e_random) {
    warning(sprintf("RE model failed %s: %s. Falling back to FE.",
                    context, e_random$message), call. = FALSE)
    tryCatch({
      meta::metagen(
        TE          = te,
        seTE        = se_te,
        studlab     = studlab,
        sm          = "MD",
        comb.random = FALSE
      )
    }, error = function(e_fixed) {
      warning(sprintf("FE model ALSO failed %s: %s. Skipping.",
                      context, e_fixed$message), call. = FALSE)
      NULL
    })
  })
}

#' Extract heterogeneity stats from a metagen result. Returns NA tibble row
#' if meta_result is NULL.
extract_heterogeneity <- function(meta_result, n_studies) {
  if (is.null(meta_result)) {
    return(tibble::tibble(
      n_studies = n_studies,
      I2 = NA_real_, tau2 = NA_real_,
      Q = NA_real_, pval_Q = NA_real_, H = NA_real_
    ))
  }
  tibble::tibble(
    n_studies = n_studies,
    I2     = meta_result$I2,
    tau2   = meta_result$tau2,
    Q      = meta_result$Q,
    pval_Q = meta_result$pval.Q,
    H      = meta_result$H
  )
}


# ── Meta-analysis 1: pool across years ──────────────────────────────────

#' Run meta-analysis 1 for one dataset pair: compute per-year log2 PR with
#' CI and z/p, then pool across years per (gender_EN x age_group x parent2)
#' via Random-Effects (FE fallback). Writes
#' {output_name_base}_diff_meta.csv into output_dir using base write.csv
#' to preserve byte-identity with the published outputs.
#'
#' Returns the bound (per-year rows + Meta rows) data.frame plus the
#' heterogeneity tibble as a list (so the driver can collect both).
run_meta1 <- function(upload1, upload2, output_name_base,
                      output_dir = "comp_data") {

  input_data1 <- clean_data(upload1)
  data1       <- par2_data(input_data1)
  input_data2 <- clean_data(upload2)
  data2       <- par2_data(input_data2)

  data1 <- data1 %>%
    dplyr::filter(age_group %in% age_levels) %>%
    dplyr::mutate(age_group = factor(age_group, levels = age_levels))
  data2 <- data2 %>%
    dplyr::filter(age_group %in% age_levels) %>%
    dplyr::mutate(age_group = factor(age_group, levels = age_levels))

  select_year   <- unique(c(data1$year,        data2$year))
  select_age    <- unique(c(data1$age_group,   data2$age_group))
  select_gender <- unique(c(data1$gender_EN,   data2$gender_EN))
  select_code   <- unique(c(data1$parent2_code, data2$parent2_code))

  process_data <- function(data) {
    data %>%
      dplyr::filter(parent2_code %in% select_code) %>%
      dplyr::filter(gender_EN    %in% select_gender) %>%
      dplyr::filter(year         %in% select_year) %>%
      dplyr::filter(age_group    %in% select_age) %>%
      dplyr::select(
        parent2_code, parent2_name, parent1_code, parent1_name,
        parent0_code, parent0_name, parent0_name_EN, short_name,
        year, gender, age_group,
        prevalence, denominator, patient_count, gender_EN, parent2_name_EN
      ) %>%
      tidyr::drop_na(prevalence, denominator, patient_count) %>%
      dplyr::filter(denominator > 0)
  }

  d1 <- process_data(data1)
  d2 <- process_data(data2)

  combined_df1 <- d1 %>%
    dplyr::left_join(d2, by = c(
      "parent2_code", "parent2_name", "parent2_name_EN",
      "parent1_code", "parent1_name",
      "parent0_code", "parent0_name", "parent0_name_EN", "short_name",
      "year", "gender", "age_group", "gender_EN"
    ), suffix = c("_data1", "_data2")) %>%
    tidyr::drop_na(prevalence_data1, prevalence_data2,
                   denominator_data1, denominator_data2,
                   patient_count_data1, patient_count_data2) %>%
    dplyr::filter(prevalence_data1 > 0 & prevalence_data2 > 0) %>%
    dplyr::mutate(
      prevalence_diff = log((prevalence_data2 / prevalence_data1), base = 2)
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(ci = list(confidence_interval(
      prevalence_data1, denominator_data1,
      prevalence_data2, denominator_data2
    ))) %>%
    tidyr::unnest_wider(ci, names_sep = "_") %>%
    dplyr::rename(se = ci_1, ci_low = ci_2, ci_high = ci_3) %>%
    dplyr::ungroup() %>%
    tidyr::drop_na(prevalence_diff, se, ci_low, ci_high) %>%
    dplyr::mutate(
      z       = prevalence_diff / se,
      p_value = 2 * (1 - pnorm(abs(z)))
    )

  data <- combined_df1 %>%
    dplyr::mutate(sig = ifelse(ci_low * ci_high < 0, "nosig", "sig"))
  data$year <- as.character(data$year)
  data$lab  <- data$age_group

  # Pool across years
  data_grouped <- data %>%
    dplyr::group_by(gender_EN, age_group, parent2_code) %>%
    dplyr::group_split()

  het_rows <- list()  # collector for heterogeneity sidecar

  meta_data <- purrr::map_df(data_grouped, function(group) {
    if (nrow(group) <= 1) return(NULL)

    ctx <- sprintf("[meta1: %s / %s / %s]",
                   dplyr::first(group$gender_EN),
                   dplyr::first(group$age_group),
                   dplyr::first(group$parent2_code))
    meta_result <- run_metagen_with_fallback(
      te = group$prevalence_diff, se_te = group$se,
      studlab = group$year, context = ctx
    )
    if (is.null(meta_result)) return(NULL)

    # Capture heterogeneity for the sidecar
    het_rows[[length(het_rows) + 1]] <<- tibble::tibble(
      level = "meta1_across_years",
      parent2_code    = dplyr::first(group$parent2_code),
      parent2_name_EN = dplyr::first(group$parent2_name_EN),
      gender_EN       = dplyr::first(group$gender_EN),
      age_group       = as.character(dplyr::first(group$age_group))
    ) %>% dplyr::bind_cols(extract_heterogeneity(meta_result, nrow(group)))

    is_random_model <- !is.null(meta_result$TE.random)
    tibble::tibble(
      prevalence_diff = if (is_random_model) meta_result$TE.random   else meta_result$TE.fixed,
      ci_low          = if (is_random_model) meta_result$lower.random else meta_result$lower.fixed,
      ci_high         = if (is_random_model) meta_result$upper.random else meta_result$upper.fixed,
      fold_diff_nat   = 2 ** (if (is_random_model) meta_result$TE.random else meta_result$TE.fixed),
      fold_diff_reg   = dplyr::if_else(fold_diff_nat >= 1, fold_diff_nat, 1 / fold_diff_nat),
      se              = if (is_random_model) meta_result$seTE.random else meta_result$seTE.fixed,
      z               = if (is_random_model) meta_result$zval.random else meta_result$zval.fixed,
      p_value         = if (is_random_model) meta_result$pval.random else meta_result$pval.fixed,
      meta_model_type = if (is_random_model) "Random-Effects"        else "Fixed-Effect (Fallback)",
      gender          = dplyr::first(group$gender),
      gender_EN       = dplyr::first(group$gender_EN),
      age_group       = dplyr::first(group$age_group),
      parent2_code    = dplyr::first(group$parent2_code),
      parent2_name    = dplyr::first(group$parent2_name),
      parent2_name_EN = dplyr::first(group$parent2_name_EN),
      parent1_code    = dplyr::first(group$parent1_code),
      parent1_name    = dplyr::first(group$parent1_name),
      parent0_code    = dplyr::first(group$parent0_code),
      parent0_name    = dplyr::first(group$parent0_name),
      parent0_name_EN = dplyr::first(group$parent0_name_EN),
      short_name      = dplyr::first(group$short_name),
      year            = "Meta"
    )
  }) %>%
    dplyr::mutate(
      sig = ifelse(is.na(prevalence_diff) | ci_low * ci_high < 0, "nosig", "sig")
    )

  data_meta_all <- dplyr::bind_rows(data, meta_data)

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  out_path <- file.path(output_dir, paste0(output_name_base, "_diff_meta.csv"))
  write.csv(data_meta_all, file = out_path, row.names = FALSE)
  message("Meta-analysis 1 saved to: ", out_path)

  list(data = data_meta_all,
       heterogeneity = dplyr::bind_rows(het_rows))
}


# ── Meta-analysis 2: pool across ages, then across ages+sexes ───────────

#' Helper to build one meta2 result row (handles RE/FE plus Pass-Through).
.extract_meta2_row <- function(meta_result, group_data, year_label,
                               gender_label, model_type, meta_cols) {
  gender_est <- if (gender_label == "Both") {
    "Koos"  # Estonian for "Both"
  } else {
    dplyr::first(group_data$gender)
  }

  if (model_type == "Pass-Through") {
    tibble::tibble(
      prevalence_diff = group_data$prevalence_diff,
      ci_low          = group_data$ci_low,
      ci_high         = group_data$ci_high,
      se              = group_data$se,
      z               = group_data$z,
      p_value         = group_data$p_value,
      meta_model_type = model_type,
      gender          = gender_est,
      gender_EN       = gender_label,
      year            = year_label,
      !!! group_data %>%
        dplyr::select(dplyr::all_of(meta_cols)) %>%
        dplyr::distinct() %>%
        dplyr::slice(1)
    )
  } else {
    is_random_model <- !is.null(meta_result$TE.random)
    tibble::tibble(
      prevalence_diff = if (is_random_model) meta_result$TE.random   else meta_result$TE.fixed,
      ci_low          = if (is_random_model) meta_result$lower.random else meta_result$lower.fixed,
      ci_high         = if (is_random_model) meta_result$upper.random else meta_result$upper.fixed,
      se              = if (is_random_model) meta_result$seTE.random  else meta_result$seTE.fixed,
      z               = if (is_random_model) meta_result$zval.random  else meta_result$zval.fixed,
      p_value         = if (is_random_model) meta_result$pval.random  else meta_result$pval.fixed,
      meta_model_type = if (is_random_model) "Random-Effects"         else "Fixed-Effect (Fallback)",
      gender          = gender_est,
      gender_EN       = gender_label,
      year            = year_label,
      !!! group_data %>%
        dplyr::select(dplyr::all_of(meta_cols)) %>%
        dplyr::distinct() %>%
        dplyr::slice(1)
    )
  }
}

#' Run meta-analysis 2 for one dataset pair, consuming meta1's full output
#' frame. Performs two pools:
#'   - across age groups, per (gender_EN x parent2)   -> year = "MetaAgeCombined"
#'   - across age groups AND sexes, per parent2       -> year = "MetaAgeGenderCombined"
#' Single-study groups are kept via Pass-Through (per article methodology).
#' Writes {output_name_base}_diff_meta_genders_parent2.csv via readr::write_csv.
run_meta2 <- function(meta_1_output_df, output_name_base,
                      output_dir = "comp_data") {

  meta_df <- meta_1_output_df %>% dplyr::filter(year == "Meta")
  if (nrow(meta_df) == 0) {
    stop("meta_1_output_df has no rows with year == 'Meta'.", call. = FALSE)
  }

  meta_cols <- c(
    "parent2_code", "parent2_name", "parent2_name_EN",
    "parent1_code", "parent1_name", "parent0_code",
    "parent0_name", "parent0_name_EN", "short_name"
  )

  het_rows <- list()

  # 1. Gender-specific pool across ages
  meta_gender_grouped <- meta_df %>%
    dplyr::group_by(gender_EN, parent2_code) %>%
    dplyr::group_split()

  meta_gender_combined <- purrr::map_df(meta_gender_grouped, function(group) {
    n_age <- dplyr::n_distinct(group$age_group)
    gender_label <- dplyr::first(group$gender_EN)

    if (n_age > 1) {
      ctx <- sprintf("[meta2 gender: %s / %s]",
                     gender_label, dplyr::first(group$parent2_code))
      meta_result <- run_metagen_with_fallback(
        te = group$prevalence_diff, se_te = group$se,
        studlab = group$age_group, context = ctx
      )
      if (is.null(meta_result)) return(NULL)

      het_rows[[length(het_rows) + 1]] <<- tibble::tibble(
        level = "meta2_across_ages",
        parent2_code    = dplyr::first(group$parent2_code),
        parent2_name_EN = dplyr::first(group$parent2_name_EN),
        gender_EN       = gender_label,
        age_group       = NA_character_
      ) %>% dplyr::bind_cols(extract_heterogeneity(meta_result, n_age))

      .extract_meta2_row(meta_result, group,
                         year_label   = "MetaAgeCombined",
                         gender_label = gender_label,
                         model_type   = "Meta-Analysis",
                         meta_cols    = meta_cols)
    } else if (n_age == 1) {
      .extract_meta2_row(NULL, group,
                         year_label   = "MetaAgeCombined",
                         gender_label = gender_label,
                         model_type   = "Pass-Through",
                         meta_cols    = meta_cols)
    } else {
      NULL
    }
  })

  # 2. Pool across both ages and sexes
  meta_genderless_grouped <- meta_df %>%
    dplyr::group_by(parent2_code) %>%
    dplyr::group_split()

  meta_genderless <- purrr::map_df(meta_genderless_grouped, function(group) {
    n_studies <- nrow(group)

    if (n_studies > 1) {
      ctx <- sprintf("[meta2 both: %s]", dplyr::first(group$parent2_code))
      meta_result <- run_metagen_with_fallback(
        te = group$prevalence_diff, se_te = group$se,
        studlab = paste(group$age_group, group$gender_EN, sep = "_"),
        context = ctx
      )
      if (is.null(meta_result)) return(NULL)

      het_rows[[length(het_rows) + 1]] <<- tibble::tibble(
        level = "meta2_across_ages_genders",
        parent2_code    = dplyr::first(group$parent2_code),
        parent2_name_EN = dplyr::first(group$parent2_name_EN),
        gender_EN       = NA_character_,
        age_group       = NA_character_
      ) %>% dplyr::bind_cols(extract_heterogeneity(meta_result, n_studies))

      .extract_meta2_row(meta_result, group,
                         year_label   = "MetaAgeGenderCombined",
                         gender_label = "Both",
                         model_type   = "Meta-Analysis",
                         meta_cols    = meta_cols)
    } else if (n_studies == 1) {
      .extract_meta2_row(NULL, group,
                         year_label   = "MetaAgeGenderCombined",
                         gender_label = "Both",
                         model_type   = "Pass-Through",
                         meta_cols    = meta_cols)
    } else {
      NULL
    }
  })

  meta_all <- dplyr::bind_rows(meta_gender_combined, meta_genderless) %>%
    dplyr::mutate(
      fold_diff_nat    = 2 ^ prevalence_diff,
      fold_ci_low_nat  = 2 ^ ci_low,
      fold_ci_high_nat = 2 ^ ci_high,
      ci_width_nat     = fold_ci_high_nat - fold_ci_low_nat,
      fold_diff_reg    = dplyr::if_else(fold_diff_nat >= 1, fold_diff_nat,
                                        1 / fold_diff_nat),
      sig = ifelse(is.na(prevalence_diff) | ci_low * ci_high < 0,
                   "nosig", "sig")
    ) %>%
    dplyr::select(
      dplyr::all_of(meta_cols),
      gender, gender_EN, year,
      prevalence_diff, ci_low, ci_high, se, z, p_value,
      fold_diff_nat, fold_ci_low_nat, fold_ci_high_nat,
      ci_width_nat, fold_diff_reg,
      sig,
      meta_model_type
    )

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  out_path <- file.path(output_dir,
                        paste0(output_name_base, "_diff_meta_genders_parent2.csv"))
  readr::write_csv(meta_all, file = out_path)
  message("Meta-analysis 2 saved to: ", out_path)

  list(data = meta_all,
       heterogeneity = dplyr::bind_rows(het_rows))
}


# ── Heterogeneity sidecar ───────────────────────────────────────────────

#' Write the heterogeneity sidecar CSV for one pair. Concatenates the
#' heterogeneity rows from run_meta1 and run_meta2.
write_heterogeneity <- function(het_meta1, het_meta2,
                                output_name_base, output_dir = "comp_data") {
  het <- dplyr::bind_rows(het_meta1, het_meta2)
  if (nrow(het) == 0) return(invisible(NULL))

  het <- het %>% dplyr::select(
    level, parent2_code, parent2_name_EN, gender_EN, age_group,
    n_studies, I2, tau2, Q, pval_Q, H
  )

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  out_path <- file.path(output_dir,
                        paste0(output_name_base, "_heterogeneity.csv"))
  readr::write_csv(het, file = out_path)
  message("Heterogeneity sidecar saved to: ", out_path)
  invisible(het)
}
