# plot_volcano.R — Volcano and volume histogram functions
# Cleaned from legacy plotFunctions.R

#' Render volcano + volume histogram (combined plotly subplot)
#'
#' @param data Filtered gender/parent2 dataset
#' @param threshold Fold difference threshold (e.g. 1.3)
#' @return A plotly subplot or NULL
render_volcano_volume_plot <- function(data, threshold) {
  if (is.null(data) || nrow(data) == 0) return(NULL)

  low_threshold <- 1 / threshold

  plot_data <- data %>%
    dplyr::mutate(plot_group = dplyr::case_when(
      p_value < 0.05 & fold_diff_nat <= low_threshold ~ "Significant Under",
      p_value < 0.05 & fold_diff_nat >= threshold     ~ "Significant Over",
      p_value < 0.05                                   ~ "Significant (Within Range)",
      TRUE                                             ~ "Not Significant"
    ))

  color_map <- c(
    "Significant Under"          = COLOR_DATA1,
    "Significant Over"           = COLOR_DATA2,
    "Significant (Within Range)" = "#555555",
    "Not Significant"            = "#D3D3D3"
  )

  x_breaks <- sort(unique(c(0.1, 0.2, 0.5, round(low_threshold, 2), 1, threshold, 2, 5, 10)))

  # Top: scatter plot
  p_scatter <- ggplot2::ggplot(plot_data, ggplot2::aes(
    x = fold_diff_nat, y = ci_width_nat,
    text = paste("Diagnosis:", parent2_code,
                 "<br>Name:", parent2_name_EN,
                 "<br>Fold Diff:", round(fold_diff_nat, 3),
                 "<br>CI Width:", round(ci_width_nat, 3),
                 "<br>p:", format.pval(p_value, digits = 3),
                 "<br>Gender:", gender_EN)
  )) +
    ggplot2::geom_point(ggplot2::aes(color = plot_group), alpha = 0.5, size = 1.5) +
    ggplot2::geom_vline(xintercept = 1, linetype = "solid", color = "black", alpha = 0.3) +
    ggplot2::geom_vline(xintercept = c(threshold, low_threshold), linetype = "dashed", color = "red", alpha = 0.4) +
    ggplot2::geom_hline(yintercept = c(0.3, 0.6), linetype = "dashed", color = "red", alpha = 0.3) +
    ggplot2::scale_x_log10(breaks = x_breaks) +
    ggplot2::scale_y_reverse() +
    ggplot2::scale_color_manual(values = color_map) +
    ggplot2::labs(y = "CI Width (Reversed)") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none", axis.text.x = ggplot2::element_blank())

  # Bottom: volume histogram
  p_volume <- ggplot2::ggplot(plot_data, ggplot2::aes(x = fold_diff_nat, fill = plot_group)) +
    ggplot2::geom_histogram(bins = 100) +
    ggplot2::geom_vline(xintercept = 1, linetype = "solid", color = "black", alpha = 0.3) +
    ggplot2::geom_vline(xintercept = c(threshold, low_threshold), linetype = "dashed", color = "red", alpha = 0.4) +
    ggplot2::scale_x_log10(breaks = x_breaks) +
    ggplot2::scale_fill_manual(values = color_map) +
    ggplot2::labs(y = "Count", x = "Fold Difference") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none",
                   axis.text.x = ggplot2::element_text(angle = 0, hjust = 1))

  plotly::subplot(
    plotly::ggplotly(p_scatter, tooltip = "text"),
    plotly::ggplotly(p_volume),
    nrows = 2, heights = c(0.7, 0.3), shareX = TRUE, titleY = TRUE
  ) %>%
    plotly::layout(showlegend = FALSE, margin = list(t = 30, b = 50))
}

#' Print comparison statistics to console
#'
#' @param df Filtered dataset
#' @param label Comparison label
#' @param threshold Fold difference threshold
#' @param p_value_threshold P-value cutoff
#' @return Invisible stats list
analyze_comparison_stats_se <- function(df, label, threshold, p_value_threshold) {
  df <- df %>% dplyr::filter(p_value <= p_value_threshold)
  col_name <- "prevalence_diff"
  low_threshold <- 1 / threshold

  get_subset_stats <- function(sub_df) {
    sub_df <- sub_df[!is.na(sub_df[[col_name]]), ]
    if (nrow(sub_df) == 0) return(list(base = 0, over = 0, under = 0))
    ratios <- 2^(sub_df[[col_name]])
    list(
      base  = dplyr::n_distinct(sub_df$parent2_code),
      over  = dplyr::n_distinct(sub_df[ratios >= threshold, ]$parent2_code),
      under = dplyr::n_distinct(sub_df[ratios <= low_threshold, ]$parent2_code)
    )
  }

  total_stats  <- get_subset_stats(df[df$gender_EN == "Both", ])
  male_stats   <- get_subset_stats(df[df$gender_EN == "M", ])
  female_stats <- get_subset_stats(df[df$gender_EN == "F", ])

  calc_pct <- function(count, base) if (base > 0) (count / base) * 100 else 0
  make_bar <- function(pct, char = "\u25a0", max_width = 15) {
    width <- round((pct / 100) * max_width)
    paste0(strrep(char, width), strrep(" ", max_width - width))
  }

  total_diff  <- total_stats$over + total_stats$under
  similar_pct <- calc_pct(total_stats$base - total_diff, total_stats$base)
  diff_pct    <- calc_pct(total_diff, total_stats$base)

  cat(paste0("ID: ", label, " | Thresh: ", threshold, "x | p <= ", p_value_threshold, "\n"))
  cat(paste0(strrep("\u2500", 60), "\n"))
  cat(sprintf("SIMILAR   [%s] %5.1f%%\n", make_bar(similar_pct, "\u2591"), similar_pct))
  cat(sprintf("DIFFERENT [%s] %5.1f%%\n", make_bar(diff_pct, "\u25a0"), diff_pct))
  cat(paste0(strrep("\u2500", 60), "\n\n"))

  cat(sprintf("BASE COHORT\n"))
  cat(sprintf("   Total Unique Codes : %d\n", total_stats$base))
  cat(sprintf("   Gender Split (F/M) : %d / %d (Ratio: %.2f)\n\n",
              female_stats$base, male_stats$base,
              if (male_stats$base > 0) female_stats$base / male_stats$base else 0))

  o_pct_t <- calc_pct(total_stats$over, total_stats$base)
  o_pct_f <- calc_pct(female_stats$over, female_stats$base)
  o_pct_m <- calc_pct(male_stats$over, male_stats$base)

  cat(sprintf("OVER-REPRESENTED (>= %.1fx)\n", threshold))
  cat(sprintf("   TOTAL   : %3d codes | %s %5.1f%%\n", total_stats$over, make_bar(o_pct_t), o_pct_t))
  cat(sprintf("   FEMALES : %3d codes | %5.1f%%\n", female_stats$over, o_pct_f))
  cat(sprintf("   MALES   : %3d codes | %5.1f%%\n\n", male_stats$over, o_pct_m))

  u_pct_t <- calc_pct(total_stats$under, total_stats$base)
  u_pct_f <- calc_pct(female_stats$under, female_stats$base)
  u_pct_m <- calc_pct(male_stats$under, male_stats$base)

  cat(sprintf("UNDER-REPRESENTED (<= %.2fx)\n", low_threshold))
  cat(sprintf("   TOTAL   : %3d codes | %s %5.1f%%\n", total_stats$under, make_bar(u_pct_t), u_pct_t))
  cat(sprintf("   FEMALES : %3d codes | %5.1f%%\n", female_stats$under, u_pct_f))
  cat(sprintf("   MALES   : %3d codes | %5.1f%%\n", male_stats$under, u_pct_m))
  cat(paste0(strrep("\u2500", 60), "\n"))

  invisible(total_stats)
}
