# plot_histograms.R — Parameterized histogram and scatter functions
# Replaces 9 create_p_histogram_* + 6 gender variants from legacy app

#' Create a prevalence ratio histogram
#'
#' @param plot_data Dataframe with the prevalence ratio column
#' @param pr_col Unquoted column name for the prevalence ratio (e.g., p_vs_bb)
#' @param left_label Text label for left side (e.g., "Est-Health-30")
#' @param right_label Text label for right side (e.g., "EstBB")
#' @param show_y_axis Whether to show y-axis label and ticks
#' @param show_x_axis Whether to show x-axis label and ticks
#' @param y_label Y-axis label (default "Count")
#' @param y_max Maximum y-axis value
#' @param show_subtitle Whether to show the 1.3x subtitle
#' @param show_border Whether to show border around plot
#' @return A ggplot object
create_pr_histogram <- function(plot_data,
                                pr_col,
                                left_label = "Est-Health-30",
                                right_label = "EstBB",
                                show_y_axis = TRUE,
                                show_x_axis = TRUE,
                                y_label = "Count",
                                y_max = 180,
                                x_label = "",
                                show_subtitle = FALSE,
                                show_border = FALSE) {

  p <- ggplot2::ggplot(ggplot2::aes(x = 2^(.data[[pr_col]])), data = plot_data) +
    ggplot2::geom_histogram(bins = 30) +
    ggplot2::scale_x_continuous(x_label, trans = "log2", limits = c(0.125, 4)) +
    ggplot2::scale_y_continuous(if (show_y_axis) y_label else "", limits = c(0, y_max)) +
    ggplot2::geom_vline(xintercept = 1, color = "grey10", linetype = 1, alpha = 0.9, linewidth = 0.8) +
    ggplot2::geom_vline(xintercept = 1.3, color = COLOR_DOTLINE, linetype = 3, alpha = 0.9, linewidth = 0.5) +
    ggplot2::geom_vline(xintercept = 1 / 1.3, color = COLOR_DOTLINE, linetype = 3, alpha = 0.9, linewidth = 0.5) +
    ggplot2::annotate("text", x = 1 / 1.7, y = y_max * 0.89, label = left_label,
                      hjust = 1, vjust = 0, size = 3, color = "black") +
    ggplot2::annotate("text", x = 1.7, y = y_max * 0.89, label = right_label,
                      hjust = 0, vjust = 0, size = 3, color = "black") +
    ggplot2::theme_bw()

  # Subtitle for first column
  if (show_subtitle) {
    p <- p + ggplot2::labs(subtitle = "\u2022\u2022\u2022\u2022 Difference 1.3 times")
  }

  # Theme adjustments based on axis visibility
  theme_args <- list(legend.position = "none")

  if (!show_x_axis) {
    theme_args <- c(theme_args, list(
      axis.text.x = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    ))
  } else {
    theme_args <- c(theme_args, list(
      axis.title.x = ggplot2::element_text(size = 8)
    ))
  }

  if (!show_y_axis) {
    theme_args <- c(theme_args, list(
      axis.text.y = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    ))
  } else {
    theme_args <- c(theme_args, list(
      axis.title.y = ggplot2::element_text(size = 9)
    ))
  }

  if (show_subtitle) {
    theme_args <- c(theme_args, list(
      plot.subtitle = ggplot2::element_text(color = COLOR_DOTLINE, size = 8),
      plot.title = ggplot2::element_text(size = 10)
    ))
  }

  if (show_border) {
    theme_args <- c(theme_args, list(
      plot.background = ggplot2::element_rect(color = "grey90", linewidth = 1)
    ))
  }

  p <- p + do.call(ggplot2::theme, theme_args)
  return(p)
}

#' Create a prevalence ratio scatter plot (burden overlay)
#'
#' @param plot_data Dataframe with prevalence ratio and burden columns
#' @param pr_col Column name for prevalence ratio (string)
#' @param p_col Column name for p-value (string)
#' @param burden_col Column name for burden size variable (string)
#' @param y_label Y-axis label
#' @param x_label X-axis label (only shown on bottom row)
#' @param show_y_axis Whether to show y-axis
#' @param show_x_axis Whether to show x-axis text
#' @return A ggplot object
create_pr_scatter <- function(plot_data,
                              pr_col,
                              p_col,
                              burden_col,
                              y_label = "Size = Years of\n Life Lost",
                              x_label = "",
                              show_y_axis = TRUE,
                              show_x_axis = FALSE) {

  p <- ggplot2::ggplot(
    ggplot2::aes(
      x = 2^(.data[[pr_col]]),
      y = 1,
      size = .data[[burden_col]],
      color = .data[[burden_col]],
      text = paste0(
        parent2_code, ", ", parent2_name_EN,
        "\nFold: ", round(2^(.data[[pr_col]]), 2),
        "\np: ", round(.data[[p_col]], 3),
        "\n"
      )
    ),
    data = plot_data %>% dplyr::arrange(.data[[burden_col]])
  ) +
    ggplot2::geom_point(position = ggplot2::position_jitter(seed = 42)) +
    ggplot2::scale_size_continuous(range = c(0.3, 4)) +
    ggplot2::scale_color_gradient(low = "grey50", high = "black") +
    ggplot2::scale_x_continuous(x_label, trans = "log2", limits = c(0.125, 4)) +
    ggplot2::geom_vline(xintercept = 1, color = "grey10", linetype = 1, alpha = 0.9, linewidth = 0.8) +
    ggplot2::geom_vline(xintercept = 1.3, color = COLOR_DOTLINE, linetype = 3, alpha = 0.9, linewidth = 0.5) +
    ggplot2::geom_vline(xintercept = 1 / 1.3, color = COLOR_DOTLINE, linetype = 3, alpha = 0.9, linewidth = 0.5) +
    ggplot2::theme_bw()

  theme_args <- list(legend.position = "none")

  if (show_y_axis) {
    p <- p + ggplot2::ylab(y_label)
  } else {
    theme_args <- c(theme_args, list(
      axis.text.y = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    ))
  }

  if (!show_x_axis) {
    theme_args <- c(theme_args, list(
      axis.text.x = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    ))
  } else {
    theme_args <- c(theme_args, list(
      axis.title.x = ggplot2::element_text(size = 8)
    ))
  }

  p <- p + do.call(ggplot2::theme, theme_args)
  return(p)
}
