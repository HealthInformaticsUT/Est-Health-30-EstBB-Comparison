# plot_heatmaps.R — Heatmap plotting functions
# Cleaned versions from legacy plotFunctions.R, using config constants

#' Chapter-level heatmap sorted by average prevalence difference
#'
#' @param dataIn Meta-analysis dataset
#' @param select_age Character vector of age groups to include
#' @param select_gender Character vector of genders to include
#' @param select_code Character vector of parent0 codes to include
#' @param name1 Name of dataset 1
#' @param name2 Name of dataset 2
#' @return A girafe (ggiraph) object
heatmap_meta_avg <- function(dataIn, select_age, select_gender, select_code, name1, name2, scale_mode = "log2") {
  data <- dataIn %>%
    dplyr::filter(parent0_code %in% select_code) %>%
    dplyr::filter(gender_EN %in% select_gender) %>%
    dplyr::filter(age_group %in% select_age) %>%
    dplyr::filter(year %in% c("Meta")) %>%
    tidyr::drop_na(prevalence_diff)

  avg_data <- data %>%
    dplyr::group_by(parent2_code) %>%
    dplyr::summarize(avg_prevalence_diff = mean(prevalence_diff, na.rm = TRUE)) %>%
    dplyr::arrange(avg_prevalence_diff) %>%
    dplyr::mutate(order = ifelse(
      is.na(avg_prevalence_diff),
      max(rank(avg_prevalence_diff, na.last = TRUE), na.rm = TRUE) + 1,
      rank(avg_prevalence_diff, na.last = TRUE)
    ))

  data <- data %>%
    dplyr::left_join(avg_data, by = "parent2_code")

  data$parent0_label <- paste(data$parent0_code,
                              stringr::str_wrap(data$parent0_name_EN, width = 30), sep = "\n")
  data$parent1_label <- paste(data$parent1_code,
                              stringr::str_wrap(data$parent1_name, width = 120), sep = ", ")
  data$age_label <- factor(data$age_group, levels = AGE_LEVELS)

  data$parent2_code <- factor(data$parent2_code, levels = rev(sort(unique(data$parent2_code))))
  data$parent2_label_alph <- paste(data$parent2_code,
                                   stringr::str_wrap(data$parent2_name_EN, width = 120), sep = ", ")
  data$parent2_label_alph <- factor(data$parent2_label_alph,
                                    levels = rev(sort(unique(data$parent2_label_alph))))
  data$parent2_label_avg <- factor(data$parent2_label_alph,
                                   levels = unique(data$parent2_label_alph[order(data$order, decreasing = FALSE)]))

  plot <- ggplot2::ggplot(data, ggplot2::aes(x = age_group, y = parent2_label_avg, fill = prevalence_diff)) +
    ggiraph::geom_tile_interactive(
      color = NA, width = 1.25,
      ggplot2::aes(
        tooltip = stringr::str_c(
          parent2_code, ", ", parent2_name_EN, ", ", gender_EN, "-", age_group,
          "\nLog2 PR: ", round(prevalence_diff, 4),
          "\n", format_fold_tooltip(prevalence_diff),
          "\np: ", round(p_value, 3),
          "\n", parent2_name
        ),
        data_id = parent0_code
      )
    ) +
    ggplot2::scale_fill_gradient2(
      low = COLOR_DATA1, mid = "#e8e8e8", high = COLOR_DATA2,
      midpoint = 0, limits = c(-2, 2), oob = scales::squish,
      name = if (scale_mode == "fold") "Fold Difference" else "Prevalence ratio",
      breaks = if (scale_mode == "fold") c(-2, -1, 0, 1, 2) else ggplot2::waiver(),
      labels = if (scale_mode == "fold") c("0.25", "0.5", "1", "2", "4") else ggplot2::waiver()
    ) +
    ggplot2::labs(
      title = paste("Prevalence higher in <span style='color:", COLOR_DATA1, ";'>", name1,
                    " (blue)</span> or <span style='color: ", COLOR_DATA2, ";'>", name2, " (orange)</span>"),
      subtitle = if (scale_mode == "fold") {
        paste0("Fold Difference: <span style='color:", COLOR_DATA2, ";'>", name2,
               "</span> / <span style='color:", COLOR_DATA1, ";'>", name1,
               "</span> (1 = equal prevalence)")
      } else {
        paste0("Prevalence ratio = log((<span style='color:", COLOR_DATA2, ";'>", name2,
               " prevalence</span>/<span style='color:", COLOR_DATA1, ";'>", name1,
               " prevalence</span>), base = 2)")
      },
      x = "Age Group", y = "Diagnoses"
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title = ggtext::element_markdown(size = 12, face = "bold", hjust = 0),
      plot.subtitle = ggtext::element_markdown(size = 10, face = "plain", hjust = 0),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_text(face = "plain", size = 8, colour = "black"),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      strip.text.x = ggplot2::element_text(size = 8),
      strip.text.y = ggplot2::element_text(size = 9, angle = 0, hjust = 0),
      strip.background = ggplot2::element_rect(fill = "#ededed", color = "white", linewidth = 0),
      panel.spacing.y = ggplot2::unit(0.1, "cm"),
      panel.spacing.x = ggplot2::unit(0, "cm")
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(parent0_label),
      cols = ggplot2::vars(gender_EN, age_label),
      scales = "free", space = "free_y",
      labeller = ggplot2::labeller(gender_EN = ggplot2::label_value,
                                   age_label = ggplot2::label_value,
                                   parent0_label = ggplot2::label_value)
    )

  ggiraph::girafe(
    ggobj = plot, height_svg = 11, width_svg = 12,
    options = list(
      ggiraph::opts_tooltip(opacity = .9,
                            css = "background-color:beige; color:black; padding:2px; border-radius:5px;"),
      ggiraph::opts_toolbar(position = "topright"),
      ggiraph::opts_selection(type = "single", only_shiny = FALSE,
                              css = "stroke:#999999;stroke-width:0.4"),
      ggiraph::opts_hover(css = "stroke:#999999;stroke-width:0.4;cursor:pointer;")
    )
  )
}

#' Detail-level heatmap sorted alphabetically
#'
#' Same parameters as heatmap_meta_avg but sorts diagnoses alphabetically
#' and uses parent2_label_alph for row faceting.
heatmap_meta_alph <- function(dataIn, select_age, select_gender, select_code, name1, name2, scale_mode = "log2") {
  data <- dataIn %>%
    dplyr::filter(parent0_code %in% select_code) %>%
    dplyr::filter(gender_EN %in% select_gender) %>%
    dplyr::filter(age_group %in% select_age) %>%
    dplyr::filter(year %in% c("Meta")) %>%
    tidyr::drop_na(prevalence_diff)

  avg_data <- data %>%
    dplyr::group_by(parent2_code) %>%
    dplyr::summarize(avg_prevalence_diff = mean(prevalence_diff, na.rm = TRUE)) %>%
    dplyr::arrange(avg_prevalence_diff) %>%
    dplyr::mutate(order = ifelse(
      is.na(avg_prevalence_diff),
      max(rank(avg_prevalence_diff, na.last = TRUE), na.rm = TRUE) + 1,
      rank(avg_prevalence_diff, na.last = TRUE)
    ))

  data <- data %>%
    dplyr::left_join(avg_data, by = "parent2_code")

  data$parent0_label <- paste(data$parent0_code,
                              stringr::str_wrap(data$parent0_name, width = 280), sep = ", ")
  data$parent1_label <- paste(data$parent1_code,
                              stringr::str_wrap(data$parent1_name, width = 280), sep = ", ")
  data$age_label <- factor(data$age_group, levels = AGE_LEVELS)
  data$age_label <- paste(substr(data$age_group,
                                 nchar(data$age_group) - nchar(data$age_group) + 1, 2), "..", sep = "")

  data$parent2_code <- factor(data$parent2_code, levels = rev(sort(unique(data$parent2_code))))
  data$parent2_label_alph <- paste(
    data$parent2_code,
    stringr::str_trunc(data$parent2_name_EN, width = 40, side = "right"),
    sep = ", "
  )
  data$parent2_label_alph <- factor(data$parent2_label_alph,
                                    levels = sort(unique(data$parent2_label_alph)))
  data$parent2_label_avg <- factor(data$parent2_label_alph,
                                   levels = unique(data$parent2_label_alph[order(data$order, decreasing = FALSE)]))

  plot <- ggplot2::ggplot(data, ggplot2::aes(x = age_group, y = parent2_label_alph, fill = prevalence_diff)) +
    ggiraph::geom_tile_interactive(
      color = NA, width = 1.25,
      ggplot2::aes(
        tooltip = paste0(
          parent2_code, ", ", parent2_name_EN,
          "\nGender: ", gender_EN,
          "\np: ", round(p_value, 3),
          "\nLog2 diff: ", round(prevalence_diff, 2),
          " [", round(ci_low, 3), ", ", round(ci_high, 2), "]",
          "\n", format_fold_tooltip(prevalence_diff),
          "\n", parent2_name
        ),
        data_id = parent2_code
      )
    ) +
    ggplot2::scale_fill_gradient2(
      low = COLOR_DATA1, mid = "#e8e8e8", high = COLOR_DATA2,
      midpoint = 0, limits = c(-2, 2), oob = scales::squish,
      name = if (scale_mode == "fold") "Fold Difference" else "Prevalence ratio",
      breaks = if (scale_mode == "fold") c(-2, -1, 0, 1, 2) else ggplot2::waiver(),
      labels = if (scale_mode == "fold") c("0.25", "0.5", "1", "2", "4") else ggplot2::waiver()
    ) +
    ggplot2::labs(
      title = paste("Meta alph Prevalence higher in <span style='color:", COLOR_DATA1, ";'>", name1,
                    " (blue)</span> or <span style='color: ", COLOR_DATA2, ";'>", name2, " (orange)</span>"),
      subtitle = if (scale_mode == "fold") {
        paste0("Fold Difference: <span style='color:", COLOR_DATA2, ";'>", name2,
               "</span> / <span style='color:", COLOR_DATA1, ";'>", name1,
               "</span> (1 = equal prevalence)")
      } else {
        paste0("Prevalence ratio = log((<span style='color:", COLOR_DATA2, ";'>", name2,
               " prevalence</span>/<span style='color:", COLOR_DATA1, ";'>", name1,
               " prevalence</span>), base = 2)")
      },
      x = "Age Group", y = "Diagnoses"
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title = ggtext::element_markdown(size = 12, face = "bold", hjust = 0),
      plot.subtitle = ggtext::element_markdown(size = 10, face = "plain", hjust = 0),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_text(face = "plain", size = 7, colour = "black"),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      strip.text.x = ggplot2::element_text(size = 8),
      strip.text.y = ggplot2::element_text(size = 9, angle = 0, hjust = 0),
      strip.background = ggplot2::element_rect(fill = "#ededed", color = "white", linewidth = 0),
      panel.spacing.y = ggplot2::unit(0, "cm"),
      panel.spacing.x = ggplot2::unit(0, "cm")
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(parent2_label_alph),
      cols = ggplot2::vars(gender_EN, age_label),
      scales = "free", space = "free_y",
      labeller = ggplot2::labeller(gender_EN = ggplot2::label_value,
                                   age_label = ggplot2::label_value,
                                   parent2_label_alph = ggplot2::label_value)
    )

  ggiraph::girafe(
    ggobj = plot, height_svg = 11, width_svg = 12,
    options = list(
      ggiraph::opts_tooltip(opacity = .9,
                            css = "background-color:beige; color:black; padding:2px; border-radius:5px;"),
      ggiraph::opts_toolbar(position = "topright"),
      ggiraph::opts_selection(type = "single", only_shiny = FALSE,
                              css = "stroke:#999999;stroke-width:0.4"),
      ggiraph::opts_hover(css = "stroke:#999999;stroke-width:0.4;cursor:pointer;")
    )
  )
}

#' Detail-level heatmap sorted by average (avgDet variant)
#'
#' Same as heatmap_meta_alph but uses parent2_label_avg for y-axis ordering.
#' This is the "Average" sorting option in the UI.
heatmap_meta_avgDet <- function(dataIn, select_age, select_gender, select_code, name1, name2, scale_mode = "log2") {
  data <- dataIn %>%
    dplyr::filter(parent0_code %in% select_code) %>%
    dplyr::filter(gender_EN %in% select_gender) %>%
    dplyr::filter(age_group %in% select_age) %>%
    dplyr::filter(year %in% c("Meta")) %>%
    tidyr::drop_na(prevalence_diff)

  avg_data <- data %>%
    dplyr::group_by(parent2_code) %>%
    dplyr::summarize(avg_prevalence_diff = mean(prevalence_diff, na.rm = TRUE)) %>%
    dplyr::arrange(avg_prevalence_diff) %>%
    dplyr::mutate(order = ifelse(
      is.na(avg_prevalence_diff),
      max(rank(avg_prevalence_diff, na.last = TRUE), na.rm = TRUE) + 1,
      rank(avg_prevalence_diff, na.last = TRUE)
    ))

  data <- data %>%
    dplyr::left_join(avg_data, by = "parent2_code")

  data$parent0_label <- paste(data$parent0_code,
                              stringr::str_wrap(data$parent0_name, width = 280), sep = ", ")
  data$parent1_label <- paste(data$parent1_code,
                              stringr::str_wrap(data$parent1_name, width = 280), sep = ", ")
  data$age_label <- factor(data$age_group, levels = AGE_LEVELS)
  data$age_label <- paste(substr(data$age_group,
                                 nchar(data$age_group) - nchar(data$age_group) + 1, 2), "..", sep = "")

  data$parent2_code <- factor(data$parent2_code, levels = rev(sort(unique(data$parent2_code))))
  data$parent2_label_alph <- paste(
    data$parent2_code,
    stringr::str_trunc(data$parent2_name_EN, width = 40, side = "right"),
    sep = ", "
  )
  data$parent2_label_alph <- factor(data$parent2_label_alph,
                                    levels = sort(unique(data$parent2_label_alph)))
  # Key difference: sorted by average prevalence difference
  data$parent2_label_avg <- factor(data$parent2_label_alph,
                                   levels = unique(data$parent2_label_alph[order(data$order, decreasing = FALSE)]))

  plot <- ggplot2::ggplot(data, ggplot2::aes(x = age_group, y = parent2_label_avg, fill = prevalence_diff)) +
    ggiraph::geom_tile_interactive(
      color = NA, width = 1.25,
      ggplot2::aes(
        tooltip = paste0(
          parent2_code, ", ", parent2_name_EN,
          "\nGender: ", gender_EN,
          "\np: ", round(p_value, 3),
          "\nLog2 diff: ", round(prevalence_diff, 2),
          " [", round(ci_low, 3), ", ", round(ci_high, 2), "]",
          "\n", format_fold_tooltip(prevalence_diff),
          "\n", parent2_name
        ),
        data_id = parent2_code
      )
    ) +
    ggplot2::scale_fill_gradient2(
      low = COLOR_DATA1, mid = "#e8e8e8", high = COLOR_DATA2,
      midpoint = 0, limits = c(-2, 2), oob = scales::squish,
      name = if (scale_mode == "fold") "Fold Difference" else "Prevalence ratio",
      breaks = if (scale_mode == "fold") c(-2, -1, 0, 1, 2) else ggplot2::waiver(),
      labels = if (scale_mode == "fold") c("0.25", "0.5", "1", "2", "4") else ggplot2::waiver()
    ) +
    ggplot2::labs(
      title = paste("Meta avg Prevalence higher in <span style='color:", COLOR_DATA1, ";'>", name1,
                    " (blue)</span> or <span style='color: ", COLOR_DATA2, ";'>", name2, " (orange)</span>"),
      subtitle = if (scale_mode == "fold") {
        paste0("Fold Difference: <span style='color:", COLOR_DATA2, ";'>", name2,
               "</span> / <span style='color:", COLOR_DATA1, ";'>", name1,
               "</span> (1 = equal prevalence)")
      } else {
        paste0("Prevalence ratio = log((<span style='color:", COLOR_DATA2, ";'>", name2,
               " prevalence</span>/<span style='color:", COLOR_DATA1, ";'>", name1,
               " prevalence</span>), base = 2)")
      },
      x = "Age Group", y = "Diagnoses"
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title = ggtext::element_markdown(size = 12, face = "bold", hjust = 0),
      plot.subtitle = ggtext::element_markdown(size = 10, face = "plain", hjust = 0),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_text(face = "plain", size = 7, colour = "black"),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      strip.text.x = ggplot2::element_text(size = 8),
      strip.text.y = ggplot2::element_text(size = 9, angle = 0, hjust = 0),
      strip.background = ggplot2::element_rect(fill = "#ededed", color = "white", linewidth = 0),
      panel.spacing.y = ggplot2::unit(0, "cm"),
      panel.spacing.x = ggplot2::unit(0, "cm")
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(parent2_label_avg),
      cols = ggplot2::vars(gender_EN, age_label),
      scales = "free", space = "free_y",
      labeller = ggplot2::labeller(gender_EN = ggplot2::label_value,
                                   age_label = ggplot2::label_value,
                                   parent2_label_avg = ggplot2::label_value)
    )

  ggiraph::girafe(
    ggobj = plot, height_svg = 11, width_svg = 12,
    options = list(
      ggiraph::opts_tooltip(opacity = .9,
                            css = "background-color:beige; color:black; padding:2px; border-radius:5px;"),
      ggiraph::opts_toolbar(position = "topright"),
      ggiraph::opts_selection(type = "single", only_shiny = FALSE,
                              css = "stroke:#999999;stroke-width:0.4"),
      ggiraph::opts_hover(css = "stroke:#999999;stroke-width:0.4;cursor:pointer;")
    )
  )
}

#' Custom heatmap — user-selected diagnosis codes, alphabetically sorted
#'
#' @param dataIn Meta-analysis dataset
#' @param select_age Character vector of age groups to include
#' @param select_gender Character vector of genders to include
#' @param select_codes Character vector of parent2 codes to include
#' @param name1 Name of dataset 1
#' @param name2 Name of dataset 2
#' @return A girafe (ggiraph) object
heatmap_custom <- function(dataIn, select_age, select_gender, select_codes, name1, name2, scale_mode = "log2") {
  data <- dataIn %>%
    dplyr::filter(parent2_code %in% select_codes) %>%
    dplyr::filter(gender_EN %in% select_gender) %>%
    dplyr::filter(age_group %in% select_age) %>%
    dplyr::filter(year %in% c("Meta")) %>%
    tidyr::drop_na(prevalence_diff)

  if (nrow(data) == 0) return(NULL)

  data$age_label <- factor(data$age_group, levels = AGE_LEVELS)
  data$age_label <- paste(substr(data$age_group, 1, 2), "..", sep = "")

  # Y-axis label: "code, name (truncated)"
  data$parent2_label <- paste(
    data$parent2_code,
    stringr::str_trunc(data$parent2_name_EN, width = 40, side = "right"),
    sep = ", "
  )
  # Alphabetical sort: A at top, Z at bottom
  data$parent2_label <- factor(data$parent2_label,
                               levels = sort(unique(data$parent2_label)))

  plot <- ggplot2::ggplot(data, ggplot2::aes(x = age_group, y = parent2_label, fill = prevalence_diff)) +
    ggiraph::geom_tile_interactive(
      color = NA, width = 1.25,
      ggplot2::aes(
        tooltip = paste0(
          parent2_code, ", ", parent2_name_EN,
          "\nGender: ", gender_EN, ", Age: ", age_group,
          "\np: ", round(p_value, 3),
          "\nLog2 diff: ", round(prevalence_diff, 2),
          " [", round(ci_low, 3), ", ", round(ci_high, 2), "]",
          "\n", format_fold_tooltip(prevalence_diff),
          "\n", parent2_name
        ),
        data_id = parent2_code
      )
    ) +
    ggplot2::scale_fill_gradient2(
      low = COLOR_DATA1, mid = "#e8e8e8", high = COLOR_DATA2,
      midpoint = 0, limits = c(-2, 2), oob = scales::squish,
      name = if (scale_mode == "fold") "Fold Difference" else "Prevalence ratio",
      breaks = if (scale_mode == "fold") c(-2, -1, 0, 1, 2) else ggplot2::waiver(),
      labels = if (scale_mode == "fold") c("0.25", "0.5", "1", "2", "4") else ggplot2::waiver()
    ) +
    ggplot2::labs(
      title = paste("Prevalence higher in <span style='color:", COLOR_DATA1, ";'>", name1,
                    " (blue)</span> or <span style='color: ", COLOR_DATA2, ";'>", name2, " (orange)</span>"),
      subtitle = if (scale_mode == "fold") {
        paste0("Fold Difference: <span style='color:", COLOR_DATA2, ";'>", name2,
               "</span> / <span style='color:", COLOR_DATA1, ";'>", name1,
               "</span> (1 = equal prevalence)")
      } else {
        paste0("Prevalence ratio = log((<span style='color:", COLOR_DATA2, ";'>", name2,
               " prevalence</span>/<span style='color:", COLOR_DATA1, ";'>", name1,
               " prevalence</span>), base = 2)")
      },
      x = "Age Group", y = "Diagnoses"
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title = ggtext::element_markdown(size = 12, face = "bold", hjust = 0),
      plot.subtitle = ggtext::element_markdown(size = 10, face = "plain", hjust = 0),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_text(face = "plain", size = 7, colour = "black"),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      strip.text.x = ggplot2::element_text(size = 8),
      strip.text.y = ggplot2::element_text(size = 9, angle = 0, hjust = 0),
      strip.background = ggplot2::element_rect(fill = "#ededed", color = "white", linewidth = 0),
      panel.spacing.y = ggplot2::unit(0, "cm"),
      panel.spacing.x = ggplot2::unit(0, "cm")
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(parent2_label),
      cols = ggplot2::vars(gender_EN, age_label),
      scales = "free", space = "free_y",
      labeller = ggplot2::labeller(gender_EN = ggplot2::label_value,
                                   age_label = ggplot2::label_value,
                                   parent2_label = ggplot2::label_value)
    )

  ggiraph::girafe(
    ggobj = plot, height_svg = 6, width_svg = 12,
    options = list(
      ggiraph::opts_tooltip(opacity = .9,
                            css = "background-color:beige; color:black; padding:2px; border-radius:5px;"),
      ggiraph::opts_toolbar(position = "topright"),
      ggiraph::opts_selection(type = "single", only_shiny = FALSE,
                              css = "stroke:#999999;stroke-width:0.4"),
      ggiraph::opts_hover(css = "stroke:#999999;stroke-width:0.4;cursor:pointer;")
    )
  )
}
