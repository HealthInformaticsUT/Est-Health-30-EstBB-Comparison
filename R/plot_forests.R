# plot_forests.R — Forest plot and point difference functions
# Cleaned from legacy plotFunctions.R, using config constants

#' Forest plot: prevalence ratios across years for a single diagnosis
#'
#' @param data Meta-analysis dataset
#' @param select_age Age groups to show
#' @param select_gender Genders to show
#' @param select_code Parent2 codes to show
#' @param name1 Dataset 1 name
#' @param name2 Dataset 2 name
#' @return A girafe object
forest1 <- function(data, select_age, select_gender, select_code, name1, name2, scale_mode = "log2") {
  data <- data %>%
    dplyr::filter(parent2_code %in% select_code) %>%
    dplyr::filter(gender_EN %in% select_gender) %>%
    dplyr::filter(age_group %in% select_age)

  data$color_fill_category <- dplyr::case_when(
    data$year == "Meta" & data$sig == "sig"   ~ "Meta_sig",
    data$year == "Meta" & data$sig == "nosig" ~ "Meta_nosig",
    data$year != "Meta" & data$sig == "sig"   ~ "Year_sig",
    data$year != "Meta" & data$sig == "nosig" ~ "Year_nosig"
  )

  plot1 <- ggplot2::ggplot(data, ggplot2::aes(
    x = prevalence_diff,
    y = factor(year, levels = unique(data$year)),
    fill = color_fill_category,
    color = sig
  )) +
    ggplot2::annotate(geom = "rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf,
                      fill = COLOR_DATA1, alpha = 0.2) +
    ggplot2::annotate(geom = "rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf,
                      fill = COLOR_DATA2, alpha = 0.1) +
    ggplot2::geom_vline(xintercept = 0, color = "grey") +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = ci_low, xmax = ci_high, color = sig), width = 0.1) +
    ggiraph::geom_point_interactive(
      shape = ifelse(data$year == "Meta", 23, 15),
      ggplot2::aes(
        color = sig,
        fill = color_fill_category,
        data_id = prevalence_diff,
        tooltip = ifelse(
          data$year == "Meta",
          paste("Log2 PR:", round(data$prevalence_diff, 3),
                "\n", format_fold_tooltip(data$prevalence_diff),
                "\n Natural fold difference:", round(data$fold_diff_nat, 3),
                "(Reg: ", round(fold_diff_reg, 3), ")",
                "\n p-value", round(data$p_value, 3)),
          paste(
            "Log2 PR:", round(data$prevalence_diff, 3),
            "\n", format_fold_tooltip(data$prevalence_diff),
            "\n p-value", round(data$p_value, 3),
            "\n", name1, ":", round(data$prevalence_data1, 3),
            "(", data$patient_count_data1, "/", data$denominator_data1, ")",
            "\n", name2, ":", round(data$prevalence_data2, 3),
            "(", data$patient_count_data2, "/", data$denominator_data2, ")"
          )
        )
      )
    ) +
    ggplot2::scale_color_manual(values = c("sig" = "black", "nosig" = "grey"), guide = "none") +
    ggplot2::scale_fill_manual(values = c("Meta_sig" = "darkred", "Meta_nosig" = "grey"), guide = "none") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = paste0(
        "Prevalence higher in <span style='color: ", COLOR_DATA1, ";'>", name1,
        " (blue)</span> or <span style='color: ", COLOR_DATA2,
        ";'>", name2, " (orange)</span> - ",
        data$parent2_code[1], " ", data$parent2_name_EN[1]
      ),
      subtitle = paste0(
        "<span style='color: black;'>Significant</span>
         <span style='color: grey;'>Not significant </span>
         <span style='color: darkred;'>Meta-analysis</span>"
      )
    ) +
    ggplot2::xlab(
      if (scale_mode == "fold") {
        paste0("Fold Difference (<span style='color:", COLOR_DATA2, ";'>", name2,
               "</span> / <span style='color:", COLOR_DATA1, ";'>", name1, "</span>)")
      } else {
        paste0("Prevalence ratio, log(<span style='color:", COLOR_DATA2, ";'>", name2,
               " prevalence</span>/<span style='color:", COLOR_DATA1,
               ";'>", name1, " prevalence</span>, base = 2)")
      }
    ) +
    ggplot2::scale_x_continuous(
      breaks = if (scale_mode == "fold") c(-2, -1, 0, 1, 2) else pretty(data$prevalence_diff, n = 1),
      labels = if (scale_mode == "fold") c("0.25", "0.5", "1", "2", "4") else ggplot2::waiver()
    ) +
    ggplot2::theme(
      plot.title = ggtext::element_markdown(size = 12, face = "bold", hjust = 0),
      plot.subtitle = ggtext::element_markdown(size = 10, face = "plain", hjust = 0),
      axis.text.x = ggplot2::element_text(face = "plain", size = 8, colour = "black"),
      axis.ticks.x = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_line(colour = "darkgray"),
      axis.title.x = ggtext::element_markdown(face = "plain", size = 8),
      axis.text.y = ggplot2::element_text(face = "plain", size = 8, colour = "black"),
      axis.ticks.y = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_line(colour = "darkgray"),
      axis.title.y = ggplot2::element_blank(),
      panel.spacing.x = ggplot2::unit(0.8, "cm"),
      strip.text = ggplot2::element_text(size = 10, face = "plain", margin = ggplot2::margin(2, 0, 2, 0))
    ) +
    ggplot2::facet_grid(
      cols = ggplot2::vars(gender_EN, age_group),
      scales = "free_y", space = "free_y", drop = TRUE,
      labeller = ggplot2::labeller(age_group = ggplot2::label_value, gender_EN = ggplot2::label_value)
    )

  ggiraph::girafe(
    ggobj = plot1, height_svg = 4, width_svg = 14,
    options = list(
      ggiraph::opts_tooltip(opacity = .9,
                            css = "background-color:beige; color:black; padding:2px; border-radius:5px;"),
      ggiraph::opts_zoom(min = 0.5, max = 5),
      ggiraph::opts_sizing(rescale = TRUE),
      ggiraph::opts_toolbar(position = "topright"),
      ggiraph::opts_selection(type = "single", only_shiny = FALSE,
                              css = "stroke:#444444;stroke-width:0.4"),
      ggiraph::opts_hover(css = "stroke:#444444;stroke-width:0.4;cursor:pointer;")
    )
  )
}

#' Forest plot: prevalence ratios by gender per diagnosis code
#'
#' @param data Gender/parent2 dataset
#' @param gender_select Genders to include
#' @param code_select Parent0 codes to include
#' @param name1 Dataset 1 name
#' @param name2 Dataset 2 name
#' @param ci_range CI width range filter (numeric vector of 2)
#' @param fold_range Fold diff range filter (numeric vector of 2)
#' @return A girafe object
forest2_parent2 <- function(data, gender_select, code_select = NULL,
                            name1, name2, ci_range = c(0, Inf), fold_range = c(0, Inf),
                            scale_mode = "log2") {
  df_filter <- data %>%
    dplyr::filter(
      if (!is.null(gender_select)) gender_EN %in% gender_select else TRUE,
      if (!is.null(code_select)) parent0_code %in% code_select else TRUE,
      ci_width_nat >= ci_range[1], ci_width_nat <= ci_range[2],
      fold_diff_reg >= fold_range[1], fold_diff_reg <= fold_range[2]
    ) %>%
    dplyr::mutate(
      parent2_label = paste(parent2_code,
                            stringr::str_trunc(parent2_name_EN, width = 40, side = "right"),
                            sep = ", "),
      parent2_code_ordered = factor(parent2_code, levels = rev(sort(unique(parent2_code))))
    )

  num_rows <- length(unique(df_filter$parent2_code))
  plot_height_inches <- max(2, num_rows * 0.25)

  plot1 <- ggplot2::ggplot(df_filter, ggplot2::aes(x = prevalence_diff, y = parent2_code_ordered)) +
    ggplot2::annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf,
                      fill = COLOR_DATA1, alpha = 0.05) +
    ggplot2::annotate("rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf,
                      fill = COLOR_DATA2, alpha = 0.05) +
    ggplot2::geom_vline(xintercept = log2(1.3), color = COLOR_DOTLINE, linetype = 3, alpha = 0.9, linewidth = 0.7) +
    ggplot2::geom_vline(xintercept = log2(1 / 1.3), color = COLOR_DOTLINE, linetype = 3, alpha = 0.9, linewidth = 0.7) +
    ggplot2::geom_vline(xintercept = 0, color = "grey") +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = ci_low, xmax = ci_high, color = gender_EN, alpha = sig),
                           width = 0.2) +
    ggiraph::geom_point_interactive(ggplot2::aes(
      color = gender_EN, fill = gender_EN, alpha = sig,
      data_id = prevalence_diff,
      tooltip = paste0(
        parent2_code, ", ", parent2_name_EN,
        "\nGender: ", gender_EN,
        "\n", format_fold_tooltip(prevalence_diff),
        "\nFold (reg): ", round(fold_diff_reg, 2),
        "\np: ", round(p_value, 3),
        "\nLog2 diff: ", round(prevalence_diff, 2),
        " [", round(ci_low, 3), ", ", round(ci_high, 2), "]",
        "\nDiff: ", round(fold_diff_nat, 2),
        " [", round(fold_ci_low_nat, 3), ", ", round(fold_ci_high_nat, 3), "]",
        "\n", parent2_name
      )
    ), shape = 21, size = 2.5) +
    ggplot2::scale_color_manual(values = c("F" = COLOR_FEMALE, "M" = COLOR_MALE, "Both" = COLOR_ALL),
                                guide = "none") +
    ggplot2::scale_fill_manual(values = c("F" = COLOR_FEMALE, "M" = COLOR_MALE, "Both" = COLOR_ALL),
                               guide = "none") +
    ggplot2::scale_alpha_manual(values = c("nosig" = 0.5, "sig" = 1), guide = "none") +
    ggplot2::scale_x_continuous(
      if (scale_mode == "fold") "Prevalence Ratio (Fold Difference)" else "Prevalence Ratio (Log2)",
      breaks = c(-2, -1, 0, 1, 2),
      labels = if (scale_mode == "fold") c("0.25", "0.5", "1", "2", "4") else c("-2", "-1", "0", "1", "2"),
      limits = c(-2.5, 2.5)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = paste0("<span style='color:", COLOR_DATA1, ";'>Higher in ", name1,
                     " (blue)</span> <---> <span style='color: ", COLOR_DATA2,
                     ";'>Higher in ", name2, " (orange) </span>"),
      subtitle = paste("<span style='color: ", COLOR_DOTLINE, ";'>\u2022\u2022\u2022 Fold difference 1.3x. </span>
    <span style='color:", COLOR_FEMALE, ";'>Female</span>
      <span style='color:", COLOR_MALE, ";'>Male</span>
      <span style='color:", COLOR_ALL, ";'>Both</span>"),
      y = ""
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(parent2_label), scales = "free_y", space = "free_y",
      labeller = ggplot2::labeller(parent2_label = ggplot2::label_value)
    ) +
    ggplot2::theme(
      plot.title = ggtext::element_markdown(size = 11, face = "bold"),
      plot.subtitle = ggtext::element_markdown(size = 9),
      axis.text.x = ggplot2::element_text(size = 9),
      axis.title.x = ggplot2::element_text(size = 9),
      strip.text = ggplot2::element_text(size = 9),
      strip.text.x = ggplot2::element_text(size = 9),
      strip.text.y = ggplot2::element_text(size = 9, angle = 0, hjust = 0),
      strip.background = ggplot2::element_rect(fill = "#ededed", color = "white", linewidth = 0),
      panel.spacing.y = ggplot2::unit(0, "cm"),
      panel.spacing.x = ggplot2::unit(0.1, "cm")
    )

  ggiraph::girafe(
    ggobj = plot1, height_svg = plot_height_inches,
    options = list(
      ggiraph::opts_tooltip(opacity = .9,
                            css = "background-color:beige; color:black; padding:2px; border-radius:5px;"),
      ggiraph::opts_zoom(min = 0.5, max = 5),
      ggiraph::opts_toolbar(position = "topright"),
      ggiraph::opts_selection(type = "single"),
      ggiraph::opts_hover(css = "stroke:#444444;stroke-width:0.4;cursor:pointer;")
    )
  )
}

#' Point difference plot: prevalence values side-by-side across years
#'
#' @param data Meta-analysis dataset
#' @param select_age Age groups
#' @param select_gender Genders
#' @param select_code Parent2 codes
#' @param name1 Dataset 1 name
#' @param name2 Dataset 2 name
#' @return A girafe object
pointDiff1 <- function(data, select_age, select_gender, select_code, name1, name2, scale_mode = "log2") {
  data <- data %>%
    dplyr::filter(parent2_code %in% select_code) %>%
    dplyr::filter(gender_EN %in% select_gender) %>%
    dplyr::filter(age_group %in% select_age) %>%
    dplyr::filter(!(year %in% c("Meta")))

  data$lab <- data$age_group

  plot <- ggplot2::ggplot(data, ggplot2::aes(x = prevalence_data2, y = year)) +
    ggiraph::geom_bar_interactive(
      ggplot2::aes(
        x = prevalence_data1, y = year, color = "bar",
        tooltip = paste("Log2 PR:", round(data$prevalence_diff, 4),
                        "\n", format_fold_tooltip(data$prevalence_diff),
                        "\n", name1, ":", round(data$prevalence_data1, 4),
                        "(", data$patient_count_data1, "/", data$denominator_data1, ")",
                        "\n", name2, ":", round(data$prevalence_data2, 4),
                        "(", data$patient_count_data2, "/", data$denominator_data2, ")")
      ),
      stat = "identity", width = 0.01, position = ggplot2::position_nudge(y = 0)
    ) +
    ggiraph::geom_bar_interactive(
      ggplot2::aes(
        x = prevalence_data2, y = year, color = "bar",
        tooltip = paste("Log2 PR:", round(data$prevalence_diff, 4),
                        "\n", format_fold_tooltip(data$prevalence_diff),
                        "\n", name1, ":", round(data$prevalence_data1, 4),
                        "(", data$patient_count_data1, "/", data$denominator_data1, ")",
                        "\n", name2, ":", round(data$prevalence_data2, 4),
                        "(", data$patient_count_data2, "/", data$denominator_data2, ")")
      ),
      stat = "identity", width = 0.01, position = ggplot2::position_nudge(y = 0)
    ) +
    ggplot2::geom_segment(ggplot2::aes(x = prevalence_data1, xend = prevalence_data2,
                                       y = year, color = ".", fill = "."),
                          stat = "identity", show.legend = FALSE) +
    ggiraph::geom_point_interactive(ggplot2::aes(
      x = prevalence_data2, y = year, color = "data2",
      data_id = data$prevalence_data2,
      tooltip = paste(name2,
                      "\nPrevalence:", round(data$prevalence_data2, 4),
                      "\nPatient count in a gender-age group:", data$patient_count_data2,
                      "\nDenominator:", data$denominator_data2)
    ), stat = "identity", position = ggplot2::position_nudge(y = 0), show.legend = FALSE) +
    ggiraph::geom_point_interactive(ggplot2::aes(
      x = prevalence_data1, y = year, color = "data1",
      data_id = data$prevalence_data1,
      tooltip = paste(name1,
                      "\nPrevalence:", round(data$prevalence_data1, 4),
                      "\nPatient count:", data$patient_count_data1,
                      "\nDenominator:", data$denominator_data1)
    ), stat = "identity", position = ggplot2::position_nudge(y = 0), show.legend = FALSE) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = paste0(
      "Prevalence values in <span style='color: ", COLOR_DATA1, ";'>", name1,
      " (blue)</span> and <span style='color: ", COLOR_DATA2, ";'>", name2, " (orange)</span> - ",
      data$parent2_code[1], " ", data$parent2_name_EN[1]
    )) +
    ggplot2::xlab("Prevalence values") +
    ggplot2::theme(
      plot.title = ggtext::element_markdown(size = 12, face = "bold", hjust = 0),
      plot.subtitle = ggplot2::element_text(size = 10, face = "plain", hjust = 0, colour = "grey"),
      axis.text.x = ggplot2::element_text(face = "plain", size = 7, colour = "black"),
      axis.line.x = ggplot2::element_line(colour = "darkgray"),
      axis.title.x = ggplot2::element_text(face = "plain", size = 8, colour = "black"),
      axis.text.y = ggplot2::element_text(face = "plain", size = 8, colour = "black"),
      axis.ticks.y = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_line(colour = "darkgray"),
      axis.title.y = ggplot2::element_blank(),
      panel.spacing.x = ggplot2::unit(0.5, "cm"),
      strip.text = ggplot2::element_text(size = 10, face = "plain", margin = ggplot2::margin(2, 0, 2, 0))
    ) +
    ggplot2::guides(color = "none") +
    ggplot2::scale_x_continuous(
      breaks = scales::breaks_pretty(n = 3),
      labels = function(x) ifelse(x == 0, "0", signif(x, 2))
    ) +
    ggplot2::scale_color_manual(
      values = c("." = "black", "data1" = COLOR_DATA1, "data2" = COLOR_DATA2, "bar" = "#aabbbb"),
      name = "", guide = ggplot2::guide_legend(order = 1)
    ) +
    ggplot2::facet_grid(
      cols = ggplot2::vars(gender_EN, age_group),
      scales = "free_y", space = "free_y", drop = TRUE,
      labeller = ggplot2::labeller(gender_EN = ggplot2::label_value, age_group = ggplot2::label_value)
    )

  ggiraph::girafe(
    ggobj = plot, height_svg = 4, width_svg = 14,
    options = list(
      ggiraph::opts_tooltip(opacity = .9,
                            css = "background-color:beige; color:black; padding:2px; border-radius:5px;"),
      ggiraph::opts_zoom(min = 0.5, max = 5),
      ggiraph::opts_sizing(rescale = TRUE),
      ggiraph::opts_toolbar(position = "topright"),
      ggiraph::opts_selection(type = "single", only_shiny = FALSE,
                              css = "stroke:#444444;stroke-width:0.4"),
      ggiraph::opts_hover(css = "stroke:#444444;stroke-width:0.4;cursor:pointer;")
    )
  )
}

#' Forest plot for custom-selected diagnosis codes (by parent2_code)
#'
#' Similar to forest2_parent2 but filters directly by parent2_code
#' and includes all genders (F, M, Both).
#'
#' @param data Gender/parent2 dataset (gp_meta)
#' @param select_codes Character vector of parent2 codes to include
#' @param select_genders Character vector of genders to include
#' @param name1 Dataset 1 name
#' @param name2 Dataset 2 name
#' @return A girafe object
forest_custom <- function(data, select_codes, select_genders, name1, name2, scale_mode = "log2") {
  df_filter <- data %>%
    dplyr::filter(parent2_code %in% select_codes) %>%
    dplyr::filter(gender_EN %in% select_genders) %>%
    dplyr::mutate(
      parent2_label = paste(parent2_code,
                            stringr::str_trunc(parent2_name_EN, width = 40, side = "right"),
                            sep = ", "),
      parent2_code_ordered = factor(parent2_code, levels = sort(unique(parent2_code)))
    )

  if (nrow(df_filter) == 0) return(NULL)

  plot1 <- ggplot2::ggplot(df_filter, ggplot2::aes(x = prevalence_diff, y = parent2_code_ordered)) +
    ggplot2::annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf,
                      fill = COLOR_DATA1, alpha = 0.05) +
    ggplot2::annotate("rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf,
                      fill = COLOR_DATA2, alpha = 0.05) +
    ggplot2::geom_vline(xintercept = log2(1.3), color = COLOR_DOTLINE, linetype = 3, alpha = 0.9, linewidth = 0.7) +
    ggplot2::geom_vline(xintercept = log2(1 / 1.3), color = COLOR_DOTLINE, linetype = 3, alpha = 0.9, linewidth = 0.7) +
    ggplot2::geom_vline(xintercept = 0, color = "grey") +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = ci_low, xmax = ci_high, color = gender_EN, alpha = sig),
                           width = 0.2) +
    ggiraph::geom_point_interactive(ggplot2::aes(
      color = gender_EN, fill = gender_EN, alpha = sig,
      data_id = prevalence_diff,
      tooltip = paste0(
        parent2_code, ", ", parent2_name_EN,
        "\nGender: ", gender_EN,
        "\n", format_fold_tooltip(prevalence_diff),
        "\nFold (reg): ", round(fold_diff_reg, 2),
        "\np: ", round(p_value, 3),
        "\nLog2 diff: ", round(prevalence_diff, 2),
        " [", round(ci_low, 3), ", ", round(ci_high, 2), "]",
        "\nDiff: ", round(fold_diff_nat, 2),
        " [", round(fold_ci_low_nat, 3), ", ", round(fold_ci_high_nat, 3), "]",
        "\n", parent2_name
      )
    ), shape = 21, size = 2.5) +
    ggplot2::scale_color_manual(values = c("F" = COLOR_FEMALE, "M" = COLOR_MALE, "Both" = COLOR_ALL),
                                guide = "none") +
    ggplot2::scale_fill_manual(values = c("F" = COLOR_FEMALE, "M" = COLOR_MALE, "Both" = COLOR_ALL),
                               guide = "none") +
    ggplot2::scale_alpha_manual(values = c("nosig" = 0.5, "sig" = 1), guide = "none") +
    ggplot2::scale_x_continuous(
      if (scale_mode == "fold") "Prevalence Ratio (Fold Difference)" else "Prevalence Ratio (Log2)",
      breaks = c(-2, -1, 0, 1, 2),
      labels = if (scale_mode == "fold") c("0.25", "0.5", "1", "2", "4") else c("-2", "-1", "0", "1", "2"),
      limits = c(-2.5, 2.5)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = paste0("<span style='color:", COLOR_DATA1, ";'>Higher in ", name1,
                     " (blue)</span> <---> <span style='color: ", COLOR_DATA2,
                     ";'>Higher in ", name2, " (orange) </span>"),
      subtitle = paste("<span style='color: ", COLOR_DOTLINE, ";'>\u2022\u2022\u2022 Fold difference 1.3x. </span>
    <span style='color:", COLOR_FEMALE, ";'>Female</span>
      <span style='color:", COLOR_MALE, ";'>Male</span>
      <span style='color:", COLOR_ALL, ";'>Both</span>"),
      y = ""
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(parent2_label), scales = "free_y", space = "free_y",
      labeller = ggplot2::labeller(parent2_label = ggplot2::label_value)
    ) +
    ggplot2::theme(
      plot.title = ggtext::element_markdown(size = 11, face = "bold"),
      plot.subtitle = ggtext::element_markdown(size = 9),
      axis.text.x = ggplot2::element_text(size = 9),
      axis.title.x = ggplot2::element_text(size = 9),
      strip.text = ggplot2::element_text(size = 9),
      strip.text.x = ggplot2::element_text(size = 9),
      strip.text.y = ggplot2::element_text(size = 9, angle = 0, hjust = 0),
      strip.background = ggplot2::element_rect(fill = "#ededed", color = "white", linewidth = 0),
      panel.spacing.y = ggplot2::unit(0, "cm"),
      panel.spacing.x = ggplot2::unit(0.1, "cm")
    )

  ggiraph::girafe(
    ggobj = plot1, height_svg = 6, width_svg = 12,
    options = list(
      ggiraph::opts_tooltip(opacity = .9,
                            css = "background-color:beige; color:black; padding:2px; border-radius:5px;"),
      ggiraph::opts_zoom(min = 0.5, max = 5),
      ggiraph::opts_toolbar(position = "topright"),
      ggiraph::opts_selection(type = "single"),
      ggiraph::opts_hover(css = "stroke:#444444;stroke-width:0.4;cursor:pointer;")
    )
  )
}

#' Detail forest plot with heatmap tile row above Meta
#'
#' Same design as forest1 but adds a colored heatmap tile as a separate
#' row above Meta, labeled "Meta" on the y-axis.
#'
#' @param data Meta-analysis dataset
#' @param select_age Age groups to show
#' @param select_gender Genders to show
#' @param select_code Parent2 code (single) to show
#' @param name1 Dataset 1 name
#' @param name2 Dataset 2 name
#' @return A girafe object
forest_detail_custom <- function(data, select_age, select_gender, select_code, name1, name2, scale_mode = "log2") {
  data <- data %>%
    dplyr::filter(parent2_code %in% select_code) %>%
    dplyr::filter(gender_EN %in% select_gender) %>%
    dplyr::filter(age_group %in% select_age)

  if (nrow(data) == 0) return(NULL)

  # Create heatmap row (duplicate of Meta with a distinct year label)
  tile_data <- data %>%
    dplyr::filter(year == "Meta") %>%
    dplyr::mutate(year = "Heatmap")

  # Combine: original data + tile row
  all_data <- dplyr::bind_rows(data, tile_data)

  # Year factor — yearly rows, Meta diamond, spacer, Heatmap tile on top
  yearly <- sort(unique(data$year[data$year != "Meta"]))
  year_ordered <- c(yearly, "Meta", "", "Heatmap")
  all_data$year_f <- factor(all_data$year, levels = year_ordered)

  # Tile y position (numeric for geom_rect)
  tile_y <- as.numeric(factor("Heatmap", levels = year_ordered))

  # Subsets for layered drawing (avoids fill scale conflict)
  yearly_data <- all_data %>% dplyr::filter(!(year %in% c("Meta", "Heatmap")))
  meta_sig    <- all_data %>% dplyr::filter(year == "Meta", sig == "sig")
  meta_nosig  <- all_data %>% dplyr::filter(year == "Meta", sig == "nosig")

  plot1 <- ggplot2::ggplot(all_data, ggplot2::aes(x = prevalence_diff, y = year_f)) +
    # Background shading
    ggplot2::annotate(geom = "rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf,
                      fill = COLOR_DATA1, alpha = 0.2) +
    ggplot2::annotate(geom = "rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf,
                      fill = COLOR_DATA2, alpha = 0.1) +
    # Heatmap tile row (separate row above Meta)
    ggplot2::geom_rect(
      data = tile_data %>% dplyr::mutate(year_f = factor("Heatmap", levels = year_ordered)),
      ggplot2::aes(fill = prevalence_diff),
      xmin = -Inf, xmax = Inf,
      ymin = tile_y - 0.45, ymax = tile_y + 0.45,
      inherit.aes = FALSE
    ) +
    # Fold diff text labels on tile row
    ggplot2::geom_text(
      data = tile_data %>% dplyr::mutate(
        year_f = factor("Heatmap", levels = year_ordered),
        fold_label = ifelse(
          round(2^prevalence_diff, 2) < 1,
          paste0(round(2^prevalence_diff, 2), " (", round(1/round(2^prevalence_diff, 2), 1), ")"),
          as.character(round(2^prevalence_diff, 2))
        )
      ),
      ggplot2::aes(x = 0, y = year_f, label = fold_label),
      inherit.aes = FALSE,
      size = 2.5, color = "black", fontface = "bold"
    ) +
    ggplot2::scale_fill_gradient2(
      low = COLOR_DATA1, mid = "#e8e8e8", high = COLOR_DATA2,
      midpoint = 0, limits = c(-2, 2), oob = scales::squish,
      guide = "none"
    ) +
    # Reference line
    ggplot2::geom_vline(xintercept = 0, color = "grey") +
    # Error bars (yearly + Meta only, not tile row)
    ggplot2::geom_errorbar(
      data = all_data %>% dplyr::filter(year != "Heatmap"),
      ggplot2::aes(xmin = ci_low, xmax = ci_high, color = sig), width = 0.1
    ) +
    # Yearly points (shape 15)
    ggiraph::geom_point_interactive(
      data = yearly_data, shape = 15,
      ggplot2::aes(
        color = sig,
        data_id = prevalence_diff,
        tooltip = paste(
          "Log2 PR:", round(prevalence_diff, 3),
          "\n", format_fold_tooltip(prevalence_diff),
          "\n p-value", round(p_value, 3),
          "\n", name1, ":", round(prevalence_data1, 3),
          "(", patient_count_data1, "/", denominator_data1, ")",
          "\n", name2, ":", round(prevalence_data2, 3),
          "(", patient_count_data2, "/", denominator_data2, ")"
        )
      )
    ) +
    # Meta sig points (shape 23 filled diamond, darkred)
    {if (nrow(meta_sig) > 0)
      ggiraph::geom_point_interactive(
        data = meta_sig, shape = 23, fill = "darkred",
        ggplot2::aes(
          color = sig,
          data_id = prevalence_diff,
          tooltip = paste("Log2 PR:", round(prevalence_diff, 3),
                          "\n", format_fold_tooltip(prevalence_diff),
                          "\n Natural fold difference:", round(fold_diff_nat, 3),
                          "(Reg: ", round(fold_diff_reg, 3), ")",
                          "\n p-value", round(p_value, 3))
        )
      )
    } +
    # Meta nosig points (shape 23 filled diamond, grey)
    {if (nrow(meta_nosig) > 0)
      ggiraph::geom_point_interactive(
        data = meta_nosig, shape = 23, fill = "grey",
        ggplot2::aes(
          color = sig,
          data_id = prevalence_diff,
          tooltip = paste("Log2 PR:", round(prevalence_diff, 3),
                          "\n", format_fold_tooltip(prevalence_diff),
                          "\n Natural fold difference:", round(fold_diff_nat, 3),
                          "(Reg: ", round(fold_diff_reg, 3), ")",
                          "\n p-value", round(p_value, 3))
        )
      )
    } +
    ggplot2::scale_color_manual(values = c("sig" = "black", "nosig" = "grey"), guide = "none") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = paste0(
        "Prevalence higher in <span style='color: ", COLOR_DATA1, ";'>", name1,
        " (blue)</span> or <span style='color: ", COLOR_DATA2,
        ";'>", name2, " (orange)</span> - ",
        data$parent2_code[1], " ", data$parent2_name_EN[1]
      ),
      subtitle = paste0(
        "<span style='color: black;'>Significant</span>
         <span style='color: grey;'>Not significant </span>
         <span style='color: darkred;'>Meta-analysis</span>"
      )
    ) +
    ggplot2::xlab(
      if (scale_mode == "fold") {
        paste0("Fold Difference (<span style='color:", COLOR_DATA2, ";'>", name2,
               "</span> / <span style='color:", COLOR_DATA1, ";'>", name1, "</span>)")
      } else {
        paste0("Prevalence ratio, log(<span style='color:", COLOR_DATA2, ";'>", name2,
               " prevalence</span>/<span style='color:", COLOR_DATA1,
               ";'>", name1, " prevalence</span>, base = 2)")
      }
    ) +
    ggplot2::scale_x_continuous(
      breaks = if (scale_mode == "fold") c(-2, -1, 0, 1, 2) else pretty(data$prevalence_diff, n = 1),
      labels = if (scale_mode == "fold") c("0.25", "0.5", "1", "2", "4") else ggplot2::waiver()
    ) +
    # Rename "Heatmap" -> "Meta" on y-axis
    ggplot2::scale_y_discrete(drop = FALSE,
      labels = function(x) dplyr::case_when(x == "Heatmap" ~ "Meta", x == "" ~ "", TRUE ~ x)) +
    ggplot2::theme(
      plot.title = ggtext::element_markdown(size = 12, face = "bold", hjust = 0),
      plot.subtitle = ggtext::element_markdown(size = 10, face = "plain", hjust = 0),
      axis.text.x = ggplot2::element_text(face = "plain", size = 8, colour = "black"),
      axis.ticks.x = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_line(colour = "darkgray"),
      axis.title.x = ggtext::element_markdown(face = "plain", size = 8),
      axis.text.y = ggplot2::element_text(face = "plain", size = 8, colour = "black"),
      axis.ticks.y = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_line(colour = "darkgray"),
      axis.title.y = ggplot2::element_blank(),
      panel.spacing.x = ggplot2::unit(0.8, "cm"),
      strip.text = ggplot2::element_text(size = 10, face = "plain", margin = ggplot2::margin(2, 0, 2, 0))
    ) +
    ggplot2::facet_grid(
      cols = ggplot2::vars(gender_EN, age_group),
      scales = "free_y", space = "free_y", drop = TRUE,
      labeller = ggplot2::labeller(age_group = ggplot2::label_value, gender_EN = ggplot2::label_value)
    )

  ggiraph::girafe(
    ggobj = plot1, height_svg = 4, width_svg = 14,
    options = list(
      ggiraph::opts_tooltip(opacity = .9,
                            css = "background-color:beige; color:black; padding:2px; border-radius:5px;"),
      ggiraph::opts_zoom(min = 0.5, max = 5),
      ggiraph::opts_sizing(rescale = TRUE),
      ggiraph::opts_toolbar(position = "topright"),
      ggiraph::opts_selection(type = "none"),
      ggiraph::opts_hover(css = "stroke:#444444;stroke-width:0.4;cursor:pointer;")
    )
  )
}
