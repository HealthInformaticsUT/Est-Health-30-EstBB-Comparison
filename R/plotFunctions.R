#' Function Plot 1 - Forest2 genders and both by chapter
#' @param d1 data frame 1
#' @param d2 data frame 2
#' chane cols for separating genders
forest2_parent2 <- function(data,
                            gender_select,
                            code_select = NULL,
                            name1, name2,
                            ci_range = c(0, Inf),
                            fold_range = c(0, Inf)) {

  df_filter <- data %>%
    filter(
      # filter by gender if provided
      if (!is.null(gender_select)) gender_EN %in% gender_select else TRUE,
      # filter by codes if provided
      if (!is.null(code_select)) parent0_code %in% code_select else TRUE,
      ci_width_nat >= ci_range[1], ci_width_nat <= ci_range[2],
      fold_diff_reg >= fold_range[1], fold_diff_reg <= fold_range[2]
    ) %>%
  mutate(
    parent2_label = paste(parent2_code, str_trunc(parent2_name_EN, width = 40, side = "right"), sep = ", "),
    parent2_code_ordered = factor(parent2_code, levels = rev(sort(unique(parent2_code))))
  )

# Use 0.4 to 0.5 inches per row for readability
num_rows <- length(unique(df_filter$parent2_code))
# Ensure a minimum height of 4 inches
plot_height_inches <- max(4, num_rows * 0.5)
#
  plot1 <- ggplot(df_filter, aes(x = prevalence_diff, y = parent2_code_ordered)) +
    annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf,
             fill = color_data1, alpha = 0.05) +
    annotate("rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf,
             fill = color_data2, alpha = 0.05) +
    geom_vline(xintercept = log2(1.3), color = color_dotline, linetype = 3, alpha=0.9, size=0.7) +
    geom_vline(xintercept = log2(1/1.3), color = color_dotline, linetype = 3, alpha=0.9, size=0.7) +
    geom_vline(xintercept = 0, color = "grey") +
    geom_errorbar(aes(xmin = ci_low, xmax = ci_high, color = gender_EN, alpha = sig),
                  width = 0.2) +
    geom_point_interactive(aes(
      color = gender_EN,
      fill = gender_EN,
      alpha = sig,
      data_id = prevalence_diff,
      tooltip = paste0(
        parent2_code, ", ", parent2_name_EN,
        "\nGender: ", gender_EN,
        "\nFold: ", round(fold_diff_reg, 2),
        "\np: ", round(p_value, 3),
        "\n",
        "\nLog2 diff: ", round(prevalence_diff, 2),
        " [", round(ci_low, 3), ", ", round(ci_high, 2), "]",
        "\nDiff: ", round(fold_diff_nat, 2),
        " [", round(fold_ci_low_nat, 3), ", ", round(fold_ci_high_nat, 3), "]",
        "\n", parent2_name
      )), shape = 21, size = 2.5) +
    scale_color_manual(values = c("F" = color_female,
                                  "M" = color_male,
                                  "Both" = color_all), guide = "none") +
    scale_fill_manual(values = c("F" = color_female,
                                 "M" = color_male,
                                 "Both" = color_all), guide = "none") +
    scale_alpha_manual(values = c("nosig" = 0.2, "sig" = 1), guide = "none") +
    scale_x_continuous("log2(prevalence ratio)") +
    theme_minimal() +
    labs(
      title = paste0("Prevalence ratios by diagnosis and gender: <span style='color:",
                     color_male, ";'>Male</span> <span style='color:",
                     color_female, ";'>Female</span>"),
      subtitle = paste("<span style='color:", color_data1, ";'>Prevalence higher in ",
                       name1, " (blue)</span> or <span style='color: ",
                       color_data2, ";'>prevalence higher in ", name2, " (orange)</span>"),
      #x = "Prevalence Difference (log2)",
      y = ""
    ) +
    facet_grid(rows = vars(parent2_label),
               #cols = vars(gender_EN),
               scales = "free_y",
               space = "free_y", #"fixed",
               labeller = labeller(parent2_label = label_value)) + #, gender_EN = label_value
    theme(
      plot.title = element_markdown(size = 12, face = "bold"),
      plot.subtitle = element_markdown(size = 9),
      axis.text.x = element_text(size = 9),
      axis.title.x = element_text(size = 7),
      strip.text = element_text(size = 9),
      strip.text.x = element_text(size=9),
      strip.text.y = element_text(size=9, angle = 0, hjust = 0),
      strip.background = element_rect(fill = "#ededed", color="white", size = 0),
      panel.spacing.y = unit(0, "cm"),
      panel.spacing.x = unit(0.1, "cm")
    )
  girafe(ggobj = plot1,
         #width_svg = ,
         height_svg = plot_height_inches,
         options = list(
           opts_tooltip(opacity = .9,
                        css = "background-color:beige; color:black; padding:2px; border-radius:5px;"),
           opts_zoom(min = 0.5, max = 5),
           opts_toolbar(position = "topright"),
           opts_selection(type = "single"),
           opts_hover(css = "stroke:#444444;stroke-width:0.4;cursor:pointer;")
         ))
}

#' Function Plot 2 - heatmap
#' @param d1 data frame 1
#' @param d2 data frame 2
heatmap_meta_alph <- function(dataIn, select_age, select_gender, select_code, name1, name2){
  data <- dataIn %>%
    filter(parent0_code %in% select_code) %>%
    filter(gender_EN %in% select_gender) %>%
    filter(age_group %in% select_age) %>%
    filter(year %in% c("Meta")) %>%
    drop_na(prevalence_diff)

  avg_data <- data %>%
    group_by(parent2_code) %>%
    summarize(avg_prevalence_diff = mean(prevalence_diff, na.rm = TRUE)) %>%
    arrange(avg_prevalence_diff) %>%
    mutate(order = ifelse(is.na(avg_prevalence_diff), max(rank(avg_prevalence_diff, na.last = TRUE), na.rm = TRUE) + 1,
                          rank(avg_prevalence_diff, na.last = TRUE)))
  data <- data %>%
    left_join(avg_data, by = "parent2_code")

  data$parent0_label <- paste(data$parent0_code, str_wrap(data$parent0_name, width = 280), sep = ", ")
  data$parent1_label <- paste(data$parent1_code, str_wrap(data$parent1_name, width = 280), sep = ", ")
  data$age_label <- factor(data$age_group, levels = age_levels)
  data$age_label <- paste(substr(data$age_group, nchar(data$age_group) - nchar(data$age_group) + 1, 2),"..", sep = "")

  data$parent2_code <- factor(data$parent2_code, levels = rev(sort(unique(data$parent2_code))))
  data$parent2_label_alph <- paste(
    data$parent2_code,
    str_trunc(data$parent2_name_EN, width = 40, side = "right"),
    sep = ", "
  )
  #data$parent2_label_alph <- paste(data$parent2_code, str_wrap(data$parent2_name, width = 120), sep = ", ")
  data$parent2_label_alph <- factor(data$parent2_label_alph, levels = sort(unique(data$parent2_label_alph)))
  data$parent2_label_avg <- factor(data$parent2_label_alph, levels = unique(data$parent2_label_alph[order(data$order, decreasing = FALSE)]))

  num_rows <- length(unique(data$parent2_code))
  # Ensure a minimum height of 4 inches
  plot_height_inches <- max(1, num_rows * 0.15)

  plot <- ggplot(data, aes(x = age_group, y = parent2_label_alph, fill = prevalence_diff)) +
    geom_tile_interactive(color = NA, width = 1.25,
                          aes(
                            tooltip = paste0(
                              parent2_code, ", ", parent2_name_EN,
                              "\nGender: ", gender_EN,
                              #"\nFold: ", round(fold_diff_reg, 2),
                              "\np: ", round(p_value, 3),
                              "\n",
                              "\nLog2 diff: ", round(prevalence_diff, 2),
                              " [", round(ci_low, 3), ", ", round(ci_high, 2), "]",
                              #"\nDiff: ", round(fold_diff_nat, 2),
                              #" [", round(fold_ci_low_nat, 3), ", ", round(fold_ci_high_nat, 3), "]",
                              "\n", parent2_name
                            ), data_id = parent2_code)

                          #   tooltip = str_c(parent2_code, ", ", parent2_name , ", ", gender, "-",age_group,
                          #                     "\nlog((prev_data2/prev_data1), base=2): ", round(prevalence_diff, 4)
                          # ), data_id = parent2_code)
              ) +
    scale_fill_gradient2(low = color_data1, mid = "#e8e8e8", high = color_data2,
                         midpoint = 0, limits = c(-2, 2), oob = scales::squish, name = "Prevalence ratio") +
    labs(title = paste("Meta alph Prevalence higher in <span style='color:", color_data1, ";'>", name1,
                       " (blue)</span> or <span style='color: ", color_data2, ";'>", name2, " (orange)</span>"),
         subtitle = paste0("Prevalence ratio = log((<span style='color:", color_data2, ";'>", name2, " prevalence</span>/
           <span style='color:", color_data1, ";'>", name1, " prevalence</span>), base = 2)"),
         x = "Age Group",
         y = "Diagnoses") +
    theme_classic() +
    theme(
      plot.title = element_markdown(size = 12, face = "bold", hjust = 0),
      plot.subtitle = element_markdown(size = 10, face = "plain", hjust = 0),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line.x = element_blank(),
      axis.title.x = element_text(face = "plain", size = 7, colour = "black"),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank(),
      axis.title.y = element_blank(),
      strip.text.x = element_text(size=8),
      strip.text.y = element_text(size=9, angle = 0, hjust = 0),
      strip.background = element_rect(fill = "#ededed", color="white", size = 0),
      panel.spacing.y = unit(0, "cm"),
      panel.spacing.x = unit(0, "cm")
    ) +
    facet_grid(rows = vars(parent2_label_alph),
               cols = vars(gender_EN, age_label),
               scales = "free",
               space = "free_y",
               labeller = labeller(gender_EN = label_value, age_label = label_value, parent2_label_alph = label_value))


  #return(plot)
  girafe(ggobj = plot,
         height_svg = 11, #plot_height_inches,
         width_svg = 12,
         options = list(
           opts_tooltip(
             opacity = .9,
             css = "background-color:beige; color:black; padding:2px; border-radius:5px;"
           ),
           opts_toolbar(position = "topright"),
           opts_selection(type = "single", only_shiny = FALSE,
                          css = "stroke:#999999;stroke-width:0.4"),
           opts_hover(css = "stroke:#999999;stroke-width:0.4;cursor:pointer;")
         ))

}
#' Function
#' @param d1 data frame 1
#' @param d2 data frame 2
forest1 <- function(data, select_age, select_gender, select_code, name1, name2) {

  process_data <- function(data, select_code, select_gender, select_age) {
    data <- data %>%
      filter(parent2_code %in% select_code) %>%
      filter(gender_EN %in% select_gender) %>%
      filter(age_group %in% select_age)
    return(data)
  }

  data <- process_data(data, select_code, select_gender, select_age)

  data$color_fill_category <- case_when(
    data$year == "Meta" & data$sig == "sig" ~ "Meta_sig",
    data$year == "Meta" & data$sig == "nosig" ~ "Meta_nosig",
    data$year != "Meta" & data$sig == "sig" ~ "Year_sig",
    data$year != "Meta" & data$sig == "nosig" ~ "Year_nosig"
  )

  avg1 <- round(mean(data$patient_count_data1, na.rm = TRUE), 0)
  avg2 <- round(mean(data$patient_count_data2, na.rm = TRUE), 0)

  plot1 <- ggplot(data, aes(
    x = prevalence_diff,
    y = factor(year, levels = c(unique(data$year))),
    fill = color_fill_category,
    color = sig
  )) +
    annotate(geom = "rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf,
             fill = color_data1, alpha = 0.2) +
    annotate(geom = "rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf,
             fill = color_data2, alpha = 0.1) +
    geom_vline(xintercept = 0, color = "grey") +
    geom_errorbar(aes(xmin = ci_low, xmax = ci_high, color = sig), width = 0.1) +
    geom_point_interactive(
      shape = ifelse(data$year == "Meta", 23, 15),
      aes(
        color = sig,
        fill = color_fill_category,
        data_id = prevalence_diff,
        tooltip = ifelse(
          data$year == "Meta",
          paste("Prevalence ratio log (base = 2):", round(data$prevalence_diff, 3),
                "\n Natural fold difference:", round(data$fold_diff_nat, 3), "(Reg: ", round(fold_diff_reg, 3), ")",
                "\n p-value", round(data$p_value, 3)),
          paste(
            "Prevalence ratio log:", round(data$prevalence_diff, 3),
            "\n p-value", round(data$p_value, 3),
            "\n", name1, ":", round(data$prevalence_data1, 3),
            "(", data$patient_count_data1, "/", data$denominator_data1, ")",
            "\n", name2, ":", round(data$prevalence_data2, 3),
            "(", data$patient_count_data2, "/", data$denominator_data2, ")"
          )
        )
      )
    ) +
    scale_color_manual(values = c("sig" = "black", "nosig" = "grey"), guide = "none") +
    scale_fill_manual(values = c("Meta_sig" = "darkred", "Meta_nosig" = "grey"), guide = "none") +
    theme_minimal() +
    labs(
      title = paste0(
        "Prevalence higher in <span style='color: ", color_data1, ";'>", name1,
        " (blue)</span> or <span style='color: ", color_data2,
        ";'>", name2, " (orange)</span> - ",
        data$parent2_code[1], " ",
        data$parent2_name_EN[1]
      ),
      subtitle = paste0(
        "<span style='color: black;'>Significant</span>
         <span style='color: grey;'>Not significant </span>
         <span style='color: darkred;'>Meta-analysis</span>"
      )
    ) +
    xlab(paste0(
      "Prevalence ratio, log(<span style='color:", color_data2, ";'>", name2,
      " prevalence</span>/<span style='color:", color_data1,
      ";'>", name1, " prevalence</span>, base = 2)"
    )) +
    scale_x_continuous(breaks = pretty(data$prevalence_diff, n = 1)) +
    theme(
      plot.title = element_markdown(size = 12, face = "bold", hjust = 0),
      plot.subtitle = element_markdown(size = 10, face = "plain", hjust = 0),
      axis.text.x = element_text(face = "plain", size = 8, colour = "black"),
      axis.ticks.x = element_blank(),
      axis.line.x = element_line(colour = "darkgray"),
      axis.title.x = element_markdown(face = "plain", size = 8),
      axis.text.y = element_text(face = "plain", size = 8, colour = "black"),
      axis.ticks.y = element_blank(),
      axis.line.y = element_line(colour = "darkgray"),
      axis.title.y = element_blank(),
      panel.spacing.x = unit(0.8, "cm"),
      strip.text = element_text(size = 10, face = "plain", margin = margin(2, 0, 2, 0))
    ) +
    facet_grid(
      cols = vars(gender_EN, age_group),
      scales = "free_y",
      space = "free_y",
      drop = TRUE,
      labeller = labeller(age_group = label_value, gender_EN = label_value)
    )
  #return(plot1)
  girafe(
    ggobj = plot1,
    height_svg = 4,
    width_svg = 14,
    options = list(
      opts_tooltip(
        opacity = .9,
        css = "background-color:beige; color:black; padding:2px; border-radius:5px;"
      ),
      opts_zoom(min = 0.5, max = 5),
      opts_sizing(rescale = TRUE),
      opts_toolbar(position = "topright"),
      opts_selection(type = "single", only_shiny = FALSE,
                     css = "stroke:#444444;stroke-width:0.4"),
      opts_hover(css = "stroke:#444444;stroke-width:0.4;cursor:pointer;")
    )
  )
}
#' Function of detailed prevalence values
#' @param d1 data frame 1
#' @param d2 data frame 2
pointDiff1 <- function(data,select_age, select_gender, select_code, name1, name2){
  process_data <- function(data, select_code, select_gender, select_age) {
    data1 <- data %>%
      filter(parent2_code %in% select_code) %>%
      filter(gender_EN %in% select_gender) %>%
      filter(age_group %in% select_age) %>%
      filter(!(year %in% c("Meta")))
    return(data1)
  }
  data <- process_data(data, select_code, select_gender, select_age)
  data$lab <- data$age_group
  avg1 <- round(mean(data$patient_count_data1, na.rm = TRUE),0)
  avg2 <- round(mean(data$patient_count_data2, na.rm = TRUE),0)

  plot <- ggplot(data, aes(x= prevalence_data2, y= year)) +
    ggiraph::geom_bar_interactive(ggplot2::aes(x=prevalence_data1, y=year, color="bar",
                                               tooltip = paste("Prevalence ratio log:", round(data$prevalence_diff, 4),
                                                               "\n", name1, ":", round(data$prevalence_data1, 4),
                                                               "(", data$patient_count_data1, "/", data$denominator_data1, ")",
                                                               "\n", name2, ":", round(data$prevalence_data2, 4),
                                                               "(", data$patient_count_data2, "/", data$denominator_data2, ")")),
                                  stat = "identity", width = 0.01, position = position_nudge(y = 0)) +
    ggiraph::geom_bar_interactive(ggplot2::aes(x=prevalence_data2, y=year, color="bar",
                                               tooltip = paste("Prevalence ratio log:", round(data$prevalence_diff, 4),
                                                               "\n", name1, ":", round(data$prevalence_data1, 4),
                                                               "(", data$patient_count_data1, "/", data$denominator_data1, ")",
                                                               "\n", name2, ":", round(data$prevalence_data2, 4),
                                                               "(", data$patient_count_data2, "/", data$denominator_data2, ")")),
                                  stat = "identity", width = 0.01, position = position_nudge(y = 0)) +

    geom_segment(aes(x=prevalence_data1, xend=prevalence_data2,
                     y=year, color=".", fill=".", tooltip = paste("Prevalence ratio log:", round(data$prevalence_diff, 4),
                                                                  "\n", name1, ":", round(data$prevalence_data1, 4),
                                                                  "(", data$patient_count_data1, "/", data$denominator_data1, ")",
                                                                  "\n", name2, ":", round(data$prevalence_data2, 4),
                                                                  "(", data$patient_count_data2, "/", data$denominator_data2, ")")),
                 stat = "identity", show.legend = FALSE)+
    geom_point_interactive(aes(x=prevalence_data2, y=year, color="data2",
                               #alpha = patient_count_data2,
                               data_id = data$prevalence_data2,
                               tooltip = paste(name2,
                                               "\nPrevalence:", round(data$prevalence_data2,4),
                                               "\nPatient count in a grnder-age group:", data$patient_count_data2,
                                               "\nDenominator:", data$denominator_data2)),
                           stat = "identity",  position = position_nudge(y = 0), show.legend = FALSE) +
    geom_point_interactive(aes(x=prevalence_data1, y=year, color="data1",
                               #alpha = patient_count_data1,
                               data_id = data$prevalence_data1,
                               tooltip = paste(name1,
                                               "\nPrevalence:", round(data$prevalence_data1,4),
                                               "\nPatient count:", data$patient_count_data1,
                                               "\nDenominator:", data$denominator_data1)),
                           stat = "identity", position = position_nudge(y = 0), show.legend = FALSE)+
    theme_minimal() +
    labs(title = paste0("Prevalence values in <span style='color: ", color_data1, ";'>", name1,
                        " (blue)</span> and <span style='color: ", color_data2, ";'>", name2, " (orange)</span> - ",
                        data$parent2_code[1], " ",
                        data$parent2_name_EN[1]))+
    xlab("Prevalence values") +
    theme(
      plot.title = element_markdown(size = 12, face = "bold", hjust = 0),
      plot.subtitle = element_text(size = 10, face = "plain", hjust = 0, colour = "grey"),
      axis.text.x = element_text(face = "plain", size = 8, colour = "black"),
      axis.line.x = element_line(colour = "darkgray"),
      axis.title.x = element_text(face = "plain", size = 8, colour = "black"),
      axis.text.y = element_text(face = "plain", size = 8, colour = "black"),
      axis.ticks.y = element_blank(),
      axis.line.y = element_line(colour = "darkgray"),
      axis.title.y = element_blank(),
      panel.spacing.x = unit(0.5, "cm"),
      strip.text = element_text(size = 10, face = "plain", margin = margin(2, 0, 2, 0))
    ) +
    guides(color = "none") +
    scale_x_continuous(breaks = pretty(data$prevalence_data2, n = 2)) +
    scale_color_manual(values = c("." = "black", #"Biobank1" = "#FDd770",
                                  "data1" = color_data1, "data2" = color_data2, "bar" = "#aabbbb"),
                       name = "",
                       guide = guide_legend(order = 1)) +
    facet_grid(
      cols =  vars(gender_EN, age_group),
      scales = "free_y", space = "free_y", drop = TRUE,
      labeller = labeller(gender_EN = label_value, age_group = label_value))
  #return(plot)
  girafe(ggobj = plot,
         height_svg =4,
         width_svg = 14,
         options = list(
           opts_tooltip(
             opacity = .9,
             css = "background-color:beige; color:black; padding:2px; border-radius:5px;"
           ),
           opts_zoom(min = 0.5, max = 5),
           opts_sizing(rescale = TRUE),
           opts_toolbar(position = "topright"),
           opts_selection(type = "single", only_shiny = FALSE,
                          css = "stroke:#444444;stroke-width:0.4"),
           opts_hover(css = "stroke:#444444;stroke-width:0.4;cursor:pointer;")
         ))

}
#' Function of chapters heatmap
#' @param d1 data frame 1
#' @param d2 data frame 2
#'
heatmap_meta_avg <- function(dataIn, select_age, select_gender, select_code, name1, name2){
  data <- dataIn %>%
    filter(parent0_code %in% select_code) %>%
    filter(gender_EN %in% select_gender) %>%
    filter(age_group %in% select_age) %>%
    filter(year %in% c("Meta")) %>%
    drop_na(prevalence_diff)

  avg_data <- data %>%
    group_by(parent2_code) %>%
    summarize(avg_prevalence_diff = mean(prevalence_diff, na.rm = TRUE)) %>%
    arrange(avg_prevalence_diff) %>%
    mutate(order = ifelse(is.na(avg_prevalence_diff), max(rank(avg_prevalence_diff, na.last = TRUE), na.rm = TRUE) + 1,
                          rank(avg_prevalence_diff, na.last = TRUE)))
  data <- data %>%
    left_join(avg_data, by = "parent2_code")

  data$parent0_label <- paste(data$parent0_code, str_wrap(data$parent0_name_EN, width = 30), sep = "\n")
  data$parent1_label <- paste(data$parent1_code, str_wrap(data$parent1_name, width = 120), sep = ", ")
  data$age_label <- factor(data$age_group, levels = age_levels)

  data$parent2_code <- factor(data$parent2_code, levels = rev(sort(unique(data$parent2_code))))
  data$parent2_label_alph <- paste(data$parent2_code, str_wrap(data$parent2_name_EN, width = 120), sep = ", ")
  data$parent2_label_alph <- factor(data$parent2_label_alph, levels = rev(sort(unique(data$parent2_label_alph))))

  data$parent2_label_avg <- factor(data$parent2_label_alph, levels = unique(data$parent2_label_alph[order(data$order, decreasing = FALSE)]))

  plot <- ggplot(data, aes(x = age_group, y = parent2_label_avg, fill = prevalence_diff)) +
    geom_tile_interactive(color = NA, width = 1.25,
                          aes(tooltip = str_c(parent2_code, ", ", parent2_name_EN , ", ", gender_EN, "-", age_group,
                                              "\nlog((prev_data2/prev_data1), base=2): ", round(prevalence_diff, 4),
                                              "\np: ", round(p_value, 3),
                                              "\n", parent2_name
                          ),
                          data_id = parent0_code)) +
    scale_fill_gradient2(low = color_data1, mid = "#e8e8e8", high = color_data2,
                         midpoint = 0, limits = c(-2, 2), oob = scales::squish, name = "Prevalence ratio") +
    labs(title = paste("Prevalence higher in <span style='color:", color_data1, ";'>", name1,
                       " (blue)</span> or <span style='color: ", color_data2, ";'>", name2, " (orange)</span>"),
         subtitle = paste0("Prevalence ratio = log((<span style='color:", color_data2, ";'>", name2, " prevalence</span>/
           <span style='color:", color_data1, ";'>", name1, " prevalence</span>), base = 2)"),
         x = "Age Group",
         y = "Diagnoses") +
    theme_classic() +
    theme(
      plot.title = element_markdown(size = 12, face = "bold", hjust = 0),
      plot.subtitle = element_markdown(size = 10, face = "plain", hjust = 0),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line.x = element_blank(),
      axis.title.x = element_text(face = "plain", size = 8, colour = "black"),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank(),
      axis.title.y = element_blank(),
      strip.text.x = element_text(size=8),
      strip.text.y = element_text(size=9, angle = 0, hjust = 0),
      strip.background = element_rect(fill = "#ededed", color="white", size = 0),
      panel.spacing.y = unit(0.1, "cm"),
      panel.spacing.x = unit(0, "cm")
    ) +
    facet_grid(rows = vars(parent0_label),
               #facet_grid(rows = vars(parent2_label),
               cols = vars(gender_EN, age_label),
               scales = "free",
               space = "free_y",
               labeller = labeller(gender_EN = label_value, age_label = label_value, parent0_label = label_value))

  girafe(ggobj = plot,
         height_svg = 11,
         width_svg = 12,
         options = list(
           opts_tooltip(
             opacity = .9,
             css = "background-color:beige; color:black; padding:2px; border-radius:5px;"
           ),
           opts_toolbar(position = "topright"),
           opts_selection(type = "single", only_shiny = FALSE,
                          css = "stroke:#999999;stroke-width:0.4"),
           opts_hover(css = "stroke:#999999;stroke-width:0.4;cursor:pointer;")
         ))

}
