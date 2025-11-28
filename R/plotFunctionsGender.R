#plotFunctionsGender.R
# This file contains functions for the Gender-Specific Histogram plots.
# Input 'dd' is the joined data frame containing columns: p_vs_bb, p_vs_bb1, p_vs_bb2, gender_EN

# ==============================================================================
# MALE PLOTS (Row 1)
# ==============================================================================

create_p_histogram_bbM <- function(dd) {
  # Filter for Males internally using gender_EN
  plot_data <- dd %>% dplyr::filter(gender_EN == "M")

  ggplot(aes(x = 2**p_vs_bb), data = plot_data) +
    geom_histogram(bins = 30) +
    ylab("MALES")+
    labs(title = "",  #"PR Distribution of Males vs Females Between Datasets",
         subtitle ="•••• Difference 1.3 times") +
    scale_x_continuous("", trans = "log2", limits = c(0.125, 4)) +
    scale_y_continuous("MALES", limits = c(0, 240)) +
    geom_vline(xintercept = 1, color = "grey10", linetype = 1, alpha=0.9, size = 0.8) +
    geom_vline(xintercept = 1.3, color = color_dotline, linetype = 3, alpha=0.9, size=0.5) +
    geom_vline(xintercept = 1/1.3, color = color_dotline, linetype = 3, alpha=0.9, size=0.5) +
    annotate("text", x = 1/1.7, y = 200, label = "Est-Health-30", hjust = 1, vjust = 0, size = 3, color = "black") +
    annotate("text", x = 1.7, y = 200, label = "EstBB", hjust = 0, vjust = 0, size = 3, color = "black") +
    theme_bw()+
    theme(
      #axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
      plot.subtitle = element_text(color = color_dotline, size = 8),
      plot.title = element_text(size = 10),
      legend.position = "none",
      plot.background = element_rect(color = "grey90", size = 1)
    )
}

create_p_histogram_bb1M <- function(dd) {
  plot_data <- dd %>% dplyr::filter(gender_EN == "M")

  ggplot(aes(x = 2**p_vs_bb1), data = plot_data) +
    geom_histogram(bins = 30) +
    scale_x_continuous("", trans = "log2", limits = c(0.125, 4)) +
    scale_y_continuous("", limits = c(0, 240)) +
    geom_vline(xintercept = 1, color = "grey10", linetype = 1, alpha=0.9, size = 0.8) +
    geom_vline(xintercept = 1.3, color = color_dotline, linetype = 3, alpha=0.9, size=0.5) +
    geom_vline(xintercept = 1/1.3, color = color_dotline, linetype = 3, alpha=0.9, size=0.5) +
    annotate("text", x = 1/1.7, y = 200, label = "Est-Health-30", hjust = 1, vjust = 0, size = 3, color = "black") +
    annotate("text", x = 1.7, y = 200, label = "EstBB1", hjust = 0, vjust = 0, size = 3, color = "black") +
    theme_bw() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none",
      plot.background = element_rect(color = "grey90", size = 1)
    )
}

create_p_histogram_bb2M <- function(dd) {
  plot_data <- dd %>% dplyr::filter(gender_EN == "M")

  ggplot(aes(x = 2**p_vs_bb2), data = plot_data) +
    geom_histogram(bins = 30) +
    scale_x_continuous("", trans = "log2", limits = c(0.125, 4)) +
    scale_y_continuous("", limits = c(0, 240)) +
    geom_vline(xintercept = 1, color = "grey10", linetype = 1, alpha=0.9, size = 0.8) +
    geom_vline(xintercept = 1.3, color = color_dotline, linetype = 3, alpha=0.9, size=0.5) +
    geom_vline(xintercept = 1/1.3, color = color_dotline, linetype = 3, alpha=0.9, size=0.5) +
    annotate("text", x = 1/1.7, y = 200, label = "Est-Health-30", hjust = 1, vjust = 0, size = 3, color = "black") +
    annotate("text", x = 1.7, y = 200, label = "EstBB2", hjust = 0, vjust = 0, size = 3, color = "black") +
    theme_bw() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none",
      plot.background = element_rect(color = "grey90", size = 1)
    )
}

# ==============================================================================
# FEMALE PLOTS (Row 2)
# ==============================================================================

create_p_histogram_bbF <- function(dd) {
  plot_data <- dd %>% dplyr::filter(gender_EN == "F")

  ggplot(aes(x = 2**p_vs_bb), data = plot_data) +
    geom_histogram(bins = 30) +
    ylab("FEMALES")+
    scale_x_continuous("prev_EstBB / prev_Est-Health-30", trans = "log2", limits = c(0.125, 4)) +
    scale_y_continuous("FEMALES", limits = c(0, 240)) +
    geom_vline(xintercept = 1, color = "grey10", linetype = 1, alpha=0.9, size = 0.8) +
    geom_vline(xintercept = 1.3, color = color_dotline, linetype = 3, alpha=0.9, size=0.5) +
    geom_vline(xintercept = 1/1.3, color = color_dotline, linetype = 3, alpha=0.9, size=0.5) +
    annotate("text", x = 1/1.7, y = 200, label = "Est-Health-30", hjust = 1, vjust = 0, size = 3, color = "black") +
    annotate("text", x = 1.7, y = 200, label = "EstBB", hjust = 0, vjust = 0, size = 3, color = "black") +
    theme_bw()+
    theme(
      #axis.title.y = element_blank(),
      axis.title.x = element_text(size=8),
      legend.position = "none",
      plot.background = element_rect(color = "grey90", size = 1)
    )
}

create_p_histogram_bb1F <- function(dd) {
  plot_data <- dd %>% dplyr::filter(gender_EN == "F")

  ggplot(aes(x = 2**p_vs_bb1), data = plot_data) +
    geom_histogram(bins = 30) +
    scale_x_continuous("prev_EstBB1 / prev_Est-Health-30", trans = "log2", limits = c(0.125, 4)) +
    scale_y_continuous("", limits = c(0, 240)) +
    geom_vline(xintercept = 1, color = "grey10", linetype = 1, alpha=0.9, size = 0.8) +
    geom_vline(xintercept = 1.3, color = color_dotline, linetype = 3, alpha=0.9, size=0.5) +
    geom_vline(xintercept = 1/1.3, color = color_dotline, linetype = 3, alpha=0.9, size=0.5) +
    annotate("text", x = 1/1.7, y = 200, label = "Est-Health-30", hjust = 1, vjust = 0, size = 3, color = "black") +
    annotate("text", x = 1.7, y = 200, label = "EstBB1", hjust = 0, vjust = 0, size = 3, color = "black") +
    theme_bw() +
    theme(
      axis.title.x = element_text(size=8),
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "none",
      plot.background = element_rect(color = "grey90", size = 1)
    )
}

create_p_histogram_bb2F <- function(dd) {
  plot_data <- dd %>% dplyr::filter(gender_EN == "F")

  ggplot(aes(x = 2**p_vs_bb2), data = plot_data) +
    geom_histogram(bins = 30) +
    scale_x_continuous("prev_EstBB2 / prev_Est-Health-30", trans = "log2", limits = c(0.125, 4)) +
    scale_y_continuous("", limits = c(0, 240)) +
    geom_vline(xintercept = 1, color = "grey10", linetype = 1, alpha=0.9, size = 0.8) +
    geom_vline(xintercept = 1.3, color = color_dotline, linetype = 3, alpha=0.9, size=0.5) +
    geom_vline(xintercept = 1/1.3, color = color_dotline, linetype = 3, alpha=0.9, size=0.5) +
    annotate("text", x = 1/1.7, y = 200, label = "Est-Health-30", hjust = 1, vjust = 0, size = 3, color = "black") +
    annotate("text", x = 1.7, y = 200, label = "EstBB2", hjust = 0, vjust = 0, size = 3, color = "black") +
    theme_bw() +
    theme(
      axis.title.x = element_text(size=8),
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "none",
      plot.background = element_rect(color = "grey90", size = 1)
    )
}
