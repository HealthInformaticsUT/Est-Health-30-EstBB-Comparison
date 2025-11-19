#### plotFunctionsHistograms.R
# This file contains functions that create ggplot objects.
# Each function takes the prepared data as an input.

# --- Plot for p_histogram_bb ---
create_p_histogram_bb <- function(plot_data) {
  # The `color_dotline` variable is available from global.R

  p <- ggplot(aes(x = 2**p_vs_bb), data = plot_data) +
    geom_histogram(bins=30) +
    ylab("Count")+
    labs(title = "", #"PR Distribution Between Datasets + Burden",
         subtitle ="•••• Difference 1.3 times") +
    scale_y_continuous(limits = c(0, 180)) +
    scale_x_continuous("", trans = "log2", limits = c(0.125, 4)) +
    geom_vline(xintercept = 1, color = "grey10", linetype = 1, alpha=0.9, size = 0.8) +
    geom_vline(xintercept = 1.3, color = color_dotline, linetype = 3, alpha=0.9, size=0.5) +
    geom_vline(xintercept = 1/1.3, color = color_dotline, linetype = 3, alpha=0.9, size=0.5) +
    annotate("text", x = 1/1.7, y = 160, label = "EH30", hjust = 1, vjust = 0, size = 3, color = "black") +
    annotate("text", x = 1.7, y = 160, label = "EstBB", hjust = 0, vjust = 0, size = 3, color = "black") +
    theme_bw()+
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
      plot.subtitle = element_text(color = color_dotline, size = 8),
      plot.title = element_text(size = 10),
      axis.title.y = element_text(size = 9),
      plot.background = element_rect(color = "grey10" , linewidth = 0.1),
      legend.position = "none"
    )
  return(p)
}

# --- Plot for p_death_bb ---
create_p_death_bb <- function(plot_data) {
  p <- ggplot(aes(x = 2**p_vs_bb, y = 1, size = YLL2021norm, color = YLL2021norm,
                  text = paste0(
                    parent2_code, ", ", parent2_name_EN,
                    "\nFold: ", round(2**p_vs_bb, 2),
                    "\np: ", round(p_vs_bb_p, 3),
                    "\n"
                  )),
              data = plot_data %>% arrange(YLL2021norm)) +
    geom_point(position = position_jitter(seed = 42)) +
    scale_size_continuous(range = c(0.3,4)) +
    scale_color_gradient(low = "grey50", high = "black")  +
    scale_x_continuous(trans = "log2", limits = c(0.125, 4)) +
    geom_vline(xintercept = 1, color = "grey10", linetype = 1, alpha=0.9, size = 0.8) +
    geom_vline(xintercept = 1.3, color = color_dotline, linetype = 3, alpha=0.9, size=0.5) +
    geom_vline(xintercept = 1/1.3, color = color_dotline, linetype = 3, alpha=0.9, size=0.5) +
    ylab("Size = Years of\n Life Lost") +
    theme_bw() +
    theme(
      axis.text = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none"
    )

  return(p)
}

# --- Plot for p_disability_bb ---
create_p_disability_bb <- function(plot_data) {
  p <- ggplot(aes(x = 2**p_vs_bb, y = 1, size = YLD2021norm, color = YLD2021norm,
                  text = paste0(
    parent2_code, ", ", parent2_name_EN,
    "\nFold: ", round(2**p_vs_bb, 2),
    "\np: ", round(p_vs_bb_p, 3),
    "\n"
  )),
              data = plot_data %>% arrange(YLL2021norm)) +
    geom_point(position = position_jitter(seed = 42)) +
    scale_size_continuous(range = c(0.3,4)) +
    ylab("Size = Years Lost\n to Disability")+
    scale_color_gradient(low = "grey50", high = "black") +
    scale_x_continuous("prev_EstBB / prev_EH30", trans = "log2", limits = c(0.125, 4)) +
    geom_vline(xintercept = 1, color = "grey10", linetype = 1, alpha=0.9, size = 0.8) +
    geom_vline(xintercept = 1.3, color = color_dotline, linetype = 3, alpha=0.9, size=0.5) +
    geom_vline(xintercept = 1/1.3, color = color_dotline, linetype = 3, alpha=0.9, size=0.5) +
    theme_bw() +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_text(size=8),
      legend.position = "none"
    )

  return(p)
}


# --- Functions for Column 2 (bb1) ---

create_p_histogram_bb1 <- function(plot_data) {
  p <- ggplot(aes(x = 2**p_vs_bb1), data = plot_data) +
    geom_histogram(bins=30) +
    scale_y_continuous("", limits = c(0, 180)) +
    scale_x_continuous("", trans = "log2", limits = c(0.125, 4)) +
    geom_vline(xintercept = 1, color = "grey10", linetype = 1, alpha=0.9, size = 0.8) +
    geom_vline(xintercept = 1.3, color = color_dotline, linetype = 3, alpha=0.9, size=0.5) +
    geom_vline(xintercept = 1/1.3, color = color_dotline, linetype = 3, alpha=0.9, size=0.5) +
    annotate("text", x = 1/1.7, y = 160, label = "EH30", hjust = 1, vjust = 0, size = 3, color = "black") +
    annotate("text", x = 1.7, y = 160, label = "EstBB1", hjust = 0, vjust = 0, size = 3, color = "black") +
    theme_bw() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none"
    )

  return(p)
}

create_p_death_bb1 <- function(plot_data) {
  p <- ggplot(aes(x = 2**p_vs_bb1, y = 1, size = YLL2021norm, color = YLL2021norm,
                  text = paste0(
                    parent2_code, ", ", parent2_name_EN,
                    "\nFold: ", round(2**p_vs_bb1, 2),
                    "\np: ", round(p_vs_bb1_p, 3),
                    "\n"
                  )),
              data = plot_data %>% arrange(YLL2021norm)) +

    geom_point(position = position_jitter(seed = 42)) +
    scale_size_continuous(range = c(0.3,4)) +
    scale_color_gradient(low = "grey50", high = "black") +
    scale_x_continuous(trans = "log2", limits = c(0.125, 4)) +
    geom_vline(xintercept = 1, color = "grey10", linetype = 1, alpha=0.9, size = 0.8) +
    geom_vline(xintercept = 1.3, color = color_dotline, linetype = 3, alpha=0.9, size=0.5) +
    geom_vline(xintercept = 1/1.3, color = color_dotline, linetype = 3, alpha=0.9, size=0.5) +
    theme_bw() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none"
    )

  return(p)
}

create_p_disability_bb1 <- function(plot_data) {
  p <- ggplot(aes(x = 2**p_vs_bb1, y = 1, size = YLD2021norm, color = YLD2021norm,
                  text = paste0(
                    parent2_code, ", ", parent2_name_EN,
                    "\nFold: ", round(2**p_vs_bb1, 2),
                    "\np: ", round(p_vs_bb1_p, 3),
                    "\n"
                  )),
              data = plot_data %>% arrange(YLL2021norm)) +
    geom_point(position = position_jitter(seed = 42)) +
    scale_size_continuous(range = c(0.3, 4)) +
    scale_color_gradient(low = "grey50", high = "black") +
    scale_x_continuous("prev_EstBB1 / prev_EH30", trans = "log2", limits = c(0.125, 4)) +
    geom_vline(xintercept = 1, color = "grey10", linetype = 1, alpha=0.9, size = 0.8) +
    geom_vline(xintercept = 1.3, color = color_dotline, linetype = 3, alpha=0.9, size=0.5) +
    geom_vline(xintercept = 1/1.3, color = color_dotline, linetype = 3, alpha=0.9, size=0.5) +
    theme_bw() +
    theme(
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_text(size = 8),
      axis.ticks.y = element_blank(),
      legend.position = "none"
    )

  return(p)
}


# --- Functions for Column 3 (bb2) ---

create_p_histogram_bb2 <- function(plot_data) {
  p <- ggplot(aes(x = 2**p_vs_bb2), data = plot_data) +
    geom_histogram(bins=30) +
    scale_y_continuous("", limits = c(0, 180)) +
    scale_x_continuous("", trans = "log2", limits = c(0.125, 4)) +
    geom_vline(xintercept = 1, color = "grey10", linetype = 1, alpha=0.9, size = 0.8) +
    geom_vline(xintercept = 1.3, color = color_dotline, linetype = 3, alpha=0.9, size=0.5) +
    geom_vline(xintercept = 1/1.3, color = color_dotline, linetype = 3, alpha=0.9, size=0.5) +
    annotate("text", x = 1/1.7, y = 160, label = "EH30", hjust = 1, vjust = 0, size = 3, color = "black") +
    annotate("text", x = 1.7, y = 160, label = "EstBB2", hjust = 0, vjust = 0, size = 3, color = "black") +
    theme_bw() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none"
    )

  return(p)
}

create_p_death_bb2 <- function(plot_data) {
  p <- ggplot(aes(x = 2**p_vs_bb2, y = 1, size = YLL2021norm, color = YLL2021norm,
                  text = paste0(
                    parent2_code, ", ", parent2_name_EN,
                    "\nFold: ", round(2**p_vs_bb2, 2),
                    "\np: ", round(p_vs_bb2_p, 3),
                    "\n"
                  )),
              data = plot_data %>% arrange(YLL2021norm)) +
    geom_point(position = position_jitter(seed = 42)) +
    scale_size_continuous(range = c(0.3,4)) +
    scale_color_gradient(low = "grey50", high = "black")  +
    scale_x_continuous(trans = "log2", limits = c(0.125, 4)) +
    geom_vline(xintercept = 1, color = "grey10", linetype = 1, alpha=0.9, size = 0.8) +
    geom_vline(xintercept = 1.3, color = color_dotline, linetype = 3, alpha=0.9, size=0.5) +
    geom_vline(xintercept = 1/1.3, color = color_dotline, linetype = 3, alpha=0.9, size=0.5) +
    theme_bw() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none"
    )

  return(p)
}

create_p_disability_bb2 <- function(plot_data) {
  p <- ggplot(aes(x = 2**p_vs_bb2, y = 1, size = YLD2021norm, color = YLD2021norm,
                  text = paste0(
                    parent2_code, ", ", parent2_name_EN,
                    "\nFold: ", round(2**p_vs_bb2, 2),
                    "\np: ", round(p_vs_bb2_p, 3),
                    "\n"
                  )),
              data = plot_data %>% arrange(YLL2021norm)) +
    geom_point(position = position_jitter(seed = 42)) +
    scale_size_continuous(range = c(0.3,4)) +
    scale_color_gradient(low = "grey50", high = "black") +
    scale_x_continuous("prev_EstBB2 / prev_EH30", trans = "log2", limits = c(0.125, 4)) +
    geom_vline(xintercept = 1, color = "grey10", linetype = 1, alpha=0.9, size = 0.8) +
    geom_vline(xintercept = 1.3, color = color_dotline, linetype = 3, alpha=0.9, size=0.5) +
    geom_vline(xintercept = 1/1.3, color = color_dotline, linetype = 3, alpha=0.9, size=0.5) +
    theme_bw() +
    theme(
      axis.text.y = element_blank(),
      axis.title = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_text(size=8),
      legend.position = "none"
    )

  return(p)
}
