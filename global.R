# global.R — Load packages, source modules, load data
# Auto-sourced by Shiny before ui.R and server.R

# Load packages
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(shinycssloaders)
library(DT)
library(gt)
library(ggplot2)
library(ggtext)
library(plotly)
library(ggiraph)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(scales)
library(webshot2)

# Pre-warm systemfonts cache.
# ggiraph calls font_family_exists() per text element when generating SVG,
# which scans every installed font (~232 ms per call). Warming the cache
# once + pre-registering default families lets subsequent lookups
# short-circuit. See PROJECT.md "Next session - plot generation
# performance" 2c for context.
local({
  if (requireNamespace("systemfonts", quietly = TRUE)) {
    invisible(systemfonts::system_fonts())
    for (fam in c("sans", "serif", "mono")) {
      try(systemfonts::register_font(
        name  = fam,
        plain = systemfonts::match_font(fam)$path
      ), silent = TRUE)
    }
  }
})

# Source R modules
source("R/config.R")
source("R/data_loading.R")
source("R/data_prep.R")
source("R/plot_histograms.R")
source("R/plot_heatmaps.R")
source("R/plot_forests.R")
# source("R/plot_volcano.R")  # Volcano tab hidden 2026-04-28; restore alongside ui.R/server.R volcano blocks.
source("R/table_helpers.R")
source("R/downloads.R")

# Load all data at startup
message("Loading all data...")
app_data <- load_all_data()
message("All data loaded.")
