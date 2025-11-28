# global.R
# Install and load the required packages
#install.packages("shinyWidgets")
#install.packages("webshot2")
#install.packages("gt")
#install.packages("webshot2")
required_packages <- c( # Packages #####
  "DT",
  "dplyr",
  "forcats",
  "ggiraph",
  "ggplot2",
  "ggtext",
  "gt",
  "meta",
  "metafor",
  "patchwork",
  "plotly",
  "purrr",
  "readr",
  "readxl",
  "shiny",
  "shinyWidgets",
  "shinycssloaders",
  "shinydashboard",
  "shinyjs",
  "stringr",
  "tibble",
  "tidyr",
  "tidyverse",
  "webshot2"
)

lapply(required_packages, function(pkg) {
  #if (!requireNamespace(pkg, quietly = TRUE)) { install.packages(pkg) }
  library(pkg, character.only = TRUE)
})

# Source the function files #####
source("R/dataPrepFunctions.R")
source("R/plotFunctions.R")
source("R/plotFunctionsHistograms.R")
source("R/plotFunctionsGender.R")
source("R/tableHelpers.R")
source("R/downloads.R")


age_levels <- c("10-19", "20-29", "30-39",
                "40-49", "50-59", "60-69",
                "70-79", "80+")

name1 = "Est-Health-30"
name2 = "EstBB"
name3 = "EstBB1"
name4 = "EstBB2"

color_data1 <- "#005Fc8"
color_data2 <- "#ff6600"
color_data3 <- "#009E73"
color_data4 <-  "#F0D442"

color_male <- "#9990FF"
color_female <- "#FF69B4"
color_all <- "navy"
color_dotline <- "orangered"
color_lines <- "#333333"

blue_alpha <- "rgba(0, 95, 200, 0.2)"
orange_alpha <- "rgba(255, 102, 0, 0.2)"
gender_blue <- "rgba(153, 144, 255, 0.2)"
gender_pink <- "rgba(255, 105, 180, 0.2)"

# #### Helpers- could be in some other file?
# message("Loading pre-processed GBD data...")
# DALY_ICD <- read_rds("DALY_ICD.rds")
# message("Global data loaded.")

# --- 2. Load DALY Data (From previous step) --- ####
if(file.exists("DALY_ICD.rds")) {
  DALY_ICD <- readRDS("DALY_ICD.rds")
}

# --- 3. Load Source Data (Global Scope) ---
# We use prefix 'G_' to denote these are Global, Static variables.

message("Loading Source Data CSVs...") ######

G_upload1 <- readr::read_csv("source_data/EH30_diagnosis_gender_year_age_group.csv", col_types = readr::cols())
G_upload2 <- readr::read_csv("source_data/GI_diagnosis_gender_year_age_group.csv", col_types = readr::cols())
G_upload3 <- readr::read_csv("source_data/GI_diagnosis_gender_year_age_group_first.csv", col_types = readr::cols())
G_upload4 <- readr::read_csv("source_data/GI_diagnosis_gender_year_age_group_second.csv", col_types = readr::cols())

# --- 4. Load Meta Data (Global Scope) ---

message("Loading Meta Data CSVs...") #####

G_meta1 <- readr::read_csv("comp_data/EH30d1_EstBBd2_diff_meta.csv", col_types = readr::cols())
G_meta2 <- readr::read_csv("comp_data/EH30d1_EstBB1d2_diff_meta.csv", col_types = readr::cols())
G_meta3 <- readr::read_csv("comp_data/EH30d1_EstBB2d2_diff_meta.csv", col_types = readr::cols())
G_meta4 <- readr::read_csv("comp_data/EstBB1d1_EstBB2d2_diff_meta.csv", col_types = readr::cols())

# --- 5. Load & Filter Gender Parent Data (Global Scope) ---
# We perform the filtering here because 'V01-Y98' removal applies to everyone.

message("Loading and Filtering Gender/Parent Data...") #####

G_gp_meta1 <- readr::read_csv("comp_data/EH30d1_EstBBd2_diff_meta_genders_parent2.csv", col_types = readr::cols()) %>%
  dplyr::filter(parent0_code != "V01-Y98")

G_gp_meta2 <- readr::read_csv("comp_data/EH30d1_EstBB1d2_diff_meta_genders_parent2.csv", col_types = readr::cols()) %>%
  dplyr::filter(parent0_code != "V01-Y98")

G_gp_meta3 <- readr::read_csv("comp_data/EH30d1_EstBB2d2_diff_meta_genders_parent2.csv", col_types = readr::cols()) %>%
  dplyr::filter(parent0_code != "V01-Y98")

G_gp_meta4 <- readr::read_csv("comp_data/EstBB1d1_EstBB2d2_diff_meta_genders_parent2.csv", col_types = readr::cols()) %>%
  dplyr::filter(parent0_code != "V01-Y98")

message("All Global Data Loaded.") #####
