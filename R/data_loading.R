# data_loading.R — Centralized data loading
# Returns a structured list replacing the 12+ global variables from legacy app

# Helper: read a data file, preferring .parquet sibling when available.
# Use parquet if (a) nanoparquet is installed and (b) the parquet exists,
# and either the CSV is missing or the parquet is at least as fresh as the CSV.
# This lets the app run from a parquet-only deploy bundle (CSVs stripped).
read_data_file <- function(csv_path) {
  parquet_path <- sub("\\.csv$", ".parquet", csv_path)
  parquet_ok <- requireNamespace("nanoparquet", quietly = TRUE) &&
    file.exists(parquet_path)
  csv_ok <- file.exists(csv_path)

  use_parquet <- parquet_ok && (
    !csv_ok ||
    file.info(parquet_path)$mtime >= file.info(csv_path)$mtime
  )

  if (use_parquet) {
    nanoparquet::read_parquet(parquet_path)
  } else if (csv_ok) {
    readr::read_csv(csv_path, col_types = readr::cols())
  } else {
    stop("Neither parquet nor CSV found for: ", csv_path)
  }
}

# Helper: time a block and emit a message with wall-clock seconds
timed <- function(label, expr) {
  t0 <- Sys.time()
  out <- force(expr)
  dt <- as.numeric(Sys.time() - t0, units = "secs")
  message(sprintf("  %-30s %6.2fs", label, dt))
  out
}

#' Load all datasets at app startup
#'
#' @return Named list with:
#'   - source: list of 4 source dataframes (d1..d4, V01-Y98 removed)
#'   - meta: list of 4 meta-analysis dataframes (V01-Y98 removed)
#'   - gp_meta: list of 4 gender/parent2 dataframes (V01-Y98 removed)
#'   - DALY: DALY_ICD dataframe (or NULL if file missing)
load_all_data <- function() {
  # Source data (4 datasets)
  source_data <- timed("source data", lapply(SOURCE_FILES, function(path) {
    df <- read_data_file(path)
    dplyr::filter(df, !(parent0_code %in% EXCLUDED_CHAPTERS))
  }))

  # Meta-analysis results (4 comparisons)
  meta_data <- timed("meta-analysis data", lapply(COMPARISONS, function(comp) {
    df <- read_data_file(comp$meta_file)
    dplyr::filter(df, !(parent0_code %in% EXCLUDED_CHAPTERS))
  }))

  # Gender/parent2 stratified data (4 comparisons)
  gp_meta_data <- timed("gender/parent2 data", lapply(COMPARISONS, function(comp) {
    df <- read_data_file(comp$gp_file)
    dplyr::filter(df, !(parent0_code %in% EXCLUDED_CHAPTERS))
  }))

  # DALY burden data
  DALY <- if (file.exists("DALY_ICD.rds")) {
    timed("DALY data", readRDS("DALY_ICD.rds"))
  } else {
    message("  DALY_ICD.rds not found, skipping.")
    NULL
  }

  list(
    source  = source_data,
    meta    = meta_data,
    gp_meta = gp_meta_data,
    DALY    = DALY
  )
}
