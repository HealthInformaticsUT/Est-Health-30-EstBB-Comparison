# scripts/build_parquet_cache.R
# Converts all CSV data files to Parquet for fast app startup.
# Run once after data files change; writes a .parquet sibling next to each .csv.
#
# Usage (from project root):
#   source("scripts/build_parquet_cache.R")
#
# Notes:
# - CSVs remain canonical (reproducibility, paper supplement).
# - Parquet files are a runtime acceleration cache; load_all_data() prefers
#   them if present and newer than the .csv, otherwise falls back to readr.

if (!requireNamespace("nanoparquet", quietly = TRUE)) {
  stop("nanoparquet is required. Install with install.packages('nanoparquet').")
}
if (!requireNamespace("readr", quietly = TRUE)) {
  stop("readr is required. Install with install.packages('readr').")
}

build_parquet <- function(csv_path) {
  parquet_path <- sub("\\.csv$", ".parquet", csv_path)
  t0 <- Sys.time()
  df <- readr::read_csv(csv_path, col_types = readr::cols())
  nanoparquet::write_parquet(df, parquet_path, compression = "zstd")
  dt_s <- as.numeric(Sys.time() - t0, units = "secs")
  csv_mb     <- file.info(csv_path)$size / 1e6
  parquet_mb <- file.info(parquet_path)$size / 1e6
  message(sprintf(
    "  %-55s %6.2fs  (%5.1f MB CSV -> %5.1f MB parquet, %.1fx)",
    basename(csv_path), dt_s, csv_mb, parquet_mb, csv_mb / parquet_mb
  ))
}

csv_files <- c(
  list.files("source_data", pattern = "\\.csv$", full.names = TRUE),
  list.files("comp_data",   pattern = "\\.csv$", full.names = TRUE)
)

if (length(csv_files) == 0) {
  stop("No CSV files found. Run from project root (where source_data/ and comp_data/ live).")
}

message("Building ", length(csv_files), " Parquet files...")
t_all <- Sys.time()
invisible(lapply(csv_files, build_parquet))
message(sprintf("Done. Total time: %.1fs",
                as.numeric(Sys.time() - t_all, units = "secs")))
