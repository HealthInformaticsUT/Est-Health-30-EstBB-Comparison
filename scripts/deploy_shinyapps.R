# deploy_shinyapps.R — Deploy EH30-EstBB-Clean to shinyapps.io
#
# Usage:
#   - From RStudio:  open this project, then `source("scripts/deploy_shinyapps.R")`
#   - From terminal: `Rscript scripts/deploy_shinyapps.R`
#
# Pre-requisite (one-time): the rsconnect account must already be authenticated
# under `j6pp26-maarja-pajusalu` on shinyapps.io. The token lives in
# ~/Library/Preferences/org.R-project.R/R/rsconnect/accounts/shinyapps.io/.
# If `rsconnect::accounts()` does not list the account, set it up via:
#   rsconnect::setAccountInfo(name="...", token="...", secret="...")

# --- Project root ----------------------------------------------------------
# Resolve the script's own folder so the script works regardless of getwd().
script_dir <- tryCatch(
  dirname(normalizePath(sys.frames()[[1]]$ofile, mustWork = FALSE)),
  error = function(e) NA_character_
)
candidate_root <- if (!is.na(script_dir) &&
                      file.exists(file.path(script_dir, "..", "global.R"))) {
  normalizePath(file.path(script_dir, ".."))
} else if (file.exists("global.R")) {
  normalizePath(".")
} else {
  stop("Cannot locate project root. Run this script from the project root, ",
       "or with `source(\"scripts/deploy_shinyapps.R\")` from any R session ",
       "where the working directory is at or below the project root.")
}
proj_root <- candidate_root
if (!file.exists(file.path(proj_root, "global.R"))) {
  stop("Cannot locate project root. Expected global.R at: ", proj_root)
}
cat("Project root:", proj_root, "\n")

# --- Required packages -----------------------------------------------------
need <- c("rsconnect", "nanoparquet")
miss <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
if (length(miss) > 0) {
  cat("Installing missing packages:", paste(miss, collapse = ", "), "\n")
  install.packages(miss)
}

# --- Pre-deploy sanity checks ----------------------------------------------
# 1. shinyapps.io account is configured on this machine
accs <- rsconnect::accounts()
if (!"j6pp26-maarja-pajusalu" %in% accs$name) {
  stop("shinyapps.io account 'j6pp26-maarja-pajusalu' not configured on this machine. ",
       "See header of this script for setup instructions.")
}

# 2. Every CSV referenced in config.R has a corresponding .parquet sibling
source(file.path(proj_root, "R/config.R"))
required_csvs <- c(
  vapply(SOURCE_FILES, identity, character(1)),
  vapply(COMPARISONS, function(x) x$meta_file, character(1)),
  vapply(COMPARISONS, function(x) x$gp_file, character(1))
)
required_parquets <- sub("\\.csv$", ".parquet", required_csvs)
missing_parquets <- required_parquets[!file.exists(file.path(proj_root, required_parquets))]
if (length(missing_parquets) > 0) {
  stop("Missing parquet files (CSV-free deploy will fail). ",
       "Run scripts/build_parquet_cache.R first. Missing:\n  ",
       paste(missing_parquets, collapse = "\n  "))
}
cat("All required parquet files present.\n")

# 3. .rscignore is in place
if (!file.exists(file.path(proj_root, ".rscignore"))) {
  warning(".rscignore missing — deploy will include CSV duplicates and bloat the bundle.")
}

# --- Deploy ----------------------------------------------------------------
cat("\nDeploying to shinyapps.io as 'eh30-estbb-comparison'...\n")
cat("Target URL: https://j6pp26-maarja-pajusalu.shinyapps.io/eh30-estbb-comparison/\n\n")

rsconnect::deployApp(
  appDir         = proj_root,
  appName        = "eh30-estbb-comparison",
  appTitle       = "EstBB vs Estonian general population — diagnosis prevalence",
  account        = "j6pp26-maarja-pajusalu",
  server         = "shinyapps.io",
  forceUpdate    = TRUE,
  launch.browser = TRUE
)
