# validate_workflow.R
#
# Phase C validation gate: run the cleaned workflow against the real
# source_data/, write the 4 meta1 + 4 meta2 CSVs to a temp directory, and
# compare each against the canonical files in comp_data/.
#
# Comparison is NUMERIC EQUIVALENCE (|new - canon| < 1e-10 absolute or
# < 1e-10 relative), not byte-identity. Strict byte-identity isn't
# achievable because the canonical files were produced with a different
# R / meta package / BLAS combination, and IEEE 754 dot products drift
# by 1 ulp under those changes. Numeric equivalence at 1e-10 is ~6 orders
# of magnitude tighter than any statistical interpretation depends on,
# so this is the correct gate for "algorithmically identical workflow".
#
# Pass = string/integer columns identical, all numeric cells within
# tolerance. Failure = a real algorithmic divergence to debug.
#
# Heterogeneity sidecars are NEW outputs so they're checked only for
# existence.

# Set WORKDIR to the absolute path of your local clone of EH30-EstBB-Clean.
# Example: WORKDIR <- "/Users/yourname/projects/EH30-EstBB-Clean"
WORKDIR <- ""  # <-- set this before running
source(file.path(WORKDIR, "demo_comparison_script", "run_workflow.R"))

TMP_OUT <- file.path(WORKDIR, "demo_comparison_script", "comp_data_check")
unlink(TMP_OUT, recursive = TRUE)
dir.create(TMP_OUT)

cat("=== Running cleaned workflow against real source_data/ ===\n\n")
run_workflow(
  source_dir  = file.path(WORKDIR, "source_data"),
  comp_dir    = TMP_OUT,
  lookups_dir = file.path(WORKDIR, "demo_comparison_script", "lookups"),
  pairs       = PAIRS
)

cat("\n=== Numeric equivalence check (tolerance 1e-10 abs or rel) ===\n")
CANON_DIR <- file.path(WORKDIR, "comp_data")
TOL <- 1e-10

suppressPackageStartupMessages(library(readr))

numeric_match <- function(path_a, path_b, tol = TOL) {
  if (!file.exists(path_a) || !file.exists(path_b))
    return(list(ok = FALSE, reason = "missing file"))
  a <- read_csv(path_a, col_types = cols(), show_col_types = FALSE)
  b <- read_csv(path_b, col_types = cols(), show_col_types = FALSE)
  if (nrow(a) != nrow(b)) return(list(ok = FALSE,
    reason = sprintf("row count: canon=%d, new=%d", nrow(a), nrow(b))))
  if (!identical(names(a), names(b))) return(list(ok = FALSE,
    reason = "column names differ"))

  max_abs <- 0; max_rel <- 0; worst_col <- ""
  for (cn in names(a)) {
    x <- a[[cn]]; y <- b[[cn]]
    if (is.numeric(x)) {
      both <- is.finite(x) & is.finite(y)
      d <- abs(x[both] - y[both])
      if (length(d) == 0) next
      ma <- max(d)
      mr <- max(d / pmax(abs(x[both]), abs(y[both]), 1e-300))
      if (ma > max_abs) { max_abs <- ma; worst_col <- cn }
      if (mr > max_rel) max_rel <- mr
      # NA pattern must match exactly
      if (!identical(is.na(x), is.na(y)))
        return(list(ok = FALSE,
          reason = sprintf("NA pattern in column %s", cn)))
    } else {
      if (!identical(x, y))
        return(list(ok = FALSE,
          reason = sprintf("non-numeric mismatch in column %s", cn)))
    }
  }
  ok <- (max_abs < tol) || (max_rel < tol)
  list(ok = ok, max_abs = max_abs, max_rel = max_rel, worst_col = worst_col)
}

bases <- vapply(PAIRS, `[[`, character(1), "base")
files <- c(
  paste0(bases, "_diff_meta.csv"),
  paste0(bases, "_diff_meta_genders_parent2.csv")
)

all_pass <- TRUE
for (f in files) {
  r <- numeric_match(file.path(TMP_OUT, f), file.path(CANON_DIR, f))
  if (isTRUE(r$ok)) {
    cat(sprintf("  %-55s  OK  (max |abs|=%.2e in %s)\n",
                f, r$max_abs, r$worst_col))
  } else {
    all_pass <- FALSE
    cat(sprintf("  %-55s  FAIL  %s\n", f,
                if (!is.null(r$reason)) r$reason else
                  sprintf("max |abs|=%.2e exceeds %.0e", r$max_abs, TOL)))
  }
}

cat("\nHeterogeneity sidecars (new files, existence check only):\n")
for (b in bases) {
  f <- paste0(b, "_heterogeneity.csv")
  exists_ok <- file.exists(file.path(TMP_OUT, f))
  cat(sprintf("  %-55s  %s\n", f, if (exists_ok) "OK (created)" else "MISSING"))
}

if (all_pass) {
  cat("\n=== PASS: cleaned workflow is numerically equivalent to canonical ===\n")
} else {
  cat("\n=== FAIL: numeric divergence detected ===\n")
}
