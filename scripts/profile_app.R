# scripts/profile_app.R
# Helper for the 4 profiling scenarios in PROJECT.md step 1.
# Each run starts the Shiny app under profvis, you interact in the browser,
# then close the browser tab to stop. Output saves to profvis_runs/.
#
# Usage from project root:
#   source("scripts/profile_app.R")
#   profile_app("ci_slider")        # then do the CI slider scenario
#   profile_app("fold_slider")      # then do the Fold slider scenario
#   profile_app("heatmap_nav")      # then do the heatmap tab scenario
#   profile_app("forest_drilldown") # then do the drill-down scenario

if (!requireNamespace("profvis", quietly = TRUE)) {
  stop("profvis is required. install.packages('profvis')")
}

SCENARIO_RECIPES <- list(
  ci_slider = c(
    "1. Wait for app to fully load (heatmaps visible).",
    "2. Switch to 'PR Values by Gender per Diagnosis' tab.",
    "3. Pick a diagnosis from the dropdown.",
    "4. Drag the CI slider slowly across the full range, end to end.",
    "5. Drag it back to the start.",
    "6. Close the browser tab to stop profiling."
  ),
  fold_slider = c(
    "1. Wait for app to fully load.",
    "2. Switch to 'PR Values by Gender per Diagnosis' tab.",
    "3. Pick a diagnosis from the dropdown.",
    "4. Drag the Fold slider slowly across the full range, end to end.",
    "5. Drag it back to the start.",
    "6. Close the browser tab to stop profiling."
  ),
  heatmap_nav = c(
    "1. Wait for app to fully load.",
    "2. On the main heatmap tab, click a chapter to drill down (first detail render).",
    "3. Switch to a second heatmap (e.g. heatmap_meta_alph2 -- another comparison).",
    "4. Switch back to the first heatmap (cache-hit test).",
    "5. Switch to a third heatmap, then back.",
    "6. Close the browser tab to stop profiling."
  ),
  forest_drilldown = c(
    "1. Wait for app to fully load.",
    "2. Click a chapter on the main heatmap.",
    "3. Click a diagnosis code in the detail heatmap (triggers forest1 + pointDiff1).",
    "4. Click a different diagnosis code (re-render).",
    "5. Click back to the first diagnosis (cache-hit test).",
    "6. Close the browser tab to stop profiling."
  )
)

profile_app <- function(scenario) {
  if (!scenario %in% names(SCENARIO_RECIPES)) {
    stop("Unknown scenario. Choose one of: ",
         paste(names(SCENARIO_RECIPES), collapse = ", "))
  }

  out_dir <- "profvis_runs"
  dir.create(out_dir, showWarnings = FALSE)
  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  out_path <- file.path(out_dir, sprintf("profvis_%s_%s.html", scenario, stamp))

  message("\n==== Scenario: ", scenario, " ====")
  message("Recipe (do these in the browser, then close the tab):")
  message(paste0("  ", SCENARIO_RECIPES[[scenario]], collapse = "\n"))
  message("\nStarting app under profvis. Output will save to:\n  ", out_path, "\n")

  p <- profvis::profvis({
    shiny::runApp(".", launch.browser = TRUE)
  })

  htmlwidgets::saveWidget(p, out_path, selfcontained = TRUE)
  message("\nSaved profile: ", out_path)
  message("Open it in any browser, or use the RStudio Viewer if it auto-opened.")
  invisible(out_path)
}
