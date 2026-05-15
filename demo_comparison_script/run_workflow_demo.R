# run_workflow_demo.R
#
# Demo-only driver: defaults are pinned to demo_source_data/ and
# demo_comp_data/, so running this with no edits can NEVER touch
# canonical source_data/ or comp_data/.
#
# Use this when you want to:
#   - exercise the workflow end-to-end on the 30-code demo set
#   - regenerate demo_comp_data/ after editing workflow_functions.R
#   - hand a reviewer something they can run without risk to canonical
#
# For runs against real data, use run_workflow.R (separate driver,
# defaults to source_data/ → comp_data/).

# Set WORKDIR to the absolute path of your local clone of EH30-EstBB-Clean.
# Example: WORKDIR <- "/Users/yourname/projects/EH30-EstBB-Clean"
WORKDIR <- ""  # <-- set this before running
source(file.path(WORKDIR, "demo_comparison_script", "R", "workflow_functions.R"))
source(file.path(WORKDIR, "demo_comparison_script", "run_workflow.R"))  # defines PAIRS + run_workflow()

# ── Hard-coded demo paths (do not change) ──────────────────────────────

DEMO_SOURCE_DIR <- file.path(WORKDIR, "demo_comparison_script", "demo_source_data")
DEMO_COMP_DIR   <- file.path(WORKDIR, "demo_comparison_script", "demo_comp_data")
LOOKUPS_DIR     <- file.path(WORKDIR, "demo_comparison_script", "lookups")

# Safety belt: refuse to overwrite canonical comp_data/ even if someone
# edits the paths above by mistake.
canonical_comp <- file.path(WORKDIR, "comp_data")
if (normalizePath(DEMO_COMP_DIR, mustWork = FALSE) ==
    normalizePath(canonical_comp, mustWork = FALSE)) {
  stop("DEMO_COMP_DIR resolves to canonical comp_data/. ",
       "Use run_workflow.R for real-data runs; this script is demo-only.",
       call. = FALSE)
}

# ── Run ────────────────────────────────────────────────────────────────

if (sys.nframe() == 0) {
  run_workflow(DEMO_SOURCE_DIR, DEMO_COMP_DIR, LOOKUPS_DIR, PAIRS)
}
