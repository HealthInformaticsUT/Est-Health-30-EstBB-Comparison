# EH30-EstBB-Clean — PROJECT.md

Modernized Shiny dashboard for EstBB vs Est-Health-30 prevalence comparison.
Companion repo to legacy `EH30_EstBB_Comparison_Repo_validate/` (the reference
that used a `global.R` multi-file Shiny convention).

---

## Architecture fix — DONE (2026-04-24)

Migrated `app.R` → `global.R`. Shiny now auto-sources `global.R` on every
launch, packages attach correctly, `%>%` errors resolved. Old `app.R` kept as
`app.R.bak` (backup; can be deleted once the setup has been stable for a
while).

---

## Theme E dashboard todos (Art2 Rev1, manuscript ties in `EH30_EstBB/Art2_Rev1/theme_E_editing_plan.md`)

Context: `theme_E_editing_plan.md` was the source of truth for dashboard edits
required by Art2 Nature Communications Rev1. This repo (`EH30-EstBB-Clean`)
was confirmed canonical for dashboard code 2026-04-23 via Tier 1 comparison
vs `EH30_EstBB_Comparison_Repo_validate`. All Theme E items are now done
(see below) — kept for reference.

### Done

- **G3 parity tweak** (manuscript) — DONE 2026-04-24
- **E3** code edit + verification across all 5 dropdowns/CSVs — DONE
- **E4** "How to use this tool" collapsible panel — DONE
- **E2** arXiv placeholder replacement (manuscript) — DONE
- **Response letter** assembly + final proofread, Discussion validated against
  `revision_plan_Rev1_v3.md` — DONE
- **E5 performance work**:
  - Phase 1 (parquet startup cache) — DONE 2026-04-24. Startup 4-6 s → 0.39 s.
  - Phase 2 (profvis profiling) — DONE 2026-04-24. Bottleneck = forest-plot
    rendering (91% wall time), every-pixel slider re-renders.
  - Phase 3 (debounce + bindCache) — DONE 2026-04-27. Partial improvement.
    Open question: `bindCache()` may silently no-op on `ggiraph::renderGirafe`.
    Fallback if cache isn't engaging: `memoise` inside render body.

---

## Plot generation performance — DONE 2026-05-06 (50% per-render improvement banked)

### Summary

Profiled with `profvis` across 4 iterations, applied 3 fixes, ~50%
reduction in per-render cost. Slider interactions still show a
perceptible pause but materially better than the pre-fix baseline.
Further wins are architectural, not micro-optimisations - documented
below for future reference.

### What worked

| Fix | File | Status | Impact |
|---|---|---|---|
| **2a. Coarsen slider steps** | `ui.R:148-153` | DONE | ci_filter step 0.1→0.5, fold_filter step 0.1→0.25. Reduces unique (low,high) cache keys from ~10k to ~440. |
| **2b. Round in debounced reactive** | `server.R:93-96` | DONE | Defense in depth - if step is ever changed back, rounding still produces stable cache keys. |
| **2c (Option 1). Pre-warm systemfonts cache** | `global.R:23-39` | DONE | Cut nested `system_fonts()` calls 1257 → 98 (12.8x). Per-render cost 215 ms → 104 ms. |

### What was tried but didn't work

**2c (Option 2). Memoise `systemfonts::font_family_exists` via namespace
patch.** Reverted 2026-05-06 - dead code. Reason: ggiraph's SVG backend
captures `font_family_exists` at package load time (early binding), so a
namespace-level swap doesn't reach the actual call site. The patched
function exists in the namespace but the hot path uses a captured
reference to the original. Confirmed via profvis: `font_family_exists`
still called ~62 times after the patch.

**Pattern A (memoise wrap on plot functions).** Considered, rejected
without trying. The cache key would be identical to what `bindCache()`
already uses, so it would hit the same ~8% rate. No gain.

### Performance numbers (CI slider scenario, profvis_runs/)

| Run | Wall time | ms/render | sys_fonts | Cache hit | Notes |
|---|---|---|---|---|---|
| Pre-fix run 1 | 115.7 s | 107 | 13.4 s | 35% | mixed activity |
| Pre-fix run 2 | 99.8 s | 137 | 12.5 s | 8% | pure slider drag |
| Post 2a+2b+2c-Opt1 | 82.8 s | **104** | 12.7 s | 4% | 50% per-render win |
| Post 2c-Opt2 (reverted) | 107.8 s | 110 | 15.1 s | 4% | no change |

**Cache hit rate of 4-8% on freeform drags is unavoidable** with
floating-point-keyed bindCache. The mechanism works (S3 dispatch
finds `bindCache.shiny.render.function` correctly), but each unique
slider position = unique cache key, and freeform drags naturally
generate many unique positions. The ~50% per-render win from font
caching is the real lever banked.

### Architectural levers for future revisits

If slider responsiveness ever needs to improve further, the wins from
here are structural:

1. **Lazy-render only the visible plot tab** instead of rendering all
   4 forest-by-gender plots simultaneously. Quartered cost per settle.
2. **Drop ggiraph for non-interactive plots.** The interactive hover
   is only essential where users click through; static `renderPlot()`
   on the rest avoids svglite/systemfonts overhead entirely.
3. **Pre-warm cache for common starting values** at app startup so
   first-touch interactions feel instant.
4. **File a ggiraph upstream issue** about `font_family_exists` being
   uncached. The fix belongs there, not in user code.

These are not on the active todo list. Document only.

### Additional UX task — graceful empty-state messages

When a slider filters the data down to nothing, the user currently sees
raw R errors bubbling up to the dashboard, e.g.
*"Faceting variables must have at least one value."*

Replace these with friendly explanatory messages in each affected
plot/render. Pattern:

```r
output$forest1 <- ggiraph::renderGirafe({
  shiny::validate(
    shiny::need(
      nrow(filtered_data()) > 0,
      "No diagnoses match the current filter combination. Try widening the CI or Fold range."
    )
  )
  forest1(filtered_data(), ...)
})
```

Check every render block that takes filtered data:
- `forest1`, `pointDiff1` (drill-down forest + dotplot)
- `forest2_parent2` × 4 (by-gender forests)
- `heatmap_meta_alph` 1-4 (detail heatmaps)
- Volcano plots
- Custom-tab plots

For each, identify the upstream filter that can produce an empty
result and add `shiny::validate(shiny::need(...))` with a message
that names the relevant filter and suggests how to fix it.

### After the fix lands

5. **Update the previous shinyapps.io deployment** with the new build:
   https://j6pp26-maarja-pajusalu.shinyapps.io/eh30-estbb-comparison/
   (redeploy via `rsconnect::deployApp()` from `EH30-EstBB-Clean/`).
6. **Add new deploy target — GitHub `healthinformaticsUT/...`** (confirm
   exact repo name on push). First-time push for this app to that org.
7. **Add new deploy target — `omop-apps.ut.ee`** (sysadmin-managed Shiny
   Server). Likely needs the flat-shiny / single-file pattern syrona-web
   uses; check whether `EH30-EstBB-Clean/` already runs as-is or needs a
   build step similar to `syrona/scripts/build_web.R`.

---

## Repo structure (reference)

```
EH30-EstBB-Clean/
├── global.R        ← auto-sourced by Shiny; loads packages, modules, data
├── app.R.bak       ← backup of old app.R (safe to delete later)
├── ui.R            ← stays as-is
├── server.R        ← stays as-is
├── speed_update_plan.md  ← Phase 3 plan for E5 (next session)
├── scripts/
│   └── build_parquet_cache.R  ← writes .parquet siblings next to CSVs
├── R/
│   ├── config.R           ← EXCLUDED_CHAPTERS <- c("V01-Y98") lives here
│   ├── data_loading.R     ← load_all_data() — edited 2026-04-24 for E3
│   ├── data_prep.R        ← cleanData(), topData(), histogram prep
│   ├── downloads.R        ← generic CSV download handler
│   ├── plot_forests.R
│   ├── plot_heatmaps.R
│   ├── plot_histograms.R
│   ├── plot_volcano.R
│   └── table_helpers.R
├── comp_data/      ← 4 comparison meta + gp_meta CSVs (8 files)
├── source_data/    ← 4 source CSVs
└── DALY_ICD.rds
```
