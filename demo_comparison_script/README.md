---
editor_options: 
  markdown: 
    wrap: 72
---

# EH30 vs EstBB pre-Syrona comparison workflow

To enrich the source diagnosis CSVs and run the two-level meta-analysis
cascade that produces every `*_diff_meta.csv` and
`*_diff_meta_genders_parent2.csv` file the dashboard consumes.

## Files

```         
demo_comparison_script/
├── run_workflow.R              # REAL-DATA driver (overwrites canonical comp_data/)
├── run_workflow_demo.R         # DEMO driver (defaults to demo paths, safety-belted)
├── R/
│   └── workflow_functions.R    # all pipeline functions, sourced by both drivers
├── lookups/
│   ├── ICD10_chapters.csv      # parent0_code -> parent0_name_EN + short_name
│   └── icd10_category_EN.csv   # parent2_code -> parent2_name_EN
├── validate_workflow.R         # validation gate vs canonical comp_data/
├── build_demo_source_data.R    # Phase A: produce small demo source CSVs
├── demo_source_data/           # 30-code subset (15 F + 15 I) for testing the workflow calculations
└── demo_comp_data/             # demo workflow outputs
```

## How to run

There are two separate drivers. **Pick the right one** -
`run_workflow.R` will overwrite `comp_data/` if using it with defaults.
This is the data that is used to run the accompanying dashboard.

### Demo data → `demo_comp_data/` (safe, recommended for testing)

``` r
Rscript demo_comparison_script/run_workflow_demo.R
```

Paths are hard-coded to `demo_source_data/` → `demo_comp_data/`. A
safety belt refuses to run if anyone repoints the output at canonical
`comp_data/`. Use this whenever you're testing changes to
`workflow_functions.R`.

### Real data → canonical `comp_data/` (dashboard-impacting)

``` r
Rscript demo_comparison_script/run_workflow.R
```

Defaults to `source_data/` → `comp_data/`, so running it WILL overwrite
the canonical files.

If you don't want to overwrite, use `run_workflow_demo.R` for testing.

Either driver produces 12 files in the configured output dir:

-   4 × `{pair}_diff_meta.csv` (meta-analysis 1: per-year + pooled
    "Meta" rows)
-   4 × `{pair}_diff_meta_genders_parent2.csv` (meta-analysis 2: pooled
    across ages, then ages+sexes)
-   4 × `{pair}_heterogeneity.csv` (sidecar with I2, tau2, Q, p-value of
    Q, H per pool for background test)

The demo source CSVs in `demo_source_data/` cover only 30 ICD-10 codes
(15 F-chapter + 15 I-chapter), same schema as real source CSVs, so the
workflow runs identically on either dataset.

## Pipeline overview

```         
source_data/*.csv  (or demo_source_data/*.csv)
    │
    ├── enrich_source()  - idempotent: adds gender_EN, parent0_name_EN,
    │                      short_name, parent2_name_EN if missing.
    ↓
[ enriched data frames, 1 per unique source file ]
    │
    │   For each of 4 pairs:
    │
    ├── run_meta1(d1, d2, base, comp_dir)
    │     │── join d1/d2 strata -> log2 PR + CI + z + p_value per year
    │     │── pool across years per (gender × age × parent2) via
    │     │   metagen() with RE->FE fallback
    │     ├── write  {base}_diff_meta.csv      (base write.csv)
    │     └── return data + heterogeneity rows
    │
    ├── run_meta2(meta1_df, base, comp_dir)
    │     │── pool across ages per (gender × parent2),
    │     │   Pass-Through if only one age group
    │     │── pool across ages+sexes per parent2,
    │     │   Pass-Through if only one study
    │     ├── write  {base}_diff_meta_genders_parent2.csv  (readr::write_csv)
    │     └── return data + heterogeneity rows
    │
    └── write_heterogeneity(het1, het2, base, comp_dir)
          └── write  {base}_heterogeneity.csv  
```

`run_meta1` and `run_meta2` both use `meta::metagen()` with
`method.tau = "PM"` (Paule-Mandel between-study variance estimator),
attempting Random-Effects first and falling back to Fixed-Effect if RE
fails to converge. This RE-\>FE fallback is the documented methodology
in the article (the published `meta_model_type` column shows which model
was used per row).

## Validation

``` r
Rscript demo_comparison_script/validate_workflow.R
```

Runs the cleaned workflow against real `source_data/`, writes to a temp
`comp_data_check/`, and compares against canonical `comp_data/` using
numeric equivalence (tolerance 1e-10 absolute or relative).

## Heterogeneity sidecar

Columns: `level`, `parent2_code`, `parent2_name_EN`, `gender_EN`,
`age_group`, `n_studies`, `I2`, `tau2`, `Q`, `pval_Q`, `H`.

`level` values:

-   `meta1_across_years` - pooled across years for one (parent2 × gender
    × age) cell

-   `meta2_across_ages` - pooled across age groups for one (parent2 ×
    gender) cell

-   `meta2_across_ages_genders` - pooled across ages + sexes for one
    parent2 cell
