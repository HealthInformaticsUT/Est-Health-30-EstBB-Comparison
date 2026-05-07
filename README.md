---
editor_options: 
  markdown: 
    wrap: 72
---

# Est-Health-30 vs EstBB - Diagnosis Prevalence Dashboard

Interactive Shiny dashboard comparing diagnosis prevalence across the
Estonian general population (Est-Health-30) and the Estonian Biobank
(EstBB), with sub-cohort comparisons (EstBB1, EstBB2). Companion to the
manuscript currently under revision at *Nature Communications*.

**Live deploy:**
<http://omop-apps.cloud.ut.ee/ShinyApps/eh30-estbb-comparison>

## What it shows

-   **Heatmaps**: prevalence ratios by ICD-10 chapter and individual
    diagnosis, faceted by sex and age group
-   **Forest plots**: prevalence ratios with confidence intervals and
    meta-analysis pooled estimates
-   **Histograms**: distribution of fold differences across the
    diagnosis catalogue
-   **Custom comparisons**: user-driven diagnosis selection across all 4
    comparison pairs

External-causes chapter (V01-Y98) is excluded throughout per the
manuscript's data scope.

## Comparison pairs

| Pair | Datasets                |
|------|-------------------------|
| 1    | Est-Health-30 vs EstBB  |
| 2    | Est-Health-30 vs EstBB1 |
| 3    | Est-Health-30 vs EstBB2 |
| 4    | EstBB1 vs EstBB2        |

## Run locally

Requirements: R \>= 4.2, with the packages listed in `global.R`.

``` r
# From project root:
shiny::runApp(".")
```

The first launch reads the `.parquet` files in `source_data/` and
`comp_data/`.

## Deployment

-   **omop-apps.ut.ee**: sysadmin-managed Shiny Server. App ships as
    flat-shiny structure (`global.R` + `ui.R` + `server.R` + `R/`).
-   **GitHub mirror**: this repository.

## Architecture

```         
Est-Health-30-EstBB-Clean/
├── global.R              # Loads packages, sources modules, loads data
├── ui.R                  # Dashboard layout
├── server.R              # Reactive logic
├── R/                    # Plot, data-prep, table, download helpers
├── source_data/          # Source CSVs (.gitignored) + parquet cache
├── comp_data/            # Comparison CSVs (.gitignored) + parquet cache
├── scripts/
│   ├── build_parquet_cache.R  # CSV -> parquet conversion
│   ├── deploy_shinyapps.R     # rsconnect deploy
│   └── profile_app.R          # profvis scenarios
├── www/                  # Static assets
└── PROJECT.md            # Internal development log
```

## Citation

If you use this dashboard or its underlying analysis, please cite the
preprint:

Pajusalu, M., Oja, M., Mooses, K., Heinsar, S., Aasmets, O., Laisk, T.,
Palta, P., Org, E., Mägi, R., Võsa, U., Fischer, K., Estonian Biobank
Research Team, Tillmann, T., Laur, S., Reisberg, S., Vilo, J. & Kolde,
R. Comparison of the prevalence of all diagnosed diseases among Estonian
Biobank participants against the general population. Preprint at
*medRxiv* <https://doi.org/10.64898/2026.02.05.26345634> (2026).

## License

MIT - see `LICENSE`.

## Authors

Pajusalu, M., Oja, M., Mooses, K., Heinsar, S., Aasmets, O., Laisk, T.,
Palta, P., Org, E., Mägi, R., Võsa, U., Fischer, K., Estonian Biobank
Research Team, Tillmann, T., Laur, S., Reisberg, S., Vilo, J. & Kolde,
R.

Lead author: Maarja Pajusalu, Institute of Computer Science, University
of Tartu. Email: maarja.pajusalu\@ut.ee
