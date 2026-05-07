# Analysis pipeline reference

The scripts in this folder document the extraction and meta-analysis
steps that produced the data in `source_data/` and `comp_data/`. They
are provided as a reference, not a self-contained runnable pipeline.

## What's here

| File | Step |
|------|------|
| `EH30_Step_1_source-data-updates.R` | One-time augmentation of source CSVs with English diagnosis/chapter names + gender recoding |
| `EH30_STEP_2_Run_metaanalysis_save_files.R` | Per-pair meta-analysis producing the `*_diff_meta.csv` and `*_diff_meta_genders_parent2.csv` outputs in `comp_data/` |

## Why the scripts won't run as-is

1. **Step 1 is entirely commented out.** It documents a one-time
   transformation that has already been applied to the source CSVs.
   The English columns (`parent0_name_EN`, `parent2_name_EN`,
   `short_name`, `gender_EN`) are now part of every source CSV.
2. **Step 2 references helper functions defined elsewhere**
   (`fileGen_data1_data2_diff_meta_Log_pval()`,
   `perform_meta_analysis2_genders()`). To run, source those
   definitions first.
3. **Step 2's four comparison blocks share the same variable names**
   (`upload1`, `upload2`, `output_name_base`). They are intended to
   be run one block at a time, not all four sequentially.

## Reproducing this analysis

For a generalised, packaged version of this extraction +
meta-analysis workflow, see the `syrona` R package:
<https://github.com/MaarjaPajusalu/Syrona>

Syrona was developed in parallel with this dashboard and provides
the same extraction and meta-analysis logic in a documented,
testable form.
