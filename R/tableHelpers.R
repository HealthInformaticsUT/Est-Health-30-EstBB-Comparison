#' Prepare Gender Dataset
#' Performs standard rounding, column formatting, and filtering for gender meta-analysis data.
#'
#' @param data The raw dataframe
#' @return A processed dataframe ready for DT/GT
prepare_gender_data <- function(data) {
  req(data)

  # 1. Remove unwanted columns
  df <- data[, !(names(data) %in% c("sig", "year"))]

  # 2. Rounding
  cols_to_round <- c(
    "prevalence_diff", "ci_low", "ci_high",
    "fold_diff_nat", "fold_ci_low_nat", "fold_ci_high_nat",
    "ci_width_nat", "fold_diff_reg", "p_value", "se", "z"
  )

  for (col in cols_to_round) {
    if (col %in% names(df)) {
      df[[col]] <- round(df[[col]], 2)
    }
  }

  # 3. Create Combined String Columns
  # Check if columns exist before pasting to avoid errors
  if (all(c("prevalence_diff", "ci_low", "ci_high") %in% names(df))) {
    df$log2_diff <- paste0(df$prevalence_diff, " (", df$ci_low, "…", df$ci_high, ")")
  }

  if (all(c("fold_diff_nat", "fold_ci_low_nat", "fold_ci_high_nat") %in% names(df))) {
    df$fold_diff <- paste0(df$fold_diff_nat, " (", df$fold_ci_low_nat, "…", df$fold_ci_high_nat, ")")
  }

  # 4. Truncate Long Names
  if ("parent2_name_EN" %in% names(df)) {
    df$parent2_name_EN <- ifelse(
      nchar(df$parent2_name_EN) > 100,
      paste0(substr(df$parent2_name_EN, 1, 97), "..."),
      df$parent2_name_EN
    )
  }

  # 5. Final Select/Reorder
  # Uses 'any_of' to be safe if a column is missing
  df <- df %>%
    dplyr::select(dplyr::any_of(c("parent2_code", "parent2_name_EN", "gender_EN", "log2_diff", "fold_diff", "fold_diff_nat")), dplyr::everything())

  return(df)
}

#' Prepare Gender Dataset
#' Performs standard rounding, column formatting, and filtering for gender meta-analysis data.
#'
#' @param data The raw dataframe
#' @return A processed dataframe ready for DT/GT
# Helper to prepare Age Data (consistent with Gender helper pattern)
prepare_age_data <- function(df) {
  req(df)
  # Remove sig and if needed more to clean the dataframe
  df <- df[, !(names(df) %in% c("sig"))]
  cols_to_round <- c("prevalence_diff", "ci_low", "ci_high",
                     "fold_diff_nat", "fold_ci_low_nat", "fold_ci_high_nat",
                     "ci_width_nat", "fold_diff_reg", "p_value", "se", "z", "prevalence_data1", "prevalence_data2" )
  for (col in cols_to_round) {
    if (col %in% names(df)) df[[col]] <- round(df[[col]], 2)
  }
  return(df)
}

#' Render Custom Data Table
#'
#' @param data_reactive A reactive expression returning the data
#' @param hidden_cols A character vector of column names to hide by default
#' @param shared_id_suffix The suffix for the input ID (e.g., "gender" for 'visible_columns_gender')
#'
#' @return A DT::renderDataTable object
render_custom_dt <- function(data_reactive, hidden_cols, shared_id_suffix) {
  DT::renderDataTable({
    DT::datatable(
      data_reactive(),
      filter = "top",
      extensions = c('Buttons', 'ColReorder'),
      options = list(
        dom = 'Bfrtip',
        buttons = list('colvis'),
        colReorder = TRUE,
        scrollX = TRUE,
        columnDefs = list(list(visible = FALSE, targets = hidden_cols))
      ),
      callback = JS(sprintf("
        table.on('column-reorder', function(e, settings, details) {
          var order = table.colReorder.order();
          Shiny.setInputValue('column_order_%s', order);
        });
        table.on('buttons-action', function(e, buttonApi, dataTable, node, config) {
          var visible = [];
          dataTable.columns().every(function(index) {
            if (this.visible()) visible.push(index);
          });
          Shiny.setInputValue('visible_columns_%s', visible);
        });
      ", shared_id_suffix, shared_id_suffix))
    )
  })
}

#' Generate Subtitle for GT Table
#' Calculates the range strings for fold_diff and ci_width
#'
#' @param df The filtered dataframe
#' @return A string containing the formatted subtitle
generate_gt_subtitle <- function(df) {
  # Handle empty or null dataframe
  if (is.null(df) || nrow(df) == 0) return("No data")

  fold_vals <- if("fold_diff_reg" %in% names(df)) df$fold_diff_reg else NULL
  p_vals <- if("p_value" %in% names(df)) df$p_value else NULL
  row_count <- nrow(df)

  fold_range <- if (!is.null(fold_vals) && length(fold_vals) > 0 && !all(is.na(fold_vals))) {
    paste0("fold_diff_reg ∈ [", round(min(fold_vals, na.rm=TRUE), 2), ", ", round(max(fold_vals, na.rm=TRUE), 2), "]")
  } else {
    "fold_diff_reg: no values"
  }

  p_range <- if (!is.null(p_vals) && length(p_vals) > 0 && !all(is.na(p_vals))) {
    paste0("p_value ∈ [", round(min(p_vals, na.rm=TRUE), 2), ", ", round(max(p_vals, na.rm=TRUE), 2), "]")
  } else {
    "p_value: no values"
  }

  # Combine with separator
  paste(fold_range, p_range, paste("Rows:", row_count), sep = " | ")
}

#' Generate a GT table for printing based on DT state
#'
#' @param df_raw The filtered dataframe (reactive output)
#' @param dt_rows The rows_all input from DT (indices of filtered rows)
#' @param dt_vis_cols The visible columns input (indices from custom JS/DT)
#' @param dt_col_order The column order input (indices from custom JS/DT)
#' @param title The title for the table header
#' @param subtitle The subtitle for the table header (optional)
#'
#' @return A gt table object
generate_print_gt <- function(df_raw,
                              dt_rows,
                              dt_vis_cols,
                              dt_col_order,
                              title = "Analysis Results",
                              subtitle = NULL,
                              default_hidden_cols = NULL) {

  # 1. Validation & Filtering
  req(df_raw)
  rows_to_include <- dt_rows
  if (is.null(rows_to_include)) rows_to_include <- 1:nrow(df_raw)
  df_full <- df_raw[rows_to_include, ]

  if (nrow(df_full) == 0) {
    return(gt::gt(data.frame(Message = "No data available.")))
  }

  # 2. Resolve Column Names (Handling 0-based JS vs 1-based R)
  original_col_names <- names(df_full)

  # Order
  if (!is.null(dt_col_order)) {
    # Ensure indices are within bounds and add 1
    # JS sends 0-based indices. R uses 1-based.
    # If we get [0, 1], R needs [1, 2].
    # If we don't add 1, R sees [0, 1] -> ignores 0 -> returns only column 1.
    safe_indices <- dt_col_order[dt_col_order >= 0 & dt_col_order < length(original_col_names)]
    reordered_names <- original_col_names[safe_indices ]
  } else {
    reordered_names <- original_col_names
  }

  # Visibility
  # Visibility
  if (!is.null(dt_vis_cols)) {
    # If the user has interacted (dt_vis_cols is sent by DT), use it
    safe_indices <- dt_vis_cols[dt_vis_cols >= 0 & dt_vis_cols < length(original_col_names)]
    visible_names <- original_col_names[safe_indices] # Ensure +1 is still needed based on JS/R index handling
  } else if (!is.null(default_hidden_cols)) {
    # *** NEW LOGIC: Use default_hidden_cols for initial rendering ***
    visible_names <- setdiff(original_col_names, default_hidden_cols)
  } else {
    # Default to all columns visible (only if no DT input AND no default list provided)
    visible_names <- original_col_names
  }

  # 3. Apply Reorder to Dataframe
  if (length(reordered_names) > 0) {
    # Only select columns that actually exist to be safe
    final_cols <- intersect(reordered_names, names(df_full))
    df_full <- df_full[, final_cols, drop = FALSE]
  }

  # 4. Handle Subtitle Default
  # If no subtitle passed, default to row count
  final_subtitle <- if (is.null(subtitle)) paste("Rows:", nrow(df_full)) else subtitle

  # 5. Initialize GT
  # Define colors
  blue_alpha   <- "rgba(0, 95, 200, 0.2)"
  orange_alpha <- "rgba(255, 102, 0, 0.2)"
  gender_blue  <- "rgba(153, 144, 255, 0.2)"
  gender_pink  <- "rgba(255, 105, 180, 0.2)"

  gt_table <- gt::gt(df_full) %>%
    gt::tab_header(title = title, subtitle = final_subtitle) %>%
    gt::fmt_number(gt::where(is.numeric), decimals = 2) %>%
    gt::tab_options(table.font.size = "small")

  # 6. Apply Specific Styles

  # Style: log2_diff based on prevalence_diff
  if (all(c("log2_diff", "prevalence_diff") %in% names(df_full))) {
    gt_table <- gt_table %>%
      gt::tab_style(
        style = gt::cell_fill(color = blue_alpha),
        locations = gt::cells_body(columns = gt::vars(log2_diff), rows = df_full$prevalence_diff < 0)
      ) %>%
      gt::tab_style(
        style = gt::cell_fill(color = orange_alpha),
        locations = gt::cells_body(columns = gt::vars(log2_diff), rows = df_full$prevalence_diff >= 0)
      )
  }

  # Style: fold_diff based on fold_diff_nat
  if (all(c("fold_diff", "fold_diff_nat") %in% names(df_full))) {
    gt_table <- gt_table %>%
      gt::tab_style(
        style = gt::cell_fill(color = orange_alpha),
        locations = gt::cells_body(columns = gt::vars(fold_diff), rows = df_full$fold_diff_nat > 1)
      ) %>%
      gt::tab_style(
        style = gt::cell_fill(color = blue_alpha),
        locations = gt::cells_body(columns = gt::vars(fold_diff), rows = df_full$fold_diff_nat < 1)
      )
  }

  # Style: Gender
  if ("gender_EN" %in% names(df_full)) {
    gt_table <- gt_table %>%
      gt::tab_style(
        style = gt::cell_fill(color = gender_blue),
        locations = gt::cells_body(columns = gt::vars(gender_EN), rows = df_full$gender_EN == "M")
      ) %>%
      gt::tab_style(
        style = gt::cell_fill(color = gender_pink),
        locations = gt::cells_body(columns = gt::vars(gender_EN), rows = df_full$gender_EN == "F")
      )
  }

  # 7. Apply Visibility (Hiding columns)
  cols_to_hide <- setdiff(names(df_full), visible_names)
  if (length(cols_to_hide) > 0) {
    gt_table <- gt_table %>% gt::cols_hide(columns = gt::all_of(cols_to_hide))
  }

  return(gt_table)
}
