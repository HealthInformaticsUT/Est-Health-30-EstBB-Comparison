# table_helpers.R — Data table preparation and rendering helpers

#' Prepare gender data for DT/GT display
#'
#' Rounds numeric columns, creates Pvalview (Nature style),
#' creates combined log2_diff and fold_diff string columns.
#'
#' @param data Raw gender/parent2 dataframe
#' @return Processed dataframe
prepare_gender_data <- function(data) {
  req(data)

  df <- data %>%
    dplyr::select(-dplyr::any_of(c("sig", "year")))

  # Create display p-value (Nature Communications style)
  if ("p_value" %in% names(df)) {
    df$Pvalview <- dplyr::case_when(
      df$p_value < 0.001 ~ "<0.001",
      df$p_value < 0.01  ~ format(round(df$p_value, 3), nsmall = 3),
      TRUE               ~ format(round(df$p_value, 2), nsmall = 2)
    )
    df$Pvalview <- trimws(df$Pvalview)
  }

  # Round p_value to 4 decimals
  if ("p_value" %in% names(df)) {
    df$p_value <- round(as.numeric(df$p_value), 4)
  }

  # Round other columns to 2 decimals
  cols_to_round_2 <- c(
    "prevalence_diff", "ci_low", "ci_high",
    "fold_diff_nat", "fold_ci_low_nat", "fold_ci_high_nat",
    "ci_width_nat", "fold_diff_reg", "se", "z"
  )
  for (col in cols_to_round_2) {
    if (col %in% names(df)) {
      df[[col]] <- round(as.numeric(df[[col]]), 2)
    }
  }

  # Combined string columns (en-dash for publication quality)
  if (all(c("prevalence_diff", "ci_low", "ci_high") %in% names(df))) {
    df$log2_diff <- paste0(df$prevalence_diff, " (", df$ci_low, "\u2013", df$ci_high, ")")
  }
  if (all(c("fold_diff_nat", "fold_ci_low_nat", "fold_ci_high_nat") %in% names(df))) {
    df$fold_diff <- paste0(df$fold_diff_nat, " (", df$fold_ci_low_nat, "\u2013", df$fold_ci_high_nat, ")")
  }

  # Truncate long names
  if ("parent2_name_EN" %in% names(df)) {
    df$parent2_name_EN <- ifelse(
      nchar(df$parent2_name_EN) > 100,
      paste0(substr(df$parent2_name_EN, 1, 97), "..."),
      df$parent2_name_EN
    )
  }

  # Reorder columns
  df %>%
    dplyr::select(
      dplyr::any_of(c("parent2_code", "parent2_name_EN", "gender_EN",
                       "log2_diff", "fold_diff", "Pvalview", "p_value")),
      dplyr::everything()
    )
}

#' Prepare age data for DT/GT display
#'
#' @param df Raw meta-analysis dataframe
#' @return Processed dataframe
prepare_age_data <- function(df) {
  req(df)
  df <- df[, !(names(df) %in% c("sig"))]
  cols_to_round <- c(
    "prevalence_diff", "ci_low", "ci_high",
    "fold_diff_nat", "fold_ci_low_nat", "fold_ci_high_nat",
    "ci_width_nat", "fold_diff_reg", "p_value", "se", "z",
    "prevalence_data1", "prevalence_data2"
  )
  for (col in cols_to_round) {
    if (col %in% names(df)) df[[col]] <- round(df[[col]], 2)
  }
  return(df)
}

#' Render a DT datatable with column visibility, reorder, and shared JS callbacks
#'
#' @param data_reactive Reactive expression returning the data
#' @param hidden_cols Character vector of column names to hide by default
#' @param shared_id_suffix Suffix for Shiny input IDs (e.g., "gender" or "age")
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
      callback = DT::JS(sprintf("
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

#' Generate subtitle with range info for GT tables
#'
#' @param df Filtered dataframe
#' @return Formatted subtitle string
generate_gt_subtitle <- function(df) {
  if (is.null(df) || nrow(df) == 0) return("No data")

  fold_vals <- if ("fold_diff_reg" %in% names(df)) df$fold_diff_reg else NULL
  p_vals    <- if ("p_value" %in% names(df)) df$p_value else NULL
  row_count <- nrow(df)

  fold_range <- if (!is.null(fold_vals) && length(fold_vals) > 0 && !all(is.na(fold_vals))) {
    paste0("fold_diff_reg \u2208 [", round(min(fold_vals, na.rm = TRUE), 2),
           ", ", round(max(fold_vals, na.rm = TRUE), 2), "]")
  } else {
    "fold_diff_reg: no values"
  }

  p_range <- if (!is.null(p_vals) && length(p_vals) > 0 && !all(is.na(p_vals))) {
    paste0("p_value \u2208 [", round(min(p_vals, na.rm = TRUE), 2),
           ", ", round(max(p_vals, na.rm = TRUE), 2), "]")
  } else {
    "p_value: no values"
  }

  paste(fold_range, p_range, paste("Rows:", row_count), sep = " | ")
}

#' Generate a publication-quality GT table from DT state
#'
#' @param df_raw Filtered dataframe
#' @param dt_rows Row indices from DT (rows_all)
#' @param dt_vis_cols Visible column indices from DT callback
#' @param dt_col_order Column order indices from DT callback
#' @param title Table title
#' @param subtitle Table subtitle (NULL for default row count)
#' @param default_hidden_cols Columns to hide when DT hasn't been interacted with
#' @return A gt table object
generate_print_gt <- function(df_raw, dt_rows, dt_vis_cols, dt_col_order,
                              title = "Analysis Results", subtitle = NULL,
                              default_hidden_cols = NULL) {
  req(df_raw)
  rows_to_include <- dt_rows
  if (is.null(rows_to_include)) rows_to_include <- 1:nrow(df_raw)
  df_full <- df_raw[rows_to_include, ]

  if (nrow(df_full) == 0) {
    return(gt::gt(data.frame(Message = "No data available.")))
  }

  original_col_names <- names(df_full)

  # Column reordering (JS sends 0-based indices)
  if (!is.null(dt_col_order)) {
    safe_indices <- dt_col_order[dt_col_order >= 0 & dt_col_order < length(original_col_names)]
    reordered_names <- original_col_names[safe_indices]
  } else {
    reordered_names <- original_col_names
  }

  # Column visibility
  if (!is.null(dt_vis_cols)) {
    safe_indices <- dt_vis_cols[dt_vis_cols >= 0 & dt_vis_cols < length(original_col_names)]
    visible_names <- original_col_names[safe_indices]
  } else if (!is.null(default_hidden_cols)) {
    visible_names <- setdiff(original_col_names, default_hidden_cols)
  } else {
    visible_names <- original_col_names
  }

  # Apply reorder
  if (length(reordered_names) > 0) {
    final_cols <- intersect(reordered_names, names(df_full))
    df_full <- df_full[, final_cols, drop = FALSE]
  }

  final_subtitle <- if (is.null(subtitle)) paste("Rows:", nrow(df_full)) else subtitle

  # Build GT table
  gt_table <- gt::gt(df_full) %>%
    gt::tab_header(title = title, subtitle = final_subtitle) %>%
    gt::fmt_number(gt::where(is.numeric), decimals = 2) %>%
    gt::tab_options(table.font.size = "small")

  # Conditional styling: log2_diff
  if (all(c("log2_diff", "prevalence_diff") %in% names(df_full))) {
    gt_table <- gt_table %>%
      gt::tab_style(
        style = gt::cell_fill(color = BLUE_ALPHA),
        locations = gt::cells_body(columns = gt::vars(log2_diff), rows = df_full$prevalence_diff < 0)
      ) %>%
      gt::tab_style(
        style = gt::cell_fill(color = ORANGE_ALPHA),
        locations = gt::cells_body(columns = gt::vars(log2_diff), rows = df_full$prevalence_diff >= 0)
      )
  }

  # Conditional styling: fold_diff
  if (all(c("fold_diff", "fold_diff_nat") %in% names(df_full))) {
    gt_table <- gt_table %>%
      gt::tab_style(
        style = gt::cell_fill(color = ORANGE_ALPHA),
        locations = gt::cells_body(columns = gt::vars(fold_diff), rows = df_full$fold_diff_nat > 1)
      ) %>%
      gt::tab_style(
        style = gt::cell_fill(color = BLUE_ALPHA),
        locations = gt::cells_body(columns = gt::vars(fold_diff), rows = df_full$fold_diff_nat < 1)
      )
  }

  # Conditional styling: gender
  if ("gender_EN" %in% names(df_full)) {
    gt_table <- gt_table %>%
      gt::tab_style(
        style = gt::cell_fill(color = GENDER_BLUE),
        locations = gt::cells_body(columns = gt::vars(gender_EN), rows = df_full$gender_EN == "M")
      ) %>%
      gt::tab_style(
        style = gt::cell_fill(color = GENDER_PINK),
        locations = gt::cells_body(columns = gt::vars(gender_EN), rows = df_full$gender_EN == "F")
      )
  }

  # Hide columns
  cols_to_hide <- setdiff(names(df_full), visible_names)
  if (length(cols_to_hide) > 0) {
    gt_table <- gt_table %>% gt::cols_hide(columns = gt::all_of(cols_to_hide))
  }

  return(gt_table)
}
