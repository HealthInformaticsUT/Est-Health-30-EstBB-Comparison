# server.R — Data-driven server logic
# Uses app_data (loaded in global.R) and config constants

server <- function(input, output, session) {
  options(shiny.maxRequestSize = 20 * 1024^2)

  # ============================================================================
  # DATA ACCESS — Reactive wrappers around app_data
  # ============================================================================
  # Source data
  upload <- lapply(1:4, function(i) shiny::reactive({ app_data$source[[i]] }))

  # Meta data
  upload_meta <- lapply(1:4, function(i) shiny::reactive({ app_data$meta[[i]] }))

  # Gender/parent2 data
  upload_gp_meta <- lapply(1:4, function(i) shiny::reactive({ app_data$gp_meta[[i]] }))

  # Cleaned/top-level source data
  inputData      <- lapply(1:4, function(i) shiny::reactive({ cleanData(upload[[i]]()) }))
  inputData_top  <- lapply(1:4, function(i) shiny::reactive({ topData(inputData[[i]]()) }))

  # ============================================================================
  # FILTER INPUTS
  # ============================================================================

  # All parent0 codes across all source datasets
  allCodes1 <- shiny::reactive({
    codes <- c()
    for (i in 1:4) {
      if (!is.null(upload[[i]]())) {
        codes <- c(codes, unique(inputData_top[[i]]()$parent0_code))
      }
    }
    unique(codes)
  })

  # All genders from gender/parent2 meta 1
  allGen1 <- shiny::reactive({
    if (!is.null(upload_gp_meta[[1]]())) {
      unique(upload_gp_meta[[1]]()$gender_EN)
    } else {
      c()
    }
  })

  # All age groups across all source datasets
  allAgegroups1 <- shiny::reactive({
    agegroups <- c()
    for (i in 1:4) {
      if (!is.null(upload[[i]]())) {
        agegroups <- c(agegroups, unique(inputData_top[[i]]()$age_group))
      }
    }
    unique(agegroups)
  })

  # Update filter UI elements
  shiny::observe({
    shiny::updateSelectInput(session, "filterCodeGroups0",
      choices = stringr::str_sort(allCodes1()),
      selected = c("G00-G99", "I00-I99", "C00-D48", "F00-F99", "E00-E90"))
    shiny::updateSelectInput(session, "code_filter0",
      choices = stringr::str_sort(allCodes1()),
      selected = stringr::str_sort(c("I00-I99")))
    shiny::updateSelectInput(session, "code_filter",
      choices = stringr::str_sort(allCodes1()),
      selected = stringr::str_sort(c("I00-I99")))
  })

  shiny::observeEvent(upload_gp_meta[[1]](), {
    df <- upload_gp_meta[[1]]()
    shiny::updateSliderInput(session, "fold_filter",
      min = 1, max = round(max(df$fold_diff_reg, na.rm = TRUE), 2),
      value = c(1.25, round(max(df$fold_diff_reg, na.rm = TRUE), 2)), step = 0.25)
    shiny::updateSliderInput(session, "ci_filter",
      min = 0, max = round(max(df$ci_width_nat, na.rm = TRUE), 2),
      value = c(0, 0.5), step = 0.5)
  })

  # Reset CI and Fold diff sliders to defaults
  shiny::observeEvent(input$resetAnalysisValues, {
    df <- upload_gp_meta[[1]]()
    shiny::updateSliderInput(session, "ci_filter",
      value = c(0, 0.5))
    shiny::updateSliderInput(session, "fold_filter",
      value = c(1.25, round(max(df$fold_diff_reg, na.rm = TRUE), 2)))
  })

  # Debounced versions of the analysis sliders (400ms).
  # Rendering reactives should read these, so mid-drag pixel changes
  # no longer re-trigger heavy forest plots on every step.
  ci_filter_d   <- shiny::debounce(
    shiny::reactive(round(input$ci_filter,   1)), 400)
  fold_filter_d <- shiny::debounce(
    shiny::reactive(round(input$fold_filter, 2)), 400)

  shiny::observe({
    shiny::updateSelectInput(session, "filterGender",
      choices = stringr::str_sort(allGen1()),
      selected = c("M", "F"))
  })

  shiny::observe({
    genders <- allGen1()
    if (length(genders) > 0) {
      sort_order <- c("F", "M", "Both")
      ordered_genders <- sort_order[sort_order %in% genders]
      shiny::updateCheckboxGroupInput(session, "gender_filter",
        choices = ordered_genders, selected = ordered_genders, inline = TRUE)
    }
  })

  shiny::observe({
    shiny::updateSelectInput(session, "filterAge",
      choices = stringr::str_sort(allAgegroups1()),
      selected = allAgegroups1()[2:9])
  })

  selectedAgegroups <- shiny::reactive({ c(input$filterAge) })
  selectedGen       <- shiny::reactive({ c(input$filterGender) })
  selectedCodes     <- shiny::reactive({ c(input$filterCodeGroups0) })

  # ============================================================================
  # HEATMAP TAB — Dataset selection + drill-down
  # ============================================================================

  # Selected comparison index (1-4)
  selected_comp_idx <- shiny::reactive({
    COMPARISON_LABELS[input$filterDatasets]
  })

  selectedDataset_meta <- shiny::reactive({
    upload_meta[[selected_comp_idx()]]()
  })

  selectedNames <- shiny::reactive({
    req(input$filterDatasets)
    comp <- COMPARISONS[[selected_comp_idx()]]
    list(name1 = comp$name1, name2 = comp$name2)
  })

  # Chapter-level heatmap
  output$heatmap_meta_avg <- ggiraph::renderGirafe({
    heatmap_meta_avg(selectedDataset_meta(), selectedAgegroups(),
      selectedGen(), selectedCodes(), selectedNames()$name1, selectedNames()$name2,
      scale_mode = input$scaleModeHeatmap)
  }) |>
    shiny::bindCache(
      input$filterDatasets, input$filterAge, input$filterGender,
      input$filterCodeGroups0, input$scaleModeHeatmap
    )

  # Drill-down: selected chapter code
  selectedCodes_sub <- shiny::reactive({ input$heatmap_meta_avg_selected })

  # Drill-down: selected diagnosis code (from detail heatmap)
  selectedCodes_sub2 <- shiny::reactive({
    if (!is.null(input$heatmap_meta_alph_selected)) {
      input$heatmap_meta_alph_selected
    } else if (!is.null(input$heatmap_meta_avgDet_selected)) {
      input$heatmap_meta_avgDet_selected
    } else {
      NULL
    }
  })

  # Sort/filter toggle state
  active_plot   <- shiny::reactiveVal("Alphabetical")
  active_filter <- shiny::reactiveVal("All")

  shiny::observeEvent(input$alphaBtn, {
    active_plot("Alphabetical")
    shinyjs::runjs("$('#alphaBtn').addClass('active').removeClass('inactive');
                    $('#avgBtn').addClass('inactive').removeClass('active');")
  })
  shiny::observeEvent(input$avgBtn, {
    active_plot("Average")
    shinyjs::runjs("$('#avgBtn').addClass('active').removeClass('inactive');
                    $('#alphaBtn').addClass('inactive').removeClass('active');")
  })
  shiny::observeEvent(input$sigBtn, {
    active_filter("Significant")
    shinyjs::runjs("$('#sigBtn').addClass('active').removeClass('inactive');
                    $('#allBtn').addClass('inactive').removeClass('active');")
  })
  shiny::observeEvent(input$allBtn, {
    active_filter("All")
    shinyjs::runjs("$('#allBtn').addClass('active').removeClass('inactive');
                    $('#sigBtn').addClass('inactive').removeClass('active');")
  })

  # Detail heatmap rendering helper
  render_detail_heatmap <- function(meta_data, name1, name2) {
    shiny::validate(
      shiny::need(!is.null(selectedCodes_sub()),
        "Click on a Diagnosis Group from Heatmap to See the Details.")
    )

    sorting_method <- active_plot()
    filter_method  <- active_filter()

    data_to_plot <- if (filter_method == "Significant") {
      meta_data %>% dplyr::filter(sig %in% c("sig"))
    } else {
      meta_data
    }

    sm <- input$scaleModeHeatmap
    if (sorting_method == "Alphabetical") {
      heatmap_meta_alph(data_to_plot, selectedAgegroups(),
        selectedGen(), selectedCodes_sub(), name1, name2, scale_mode = sm)
    } else {
      heatmap_meta_avgDet(data_to_plot, selectedAgegroups(),
        selectedGen(), selectedCodes_sub(), name1, name2, scale_mode = sm)
    }
  }

  # Detail heatmaps for all 4 comparisons
  # Cache keys cover every reactive input read by render_detail_heatmap:
  # selected comparison, age/gender filters, drill-down code, sort/filter
  # toggles, and scale mode.
  output$heatmap_meta_alph <- ggiraph::renderGirafe({
    render_detail_heatmap(selectedDataset_meta(),
      selectedNames()$name1, selectedNames()$name2)
  }) |>
    shiny::bindCache(
      input$filterDatasets, input$filterAge, input$filterGender,
      selectedCodes_sub(), active_plot(), active_filter(),
      input$scaleModeHeatmap
    )

  output$heatmap_meta_alph2 <- ggiraph::renderGirafe({
    render_detail_heatmap(upload_meta[[2]](),
      COMPARISONS[[2]]$name1, COMPARISONS[[2]]$name2)
  }) |>
    shiny::bindCache(
      2L, input$filterAge, input$filterGender,
      selectedCodes_sub(), active_plot(), active_filter(),
      input$scaleModeHeatmap
    )

  output$heatmap_meta_alph3 <- ggiraph::renderGirafe({
    render_detail_heatmap(upload_meta[[3]](),
      COMPARISONS[[3]]$name1, COMPARISONS[[3]]$name2)
  }) |>
    shiny::bindCache(
      3L, input$filterAge, input$filterGender,
      selectedCodes_sub(), active_plot(), active_filter(),
      input$scaleModeHeatmap
    )

  output$heatmap_meta_alph4 <- ggiraph::renderGirafe({
    render_detail_heatmap(upload_meta[[4]](),
      COMPARISONS[[4]]$name1, COMPARISONS[[4]]$name2)
  }) |>
    shiny::bindCache(
      4L, input$filterAge, input$filterGender,
      selectedCodes_sub(), active_plot(), active_filter(),
      input$scaleModeHeatmap
    )

  # ============================================================================
  # FOREST + POINT DIFF (Drill-down from detail heatmap)
  # ============================================================================

  output$forest1 <- ggiraph::renderGirafe({
    shiny::validate(shiny::need(!is.null(selectedCodes_sub2()),
      "Prevalence Ratios Across Years: Click on a diagnosis code from detailed, diagnosis-level heatmap."))
    forest1(selectedDataset_meta(), selectedAgegroups(), selectedGen(),
      selectedCodes_sub2(), selectedNames()$name1, selectedNames()$name2,
      scale_mode = input$scaleModeHeatmap)
  })

  output$pointDiff1 <- ggiraph::renderGirafe({
    shiny::validate(shiny::need(!is.null(selectedCodes_sub2()),
      "Prevalence Values Across Years: Click on a diagnosis code from detailed, diagnosis-level heatmap."))
    pointDiff1(selectedDataset_meta(), selectedAgegroups(), selectedGen(),
      selectedCodes_sub2(), selectedNames()$name1, selectedNames()$name2,
      scale_mode = input$scaleModeHeatmap)
  })

  # ============================================================================
  # FOREST BY GENDER (4 side-by-side plots)
  # ============================================================================

  # Config for the 4 forest-by-gender plots
  forest_gender_config <- list(
    list(output_id = "forest2_parent2",          gp_idx = 1, n1 = DATASET_NAMES$d1, n2 = DATASET_NAMES$d2),
    list(output_id = "forest2_parent2_genders2", gp_idx = 2, n1 = DATASET_NAMES$d1, n2 = DATASET_NAMES$d3),
    list(output_id = "forest2_parent2_genders3", gp_idx = 3, n1 = DATASET_NAMES$d1, n2 = DATASET_NAMES$d4),
    list(output_id = "forest2_parent2_genders4", gp_idx = 4, n1 = DATASET_NAMES$d3, n2 = DATASET_NAMES$d4)
  )

  lapply(forest_gender_config, function(cfg) {
    local({
      my_cfg <- cfg
      output[[my_cfg$output_id]] <- ggiraph::renderGirafe({
        shiny::validate(shiny::need(
          !is.null(input$code_filter) && length(input$code_filter) > 0,
          "Prevalence difference on forestplot: Select at least one diagnosis code."))
        forest2_parent2(
          upload_gp_meta[[my_cfg$gp_idx]](),
          gender_select = input$gender_filter,
          code_select   = input$code_filter,
          name1 = my_cfg$n1,
          name2 = my_cfg$n2,
          ci_range   = ci_filter_d(),
          fold_range = fold_filter_d(),
          scale_mode = input$scaleModeGender
        )
      }) |>
        shiny::bindCache(
          my_cfg$gp_idx, input$code_filter, input$gender_filter,
          ci_filter_d(), fold_filter_d(), input$scaleModeGender
        )
    })
  })

  # ============================================================================
  # HISTOGRAM GRID (3x3 — PR Distribution)
  # ============================================================================

  filtered_data_dd <- shiny::reactive({
    prepare_histogram_data(
      upload_gp_meta[[1]](), upload_gp_meta[[2]](), upload_gp_meta[[3]](), app_data$DALY)
  })

  # Helper: render a plotly histogram with standard layout
  render_plotly_hist <- function(plot_fn, ...) {
    plotly::renderPlotly({
      p <- plot_fn(...)
      plotly::ggplotly(p) %>%
        plotly::layout(
          xaxis = list(showline = TRUE, linecolor = plotly::toRGB(COLOR_LINES), linewidth = 1),
          yaxis = list(showline = TRUE, linecolor = plotly::toRGB(COLOR_LINES), linewidth = 1)
        )
    })
  }

  # Helper: render a plotly scatter with standard hover layout
  render_plotly_scatter <- function(plot_fn, ...) {
    plotly::renderPlotly({
      p <- plot_fn(...)
      plotly::ggplotly(p, tooltip = c("text")) %>%
        plotly::layout(
          hoverlabel = list(
            bgcolor = "rgba(245, 245, 220, 0.9)",
            font = list(color = "black", size = 11, align = 'left'),
            bordercolor = "rgba(0, 0, 0, 0)", namelength = 20
          ),
          xaxis = list(showline = TRUE, linecolor = plotly::toRGB(COLOR_LINES), linewidth = 1),
          yaxis = list(showline = TRUE, linecolor = plotly::toRGB(COLOR_LINES), linewidth = 1)
        )
    })
  }

  # Histogram grid config: each column has histogram + death + disability
  hist_grid <- list(
    list(pr = "p_vs_bb",  p = "p_vs_bb_p",  right = DATASET_NAMES$d2),
    list(pr = "p_vs_bb1", p = "p_vs_bb1_p", right = DATASET_NAMES$d3),
    list(pr = "p_vs_bb2", p = "p_vs_bb2_p", right = DATASET_NAMES$d4)
  )

  hist_suffixes <- c("bb", "bb1", "bb2")

  for (col_idx in seq_along(hist_grid)) {
    local({
      cfg <- hist_grid[[col_idx]]
      suffix <- hist_suffixes[[col_idx]]
      is_first <- col_idx == 1

      # Row 1: Histogram
      output[[paste0("p_histogram_", suffix)]] <- render_plotly_hist(
        create_pr_histogram,
        plot_data = filtered_data_dd(),
        pr_col = cfg$pr,
        left_label = DATASET_NAMES$d1,
        right_label = cfg$right,
        show_y_axis = is_first,
        show_x_axis = FALSE,
        y_label = if (is_first) "Count" else "",
        show_subtitle = is_first
      )

      # Row 2: Death burden scatter
      output[[paste0("p_death_", suffix)]] <- render_plotly_scatter(
        create_pr_scatter,
        plot_data = filtered_data_dd(),
        pr_col = cfg$pr,
        p_col = cfg$p,
        burden_col = "YLL2021norm",
        y_label = "Size = Years of\n Life Lost",
        show_y_axis = is_first,
        show_x_axis = FALSE
      )

      # Row 3: Disability burden scatter
      x_lab <- paste0("prev_", cfg$right, " / prev_", DATASET_NAMES$d1)
      output[[paste0("p_disability_", suffix)]] <- render_plotly_scatter(
        create_pr_scatter,
        plot_data = filtered_data_dd(),
        pr_col = cfg$pr,
        p_col = cfg$p,
        burden_col = "YLD2021norm",
        y_label = "Size = Years Lost\n to Disability",
        x_label = x_lab,
        show_y_axis = is_first,
        show_x_axis = TRUE
      )
    })
  }

  # ============================================================================
  # GENDER HISTOGRAM GRID (3x2 — M/F)
  # ============================================================================

  filtered_data_dd_gen <- shiny::reactive({
    prepare_histogram_data_gender(
      upload_gp_meta[[1]](), upload_gp_meta[[2]](), upload_gp_meta[[3]]())
  })

  gender_hist_config <- list(
    list(suffix = "bb",  pr = "p_vs_bb",  right = DATASET_NAMES$d2),
    list(suffix = "bb1", pr = "p_vs_bb1", right = DATASET_NAMES$d3),
    list(suffix = "bb2", pr = "p_vs_bb2", right = DATASET_NAMES$d4)
  )

  for (col_idx in seq_along(gender_hist_config)) {
    local({
      cfg <- gender_hist_config[[col_idx]]
      is_first <- col_idx == 1

      for (gender in c("M", "F")) {
        local({
          my_gender <- gender
          my_cfg <- cfg
          my_is_first <- is_first
          is_bottom <- my_gender == "F"

          output_id <- paste0("p_histogram_", my_cfg$suffix, my_gender)
          x_lab <- if (is_bottom) paste0("prev_", my_cfg$right, " / prev_", DATASET_NAMES$d1) else ""

          output[[output_id]] <- render_plotly_hist(
            create_pr_histogram,
            plot_data = filtered_data_dd_gen() %>% dplyr::filter(gender_EN == my_gender),
            pr_col = my_cfg$pr,
            left_label = DATASET_NAMES$d1,
            right_label = my_cfg$right,
            show_y_axis = my_is_first,
            show_x_axis = is_bottom,
            y_label = if (my_is_first) (if (my_gender == "M") "MALES" else "FEMALES") else "",
            y_max = 240,
            x_label = x_lab,
            show_subtitle = (my_is_first && my_gender == "M"),
            show_border = TRUE
          )
        })
      }
    })
  }

  # ============================================================================
  # DEMOGRAPHY IMAGE
  # ============================================================================

  output$img1 <- shiny::renderImage(
    list(src = 'www/img/Fig1-EH30-demography.png', width = "auto%", height = "100%", class = "center-img"),
    deleteFile = FALSE
  )

  # ============================================================================
  # GENDER DATA TABLES (DT + GT + CSV + PDF) — 4 comparisons
  # ============================================================================

  # Prepared data reactives
  filtered_gender <- lapply(1:4, function(i) {
    shiny::reactive({ prepare_gender_data(upload_gp_meta[[i]]()) })
  })

  # DT tables
  for (i in 1:4) {
    local({
      my_i <- i
      output[[paste0("upload_diff_genders_meta", my_i, "_DT")]] <- render_custom_dt(
        data_reactive = filtered_gender[[my_i]],
        hidden_cols = GENDER_HIDDEN_COLS,
        shared_id_suffix = "gender"
      )
    })
  }

  # CSV downloads
  gender_csv_labels <- c(
    paste0("Gender_", DATASET_NAMES$d2, "-", DATASET_NAMES$d1),
    paste0("Gender_", DATASET_NAMES$d3, "-", DATASET_NAMES$d1),
    paste0("Gender_", DATASET_NAMES$d4, "-", DATASET_NAMES$d1),
    paste0("Gender_", DATASET_NAMES$d4, "-", DATASET_NAMES$d3)
  )

  for (i in 1:4) {
    local({
      my_i <- i
      output[[paste0("download_csv_gender_meta", my_i)]] <- create_csv_download(
        data_reactive = shiny::reactive({
          shiny::req(filtered_gender[[my_i]]())
          shiny::req(input[[paste0("upload_diff_genders_meta", my_i, "_DT_rows_all")]])
          filtered_gender[[my_i]]()[input[[paste0("upload_diff_genders_meta", my_i, "_DT_rows_all")]], ]
        }),
        filename_prefix = gender_csv_labels[my_i]
      )
    })
  }

  # GT tables (reactive)
  gt_gender <- lapply(1:4, function(i) {
    local({
      my_i <- i
      comp <- COMPARISONS[[my_i]]
      shiny::reactive({
        shiny::req(filtered_gender[[my_i]](),
                   input[[paste0("upload_diff_genders_meta", my_i, "_DT_rows_all")]])
        generate_print_gt(
          df_raw = filtered_gender[[my_i]](),
          dt_rows = input[[paste0("upload_diff_genders_meta", my_i, "_DT_rows_all")]],
          dt_vis_cols = input$visible_columns_gender,
          dt_col_order = input$column_order_gender,
          default_hidden_cols = GENDER_HIDDEN_COLS,
          title = paste("Gender Analysis:", comp$name2, "/", comp$name1),
          subtitle = NULL
        )
      })
    })
  })

  # GT render outputs
  for (i in 1:4) {
    local({
      my_i <- i
      output[[paste0("upload_diff_genders_meta", my_i, "_GT")]] <- gt::render_gt({
        gt_gender[[my_i]]()
      })
    })
  }

  # PDF downloads
  gender_pdf_labels <- c(
    paste0("Gender_Meta1_", DATASET_NAMES$d2, "_", DATASET_NAMES$d1),
    paste0("Gender_Meta2_", DATASET_NAMES$d3, "_", DATASET_NAMES$d1),
    paste0("Gender_Meta3_", DATASET_NAMES$d4, "_", DATASET_NAMES$d1),
    paste0("Gender_Meta4_", DATASET_NAMES$d4, "_", DATASET_NAMES$d3)
  )

  for (i in 1:4) {
    local({
      my_i <- i
      output[[paste0("download_pdf_gender_meta", my_i, "_GT")]] <- shiny::downloadHandler(
        filename = function() {
          paste0(gender_pdf_labels[my_i], "_", Sys.Date(), ".pdf")
        },
        content = gt_download_pdf(gt_table_reactive = gt_gender[[my_i]],
                                  filename_prefix = paste0("Gender_Meta", my_i))
      )
    })
  }

  # ============================================================================
  # AGE DATA TABLES (DT + GT + CSV + PDF) — 4 comparisons
  # ============================================================================

  filtered_age <- lapply(1:4, function(i) {
    shiny::reactive({ prepare_age_data(upload_meta[[i]]()) })
  })

  # DT tables
  for (i in 1:4) {
    local({
      my_i <- i
      output[[paste0("age_meta", my_i)]] <- render_custom_dt(
        data_reactive = filtered_age[[my_i]],
        hidden_cols = AGE_HIDDEN_COLS,
        shared_id_suffix = "age"
      )
    })
  }

  # CSV downloads
  age_csv_labels <- c(
    paste0("Age_", DATASET_NAMES$d2, "-", DATASET_NAMES$d1),
    paste0("Age_", DATASET_NAMES$d3, "-", DATASET_NAMES$d1),
    paste0("Age_", DATASET_NAMES$d4, "-", DATASET_NAMES$d1),
    paste0("Age_", DATASET_NAMES$d4, "-", DATASET_NAMES$d3)
  )

  for (i in 1:4) {
    local({
      my_i <- i
      output[[paste0("download_csv_age_meta", my_i)]] <- create_csv_download(
        data_reactive = shiny::reactive({
          shiny::req(filtered_age[[my_i]]())
          shiny::req(input[[paste0("age_meta", my_i, "_rows_all")]])
          filtered_age[[my_i]]()[input[[paste0("age_meta", my_i, "_rows_all")]], ]
        }),
        filename_prefix = age_csv_labels[my_i]
      )
    })
  }

  # GT tables (reactive)
  gt_age <- lapply(1:4, function(i) {
    local({
      my_i <- i
      comp <- COMPARISONS[[my_i]]
      shiny::reactive({
        shiny::req(filtered_age[[my_i]](),
                   input[[paste0("age_meta", my_i, "_rows_all")]])
        generate_print_gt(
          df_raw = filtered_age[[my_i]](),
          dt_rows = input[[paste0("age_meta", my_i, "_rows_all")]],
          dt_vis_cols = input$visible_columns_age,
          dt_col_order = input$column_order_age,
          default_hidden_cols = AGE_HIDDEN_COLS,
          title = paste("Age Group Analysis:", comp$name2, "/", comp$name1),
          subtitle = NULL
        )
      })
    })
  })

  # GT render outputs
  for (i in 1:4) {
    local({
      my_i <- i
      output[[paste0("age_meta", my_i, "_GT")]] <- gt::render_gt({
        gt_age[[my_i]]()
      })
    })
  }

  # PDF downloads
  age_pdf_labels <- c(
    paste0("Age_Meta1_", DATASET_NAMES$d2, "_", DATASET_NAMES$d1),
    paste0("Age_Meta2_", DATASET_NAMES$d3, "_", DATASET_NAMES$d1),
    paste0("Age_Meta3_", DATASET_NAMES$d4, "_", DATASET_NAMES$d1),
    paste0("Age_Meta4_", DATASET_NAMES$d4, "_", DATASET_NAMES$d3)
  )

  for (i in 1:4) {
    local({
      my_i <- i
      output[[paste0("download_pdf_age_meta", my_i, "_GT")]] <- shiny::downloadHandler(
        filename = function() {
          paste0(age_pdf_labels[my_i], "_", Sys.Date(), ".pdf")
        },
        content = gt_download_pdf(gt_table_reactive = gt_age[[my_i]],
                                  filename_prefix = paste0("Age_Meta", my_i))
      )
    })
  }

  # ============================================================================
  # VOLCANO & STATS — HIDDEN 2026-04-28 (UI also commented out in ui.R).
  # Restore by uncommenting this block and the volcano UI in ui.R.
  # ============================================================================

  # base_data <- shiny::reactive({
  #   idx <- COMPARISON_LABELS[input$filterDatasetsVolc]
  #   app_data$gp_meta[[idx]]
  # })
  #
  # shiny::observeEvent(base_data(), {
  #   shiny::updateSelectizeInput(session, "p0_filter",
  #     choices = unique(base_data()$parent0_code), server = TRUE)
  #   shiny::updateSelectizeInput(session, "p2_filter",
  #     choices = unique(base_data()$parent2_code), server = TRUE)
  # })
  #
  # filtered_dataVolc <- shiny::reactive({
  #   df <- base_data() %>%
  #     dplyr::filter(gender_EN %in% input$gender_filter) %>%
  #     dplyr::filter(ci_width_nat <= input$ci_filterVolc)
  #
  #   if (!is.null(input$p0_filter)) df <- df %>% dplyr::filter(parent0_code %in% input$p0_filter)
  #   if (!is.null(input$p2_filter)) df <- df %>% dplyr::filter(parent2_code %in% input$p2_filter)
  #
  #   return(df)
  # })
  #
  # output$volcanoPlot <- plotly::renderPlotly({
  #   render_volcano_volume_plot(filtered_dataVolc(), threshold = input$fold_filterVolc)
  # })
  #
  # output$statsText <- shiny::renderPrint({
  #   analyze_comparison_stats_se(
  #     df = filtered_dataVolc(),
  #     label = input$filterDatasetsVolc,
  #     threshold = input$fold_filterVolc,
  #     p_value_threshold = as.numeric(input$p_val_filter)
  #   )
  # })

  # ============================================================================
  # CUSTOM TAB — Filters
  # ============================================================================

  # Selected diagnosis codes (chip state)
  customDiagCodes <- shiny::reactiveVal(c("B23", "I10", "I50", "I43", "I48", "D22", "F33", "I69", "I63", "C16", "C61", "I42", "G45", "I20", "F10", "C34", "G43", "F32", "M50", "F03", "F20"))

  # Debounced version — avoids re-rendering plots on every chip add/remove
  customDiagCodesDebounced <- shiny::debounce(
    shiny::reactive(customDiagCodes()), 500
  )

  # Shared data reactive — avoids redundant lookups across all Custom outputs
  customData <- shiny::reactive({
    idx  <- COMPARISON_LABELS[input$filterDatasetsCustom]
    list(
      meta = upload_meta[[idx]](),
      gp   = upload_gp_meta[[idx]](),
      comp = COMPARISONS[[idx]]
    )
  })

  # Build diagnosis category choices from the selected comparison dataset
  shiny::observe({
    idx <- COMPARISON_LABELS[input$filterDatasetsCustom]
    df <- app_data$gp_meta[[idx]]
    if (!is.null(df)) {
      cats <- unique(df[, c("parent2_code", "parent2_name_EN")])
      cats <- cats[order(cats$parent2_code), ]
      choices <- setNames(cats$parent2_code,
                          paste0(cats$parent2_code, " \u2014 ", cats$parent2_name_EN))
      shiny::updateSelectizeInput(session, "filterDiagCustom",
        choices = choices, selected = "", server = TRUE)
    }
  })

  # When user picks a diagnosis, add to chips and clear the input

  shiny::observeEvent(input$filterDiagCustom, {
    code <- input$filterDiagCustom
    if (!is.null(code) && nchar(code) > 0 && !(code %in% customDiagCodes())) {
      customDiagCodes(c(customDiagCodes(), code))
    }
    shiny::updateSelectizeInput(session, "filterDiagCustom", selected = "")
  }, ignoreInit = TRUE)

  # Bulk add comma-separated codes
  shiny::observeEvent(input$bulkDiagAdd, {
    raw <- input$bulkDiagInput
    if (!is.null(raw) && nchar(trimws(raw)) > 0) {
      # Parse: split by comma/semicolon/space, trim, uppercase
      codes <- trimws(unlist(strsplit(raw, "[,;\\s]+")))
      codes <- toupper(codes[nchar(codes) > 0])
      # Only keep codes that exist in the current dataset
      idx <- COMPARISON_LABELS[input$filterDatasetsCustom]
      valid <- unique(app_data$gp_meta[[idx]]$parent2_code)
      codes <- codes[codes %in% valid]
      if (length(codes) > 0) {
        customDiagCodes(unique(c(customDiagCodes(), codes)))
      }
      shiny::updateTextInput(session, "bulkDiagInput", value = "")
    }
  })

  # Remove chip when "x" is clicked
  shiny::observeEvent(input$removeCustomDiag, {
    code_to_remove <- input$removeCustomDiag
    customDiagCodes(setdiff(customDiagCodes(), code_to_remove))
  })

  # Empty all diagnosis codes
  shiny::observeEvent(input$emptyDiagCodes, {
    customDiagCodes(character(0))
  })

  # Populate age and gender filters (same source as heatmap tab)
  shiny::observe({
    shiny::updateSelectInput(session, "filterAgeCustom",
      choices = stringr::str_sort(allAgegroups1()),
      selected = allAgegroups1()[2:9])
  })

  shiny::observe({
    shiny::updateSelectInput(session, "filterGenderCustom",
      choices = stringr::str_sort(allGen1()),
      selected = c("M", "F"))
  })

  # Render chips
  output$customDiagChips <- shiny::renderUI({
    codes <- customDiagCodes()
    if (length(codes) == 0) {
      return(shiny::tags$small(style = "color: #999; font-style: italic;",
        "No diagnosis codes selected. Use the search or paste field above to add codes."))
    }
    chips <- lapply(codes, function(code) {
      shiny::tags$span(class = "diag-chip",
        code,
        shiny::tags$span(class = "remove-chip",
          onclick = sprintf(
            "Shiny.setInputValue('removeCustomDiag', '%s', {priority: 'event'})",
            code),
          "\u00d7")
      )
    })
    shiny::div(class = "diag-chips-row", chips)
  })

  # Custom heatmap
  output$heatmap_custom <- ggiraph::renderGirafe({
    codes <- customDiagCodesDebounced()
    shiny::validate(shiny::need(length(codes) > 0, "Select at least one diagnosis code."))
    shiny::validate(shiny::need(length(input$filterAgeCustom) > 0, "Select at least one age group."))
    shiny::validate(shiny::need(length(input$filterGenderCustom) > 0, "Select at least one gender."))

    d <- customData()
    heatmap_custom(d$meta, input$filterAgeCustom, input$filterGenderCustom,
                   codes, d$comp$name1, d$comp$name2,
                   scale_mode = input$scaleModeCustom)
  })

  # Custom forest plot
  output$forest_custom <- ggiraph::renderGirafe({
    codes <- customDiagCodesDebounced()
    shiny::validate(shiny::need(length(codes) > 0, "Select at least one diagnosis code."))

    d <- customData()
    forest_custom(d$gp, select_codes = codes, select_genders = c("F", "M", "Both"),
                  name1 = d$comp$name1, name2 = d$comp$name2,
                  scale_mode = input$scaleModeCustom)
  })

  # ---------- Custom detail panel (right side) ----------

  # Capture selected parent2_code from click, or default to first code
  customSelectedCode <- shiny::reactive({
    hm_sel <- input$heatmap_custom_selected
    fr_sel <- input$forest_custom_selected
    if (!is.null(hm_sel)) hm_sel
    else if (!is.null(fr_sel)) fr_sel
    else {
      codes <- customDiagCodes()
      if (length(codes) > 0) sort(codes)[1] else NULL
    }
  })

  # Detail panel title
  output$customDetailTitle <- shiny::renderUI({
    code <- customSelectedCode()
    if (is.null(code)) {
      "Diagnosis Detail"
    } else {
      d <- customData()
      name_en <- d$meta %>%
        dplyr::filter(parent2_code == code) %>%
        dplyr::pull(parent2_name_EN) %>%
        unique() %>%
        dplyr::first()
      paste0("Prevalence Ratios and Values of: ", code, " \u2014 ", name_en)
    }
  })

  # Yearly forest with heatmap tile on Meta row
  output$customDetailForest <- ggiraph::renderGirafe({
    code <- customSelectedCode()
    shiny::validate(shiny::need(!is.null(code),
      "Click a diagnosis on the heatmap or forest plot to see details."))

    d <- customData()
    forest_detail_custom(d$meta, input$filterAgeCustom, input$filterGenderCustom,
                         code, d$comp$name1, d$comp$name2,
                         scale_mode = input$scaleModeCustom)
  })

  # Point difference plot for selected diagnosis
  output$customDetailPointDiff <- ggiraph::renderGirafe({
    code <- customSelectedCode()
    shiny::validate(shiny::need(!is.null(code),
      "Click a diagnosis to see prevalence values comparison."))

    d <- customData()
    pointDiff1(d$meta, input$filterAgeCustom, input$filterGenderCustom,
               code, d$comp$name1, d$comp$name2,
               scale_mode = input$scaleModeCustom)
  })

  # ============================================================================
  # PERFORMANCE — Suspend hidden outputs
  # ============================================================================
  outputOptions(output, "heatmap_custom", suspendWhenHidden = TRUE)
  outputOptions(output, "forest_custom", suspendWhenHidden = TRUE)
  outputOptions(output, "customDetailForest", suspendWhenHidden = TRUE)
  outputOptions(output, "customDetailPointDiff", suspendWhenHidden = TRUE)

} # server end
