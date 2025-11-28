
server <- function(input, output, session) {
  #### Default limit is 5MB a file. To increase the limit:
  options(shiny.maxRequestSize = 20 * 1024^2)

  ## DATA Wrappers #####
  # These functions connect the Global data to your reactive logic.
  # Downstream code can still call upload1(), upload_meta1(), etc.

  # 1. Source Data
  upload1 <- shiny::reactive({ G_upload1 })
  upload2 <- shiny::reactive({ G_upload2 })
  upload3 <- shiny::reactive({ G_upload3 })
  upload4 <- shiny::reactive({ G_upload4 })

  # 2. Meta Data
  upload_meta1 <- shiny::reactive({ G_meta1 })
  upload_meta2 <- shiny::reactive({ G_meta2 })
  upload_meta3 <- shiny::reactive({ G_meta3 })
  upload_meta4 <- shiny::reactive({ G_meta4 })

  # 3. Gender Parent Data (Already filtered in Global)
  upload_gender_parent2_meta1 <- shiny::reactive({ G_gp_meta1 })
  upload_gender_parent2_meta2 <- shiny::reactive({ G_gp_meta2 })
  upload_gender_parent2_meta3 <- shiny::reactive({ G_gp_meta3 })
  upload_gender_parent2_meta4 <- shiny::reactive({ G_gp_meta4 })

  # Create reactive values to store the dataset names
  datasetNames <- shiny::reactiveValues(
    name1 = "Est-Health-30",
    name2 = "EstBB",
    name3 = "EstBB1",
    name4 = "EstBB2"
  )

  # Update the UI elements with the new dataset names
  shiny::observe({
    shiny::updateRadioButtons(session, "filterDataset1", choices = c(datasetNames$name1, datasetNames$name2, datasetNames$name3, datasetNames$name4), selected = datasetNames$name1)
    shiny::updateRadioButtons(session, "filterDataset2", choices = c(datasetNames$name1, datasetNames$name2, datasetNames$name3, datasetNames$name4), selected = datasetNames$name2)
  })

  avgData1 <- shiny::reactive({ avgData(upload1()) })
  inputData1 <- shiny::reactive({ cleanData(upload1()) })
  inputData1_top <- shiny::reactive({ topData(inputData1()) })
  # inputData1_par1 <- shiny::reactive({ par1Data(inputData1()) })
  # inputData1_par2 <- shiny::reactive({ par2Data(inputData1()) })

  avgData2 <- shiny::reactive({ avgData(upload2()) })
  inputData2 <- shiny::reactive({ cleanData(upload2()) })
  inputData2_top <- shiny::reactive({ topData(inputData2()) })
  # inputData2_par1 <- shiny::reactive({ par1Data(inputData2()) })
  # inputData2_par2 <- shiny::reactive({ par2Data(inputData2()) })

  avgData3 <- shiny::reactive({ avgData(upload3()) })
  inputData3 <- shiny::reactive({ cleanData(upload3()) })
  inputData3_top <- shiny::reactive({ topData(inputData3()) })
  # inputData3_par1 <- shiny::reactive({ par1Data(inputData3()) })
  # inputData3_par2 <- shiny::reactive({ par2Data(inputData3()) })

  avgData4 <- shiny::reactive({ avgData(upload4()) })
  inputData4 <- shiny::reactive({ cleanData(upload4()) })
  inputData4_top <- shiny::reactive({ topData(inputData4()) })
  # inputData4_par1 <- shiny::reactive({ par1Data(inputData4()) })
  # inputData4_par2 <- shiny::reactive({ par2Data(inputData4()) })

  upload_meta1_add <- reactive({
    df <- upload_meta1() %>%
      #filter(!(age_group %in% c("80+"))) %>%
      rename(log2_diff = prevalence_diff)

    df$fold_diff <- pmax(df$prevalence_data1, df$prevalence_data2) /
      pmin(df$prevalence_data1, df$prevalence_data2)

    df$direction <- ifelse(
      df$prevalence_data2 > df$prevalence_data1, paste0("Higher in ", name2),
      ifelse(df$prevalence_data2 < df$prevalence_data1, paste0("Higher in ", name1), "Equal")
    )
    # Add styling class for coloring
    df$direction_color_class <- ifelse(
      df$direction == "Equal", "Equal",
      ifelse(grepl(name2, df$direction), "Higher in 2", "Higher in 1")
    )
    df <- df %>%
      mutate(across(c(log2_diff, ci_high, ci_low, fold_diff), ~ round(.x, 3)))

    df
  })

  #### Filters Input Values ####
  # Reactive expression to get unique codes from all user-uploaded datasets
  allCodes1 <- shiny::reactive({
    codes <- c()
    if (!is.null(upload1())) {
      diagData1 <- inputData1_top()
      codes <- c(codes, unique(diagData1$parent0_code))
    }
    if (!is.null(upload2())) {
      diagData2 <- inputData2_top()
      codes <- c(codes, unique(diagData2$parent0_code))
    }
    if (!is.null(upload3())) {
      diagData3 <- inputData3_top()
      codes <- c(codes, unique(diagData3$parent0_code))
    }
    if (!is.null(upload4())) {
      diagData4 <- inputData4_top()
      codes <- c(codes, unique(diagData4$parent0_code))
    }
    unique(codes)
  })

  # Observe events for all user uploads to update the selectInput
  shiny::observe({
    shiny::updateSelectInput(session, "filterCodeGroups0",
                             choices = stringr::str_sort(c(allCodes1())),
                             selected =  c("G00-G99", "I00-I99", "C00-D48","F00-F99", "E00-E90")) #c(allCodes1()))
    shiny::updateSelectInput(session, "code_filter0", #overview
                             choices = stringr::str_sort(c(allCodes1())),
                             selected = stringr::str_sort(c("I00-I99")))#stringr::str_sort(c(allCodes1()))) #c("G00-G99", "I00-I99", "C00-D48", "F00-F99", "E00-E90")). c("C00-D48")) #
    shiny::updateSelectInput(session, "code_filter", #overview
                             choices = stringr::str_sort(c(allCodes1())),
                             selected = stringr::str_sort(c("I00-I99"))) #stringr::str_sort(c(allCodes1())))#stringr::str_sort(c("C00-D48"))) #c("G00-G99", "I00-I99", "C00-D48", "F00-F99", "E00-E90")). c("C00-D48")) #
    })
  observeEvent(upload_gender_parent2_meta1(), {
    df <- upload_gender_parent2_meta1() ## todo  reactive
    shiny::updateSliderInput(
      session,
      "fold_filter",
      min = 1,
      max = round(max(df$fold_diff_reg, na.rm = TRUE),2),
      value = c(1.3, round(max(df$fold_diff_reg, na.rm = TRUE),2)),
      step = 0.1
    )

    shiny::updateSliderInput(
      session,
      "ci_filter",
      min = 0,
      max = round(max(df$ci_width_nat, na.rm = TRUE),2),
      value = c(0, 0.3),
      step = 0.1
    )

  })

  output$ci_filter_ui <- renderUI({
    req(upload_gender_parent2_meta1())  # Ensure the reactive data is available
    max_ci <- round(max(upload_gender_parent2_meta1()$ci_width_nat, na.rm = TRUE), 2)
    sliderInput("ci_filter", "CI width:",
                min = 0,
                max = max_ci,
                value = max_ci)
  })

  # Reactive expression to get unique genders from all user-uploaded datasets
  allGen1 <- shiny::reactive({
    genders <- c()
    if (!is.null(upload_gender_parent2_meta1())) {
      diagData1 <-  upload_gender_parent2_meta1()
      genders <- c(genders, unique(diagData1$gender_EN))
    }
    # if (!is.null(upload2())) {
    #   diagData2 <- inputData2_top()
    #   genders <- c(genders, unique(diagData2$gender_EN))
    # }
    # if (!is.null(upload3())) {
    #   diagData3 <- inputData3_top()
    #   genders <- c(genders, unique(diagData3$gender_EN))
    # }
    # if (!is.null(upload4())) {
    #   diagData4 <- inputData4_top()
    #   genders <- c(genders, unique(diagData4$gender_EN))
    # }
    unique(genders)
  })

  # Observe events for all user uploads to update the selectInput for genders
  shiny::observe({
    shiny::updateSelectInput(session, "filterGender",
                             choices = stringr::str_sort(c(allGen1())),
                             selected = c("M", "F"))
  })

  # In your server.R file:
  shiny::observe({
    # Get the complete, unique list of genders from the loaded data
    genders <- allGen1()
    if (length(genders) > 0) {
      sort_order <- c("F", "M", "Both")
      ordered_genders <- sort_order[sort_order %in% genders]
      shiny::updateCheckboxGroupInput(session, "gender_filter",
                                      choices = ordered_genders,
                                      selected = ordered_genders,
                                      inline = TRUE)
    }
  })

  # Reactive expression to get unique age groups from all user-uploaded datasets
  allAgegroups1 <- shiny::reactive({
    agegroups <- c()
    if (!is.null(upload1())) {
      diagData1 <- inputData1_top()
      agegroups <- c(agegroups, unique(diagData1$age_group))
    }
    if (!is.null(upload2())) {
      diagData2 <- inputData2_top()
      agegroups <- c(agegroups, unique(diagData2$age_group))
    }
    if (!is.null(upload3())) {
      diagData3 <- inputData3_top()
      agegroups <- c(agegroups, unique(diagData3$age_group))
    }
    if (!is.null(upload4())) {
      diagData4 <- inputData4_top()
      agegroups <- c(agegroups, unique(diagData4$age_group))
    }
    unique(agegroups)
  })

  # Observe events for all user uploads to update the selectInput for age groups
  shiny::observe({
    shiny::updateSelectInput(session, "filterAge",
                             choices = stringr::str_sort(c(allAgegroups1())),
                             selected = c(allAgegroups1()[2:9]))
  })

  selectedAgegroups <- shiny::reactive({ c(input$filterAge) })
  selectedGen <- shiny::reactive({ c(input$filterGender) })
  selectedCodes <- shiny::reactive({ c(input$filterCodeGroups0) })

  # Update the filter when the 'update' button is clicked
  shiny::observeEvent(input$filterButton, {
    selectedAgegroups <- c(input$filterAge)
    selectedCodes <- c(input$filterCodeGroups0)
    selectedGen <- c(input$filterGender)
  })

  shiny::observeEvent(input$resetButton, {
    shiny::updateSelectInput(session, "filterAge", selected = allAgegroups1())
    shiny::updateSelectInput(session, "filterCodeGroups0", selected = allCodes1())
    shiny::updateSelectInput(session, "filterGender", selected = c("M", "N")) #c(allGen1())) #, "Both"
  })

  plotTitle <- shiny::reactive({
    diag_str <- paste(input$agefilterCodes, collapse = ", ")
    sprintf("Patients in agegroups with diagnoses: %s", diag_str)
  })
  plotsubTitle <- shiny::reactive({
    gender_str <- paste(input$filterGender, collapse = ", ")
    age_str <- paste(input$filterAge, collapse = ", ")
    sprintf("Gender: %s \n Age groups: %s", gender_str, age_str)
  })
  plotTitle_plotDet0 <- shiny::reactive({
    diag_str <- paste(input$filterCodes, collapse = ", ")
    sprintf("Prevalence of diagnoses groups")
  })
  plotTitle_plotDet1 <- shiny::reactive({
    diag_str <- paste(selectedCodes1(), collapse = ", ")
    sprintf("Prevalence of diagnoses subgroups: %s", diag_str)
  })
  plotTitle_plotDet2 <- shiny::reactive({
    diag_str <- paste(selectedCodes2(), collapse = ", ")
    sprintf("Prevalence of diagnoses: %s", diag_str)
  })
  plotsubTitle_plotDet0 <- shiny::reactive({
    gender_str <- paste(input$filterGender, collapse = ", ")
    sprintf("Gender: %s", gender_str)
  })

 #### PLOTS: Heatmap META #####
  selectedDataset_meta <- shiny::reactive({
    name <- input$filterDatasets
    if (name == "Est-Health-30 vs EstBB") {
      upload_meta1()
    } else if (name == "Est-Health-30 vs EstBB1") {
      upload_meta2()
    } else if (name == "Est-Health-30 vs EstBB2") {
      upload_meta3()
    } else if (name == "EstBB1 vs EstBB2") {
      upload_meta4()
    }
  })

  selectedNames <- shiny::reactive({
    req(input$filterDatasets)  # Ensure selection is made

    switch(input$filterDatasets,
           "Est-Health-30 vs EstBB" = list(name2 = "EstBB", name1 = "Est-Health-30"),
           "Est-Health-30 vs EstBB1" = list(name2 = "EstBB1", name1 = "Est-Health-30"),
           "Est-Health-30 vs EstBB2" = list(name2 = "EstBB2", name1 = "Est-Health-30"),
           "EstBB1 vs EstBB2" = list(name1 = "EstBB1", name2 = "EstBB2"),
           list(name1 = "Unknown", name2 = "Unknown")  # Default case for unexpected input
    )
  })
  output$heatmap_meta_avg <- ggiraph::renderGirafe({
    # shiny::validate(
    #   shiny::need(
    #     (!is.null(input$selectedDatasets)),
    #     "Select datasets to compare."
    #   )
    # )
    heatmap_meta_avg(selectedDataset_meta(), selectedAgegroups(),
                 selectedGen(), selectedCodes(), selectedNames()$name1, selectedNames()$name2)
  })
  output$value1 <- renderText({ input$switch1 })

  output$value2 <- renderText({ input$switch2 })
  selectedCodes_sub <- shiny::reactive({ input$heatmap_meta_avg_selected })
  selectedCodes_sub2 <- shiny::reactive({
    if (!is.null(input$heatmap_meta_alph_selected)) {
      input$heatmap_meta_alph_selected
    } else if (!is.null(input$heatmap_meta_avgDet_selected)) {
      input$heatmap_meta_avgDet_selected
    } else {
      NULL  # Default case when neither is selected
    }
  })


  active_plot <- reactiveVal("Alphabetical")
  active_filter <- reactiveVal("All")  # New reactive value for filtering

  # Sorting button logic
  observeEvent(input$alphaBtn, {
    active_plot("Alphabetical")
    runjs("$('#alphaBtn').addClass('active').removeClass('inactive');
         $('#avgBtn').addClass('inactive').removeClass('active');")
  })

  observeEvent(input$avgBtn, {
    active_plot("Average")
    runjs("$('#avgBtn').addClass('active').removeClass('inactive');
         $('#alphaBtn').addClass('inactive').removeClass('active');")
  })

  # Filtering button logic
  observeEvent(input$sigBtn, {
    active_filter("Significant")
    runjs("$('#sigBtn').addClass('active').removeClass('inactive');
         $('#allBtn').addClass('inactive').removeClass('active');")
  })

  observeEvent(input$allBtn, {
    active_filter("All")
    runjs("$('#allBtn').addClass('active').removeClass('inactive');
         $('#sigBtn').addClass('inactive').removeClass('active');")
  })

  output$heatmap_meta_alph <- ggiraph::renderGirafe({
    shiny::validate(
      shiny::need(
        (!is.null(selectedCodes_sub())),
        "Click on a Diagnosis Group from Heatmap to See the Details."
      )
    )
    # Get active settings
    sorting_method <- active_plot()
    filter_method <- active_filter()
    # Apply filtering based on the selected button
    data_to_plot1 <- if (filter_method == "Significant") {
      selectedDataset_meta() %>% filter(sig %in% c("sig"))
    } else {
      selectedDataset_meta()
    }

    # Apply sorting and filtering combination logic
    if (sorting_method == "Alphabetical" && filter_method == "Significant") {
      heatmap_meta_alph(data_to_plot1, selectedAgegroups(),
                        selectedGen(), selectedCodes_sub(), selectedNames()$name1, selectedNames()$name2)
    } else if (sorting_method == "Alphabetical" && filter_method == "All") {
      heatmap_meta_alph(selectedDataset_meta(), selectedAgegroups(),
                        selectedGen(), selectedCodes_sub(), selectedNames()$name1, selectedNames()$name2)
    } else if (sorting_method == "Average" && filter_method == "Significant") {
      heatmap_meta_avgDet(data_to_plot1, selectedAgegroups(),
                          selectedGen(), selectedCodes_sub(), selectedNames()$name1, selectedNames()$name2)
    } else if (sorting_method == "Average" && filter_method == "All") {
      heatmap_meta_avgDet(selectedDataset_meta(), selectedAgegroups(),
                          selectedGen(), selectedCodes_sub(), selectedNames()$name1, selectedNames()$name2)
    }
  })

  output$heatmap_meta_alph2 <- ggiraph::renderGirafe({
    shiny::validate(
      shiny::need(
        (!is.null(selectedCodes_sub())),
        "Select a diagnosis group from Heatmap to zoom in."
      )
    )
    # Get active settings
    sorting_method <- active_plot()
    filter_method <- active_filter()
    # Apply filtering based on the selected button
    data_to_plot2 <- if (filter_method == "Significant") {
      upload_meta2() %>% filter(sig %in% c("sig"))
    } else {
      upload_meta2()
    }

    # Apply sorting and filtering combination logic
    if (sorting_method == "Alphabetical" && filter_method == "Significant") {
      heatmap_meta_alph(data_to_plot2, selectedAgegroups(),
                        selectedGen(), selectedCodes_sub(), "EstBB1", "Est-Health-30")
    } else if (sorting_method == "Alphabetical" && filter_method == "All") {
      heatmap_meta_alph(upload_meta2(), selectedAgegroups(),
                        selectedGen(), selectedCodes_sub(), "EstBB1", "Est-Health-30")
    } else if (sorting_method == "Average" && filter_method == "Significant") {
      heatmap_meta_avgDet(data_to_plot2, selectedAgegroups(),
                          selectedGen(), selectedCodes_sub(), "EstBB1", "Est-Health-30")
    } else if (sorting_method == "Average" && filter_method == "All") {
      heatmap_meta_avgDet(upload_meta2(), selectedAgegroups(),
                          selectedGen(), selectedCodes_sub(), "EstBB1", "Est-Health-30")
    }
  })
  output$heatmap_meta_alph3 <- ggiraph::renderGirafe({
    shiny::validate(
      shiny::need(
        (!is.null(selectedCodes_sub())),
        "Select a diagnosis group from Heatmap to zoom in."
      )
    )
    # Get active settings
    sorting_method <- active_plot()
    filter_method <- active_filter()
    # Apply filtering based on the selected button
    data_to_plot3 <- if (filter_method == "Significant") {
      upload_meta3() %>% filter(sig %in% c("sig"))
    } else {
      upload_meta3()
    }
    # Apply sorting and filtering combination logic
    if (sorting_method == "Alphabetical" && filter_method == "Significant") {
      heatmap_meta_alph(data_to_plot3, selectedAgegroups(),
                        selectedGen(), selectedCodes_sub(), "EstBB2", "Est-Health-30")
    } else if (sorting_method == "Alphabetical" && filter_method == "All") {
      heatmap_meta_alph(upload_meta3(), selectedAgegroups(),
                        selectedGen(), selectedCodes_sub(), "EstBB2", "Est-Health-30")
    } else if (sorting_method == "Average" && filter_method == "Significant") {
      heatmap_meta_avgDet(data_to_plot3, selectedAgegroups(),
                          selectedGen(), selectedCodes_sub(), "EstBB2", "Est-Health-30")
    } else if (sorting_method == "Average" && filter_method == "All") {
      heatmap_meta_avgDet(upload_meta3(), selectedAgegroups(),
                          selectedGen(), selectedCodes_sub(), "EstBB2", "Est-Health-30")
    }
  })

  output$heatmap_meta_alph4 <- ggiraph::renderGirafe({
    shiny::validate(
      shiny::need(
        (!is.null(selectedCodes_sub())),
        "Select a diagnosis group from Heatmap to zoom in."
      )
    )
    # Get active settings
    sorting_method <- active_plot()
    filter_method <- active_filter()
    # Apply filtering based on the selected button
    data_to_plot4 <- if (filter_method == "Significant") {
      upload_meta4() %>% filter(sig %in% c("sig"))
    } else {
      upload_meta4()
    }
    # Apply sorting and filtering combination logic
    if (sorting_method == "Alphabetical" && filter_method == "Significant") {
      heatmap_meta_alph(data_to_plot4, selectedAgegroups(),
                        selectedGen(), selectedCodes_sub(), "EstBB2", "EstBB1")
    } else if (sorting_method == "Alphabetical" && filter_method == "All") {
      heatmap_meta_alph(upload_meta4(), selectedAgegroups(),
                        selectedGen(), selectedCodes_sub(), "EstBB2", "EstBB1")
    } else if (sorting_method == "Average" && filter_method == "Significant") {
      heatmap_meta_avgDet(data_to_plot4, selectedAgegroups(),
                          selectedGen(), selectedCodes_sub(), "EstBB2", "EstBB1")
    } else if (sorting_method == "Average" && filter_method == "All") {
      heatmap_meta_avgDet(upload_meta4(), selectedAgegroups(),
                          selectedGen(), selectedCodes_sub(), "EstBB2", "EstBB1")
    }
  })

  output$selectedCodes2 <- shiny::renderText(selectedCodes2())
  output$selectedCodes2 <- shiny::renderText(selectedCodes2())
  output$prev_diff_filtered_BIG <- ggiraph::renderGirafe({ # Prevalence RAtios per Age Group and per Chapter - Heatmap #####
    shiny::validate(
      shiny::need(
        (!is.null(selectedDataset1()) & !is.null(selectedDataset2())),
        "Upload 2 datasets."
      )
    )
    heatMap_big(selectedDataset1(), selectedDataset2(),
                selectedYearMultiple(), selectedAgegroups(), selectedGen(), selectedCodes(),
                input$filterDataset1, input$filterDataset2)
  })
  selectedCodesHeat <- shiny::reactive({ input$prev_diff_filtered_BIG_selected })
  output$prev_diff_filtered2 <- ggiraph::renderGirafe({
    shiny::validate(
      shiny::need(
        (!is.null(input$prev_diff_filtered_BIG_selected)),
        "Select a diagnosis group from left to zoom in."
      )
    )
    heatMap_p2(selectedDataset1(), selectedDataset2(),
               selectedYearMultiple(), selectedAgegroups(), selectedGen(), selectedCodesHeat(),
               input$filterDataset1, input$filterDataset2)
  })
  selectedCodesHeat2 <- shiny::reactive({ input$prev_diff_filtered2_selected })
#### PLOTS: Histogram, prevalences #####
  filtered_data_dd <- reactive({ ### histogram data prep #####

    # Get the data from the reactive loaders
    pop_bb <-  upload_gender_parent2_meta1()
    pop_bb1 <-  upload_gender_parent2_meta2()
    pop_bb2 <-  upload_gender_parent2_meta3()

    # `DALY_ICD` is available from global.R

    # Perform the join as in your helper script
    d <- pop_bb1 %>%
      select(parent0_code, parent2_code, parent2_name_EN, gender_EN, p_vs_bb1 = prevalence_diff, p_vs_bb1_p = p_value) %>%
      left_join(
        pop_bb2 %>%
          select(parent0_code, parent2_code, parent2_name_EN, gender_EN, p_vs_bb2 = prevalence_diff, p_vs_bb2_p = p_value),
        by = c("parent0_code", "parent2_code", "parent2_name_EN", "gender_EN")
      ) %>%
      left_join(
        pop_bb %>%
          select(parent0_code, parent2_code, parent2_name_EN, gender_EN, p_vs_bb = prevalence_diff, p_vs_bb_p = p_value),
        by = c("parent0_code", "parent2_code", "parent2_name_EN", "gender_EN")
      )

    # Join with DALY_ICD and filter
    dd <- d %>%
      inner_join(DALY_ICD %>% select(parent2_code = ICD10, YLL2021, YLL2021norm, YLD2021, YLD2021norm),
                 by = "parent2_code") %>%
      filter(gender_EN == "Both") %>%
      filter(!(parent0_code == "V01-Y98"))

    return(dd)
  })
  # --- 3. Render Plots ---
  # Get the prepared reactive data and pass it to the plot functions

  # --- Column 1 Plots ---
  output$p_histogram_bb <- plotly::renderPlotly({
    plot_data <- filtered_data_dd()
    p <- create_p_histogram_bb(plot_data)
    plotly::ggplotly(p) %>%
      layout(
        xaxis = list(
          showline = TRUE,
          linecolor = toRGB(color_lines),
          linewidth = 1
        ),
        yaxis = list(
          showline = TRUE,
          linecolor = toRGB(color_lines),
          linewidth = 1
        )
      )
  })

  output$p_death_bb <- plotly::renderPlotly({
    plot_data <- filtered_data_dd()
    p <- create_p_death_bb(plot_data)
    plotly::ggplotly(p, tooltip = c("text")) %>%
      layout(
        hoverlabel = list(
          bgcolor = "rgba(245, 245, 220, 0.9)",
          font = list(color = "black", size = 11,
                      align = 'left'),
          bordercolor = "rgba(0, 0, 0, 0)",
          namelength = 20
        ),
        xaxis = list(
          showline = TRUE,
          linecolor = toRGB(color_lines),
          linewidth = 1
        ),
        yaxis = list(
          showline = TRUE,
          linecolor = toRGB(color_lines),
          linewidth = 1
        )
      )
  })

  output$p_disability_bb <- plotly::renderPlotly({
    plot_data <- filtered_data_dd()
    p <- create_p_disability_bb(plot_data)
    plotly::ggplotly(p, tooltip = c("text")) %>%
      layout(
        hoverlabel = list(
          bgcolor = "rgba(245, 245, 220, 0.9)",
          font = list(color = "black", size = 11,
                      align = 'left'),
          bordercolor = "rgba(0, 0, 0, 0)",
          namelength = 20
        ),
        xaxis = list(
          showline = TRUE,
          linecolor = toRGB(color_lines),
          linewidth = 1
        ),
        yaxis = list(
          showline = TRUE,
          linecolor = toRGB(color_lines),
          linewidth = 1
        )
      )
  })

  # --- Column 2 Plots ---
  output$p_histogram_bb1 <- plotly::renderPlotly({
    plot_data <- filtered_data_dd()
    p <- create_p_histogram_bb1(plot_data)
    plotly::ggplotly(p) %>%
      layout(
        xaxis = list(
          showline = TRUE,
          linecolor = toRGB(color_lines),
          linewidth = 1
        ),
        yaxis = list(
          showline = TRUE,
          linecolor = toRGB(color_lines),
          linewidth = 1
        )
      )
  })

  output$p_death_bb1 <- plotly::renderPlotly({
    plot_data <- filtered_data_dd()
    p <- create_p_death_bb1(plot_data)
    plotly::ggplotly(p, tooltip = c("text")) %>%
      layout(
        hoverlabel = list(
          bgcolor = "rgba(245, 245, 220, 0.9)",
          font = list(color = "black", size = 11,
                      align = 'left'),
          bordercolor = "rgba(0, 0, 0, 0)",
          namelength = 20
        ),
        xaxis = list(
          showline = TRUE,
          linecolor = toRGB(color_lines),
          linewidth = 1
        ),
        yaxis = list(
          showline = TRUE,
          linecolor = toRGB(color_lines),
          linewidth = 1
        )
      )
  })

  output$p_disability_bb1 <- plotly::renderPlotly({
    plot_data <- filtered_data_dd()
    p <- create_p_disability_bb1(plot_data)
    plotly::ggplotly(p, tooltip = c("text")) %>%
      layout(
        hoverlabel = list(
          bgcolor = "rgba(245, 245, 220, 0.9)",
          font = list(color = "black", size = 11,
                      align = 'left'),
          bordercolor = "rgba(0, 0, 0, 0)",
          namelength = 20
        ),
        xaxis = list(
          showline = TRUE,
          linecolor = toRGB(color_lines),
          linewidth = 1
        ),
        yaxis = list(
          showline = TRUE,
          linecolor = toRGB(color_lines),
          linewidth = 1
        )
      )
  })

  # --- Column 3 Plots ---
  output$p_histogram_bb2 <- plotly::renderPlotly({
    plot_data <- filtered_data_dd()
    p <- create_p_histogram_bb2(plot_data)
    plotly::ggplotly(p) %>%
      layout(
        xaxis = list(
          showline = TRUE,
          linecolor = toRGB(color_lines),
          linewidth = 1
        ),
        yaxis = list(
          showline = TRUE,
          linecolor = toRGB(color_lines),
          linewidth = 1
        )
      )
  })

  output$p_death_bb2 <- plotly::renderPlotly({
    plot_data <- filtered_data_dd()
    p <- create_p_death_bb2(plot_data)
    plotly::ggplotly(p, tooltip = c("text")) %>%
      layout(
        hoverlabel = list(
          bgcolor = "rgba(245, 245, 220, 0.9)",
          font = list(color = "black", size = 11,
                      align = 'left'),
          bordercolor = "rgba(0, 0, 0, 0)",
          namelength = 20
        ),
        xaxis = list(
          showline = TRUE,
          linecolor = toRGB(color_lines),
          linewidth = 1
        ),
        yaxis = list(
          showline = TRUE,
          linecolor = toRGB(color_lines),
          linewidth = 1
        )
      )
  })

  output$p_disability_bb2 <- plotly::renderPlotly({
    plot_data <- filtered_data_dd()
    p <- create_p_disability_bb2(plot_data)
    plotly::ggplotly(p, tooltip = c("text")) %>%
      layout(
        hoverlabel = list(
          bgcolor = "rgba(245, 245, 220, 0.9)",
          font = list(color = "black", size = 11,
                      align = 'left'),
          bordercolor = "rgba(0, 0, 0, 0)",
          namelength = 20
        ),
        xaxis = list(
          showline = TRUE,
          linecolor = toRGB(color_lines),
          linewidth = 1
        ),
        yaxis = list(
          showline = TRUE,
          linecolor = toRGB(color_lines),
          linewidth = 1
        )
      )
  })

  # ----------------------------------------------------------------------
  # Server Logic: Distribution by Gender (M/F)
  # ----------------------------------------------------------------------
  # ----------------------------------------------------------------------
  # Reactive Data Source for Gender Plots (dd_gen)
  # ----------------------------------------------------------------------
  filtered_data_dd_gen <- reactive({
    pop_bb <-  upload_gender_parent2_meta1()
    pop_bb1 <-  upload_gender_parent2_meta2()
    pop_bb2 <-  upload_gender_parent2_meta3()
    # Base data from pop_bb1
    pop_bb1 %>%
      dplyr::filter(gender_EN %in% c("M", "F")) %>%
      dplyr::select(parent0_code, parent2_code, parent2_name_EN, gender_EN, p_vs_bb1 = prevalence_diff) %>%

      # Join with pop_bb2
      dplyr::left_join(
        pop_bb2 %>%
          dplyr::filter(gender_EN %in% c("M", "F")) %>%
          dplyr::select(parent0_code, parent2_code, parent2_name_EN, gender_EN, p_vs_bb2 = prevalence_diff),
        by = c("parent0_code", "parent2_code", "parent2_name_EN", "gender_EN")
      ) %>%

      # Join with pop_bb
      dplyr::left_join(
        pop_bb %>%
          dplyr::filter(gender_EN %in% c("M", "F")) %>%
          dplyr::select(parent0_code, parent2_code, parent2_name_EN, gender_EN, p_vs_bb = prevalence_diff),
        by = c("parent0_code", "parent2_code", "parent2_name_EN", "gender_EN")
      ) %>%

      # Final Exclusion Filter
      dplyr::filter(!(parent0_code == "V01-Y98"))

  })
  # --- Column 1 Plots (Dataset bb) ---
  output$p_histogram_bbM <- plotly::renderPlotly({
    plot_data <- filtered_data_dd_gen()
    p <- create_p_histogram_bbM(plot_data)
    plotly::ggplotly(p) %>%
      layout(
        xaxis = list(
          showline = TRUE,
          linecolor = toRGB(color_lines),
          linewidth = 1
        ),
        yaxis = list(
          showline = TRUE,
          linecolor = toRGB(color_lines),
          linewidth = 1
        )
      )
  })

  output$p_histogram_bbF <- plotly::renderPlotly({
    plot_data <- filtered_data_dd_gen()
    p <- create_p_histogram_bbF(plot_data)
    plotly::ggplotly(p) %>%
      layout(
        xaxis = list(
          showline = TRUE,
          linecolor = toRGB(color_lines),
          linewidth = 1
        ),
        yaxis = list(
          showline = TRUE,
          linecolor = toRGB(color_lines),
          linewidth = 1
        )
      )
  })

  # --- Column 2 Plots (Dataset bb1) ---
  output$p_histogram_bb1M <- plotly::renderPlotly({
    plot_data <- filtered_data_dd_gen()
    p <- create_p_histogram_bb1M(plot_data)
    plotly::ggplotly(p) %>%
      layout(
        xaxis = list(
          showline = TRUE,
          linecolor = toRGB(color_lines),
          linewidth = 1
        ),
        yaxis = list(
          showline = TRUE,
          linecolor = toRGB(color_lines),
          linewidth = 1
        )
      )
  })

  output$p_histogram_bb1F <- plotly::renderPlotly({
    plot_data <- filtered_data_dd_gen()
    p <- create_p_histogram_bb1F(plot_data)
    plotly::ggplotly(p) %>%
      layout(
        xaxis = list(
          showline = TRUE,
          linecolor = toRGB(color_lines),
          linewidth = 1
        ),
        yaxis = list(
          showline = TRUE,
          linecolor = toRGB(color_lines),
          linewidth = 1
        )
      )
  })

  # --- Column 3 Plots (Dataset bb2) ---
  output$p_histogram_bb2M <- plotly::renderPlotly({
    plot_data <- filtered_data_dd_gen()
    p <- create_p_histogram_bb2M(plot_data)
    plotly::ggplotly(p) %>%
      layout(
        xaxis = list(
          showline = TRUE,
          linecolor = toRGB(color_lines),
          linewidth = 1
        ),
        yaxis = list(
          showline = TRUE,
          linecolor = toRGB(color_lines),
          linewidth = 1
        )
      )
  })

  output$p_histogram_bb2F <- plotly::renderPlotly({
    plot_data <- filtered_data_dd_gen()
    p <- create_p_histogram_bb2F(plot_data)
    plotly::ggplotly(p) %>%
      layout(
        xaxis = list(
          showline = TRUE,
          linecolor = toRGB(color_lines),
          linewidth = 1
        ),
        yaxis = list(
          showline = TRUE,
          linecolor = toRGB(color_lines),
          linewidth = 1
        )
      )
  })
  #### PLOTS: Forest PointDiffs #####
  output$forest1 <- ggiraph::renderGirafe({ # Prevalence Ratios per Age Group, Forest Plot#####
    shiny::validate(
      shiny::need(
        (!is.null(selectedCodes_sub2())),
        "Prevalence Ratios Across Years: Click on a diagnosis code from detailed, diagnosis-level heatmap."
      )
    )
    forest1(selectedDataset_meta(),
           selectedAgegroups(), selectedGen(), selectedCodes_sub2(), selectedNames()$name1, selectedNames()$name2) #
  })
  output$pointDiff1 <- ggiraph::renderGirafe({ # Prevalence Values, Bar chart#####
    shiny::validate(
      shiny::need(
        (!is.null(selectedCodes_sub2())),
        "Prevalence Values Across Years: Click on a diagnosis code from detailed, diagnosis-level heatmap."
      )
    )
    pointDiff1(selectedDataset_meta(),
               selectedAgegroups(), selectedGen(), selectedCodes_sub2(),
               selectedNames()$name1, selectedNames()$name2)
  })
  output$forest2_parent2 <- ggiraph::renderGirafe({ # Prevalence Ratios pre Gender - Forest plot  #####
    shiny::validate(
      shiny::need(
        (!is.null(input$code_filter) && length(input$code_filter) > 0),
        "Prevalence difference on forestplot: Select at least one diagnosis code."
      )
    )
    forest2_parent2(
      upload_gender_parent2_meta1(),
      gender_select = input$gender_filter,
      code_select   = input$code_filter,
      name1 = name1,
      name2 = name2,
      ci_range   = input$ci_filter,
      fold_range = input$fold_filter
    )
  })

  output$forest2_parent2_genders2 <- ggiraph::renderGirafe({ # Prevalence Ratios pre Gender - Forest plot  #####
    shiny::validate(
      shiny::need(
        (!is.null(input$code_filter) && length(input$code_filter) > 0),
        "Prevalence difference on forestplot: Select at least one diagnosis code."
      )
    )
    forest2_parent2(
      upload_gender_parent2_meta2(),
      gender_select = input$gender_filter,
      code_select   = input$code_filter,
      name1 = name1,
      name2 = name3,
      ci_range   = input$ci_filter,
      fold_range = input$fold_filter
    )
  })

  output$forest2_parent2_genders3 <- ggiraph::renderGirafe({ # Prevalence Ratios pre Gender - Forest plot  #####
    shiny::validate(
      shiny::need(
        (!is.null(input$code_filter) && length(input$code_filter) > 0),
        "Prevalence difference on forestplot: Select at least one diagnosis code."
      )
    )
    forest2_parent2(
      upload_gender_parent2_meta3(),
      gender_select = input$gender_filter,
      code_select   = input$code_filter,
      name1 = name1,
      name2 = name4,
      ci_range   = input$ci_filter,
      fold_range = input$fold_filter
    )
  })

  output$forest2_parent2_genders4 <- ggiraph::renderGirafe({ # Prevalence Ratios pre Gender - Forest plot  #####
    shiny::validate(
      shiny::need(
        (!is.null(input$code_filter) && length(input$code_filter) > 0),
        "Prevalence difference on forestplot: Select at least one diagnosis code."
      )
    )
    forest2_parent2(
      upload_gender_parent2_meta4(),
      gender_select = input$gender_filter,
      code_select   = input$code_filter,
      name1 = name3,
      name2 = name4,
      ci_range   = input$ci_filter,
      fold_range = input$fold_filter
    )
  })

  ###### Images ######

  output$img1 <- shiny::renderImage(list(src='www/img/Fig1-EH30-demography.png', width ="auto%",  height = "100%",  class = "center-img"), #Bio-Pop-rel.png
                                    deleteFile = FALSE)

##### META GENDERS #####

  # Columns to hide by default for all GENDER tables
  gender_hidden_targets <- c(
    "parent2_name", "parent1_code", "parent1_name",
    "parent0_code", "parent0_name", "parent0_name_EN",
    "gender", "prevalence_diff", "ci_low", "ci_high",
    "se", "z", "fold_ci_low_nat", "fold_ci_high_nat",
    "ci_width_nat", "meta_model_type"
  )
  # Columns to hide by default for all AGE tables (Adjust targets as needed)
  age_hidden_targets <- c(
    "parent2_name", "parent1_code", "parent1_name",
    "parent0_code", "parent0_name", "meta_model_type"
  )

  # ==============================================================================
  # GENDER ANALYSIS TABS (Meta 1-4)
  # ==============================================================================
  # --- META 1 ---
  filtered_data_meta1 <- reactive({ prepare_gender_data(upload_gender_parent2_meta1()) })
  output$upload_diff_genders_meta1_DT <- render_custom_dt(
    data_reactive = filtered_data_meta1, hidden_cols = gender_hidden_targets, shared_id_suffix = "gender"
  )
  # --- META 2 ---
  filtered_data_meta2 <- reactive({ prepare_gender_data(upload_gender_parent2_meta2()) })
  output$upload_diff_genders_meta2_DT <- render_custom_dt(
    data_reactive = filtered_data_meta2, hidden_cols = gender_hidden_targets, shared_id_suffix = "gender"
  )
  # --- META 3 ---
  filtered_data_meta3 <- reactive({ prepare_gender_data(upload_gender_parent2_meta3()) })
  output$upload_diff_genders_meta3_DT <- render_custom_dt(
    data_reactive = filtered_data_meta3, hidden_cols = gender_hidden_targets, shared_id_suffix = "gender"
  )
  # --- META 4 ---
  filtered_data_meta4 <- reactive({ prepare_gender_data(upload_gender_parent2_meta4()) })
  output$upload_diff_genders_meta4_DT <- render_custom_dt(
    data_reactive = filtered_data_meta4, hidden_cols = gender_hidden_targets, shared_id_suffix = "gender"
  )

  # ==============================================================================
  # CSV DOWNLOAD HANDLERS GENDER
  # ==============================================================================
  # --- Gender Analysis Downloads ---
  output$download_csv_gender_meta1 <- create_csv_download(
    data_reactive = reactive({
      req(filtered_data_meta1())
      req(input$upload_diff_genders_meta1_DT_rows_all)
      filtered_data_meta1()[input$upload_diff_genders_meta1_DT_rows_all, ]
    }),
    filename_prefix = paste0("Gender_", name2, "-", name1)
  )

  output$download_csv_gender_meta2 <- create_csv_download(
    data_reactive = reactive({
      req(filtered_data_meta2())
      req(input$upload_diff_genders_meta2_DT_rows_all)
      filtered_data_meta2()[input$upload_diff_genders_meta2_DT_rows_all, ]
    }),
    filename_prefix = paste0("Gender_", name3, "-", name1)
  )

  output$download_csv_gender_meta3 <- create_csv_download(
    data_reactive = reactive({
      req(filtered_data_meta3())
      req(input$upload_diff_genders_meta3_DT_rows_all)
      filtered_data_meta3()[input$upload_diff_genders_meta3_DT_rows_all, ]
    }),
    filename_prefix = paste0("Gender_", name4, "-", name1)
  )

  output$download_csv_gender_meta4 <- create_csv_download(
    data_reactive = reactive({
      req(filtered_data_meta4())
      req(input$upload_diff_genders_meta4_DT_rows_all)
      filtered_data_meta4()[input$upload_diff_genders_meta4_DT_rows_all, ]
    }),
    filename_prefix = paste0("Gender_", name4, "-", name3)
  )

  # Gender Meta 1 GT Table
  gt_gender_meta1_r <- reactive({
    req(filtered_data_meta1(), input$upload_diff_genders_meta1_DT_rows_all)
    generate_print_gt(
      df_raw = filtered_data_meta1(),
      dt_rows = input$upload_diff_genders_meta1_DT_rows_all,
      dt_vis_cols = input$visible_columns_gender,
      dt_col_order = input$column_order_gender,
      # *** Keep this for initial state handling ***
      default_hidden_cols = gender_hidden_targets,
      title = paste("Gender Analysis:", name2, "/", name1),
      subtitle = NULL # Uses default row count
    )
  })

  # Gender Meta 2 GT Table
  gt_gender_meta2_r <- reactive({
    req(filtered_data_meta2(), input$upload_diff_genders_meta2_DT_rows_all)
    generate_print_gt(
      df_raw = filtered_data_meta2(),
      dt_rows = input$upload_diff_genders_meta2_DT_rows_all,
      dt_vis_cols = input$visible_columns_gender,
      dt_col_order = input$column_order_gender,
      # *** Keep this for initial state handling ***
      default_hidden_cols = gender_hidden_targets,
      title = paste("Gender Analysis:", name3, "/", name1),
      subtitle = NULL
    )
  })

  # Gender Meta 3 GT Table
  gt_gender_meta3_r <- reactive({
    req(filtered_data_meta3(), input$upload_diff_genders_meta3_DT_rows_all)
    generate_print_gt(
      df_raw = filtered_data_meta3(),
      dt_rows = input$upload_diff_genders_meta3_DT_rows_all,
      dt_vis_cols = input$visible_columns_gender,
      dt_col_order = input$column_order_gender,
      # *** Keep this for initial state handling ***
      default_hidden_cols = gender_hidden_targets,
      title = paste("Gender Analysis:", name4, "/", name1),

      subtitle = NULL
    )
  })

  # Gender Meta 4 GT Table
  gt_gender_meta4_r <- reactive({
    req(filtered_data_meta4(), input$upload_diff_genders_meta4_DT_rows_all)
    generate_print_gt(
      df_raw = filtered_data_meta4(),
      dt_rows = input$upload_diff_genders_meta4_DT_rows_all,
      dt_vis_cols = input$visible_columns_gender,
      dt_col_order = input$column_order_gender,
      # *** Keep this for initial state handling ***
      default_hidden_cols = gender_hidden_targets,
      title = paste("Gender Analysis:", name4, "/", name3),
      subtitle = NULL
    )
  })

  # --- GT Table Display Outputs ---
  output$upload_diff_genders_meta1_GT <- gt::render_gt({
    gt_gender_meta1_r()
  })
  output$upload_diff_genders_meta2_GT <- gt::render_gt({
    gt_gender_meta2_r()
  })
  output$upload_diff_genders_meta3_GT <- gt::render_gt({
    gt_gender_meta3_r()
  })
  output$upload_diff_genders_meta4_GT <- gt::render_gt({
    gt_gender_meta4_r()
  })

  # --- Download Handlers for PDF ---
  output$download_pdf_gender_meta1_GT <- shiny::downloadHandler(
    filename = function() {
      paste0("Gender_Meta1_", name2, "_", name1, "_", Sys.Date(), ".pdf")
    },
    content = gt_download_pdf(gt_table_reactive = gt_gender_meta1_r, filename_prefix = "Gender_Meta1")
  )
  output$download_pdf_gender_meta2_GT <- shiny::downloadHandler(
    filename = function() {
      paste0("Gender_Meta2_", name3, "_", name1, "_", Sys.Date(), ".pdf")
    },
    content = gt_download_pdf(gt_table_reactive = gt_gender_meta2_r, filename_prefix = "Gender_Meta2")
  )
  output$download_pdf_gender_meta3_GT <- shiny::downloadHandler(
    filename = function() {
      paste0("Gender_Meta3_", name4, "_", name1, "_", Sys.Date(), ".pdf")
    },
    content = gt_download_pdf(gt_table_reactive = gt_gender_meta3_r, filename_prefix = "Gender_Meta3")
  )
  output$download_pdf_gender_meta4_GT <- shiny::downloadHandler(
    filename = function() {
      paste0("Gender_Meta4_", name4, "_", name3, "_", Sys.Date(), ".pdf")
    },
    content = gt_download_pdf(gt_table_reactive = gt_gender_meta4_r, filename_prefix = "Gender_Meta4")
  )

  # ==============================================================================
  # AGE GROUP ANALYSIS TABS (Meta 1-4)
  # ==============================================================================
  # --- META 1 ---
  filtered_data_age_meta1 <- reactive({prepare_age_data(upload_meta1()) })
  output$age_meta1 <- render_custom_dt(
    data_reactive = filtered_data_age_meta1, hidden_cols = age_hidden_targets, shared_id_suffix = "age"
  )
  # --- META 2 ---
  filtered_data_age_meta2 <- reactive({ prepare_age_data(upload_meta2()) })
  output$age_meta2 <- render_custom_dt(
    data_reactive = filtered_data_age_meta2, hidden_cols = age_hidden_targets, shared_id_suffix = "age"
  )
  # --- META 3 ---
  filtered_data_age_meta3 <- reactive({ prepare_age_data(upload_meta3()) })
  output$age_meta3 <- render_custom_dt(
    data_reactive = filtered_data_age_meta3, hidden_cols = age_hidden_targets, shared_id_suffix = "age"
  )
  # --- META 4 ---
  filtered_data_age_meta4 <- reactive({ prepare_age_data(upload_meta4()) })
  output$age_meta4 <- render_custom_dt(
    data_reactive = filtered_data_age_meta4, hidden_cols = age_hidden_targets, shared_id_suffix = "age"
  )

  # ==============================================================================
  # CSV DOWNLOAD HANDLERS AGE
  # ==============================================================================
  output$download_csv_age_meta1 <- create_csv_download(
    data_reactive = reactive({
      req(filtered_data_age_meta1())
      req(input$age_meta1_rows_all)
      filtered_data_age_meta1()[input$age_meta1_rows_all, ]
    }),
    filename_prefix = paste0("Age_", name2, "-", name1)
  )

  output$download_csv_age_meta2 <- create_csv_download(
    data_reactive = reactive({
      req(filtered_data_age_meta2())
      req(input$age_meta2_rows_all)
      filtered_data_age_meta2()[input$age_meta2_rows_all, ]
    }),
    filename_prefix = paste0("Age_", name3, "-", name1)
  )

  output$download_csv_age_meta3 <- create_csv_download(
    data_reactive = reactive({
      req(filtered_data_age_meta3())
      req(input$age_meta3_rows_all)
      filtered_data_age_meta3()[input$age_meta3_rows_all, ]
    }),
    filename_prefix = paste0("Age_", name4, "-", name1)
  )

  output$download_csv_age_meta4 <- create_csv_download(
    data_reactive = reactive({
      req(filtered_data_age_meta4())
      req(input$age_meta4_rows_all)
      filtered_data_age_meta4()[input$age_meta4_rows_all, ]
    }),
    filename_prefix = paste0("Age_", name4, "-", name3)
  )

  # ==============================================================================
  # AGE GROUP GT REACTIVE TABLES (Meta 1-4)
  # ==============================================================================

  # Age Meta 1 GT Table
  gt_age_meta1_r <- reactive({
    # Use the DT output ID (age_meta1) to get the rows_all input
    req(filtered_data_age_meta1(), input$age_meta1_rows_all)
    generate_print_gt(
      df_raw = filtered_data_age_meta1(),
      dt_rows = input$age_meta1_rows_all,
      # Use the custom shared JS inputs for visibility and order
      dt_vis_cols = input$visible_columns_age,
      dt_col_order = input$column_order_age,
      # Pass the default hidden columns for initial state consistency
      default_hidden_cols = age_hidden_targets,
      title = paste("Age Group Analysis:", name2, "/", name1),
      subtitle = NULL # Uses default row count
    )
  })

  # Age Meta 2 GT Table
  gt_age_meta2_r <- reactive({
    req(filtered_data_age_meta2(), input$age_meta2_rows_all)
    generate_print_gt(
      df_raw = filtered_data_age_meta2(),
      dt_rows = input$age_meta2_rows_all,
      dt_vis_cols = input$visible_columns_age,
      dt_col_order = input$column_order_age,
      default_hidden_cols = age_hidden_targets,
      title = paste("Age Group Analysis:", name3, "/", name1),
      subtitle = NULL
    )
  })

  # Age Meta 3 GT Table
  gt_age_meta3_r <- reactive({
    req(filtered_data_age_meta3(), input$age_meta3_rows_all)
    generate_print_gt(
      df_raw = filtered_data_age_meta3(),
      dt_rows = input$age_meta3_rows_all,
      dt_vis_cols = input$visible_columns_age,
      dt_col_order = input$column_order_age,
      default_hidden_cols = age_hidden_targets,
      title = paste("Age Group Analysis:", name4, "/", name1),
      subtitle = NULL
    )
  })

  # Age Meta 4 GT Table
  gt_age_meta4_r <- reactive({
    req(filtered_data_age_meta4(), input$age_meta4_rows_all)
    generate_print_gt(
      df_raw = filtered_data_age_meta4(),
      dt_rows = input$age_meta4_rows_all,
      dt_vis_cols = input$visible_columns_age,
      dt_col_order = input$column_order_age,
      default_hidden_cols = age_hidden_targets,
      title = paste("Age Group Analysis:", name4, "/", name3),
      subtitle = NULL
    )
  })

  ##  Age Group GT Table Display Outputs
  output$age_meta1_GT <- gt::render_gt({
    gt_age_meta1_r()
  })
  output$age_meta2_GT <- gt::render_gt({
    gt_age_meta2_r()
  })
  output$age_meta3_GT <- gt::render_gt({
    gt_age_meta3_r()
  })
  output$age_meta4_GT <- gt::render_gt({
    gt_age_meta4_r()
  })

  # Note: This requires the gt_download_pdf function to be defined.

  output$download_pdf_age_meta1_GT <- shiny::downloadHandler(
    filename = function() {
      paste0("Age_Meta1_", name2, "_", name1, "_", Sys.Date(), ".pdf")
    },
    content = gt_download_pdf(gt_table_reactive = gt_age_meta1_r, filename_prefix = "Age_Meta1")
  )
  output$download_pdf_age_meta2_GT <- shiny::downloadHandler(
    filename = function() {
      paste0("Age_Meta2_", name3, "_", name1, "_", Sys.Date(), ".pdf")
    },
    content = gt_download_pdf(gt_table_reactive = gt_age_meta2_r, filename_prefix = "Age_Meta2")
  )
  output$download_pdf_age_meta3_GT <- shiny::downloadHandler(
    filename = function() {
      paste0("Age_Meta3_", name4, "_", name1, "_", Sys.Date(), ".pdf")
    },
    content = gt_download_pdf(gt_table_reactive = gt_age_meta3_r, filename_prefix = "Age_Meta3")
  )
  output$download_pdf_age_meta4_GT <- shiny::downloadHandler(
    filename = function() {
      paste0("Age_Meta4_", name4, "_", name3, "_", Sys.Date(), ".pdf")
    },
    content = gt_download_pdf(gt_table_reactive = gt_age_meta4_r, filename_prefix = "Age_Meta4")
  )


} # Server end
