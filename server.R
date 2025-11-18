
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
    name1 = "EH30",
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
  inputData1_par1 <- shiny::reactive({ par1Data(inputData1()) })
  inputData1_par2 <- shiny::reactive({ par2Data(inputData1()) })

  avgData2 <- shiny::reactive({ avgData(upload2()) })
  inputData2 <- shiny::reactive({ cleanData(upload2()) })
  inputData2_top <- shiny::reactive({ topData(inputData2()) })
  inputData2_par1 <- shiny::reactive({ par1Data(inputData2()) })
  inputData2_par2 <- shiny::reactive({ par2Data(inputData2()) })

  avgData3 <- shiny::reactive({ avgData(upload3()) })
  inputData3 <- shiny::reactive({ cleanData(upload3()) })
  inputData3_top <- shiny::reactive({ topData(inputData3()) })
  inputData3_par1 <- shiny::reactive({ par1Data(inputData3()) })
  inputData3_par2 <- shiny::reactive({ par2Data(inputData3()) })

  avgData4 <- shiny::reactive({ avgData(upload4()) })
  inputData4 <- shiny::reactive({ cleanData(upload4()) })
  inputData4_top <- shiny::reactive({ topData(inputData4()) })
  inputData4_par1 <- shiny::reactive({ par1Data(inputData4()) })
  inputData4_par2 <- shiny::reactive({ par2Data(inputData4()) })

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

# observeEvent(upload_gender_parent0_meta1(), {
#   df <- upload_gender_parent0_meta1() ## todo  reactive
#   shiny::updateSliderInput(
#     session,
#     "fold_filter0",
#     min = 0,
#     max = round(max(df$fold_diff_reg, na.rm = TRUE),2),
#     value = c(0, round(max(df$fold_diff_reg, na.rm = TRUE),2)),
#     step = 0.1
#   )
#   shiny::updateSliderInput(
#     session,
#     "ci_filter0",
#     min = 0,
#     max = round(max(df$ci_width_nat, na.rm = TRUE),2),
#     value = c(0, round(max(df$ci_width_nat, na.rm = TRUE),2)),
#     step = 0.1
#   )
# })

  output$ci_filter_ui <- renderUI({
    req(upload_gender_parent2_meta1())  # Ensure the reactive data is available
    max_ci <- round(max(upload_gender_parent2_meta1()$ci_width_nat, na.rm = TRUE), 2)
    sliderInput("ci_filter", "CI width:",
                min = 0,
                max = max_ci,
                value = max_ci)
  })

  # output$p_filter <- renderUI({
  #   req(upload_gender_parent2_meta1())  # Ensure the reactive data is available
  #   max_p <- round(max(upload_gender_parent2_meta1()$p_value, na.rm = TRUE), 2)
  #   sliderInput("p_filter", "p-value:",
  #               min = 0,
  #               max = max_p,
  #               value = 0.05)
  # })

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

  #selectedYear <- shiny::reactive({ c(input$filterYear) })
 # selectedYearMultiple <- shiny::reactive({ c(input$filterYearMultiple) })

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


  #### PLOTS: Bars on TOP WITHOUT year ####
  # datasetAvg1 <- shiny::reactive({
  #   if (input$barsDataset1 == TRUE) { avgData1() }
  #   else { NULL }
  # })
  # datasetAvg2 <- shiny::reactive({
  #   if (input$barsDataset2 == TRUE) { avgData2() }
  #   else { NULL }
  # })
  # datasetAvg3 <- shiny::reactive({
  #   if (input$barsDataset3 == TRUE) { avgData3() }
  #   else { NULL }
  # })
  # datasetAvg4 <- shiny::reactive({
  #   if (input$barsDataset4 == TRUE) { avgData4() }
  #   else { NULL }
  # })

  # output$plot0 <- ggiraph::renderGirafe({
  #   shiny::validate(
  #     shiny::need(
  #       !is.null(datasetAvg1()),
  #       "Upload and select at least one dataset"
  #     )
  #   )
  #   diagGroupBars_avg0(datasetAvg1(), datasetAvg2(), datasetAvg3(), datasetAvg4(),
  #                      datasetNames$name1, datasetNames$name2, datasetNames$name3, datasetNames$name4,
  #                      selectedCodes(), plotTitle_plotDet0(), NULL) # plotTitle_plotDet0(), plotsubTitle()
  # })

  # selectedCodes1 <- shiny::reactive({ input$plot0_selected })
  # output$selectedCodes1 <- shiny::renderText(selectedCodes1())
  #
  # dataset1_par1 <- shiny::reactive({
  #   if (input$barsDataset1 == TRUE) { inputData1_par1() }
  #   else { NULL }
  # })
  # dataset2_par1 <- shiny::reactive({
  #   if (input$barsDataset2 == TRUE) { inputData2_par1() }
  #   else { NULL }
  # })
  # dataset3_par1 <- shiny::reactive({
  #   if (input$barsDataset3 == TRUE) { inputData3_par1() }
  #   else { NULL }
  # })
  # dataset4_par1 <- shiny::reactive({
  #   if (input$barsDataset4 == TRUE) { inputData4_par1() }
  #   else { NULL }
  # })

  # output$plot1 <- ggiraph::renderGirafe({
  #   shiny::validate(
  #     shiny::need(
  #       !is.null(input$plot0_selected),
  #       "Click on a diagnosis code group bar on the first chart."
  #     )
  #   )
  #   diagGroupBars_avg1(datasetAvg1(), datasetAvg2(), datasetAvg3(), datasetAvg4(),
  #                      datasetNames$name1, datasetNames$name2, datasetNames$name3, datasetNames$name4,
  #                      selectedCodes1(), plotTitle_plotDet1(), NULL) # plotTitle_plotDet0(), plotsubTitle()
  # })
  #
  # selectedCodes2 <- shiny::reactive({ input$plot1_selected })
  # output$selectedCodes2 <- shiny::renderText(selectedCodes2())

  # dataset1_par2 <- shiny::reactive({
  #   if (input$barsDataset1 == TRUE) { inputData1_par2() }
  #   else { NULL }
  # })
  # dataset2_par2 <- shiny::reactive({
  #   if (input$barsDataset2 == TRUE) { inputData2_par2() }
  #   else { NULL }
  # })
  # dataset3_par2 <- shiny::reactive({
  #   if (input$barsDataset3 == TRUE) { inputData3_par2() }
  #   else { NULL }
  # })
  # dataset4_par2 <- shiny::reactive({
  #   if (input$barsDataset4 == TRUE) { inputData4_par2() }
  #   else { NULL }
  # })

  # output$plot2 <- ggiraph::renderGirafe({
  #   shiny::validate(
  #     shiny::need(
  #       !is.null(input$plot1_selected),
  #       "Click on a diagnosis code group bar on the second chart."
  #     )
  #   )
  #   diagGroupBars_avg2(datasetAvg1(), datasetAvg2(), datasetAvg3(), datasetAvg4(),
  #                      datasetNames$name1, datasetNames$name2, datasetNames$name3, datasetNames$name4,
  #                      selectedCodes2(), plotTitle_plotDet2(), NULL) # plotTitle_plotDet0(), plotsubTitle()
  # })
  #
  # selectedCodes3 <- shiny::reactive({ input$plot2_selected })
  # plotTitle_plotDet3 <- shiny::reactive({
  #   diag_str <- paste(input$plot2_selected, collapse = ", ")
  #   sprintf("Prevalence of: %s", diag_str)
  # })

  # output$plot_diag_age <- ggiraph::renderGirafe({
  #   shiny::validate(
  #     shiny::need(
  #       !is.null(input$plot2_selected),
  #       "Click on a diagnosis code bar on the third chart."
  #     ),
  #     shiny::need(
  #       !is.null(datasetAvg1()),
  #       "Upload and select at least one dataset"
  #     )
  #   )
  #   plot_diag_age(dataset1_par2(), dataset2_par2(), dataset3_par2(), dataset4_par2(),
  #                 datasetNames$name1, datasetNames$name2, datasetNames$name3, datasetNames$name4,
  #                 selectedCodes3(), plotTitle_plotDet3(), NULL) # plotTitle_plotDet0()
  # })


  #### PLOTS: Heatmap META #####
  # ("Biobank vs Population 10%",
  #"Biobank 1 vs Population 10%",
  #"Biobank 2 vs Population 10%",
  #"Biobank 2 vs Biobank 1"),
  selectedDataset_meta <- shiny::reactive({
    name <- input$filterDatasets
    if (name == "EH30 vs EstBB") {
      upload_meta1()
    } else if (name == "EH30 vs EstBB1") {
      upload_meta2()
    } else if (name == "EH30 vs EstBB2") {
      upload_meta3()
    } else if (name == "EstBB1 vs EstBB2") {
      upload_meta4()
    }
  })

  selectedNames <- shiny::reactive({
    req(input$filterDatasets)  # Ensure selection is made

    switch(input$filterDatasets,
           "EH30 vs EstBB" = list(name2 = "EstBB", name1 = "EH30"),
           "EH30 vs EstBB1" = list(name2 = "EstBB1", name1 = "EH30"),
           "EH30 vs EstBB2" = list(name2 = "EstBB2", name1 = "EH30"),
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
                        selectedGen(), selectedCodes_sub(), "EstBB1", "EH30")
    } else if (sorting_method == "Alphabetical" && filter_method == "All") {
      heatmap_meta_alph(upload_meta2(), selectedAgegroups(),
                        selectedGen(), selectedCodes_sub(), "EstBB1", "EH30")
    } else if (sorting_method == "Average" && filter_method == "Significant") {
      heatmap_meta_avgDet(data_to_plot2, selectedAgegroups(),
                          selectedGen(), selectedCodes_sub(), "EstBB1", "EH30")
    } else if (sorting_method == "Average" && filter_method == "All") {
      heatmap_meta_avgDet(upload_meta2(), selectedAgegroups(),
                          selectedGen(), selectedCodes_sub(), "EstBB1", "EH30")
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
                        selectedGen(), selectedCodes_sub(), "EstBB2", "EH30")
    } else if (sorting_method == "Alphabetical" && filter_method == "All") {
      heatmap_meta_alph(upload_meta3(), selectedAgegroups(),
                        selectedGen(), selectedCodes_sub(), "EstBB2", "EH30")
    } else if (sorting_method == "Average" && filter_method == "Significant") {
      heatmap_meta_avgDet(data_to_plot3, selectedAgegroups(),
                          selectedGen(), selectedCodes_sub(), "EstBB2", "EH30")
    } else if (sorting_method == "Average" && filter_method == "All") {
      heatmap_meta_avgDet(upload_meta3(), selectedAgegroups(),
                          selectedGen(), selectedCodes_sub(), "EstBB2", "EH30")
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
  # output$heatmap_meta_alph4 <- ggiraph::renderGirafe({
  #   shiny::validate(
  #     shiny::need(
  #       (!is.null(selectedCodes_sub())),
  #       "Select a diagnosis group from Heatmap to zoom in."
  #     )
  #   )
  #   # Get active settings
  #   sorting_method <- active_plot()
  #   filter_method <- active_filter()
  #   # Apply filtering based on the selected button
  #   data_to_plot4 <- if (filter_method == "Significant") {
  #     upload_meta4() %>% filter(sig %in% c("sig"))
  #   } else {
  #     upload_meta4()
  #   }
  #   # Apply sorting and filtering combination logic
  #   if (sorting_method == "Alphabetical" && filter_method == "Significant") {
  #     heatmap_meta_alph(data_to_plot4, selectedAgegroups(),
  #                       selectedGen(), selectedCodes_sub(), "EstBB2", "EstBB1")
  #   } else if (sorting_method == "Alphabetical" && filter_method == "All") {
  #     heatmap_meta_alph(upload_meta4(), selectedAgegroups(),
  #                       selectedGen(), selectedCodes_sub(), "EstBB2", "EstBB1")
  #   } else if (sorting_method == "Average" && filter_method == "Significant") {
  #     heatmap_meta_avgDet(data_to_plot4, selectedAgegroups(),
  #                         selectedGen(), selectedCodes_sub(), "EstBB2", "EstBB1")
  #   } else if (sorting_method == "Average" && filter_method == "All") {
  #     heatmap_meta_avgDet(upload_meta4(), selectedAgegroups(),
  #                         selectedGen(), selectedCodes_sub(), "EstBB2", "EstBB1")
  #   }
  # })

  #### PLOTS: Prev Diff Heatmap #####
  # selectedDataset1 <- shiny::reactive({
  #   name <- input$filterDataset1
  #   if (name == datasetNames$name1) {
  #     dataset1_par2()
  #   } else if (name == datasetNames$name2) {
  #     dataset2_par2()
  #   } else if (name == datasetNames$name3) {
  #     dataset3_par2()
  #   } else if (name == datasetNames$name4) {
  #     dataset4_par2()
  #   }
  # })
  #
  # selectedDataset2 <- shiny::reactive({
  #   name <- input$filterDataset2
  #   if (name == datasetNames$name1) {
  #     dataset1_par2()
  #   } else if (name == datasetNames$name2) {
  #     dataset2_par2()
  #   } else if (name == datasetNames$name3) {
  #     dataset3_par2()
  #   } else if (name == datasetNames$name4) {
  #     dataset4_par2()
  #   }
  # })
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
      select(parent0_code, parent2_code, parent2_name_EN, gender_EN, p_vs_bb1 = prevalence_diff) %>%
      left_join(
        pop_bb2 %>%
          select(parent0_code, parent2_code, parent2_name_EN, gender_EN, p_vs_bb2 = prevalence_diff),
        by = c("parent0_code", "parent2_code", "parent2_name_EN", "gender_EN")
      ) %>%
      left_join(
        pop_bb %>%
          select(parent0_code, parent2_code, parent2_name_EN, gender_EN, p_vs_bb = prevalence_diff),
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

  output$p_disability_bb <- plotly::renderPlotly({
    plot_data <- filtered_data_dd()
    p <- create_p_disability_bb(plot_data)
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

  output$p_disability_bb1 <- plotly::renderPlotly({
    plot_data <- filtered_data_dd()
    p <- create_p_disability_bb1(plot_data)
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

  output$p_disability_bb2 <- plotly::renderPlotly({
    plot_data <- filtered_data_dd()
    p <- create_p_disability_bb2(plot_data)
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

#####
  output$download_gt_pdf <- downloadHandler(
    filename = function() {
      paste0("gt_table_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      # --- 1. DATA PREPARATION (MUST BE REPEATED) ---
      df_temp <- filtered_data_meta1()[input$upload_diff_genders_meta1_DT_rows_all, ]

      # Define shortening variables
      CHAR_START <- 2
      CHAR_END <- 30

      # Apply data filtering and shortening
      df_full <- df_temp %>%
        dplyr::mutate(
          parent2_name_SHORT = paste0(
            substr(parent2_name_EN, CHAR_START, CHAR_END),
            "..."
          )
        )

      # Apply column selection/reordering from input (using column NAMES/INDICES)
      if (!is.null(input$visible_columns)) {
        df_full <- df_full[, input$visible_columns]
      }
      if (!is.null(input$column_order)) {
        df_full <- df_full[, input$column_order]
      }

      # --- 2. STYLE DEFINITIONS (MUST BE REPEATED) ---
      blue_alpha <- "rgba(0, 95, 200, 0.2)"
      orange_alpha <- "rgba(255, 102, 0, 0.2)"
      gender_blue <- "rgba(153, 144, 255, 0.2)"
      gender_pink <- "rgba(255, 105, 180, 0.2)"

      # Calculate filter info for subtitle (must also be recreated)
      parent0_vals <- unique(df_full$parent0_code)
      fold_vals <- df_full$fold_diff_reg
      ci_vals <- df_full$ci_width_nat
      row_count <- nrow(df_full)

      fold_range <- if (length(fold_vals) > 0) {
        paste0("fold_diff_reg ∈ [", round(min(fold_vals), 2), ", ", round(max(fold_vals), 2), "]")
      } else {
        "fold_diff_reg: no values"
      }
      ci_range <- if (length(ci_vals) > 0) {
        paste0("ci_width_nat ∈ [", round(min(ci_vals), 2), ", ", round(max(ci_vals), 2), "]")
      } else {
        "ci_width_nat: no values"
      }
      parent0_info <- if (length(parent0_vals) > 0) {
        paste0("")
      } else {
        "parent0_code: no values"
      }
      filter_info <- paste(parent0_info, fold_range, ci_range, paste("Rows:", row_count), sep = " | ")

      # --- 3. BUILD FULLY STYLED GT TABLE ---
      gt_table <- gt(df_full) %>%
        tab_header(
          title = paste("Prevalence ratios for:", name2, "vs", name1),
          subtitle = filter_info
        ) %>%
        fmt_number(where(is.numeric), decimals = 2) %>%
        tab_options(table.font.size = "small") %>%

        # Re-add TEXT TRANSFORM and COLUMN LABEL
        text_transform(
          locations = cells_body(columns = c(parent2_name_SHORT)),
          fn = function(x) {
            mapply(function(en, orig) {
              html(paste0("<span title='", orig, "'>", en, "</span>"))
            }, x, df_full$parent2_name_EN)
          }
        ) %>%
        cols_label(
          parent2_name_SHORT = "parent2_name_EN"
        ) %>%

        # Re-add ALL STYLING
        tab_style(
          style = cell_fill(color = blue_alpha),
          locations = cells_body(columns = vars(log2_diff), rows = df_full$prevalence_diff < 0)
        ) %>%
        tab_style(
          style = cell_fill(color = orange_alpha),
          locations = cells_body(columns = vars(log2_diff), rows = df_full$prevalence_diff >= 0)
        ) %>%
        tab_style(
          style = cell_fill(color = gender_blue),
          locations = cells_body(columns = vars(gender_EN), rows = df_full$gender_EN == "M")
        ) %>%
        tab_style(
          style = cell_fill(color = gender_pink),
          locations = cells_body(columns = vars(gender_EN), rows = df_full$gender_EN == "F")
        ) %>%
        tab_style(
          style = cell_fill(color = orange_alpha),
          locations = cells_body(columns = vars(fold_diff), rows = df_full$fold_diff_nat > 1)
        ) %>%
        tab_style(
          style = cell_fill(color = blue_alpha),
          locations = cells_body(columns = vars(fold_diff), rows = df_full$fold_diff_nat < 1)
        ) %>%
        tab_style(
          style = cell_fill(color = gender_blue),
          locations = cells_body(columns = vars(gender_EN), rows = df_full$gender_EN == "M")
        ) %>%
        tab_style(
          style = cell_fill(color = gender_pink),
          locations = cells_body(columns = vars(gender_EN), rows = df_full$gender_EN == "F")
        )

      # Final save to file path provided by downloadHandler
      gtsave(gt_table, filename = file)
    }
  )
  output$download_gt_html <- downloadHandler( #####
                                              filename = function() {
                                                paste0("gt_table_", Sys.Date(), ".html")
                                              },
                                              content = function(file) {
                                                df <- filtered_data_meta1()[input$upload_diff_genders_meta1_DT_rows_all, ]
                                                if (!is.null(input$visible_columns)) {
                                                  df <- df[, input$visible_columns]
                                                }
                                                if (!is.null(input$column_order)) {
                                                  df <- df[, input$column_order]
                                                }

                                                gt_table <- gt(df) %>%
                                                  tab_header(
                                                    title = paste("Prevalence ratios for:", name2, "vs", name1),
                                                    subtitle = paste("Comparison in log2 and natural fold")
                                                  ) %>%
                                                  fmt_number(where(is.numeric), decimals = 2) %>%
                                                  tab_options(table.font.size = "small")

                                                gtsave(gt_table, file)
                                              }
  )

##### META GENDERS #####

  # cols_genders_meta <- c("parent2_code", "parent2_name", "parent2_name_EN", "parent1_code", "parent1_name", "parent0_code",
  #                        "parent0_name", "parent0_name_EN", "short_name", "gender", "gender_EN", "year", "prevalence_diff",
  #                        "ci_low", "ci_high", "se", "z", "p_value", "fold_diff_nat",
  #                        "fold_ci_low_nat", "fold_ci_high_nat", "ci_width_nat", "fold_diff_reg", "sig", "meta_model_type")


  ### === META1 Genders ===
  filtered_data_meta1 <- reactive({
    req(upload_gender_parent2_meta1())
    df <- upload_gender_parent2_meta1()[, !(names(upload_gender_parent2_meta1()) %in% c("sig", "year"))]
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

    df$log2_diff <- paste0(df$prevalence_diff, " (", df$ci_low, "…", df$ci_high, ")")
    df$fold_diff <- paste0(df$fold_diff_nat, " (", df$fold_ci_low_nat, "…", df$fold_ci_high_nat, ")")

    df$parent2_name_EN <- ifelse(
      nchar(df$parent2_name_EN) > 100,
      paste0(substr(df$parent2_name_EN, 1, 97), "..."),
      df$parent2_name_EN
    )

    df <- df %>%
      select(parent2_code, parent2_name_EN, gender_EN, log2_diff, fold_diff, fold_diff_nat, everything())

    df
  })

  output$upload_diff_genders_meta1_DT <- DT::renderDataTable({
    DT::datatable(
      filtered_data_meta1(),
      filter = "top",
      extensions = c('Buttons', 'ColReorder'),
      options = list(
        dom = 'Bfrtip',
        buttons = list('colvis'),
        colReorder = TRUE,
        scrollX = TRUE,
        columnDefs = list(
          list(visible = FALSE, targets = c("parent2_name", "parent1_code", "parent1_name",
                                            "parent0_code", "parent0_name", "parent0_name_EN",
                                            "gender", "prevalence_diff", "ci_low", "ci_high",
                                            "se", "z", "fold_ci_low_nat", "fold_ci_high_nat",
                                            "ci_width_nat", "meta_model_type")) #, "cause", "DALY"

        )
      ),
      callback = JS("
      table.on('column-reorder', function(e, settings, details) {
        var order = table.colReorder.order();
        Shiny.setInputValue('column_order', order);
      });

      table.on('buttons-action', function(e, buttonApi, dataTable, node, config) {
        var visible = [];
        dataTable.columns().every(function(index) {
          if (this.visible()) visible.push(index);
        });
        Shiny.setInputValue('visible_columns', visible);
      });
    ")
    )
  })

  output$upload_diff_genders_meta1_GT <- render_gt({
    req(filtered_data_meta1())
    req(input$upload_diff_genders_meta1_DT_rows_all)

    df_full <- filtered_data_meta1()[input$upload_diff_genders_meta1_DT_rows_all, ]

    parent0_vals <- unique(df_full$parent0_code)
    fold_vals <- df_full$fold_diff_reg
    ci_vals <- df_full$ci_width_nat
    row_count <- nrow(df_full)

    fold_range <- if (length(fold_vals) > 0) {
      paste0("fold_diff_reg ∈ [", round(min(fold_vals), 2), ", ", round(max(fold_vals), 2), "]")
    } else {
      "fold_diff_reg: no values"
    }

    ci_range <- if (length(ci_vals) > 0) {
      paste0("ci_width_nat ∈ [", round(min(ci_vals), 2), ", ", round(max(ci_vals), 2), "]")
    } else {
      "ci_width_nat: no values"
    }

    parent0_info <- if (length(parent0_vals) > 0) {
      paste0("")
    } else {
      "parent0_code: no values"
    }

    filter_info <- paste(parent0_info, fold_range, ci_range, paste("Rows:", row_count), sep = " | ")

    blue_alpha <- "rgba(0, 95, 200, 0.2)"
    orange_alpha <- "rgba(255, 102, 0, 0.2)"
    gender_blue <- "rgba(153, 144, 255, 0.2)"
    gender_pink <- "rgba(255, 105, 180, 0.2)"

    gt_table <- gt(df_full) %>%
      tab_header(
        title = paste("Prevalence ratios for:", name2, "vs", name1),
        subtitle = filter_info
      ) %>%
      fmt_number(where(is.numeric), decimals = 2) %>%
      tab_options(table.font.size = "small") %>%
      tab_style(
        style = cell_fill(color = blue_alpha),
        locations = cells_body(columns = vars(log2_diff), rows = df_full$prevalence_diff < 0)
      ) %>%
      tab_style(
        style = cell_fill(color = orange_alpha),
        locations = cells_body(columns = vars(log2_diff), rows = df_full$prevalence_diff >= 0)
      ) %>%
      tab_style(
        style = cell_fill(color = gender_blue),
        locations = cells_body(columns = vars(gender_EN), rows = df_full$gender_EN == "M")
      ) %>%
      tab_style(
        style = cell_fill(color = gender_pink),
        locations = cells_body(columns = vars(gender_EN), rows = df_full$gender_EN == "F")
      ) %>%
      tab_style(
        style = cell_fill(color = orange_alpha),
        locations = cells_body(columns = vars(fold_diff), rows = df_full$fold_diff_nat > 1)
      ) %>%
      tab_style(
        style = cell_fill(color = blue_alpha),
        locations = cells_body(columns = vars(fold_diff), rows = df_full$fold_diff_nat < 1)
      ) %>%
      tab_style(
        style = cell_fill(color = gender_blue),
        locations = cells_body(columns = vars(gender_EN), rows = df_full$gender_EN == "M")
      ) %>%
      tab_style(
        style = cell_fill(color = gender_pink),
        locations = cells_body(columns = vars(gender_EN), rows = df_full$gender_EN == "F")
      )

    if (!is.null(input$visible_columns)) {
      gt_table <- gt_table %>%
        cols_hide(columns = setdiff(names(df_full), names(df_full)[input$visible_columns]))
    }

    if (!is.null(input$column_order)) {
      gt_table <- gt_table %>%
        cols_move_to_start(columns = names(df_full)[input$column_order])
    }
    gt_table
  })


  ### === META2 Genders ===
  filtered_data_meta2 <- reactive({
    req(upload_gender_parent2_meta2())
    df <- upload_gender_parent2_meta2()[, !(names(upload_gender_parent2_meta2()) %in% c("sig", "year"))]
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

    df$log2_diff <- paste0(df$prevalence_diff, " (", df$ci_low, "…", df$ci_high, ")")
    df$fold_diff <- paste0(df$fold_diff_nat, " (", df$fold_ci_low_nat, "…", df$fold_ci_high_nat, ")")

    df$parent2_name_EN <- ifelse(
      nchar(df$parent2_name_EN) > 100,
      paste0(substr(df$parent2_name_EN, 1, 97), "..."),
      df$parent2_name_EN
    )

    df <- df %>%
      select(parent2_code, parent2_name_EN, gender_EN, log2_diff, fold_diff, fold_diff_nat, everything())

    df
  })

  output$upload_diff_genders_meta2_DT <- DT::renderDataTable({
    DT::datatable(
      filtered_data_meta2(),
      filter = "top",
      extensions = c('Buttons', 'ColReorder'),
      options = list(
        dom = 'Bfrtip',
        buttons = list('colvis'),
        colReorder = TRUE,
        scrollX = TRUE,
        columnDefs = list(
          list(visible = FALSE, targets = c("parent2_name", "parent1_code", "parent1_name",
                                            "parent0_code", "parent0_name", "parent0_name_EN",
                                            "gender", "prevalence_diff", "ci_low", "ci_high",
                                            "se", "z", "fold_ci_low_nat", "fold_ci_high_nat",
                                            "ci_width_nat", "meta_model_type")) #, "cause", "DALY"

        )
      ),
      callback = JS("
      table.on('column-reorder', function(e, settings, details) {
        var order = table.colReorder.order();
        Shiny.setInputValue('column_order', order);
      });

      table.on('buttons-action', function(e, buttonApi, dataTable, node, config) {
        var visible = [];
        dataTable.columns().every(function(index) {
          if (this.visible()) visible.push(index);
        });
        Shiny.setInputValue('visible_columns', visible);
      });
    ")
    )
  })

  output$upload_diff_genders_meta2_GT <- render_gt({
    req(filtered_data_meta2())
    req(input$upload_diff_genders_meta2_DT_rows_all)

    df_full <- filtered_data_meta2()[input$upload_diff_genders_meta2_DT_rows_all, ]

    parent0_vals <- unique(df_full$parent0_code)
    fold_vals <- df_full$fold_diff_reg
    ci_vals <- df_full$ci_width_nat
    row_count <- nrow(df_full)

    fold_range <- if (length(fold_vals) > 0) {
      paste0("fold_diff_reg ∈ [", round(min(fold_vals), 2), ", ", round(max(fold_vals), 2), "]")
    } else {
      "fold_diff_reg: no values"
    }

    ci_range <- if (length(ci_vals) > 0) {
      paste0("ci_width_nat ∈ [", round(min(ci_vals), 2), ", ", round(max(ci_vals), 2), "]")
    } else {
      "ci_width_nat: no values"
    }

    parent0_info <- if (length(parent0_vals) > 0) {
      paste0("")
    } else {
      "parent0_code: no values"
    }

    filter_info <- paste(parent0_info, fold_range, ci_range, paste("Rows:", row_count), sep = " | ")

    blue_alpha <- "rgba(0, 95, 200, 0.2)"
    orange_alpha <- "rgba(255, 102, 0, 0.2)"
    gender_blue <- "rgba(153, 144, 255, 0.2)"
    gender_pink <- "rgba(255, 105, 180, 0.2)"

    gt_table <- gt(df_full) %>%
      tab_header(
        title = paste("Prevalence ratios for:", name3, "vs", name1),
        subtitle = filter_info
      ) %>%
      fmt_number(where(is.numeric), decimals = 2) %>%
      tab_options(table.font.size = "small") %>%
      tab_style(
        style = cell_fill(color = blue_alpha),
        locations = cells_body(columns = vars(log2_diff), rows = df_full$prevalence_diff < 0)
      ) %>%
      tab_style(
        style = cell_fill(color = orange_alpha),
        locations = cells_body(columns = vars(log2_diff), rows = df_full$prevalence_diff >= 0)
      ) %>%
      tab_style(
        style = cell_fill(color = gender_blue),
        locations = cells_body(columns = vars(gender_EN), rows = df_full$gender_EN == "M")
      ) %>%
      tab_style(
        style = cell_fill(color = gender_pink),
        locations = cells_body(columns = vars(gender_EN), rows = df_full$gender_EN == "F")
      ) %>%
      tab_style(
        style = cell_fill(color = orange_alpha),
        locations = cells_body(columns = vars(fold_diff), rows = df_full$fold_diff_nat > 1)
      ) %>%
      tab_style(
        style = cell_fill(color = blue_alpha),
        locations = cells_body(columns = vars(fold_diff), rows = df_full$fold_diff_nat < 1)
      ) %>%
      tab_style(
        style = cell_fill(color = gender_blue),
        locations = cells_body(columns = vars(gender_EN), rows = df_full$gender_EN == "M")
      ) %>%
      tab_style(
        style = cell_fill(color = gender_pink),
        locations = cells_body(columns = vars(gender_EN), rows = df_full$gender_EN == "F")
      )

    if (!is.null(input$visible_columns)) {
      gt_table <- gt_table %>%
        cols_hide(columns = setdiff(names(df_full), names(df_full)[input$visible_columns]))
    }

    if (!is.null(input$column_order)) {
      gt_table <- gt_table %>%
        cols_move_to_start(columns = names(df_full)[input$column_order])
    }

    gt_table
  })

  ### === META3 Genders ===
  filtered_data_meta3 <- reactive({
    req(upload_gender_parent2_meta3())
    df <- upload_gender_parent2_meta3()[, !(names(upload_gender_parent2_meta3()) %in% c("sig", "year"))]
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

    df$log2_diff <- paste0(df$prevalence_diff, " (", df$ci_low, "…", df$ci_high, ")")
    df$fold_diff <- paste0(df$fold_diff_nat, " (", df$fold_ci_low_nat, "…", df$fold_ci_high_nat, ")")

    df$parent2_name_EN <- ifelse(
      nchar(df$parent2_name_EN) > 100,
      paste0(substr(df$parent2_name_EN, 1, 97), "..."),
      df$parent2_name_EN
    )

    df <- df %>%
      select(parent2_code, parent2_name_EN, gender_EN, log2_diff, fold_diff, fold_diff_nat, everything())

    df
  })

  output$upload_diff_genders_meta3_DT <- DT::renderDataTable({
    DT::datatable(
      filtered_data_meta3(),
      filter = "top",
      extensions = c('Buttons', 'ColReorder'),
      options = list(
        dom = 'Bfrtip',
        buttons = list('colvis'),
        colReorder = TRUE,
        scrollX = TRUE,
        columnDefs = list(
          list(visible = FALSE, targets = c("parent2_name", "parent1_code", "parent1_name",
                                            "parent0_code", "parent0_name", "parent0_name_EN",
                                            "gender", "prevalence_diff", "ci_low", "ci_high",
                                            "se", "z", "fold_ci_low_nat", "fold_ci_high_nat",
                                            "ci_width_nat", "meta_model_type")) # , "cause", "DALY"

        )
      ),
      callback = JS("
      table.on('column-reorder', function(e, settings, details) {
        var order = table.colReorder.order();
        Shiny.setInputValue('column_order', order);
      });

      table.on('buttons-action', function(e, buttonApi, dataTable, node, config) {
        var visible = [];
        dataTable.columns().every(function(index) {
          if (this.visible()) visible.push(index);
        });
        Shiny.setInputValue('visible_columns', visible);
      });
    ")
    )
  })

  output$upload_diff_genders_meta3_GT <- render_gt({
    req(filtered_data_meta3())
    req(input$upload_diff_genders_meta3_DT_rows_all)

    df_full <- filtered_data_meta3()[input$upload_diff_genders_meta3_DT_rows_all, ]

    parent0_vals <- unique(df_full$parent0_code)
    fold_vals <- df_full$fold_diff_reg
    ci_vals <- df_full$ci_width_nat
    row_count <- nrow(df_full)

    fold_range <- if (length(fold_vals) > 0) {
      paste0("fold_diff_reg ∈ [", round(min(fold_vals), 2), ", ", round(max(fold_vals), 2), "]")
    } else {
      "fold_diff_reg: no values"
    }

    ci_range <- if (length(ci_vals) > 0) {
      paste0("ci_width_nat ∈ [", round(min(ci_vals), 2), ", ", round(max(ci_vals), 2), "]")
    } else {
      "ci_width_nat: no values"
    }

    parent0_info <- if (length(parent0_vals) > 0) {
      paste0("")
    } else {
      "parent0_code: no values"
    }

    filter_info <- paste(parent0_info, fold_range, ci_range, paste("Rows:", row_count), sep = " | ")

    blue_alpha <- "rgba(0, 95, 200, 0.2)"
    orange_alpha <- "rgba(255, 102, 0, 0.2)"
    gender_blue <- "rgba(153, 144, 255, 0.2)"
    gender_pink <- "rgba(255, 105, 180, 0.2)"

    gt_table <- gt(df_full) %>%
      tab_header(
        title = paste("Prevalence ratios for:", name4, "vs", name1),
        subtitle = filter_info
      ) %>%
      fmt_number(where(is.numeric), decimals = 2) %>%
      tab_options(table.font.size = "small") %>%
      tab_style(
        style = cell_fill(color = blue_alpha),
        locations = cells_body(columns = vars(log2_diff), rows = df_full$prevalence_diff < 0)
      ) %>%
      tab_style(
        style = cell_fill(color = orange_alpha),
        locations = cells_body(columns = vars(log2_diff), rows = df_full$prevalence_diff >= 0)
      ) %>%
      tab_style(
        style = cell_fill(color = gender_blue),
        locations = cells_body(columns = vars(gender_EN), rows = df_full$gender_EN == "M")
      ) %>%
      tab_style(
        style = cell_fill(color = gender_pink),
        locations = cells_body(columns = vars(gender_EN), rows = df_full$gender_EN == "F")
      ) %>%
      tab_style(
        style = cell_fill(color = orange_alpha),
        locations = cells_body(columns = vars(fold_diff), rows = df_full$fold_diff_nat > 1)
      ) %>%
      tab_style(
        style = cell_fill(color = blue_alpha),
        locations = cells_body(columns = vars(fold_diff), rows = df_full$fold_diff_nat < 1)
      ) %>%
      tab_style(
        style = cell_fill(color = gender_blue),
        locations = cells_body(columns = vars(gender_EN), rows = df_full$gender_EN == "M")
      ) %>%
      tab_style(
        style = cell_fill(color = gender_pink),
        locations = cells_body(columns = vars(gender_EN), rows = df_full$gender_EN == "F")
      )

    if (!is.null(input$visible_columns)) {
      gt_table <- gt_table %>%
        cols_hide(columns = setdiff(names(df_full), names(df_full)[input$visible_columns]))
    }

    if (!is.null(input$column_order)) {
      gt_table <- gt_table %>%
        cols_move_to_start(columns = names(df_full)[input$column_order])
    }

    gt_table
  })

  ### === META4 Genders ===
  filtered_data_meta4 <- reactive({
    req(upload_gender_parent2_meta4())
    df <- upload_gender_parent2_meta4()[, !(names(upload_gender_parent2_meta4()) %in% c("sig", "year"))]
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

    df$log2_diff <- paste0(df$prevalence_diff, " (", df$ci_low, "…", df$ci_high, ")")
    df$fold_diff <- paste0(df$fold_diff_nat, " (", df$fold_ci_low_nat, "…", df$fold_ci_high_nat, ")")

    df$parent2_name_EN <- ifelse(
      nchar(df$parent2_name_EN) > 100,
      paste0(substr(df$parent2_name_EN, 1, 97), "..."),
      df$parent2_name_EN
    )

    df <- df %>%
      select(parent2_code, parent2_name_EN, gender_EN, log2_diff, fold_diff, fold_diff_nat, everything())

    df
  })

  output$upload_diff_genders_meta4_DT <- DT::renderDataTable({
    DT::datatable(
      filtered_data_meta4(),
      filter = "top",
      extensions = c('Buttons', 'ColReorder'),
      options = list(
        dom = 'Bfrtip',
        buttons = list('colvis'),
        colReorder = TRUE,
        scrollX = TRUE,
        columnDefs = list(
          list(visible = FALSE, targets = c("parent2_name", "parent1_code", "parent1_name",
                                            "parent0_code", "parent0_name", "parent0_name_EN",
                                            "gender", "prevalence_diff", "ci_low", "ci_high",
                                            "se", "z", "fold_ci_low_nat", "fold_ci_high_nat",
                                            "ci_width_nat", "meta_model_type")) #, "cause", "DALY"

        )
      ),
      callback = JS("
      table.on('column-reorder', function(e, settings, details) {
        var order = table.colReorder.order();
        Shiny.setInputValue('column_order', order);
      });

      table.on('buttons-action', function(e, buttonApi, dataTable, node, config) {
        var visible = [];
        dataTable.columns().every(function(index) {
          if (this.visible()) visible.push(index);
        });
        Shiny.setInputValue('visible_columns', visible);
      });
    ")
    )
  })

  output$upload_diff_genders_meta4_GT <- render_gt({
    req(filtered_data_meta4())
    req(input$upload_diff_genders_meta4_DT_rows_all)

    df_full <- filtered_data_meta4()[input$upload_diff_genders_meta4_DT_rows_all, ]

    parent0_vals <- unique(df_full$parent0_code)
    fold_vals <- df_full$fold_diff_reg
    ci_vals <- df_full$ci_width_nat
    row_count <- nrow(df_full)

    fold_range <- if (length(fold_vals) > 0) {
      paste0("fold_diff_reg ∈ [", round(min(fold_vals), 2), ", ", round(max(fold_vals), 2), "]")
    } else {
      "fold_diff_reg: no values"
    }

    ci_range <- if (length(ci_vals) > 0) {
      paste0("ci_width_nat ∈ [", round(min(ci_vals), 2), ", ", round(max(ci_vals), 2), "]")
    } else {
      "ci_width_nat: no values"
    }

    parent0_info <- if (length(parent0_vals) > 0) {
      paste0("")
    } else {
      "parent0_code: no values"
    }

    filter_info <- paste(parent0_info, fold_range, ci_range, paste("Rows:", row_count), sep = " | ")

    blue_alpha <- "rgba(0, 95, 200, 0.2)"
    orange_alpha <- "rgba(255, 102, 0, 0.2)"
    gender_blue <- "rgba(153, 144, 255, 0.2)"
    gender_pink <- "rgba(255, 105, 180, 0.2)"

    gt_table <- gt(df_full) %>%
      tab_header(
        title = paste("Prevalence ratios for:", name4, "vs", name3),
        subtitle = filter_info
      ) %>%
      fmt_number(where(is.numeric), decimals = 2) %>%
      tab_options(table.font.size = "small") %>%
      tab_style(
        style = cell_fill(color = blue_alpha),
        locations = cells_body(columns = vars(log2_diff), rows = df_full$prevalence_diff < 0)
      ) %>%
      tab_style(
        style = cell_fill(color = orange_alpha),
        locations = cells_body(columns = vars(log2_diff), rows = df_full$prevalence_diff >= 0)
      ) %>%
      tab_style(
        style = cell_fill(color = gender_blue),
        locations = cells_body(columns = vars(gender_EN), rows = df_full$gender_EN == "M")
      ) %>%
      tab_style(
        style = cell_fill(color = gender_pink),
        locations = cells_body(columns = vars(gender_EN), rows = df_full$gender_EN == "F")
      ) %>%
      tab_style(
        style = cell_fill(color = orange_alpha),
        locations = cells_body(columns = vars(fold_diff), rows = df_full$fold_diff_nat > 1)
      ) %>%
      tab_style(
        style = cell_fill(color = blue_alpha),
        locations = cells_body(columns = vars(fold_diff), rows = df_full$fold_diff_nat < 1)
      ) %>%
      tab_style(
        style = cell_fill(color = gender_blue),
        locations = cells_body(columns = vars(gender_EN), rows = df_full$gender_EN == "M")
      ) %>%
      tab_style(
        style = cell_fill(color = gender_pink),
        locations = cells_body(columns = vars(gender_EN), rows = df_full$gender_EN == "F")
      )

    if (!is.null(input$visible_columns)) {
      gt_table <- gt_table %>%
        cols_hide(columns = setdiff(names(df_full), names(df_full)[input$visible_columns]))
    }

    if (!is.null(input$column_order)) {
      gt_table <- gt_table %>%
        cols_move_to_start(columns = names(df_full)[input$column_order])
    }
    gt_table
  })



  # output$upload_diff_genders_meta4_GT <- render_gt({ #####
  #   req(filtered_data_meta4())
  #   req(input$upload_diff_genders_meta4_DT_rows_all)
  #
  #   df_full <- filtered_data_meta4()[input$upload_diff_genders_meta4_DT_rows_all, ]
  #
  #   parent0_vals <- unique(df_full$parent0_code)
  #   fold_vals <- df_full$fold_diff_reg
  #   ci_vals <- df_full$ci_width_nat
  #   row_count <- nrow(df_full)
  #
  #   fold_range <- if (length(fold_vals) > 0) {
  #     paste0("fold_diff_reg ∈ [", round(min(fold_vals), 2), ", ", round(max(fold_vals), 2), "]")
  #   } else {
  #     "fold_diff_reg: no values"
  #   }
  #
  #   ci_range <- if (length(ci_vals) > 0) {
  #     paste0("ci_width_nat ∈ [", round(min(ci_vals), 2), ", ", round(max(ci_vals), 2), "]")
  #   } else {
  #     "ci_width_nat: no values"
  #   }
  #
  #   parent0_info <- if (length(parent0_vals) > 0) {
  #     paste0("parent0_code ∈ [", paste(parent0_vals, collapse = ", "), "]")
  #   } else {
  #     "parent0_code: no values"
  #   }
  #
  #   filter_info <- paste(parent0_info, fold_range, ci_range, paste("Rows:", row_count), sep = " | ")
  #
  #   blue_alpha <- "rgba(0, 95, 200, 0.2)"
  #   orange_alpha <- "rgba(255, 102, 0, 0.2)"
  #   gender_blue <- "rgba(153, 144, 255, 0.2)"
  #   gender_pink <- "rgba(255, 105, 180, 0.2)"
  #
  #   gt_table <- gt(df_full) %>%
  #     tab_header(
  #       title = paste("Prevalence ratios for:", name4, "vs", name3),
  #       subtitle = filter_info
  #     ) %>%
  #     fmt_number(where(is.numeric), decimals = 2) %>%
  #     tab_options(table.font.size = "small") %>%
  #          tab_style(
  #       style = cell_fill(color = blue_alpha),
  #       locations = cells_body(columns = vars(log2_diff), rows = df_full$prevalence_diff < 0)
  #     ) %>%
  #     tab_style(
  #       style = cell_fill(color = orange_alpha),
  #       locations = cells_body(columns = vars(log2_diff), rows = df_full$prevalence_diff >= 0)
  #     ) %>%
  #     tab_style(
  #       style = cell_fill(color = gender_blue),
  #       locations = cells_body(columns = vars(gender_EN), rows = df_full$gender_EN == "M")
  #     ) %>%
  #     tab_style(
  #       style = cell_fill(color = gender_pink),
  #       locations = cells_body(columns = vars(gender_EN), rows = df_full$gender_EN == "N")
  #     ) %>%
  #     tab_style(
  #       style = cell_fill(color = orange_alpha),
  #       locations = cells_body(columns = vars(fold_diff), rows = df_full$fold_diff_nat > 1)
  #     ) %>%
  #     tab_style(
  #       style = cell_fill(color = blue_alpha),
  #       locations = cells_body(columns = vars(fold_diff), rows = df_full$fold_diff_nat < 1)
  #     ) %>%
  #     tab_style(
  #       style = cell_fill(color = gender_blue),
  #       locations = cells_body(columns = vars(gender_EN), rows = df_full$gender_EN == "M")
  #     ) %>%
  #     tab_style(
  #       style = cell_fill(color = gender_pink),
  #       locations = cells_body(columns = vars(gender_EN), rows = df_full$gender_EN == "F")
  #     )
  #
  #   if (!is.null(input$visible_columns)) {
  #     gt_table <- gt_table %>%
  #       cols_hide(columns = setdiff(names(df_full), names(df_full)[input$visible_columns]))
  #   }
  #
  #   if (!is.null(input$column_order)) {
  #     gt_table <- gt_table %>%
  #       cols_move_to_start(columns = names(df_full)[input$column_order])
  #   }
  #
  #   gt_table
  # })


  output$download_gt_pdf_meta4 <- downloadHandler( #####
    filename = function() {
      paste0("gt_table_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      df <- filtered_data_meta4()[input$upload_diff_genders_meta4_DT_rows_all, ]
      if (!is.null(input$visible_columns)) {
        df <- df[, input$visible_columns]
      }
      if (!is.null(input$column_order)) {
        df <- df[, input$column_order]
      }

      gt_table <- gt(df) %>%
        tab_header(
          title = paste("Prevalence ratios for:", name4, "vs", name3),
          subtitle = paste("Comparison in log2 and natural fold")
        ) %>%
        fmt_number(where(is.numeric), decimals = 2) %>%
        tab_options(table.font.size = "small")

      gtsave(gt_table, file)
    }
  )

  output$download_gt_html_meta4 <- downloadHandler( #####
    filename = function() {
      paste0("gt_table_", Sys.Date(), ".html")
    },
    content = function(file) {
      df <- filtered_data_meta4()[input$upload_diff_genders_meta4_DT_rows_all, ]
      if (!is.null(input$visible_columns)) {
        df <- df[, input$visible_columns]
      }
      if (!is.null(input$column_order)) {
        df <- df[, input$column_order]
      }

      gt_table <- gt(df) %>%
        tab_header(
          title = paste("Prevalence ratios for:", name4, "vs", name3),
          subtitle = paste("Comparison in log2 and natural fold")
        ) %>%
        fmt_number(where(is.numeric), decimals = 2) %>%
        tab_options(table.font.size = "small")

      gtsave(gt_table, file)
    }
  )





  ###### dataTable outputs: Gender, Both Analysis  ######

  ###### dataTable outputs: Age Group Analysis 4 tables ######

  cols_to_round <- c(
    "prevalence_diff", "ci_low", "ci_high",
    "fold_diff_nat", "fold_ci_low_nat", "fold_ci_high_nat",
    "ci_width_nat", "fold_diff_reg", "p_value", "se", "z", "prevalence_data1", "prevalence_data2"
  )

  # Function to apply the cleaning and rounding logic
  apply_filters_and_rounding <- function(df_reactive) {
    df <- df_reactive() # De-reactive the data
    df <- df[, !(names(df) %in% c("sig", "year"))]
    for (col in cols_to_round) {
      if (col %in% names(df)) {
        df[[col]] <- round(df[[col]], 2)
      }
    }
    return(df)
  }
  # --- Filtered Reactive Dataframes (The new frames) --- #####
  filtered_data_age_meta1 <- reactive({
    req(upload_meta1())
    apply_filters_and_rounding(upload_meta1)
  })

  filtered_data_age_meta2 <- reactive({
    req(upload_meta2())
    apply_filters_and_rounding(upload_meta2)
  })

  filtered_data_age_meta3 <- reactive({
    req(upload_meta3())
    apply_filters_and_rounding(upload_meta3)
  })

  filtered_data_age_meta4 <- reactive({
    req(upload_meta4())
    apply_filters_and_rounding(upload_meta4)
  })

  # output$age_meta1 <- DT::renderDataTable({
  #   df <- filtered_data_age_meta1()
  #   req(df)
  #   DT::datatable(df, options = list(pageLength = 10))
  # })
  #
  # output$age_meta2 <- DT::renderDataTable({
  #   df <- filtered_data_age_meta2()
  #   req(df)
  #   DT::datatable(df, options = list(pageLength = 10))
  # })
  #
  # output$age_meta3 <- DT::renderDataTable({
  #   df <- filtered_data_age_meta3()
  #   req(df)
  #   DT::datatable(df, options = list(pageLength = 10))
  # })
  #
  # output$age_meta4 <- DT::renderDataTable({
  #   df <- filtered_data_age_meta4()
  #   req(df)
  #   DT::datatable(df, options = list(pageLength = 10))
  # })

  # --- DT Output for (name2) / (name1) ---
  output$age_meta1 <- DT::renderDataTable({
    df <- filtered_data_age_meta1()
    req(df)

    DT::datatable(
      df,
      filter = "top",
      extensions = c('Buttons', 'ColReorder'),
      options = list(
        dom = 'Bfrtip',
        buttons = list('colvis'),
        colReorder = TRUE,
        scrollX = TRUE,
        # Adjust these targets based on the actual columns in your meta1 data
        columnDefs = list(
          list(visible = FALSE, targets = c(0, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 21, 22, 23, 25, 26, 27))
        )
      ),
      # Ensure the input names are unique if you plan to use multiple DT tables
      # to drive a single GT table (typically not done, but safe to be explicit).
      callback = JS("
            table.on('column-reorder', function(e, settings, details) {
                var order = table.colReorder.order();
                Shiny.setInputValue('column_order_age1', order); // Changed input name
            });

            table.on('buttons-action', function(e, buttonApi, dataTable, node, config) {
                var visible = [];
                dataTable.columns().every(function(index) {
                    if (this.visible()) visible.push(index);
                });
                Shiny.setInputValue('visible_columns_age1', visible); // Changed input name
            });
        ")
    )
  })

  # --- DT Output for (name3) / (name1) ---
  output$age_meta2 <- DT::renderDataTable({
    df <- filtered_data_age_meta2()
    req(df)

    DT::datatable(
      df,
      filter = "top",
      extensions = c('Buttons', 'ColReorder'),
      options = list(
        dom = 'Bfrtip',
        buttons = list('colvis'),
        colReorder = TRUE,
        scrollX = TRUE,
        columnDefs = list(
          list(visible = FALSE, targets = c(0, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 21, 22, 23, 25, 26, 27))
        )
      ),
      callback = JS("
            table.on('column-reorder', function(e, settings, details) {
                var order = table.colReorder.order();
                Shiny.setInputValue('column_order_age2', order);
            });

            table.on('buttons-action', function(e, buttonApi, dataTable, node, config) {
                var visible = [];
                dataTable.columns().every(function(index) {
                    if (this.visible()) visible.push(index);
                });
                Shiny.setInputValue('visible_columns_age2', visible);
            });
        ")
    )
  })

  # --- DT Output for (name4) / (name1) ---
  output$age_meta3 <- DT::renderDataTable({
    df <- filtered_data_age_meta3()
    req(df)

    DT::datatable(
      df,
      filter = "top",
      extensions = c('Buttons', 'ColReorder'),
      options = list(
        dom = 'Bfrtip',
        buttons = list('colvis'),
        colReorder = TRUE,
        scrollX = TRUE,
        columnDefs = list(
          list(visible = FALSE, targets = c(0, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 21, 22, 23, 25, 26, 27))
        )
      ),
      callback = JS("
            table.on('column-reorder', function(e, settings, details) {
                var order = table.colReorder.order();
                Shiny.setInputValue('column_order_age3', order);
            });

            table.on('buttons-action', function(e, buttonApi, dataTable, node, config) {
                var visible = [];
                dataTable.columns().every(function(index) {
                    if (this.visible()) visible.push(index);
                });
                Shiny.setInputValue('visible_columns_age3', visible);
            });
        ")
    )
  })

  # --- DT Output for (name4) / (name3) ---
  output$age_meta4 <- DT::renderDataTable({
    df <- filtered_data_age_meta4()
    req(df)

    DT::datatable(
      df,
      filter = "top",
      extensions = c('Buttons', 'ColReorder'),
      options = list(
        dom = 'Bfrtip',
        buttons = list('colvis'),
        colReorder = TRUE,
        scrollX = TRUE,
        columnDefs = list(
          list(visible = FALSE, targets = c(0, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 21, 22, 23, 25, 26, 27))
        )
      ),
      callback = JS("
            table.on('column-reorder', function(e, settings, details) {
                var order = table.colReorder.order();
                Shiny.setInputValue('column_order_age4', order);
            });

            table.on('buttons-action', function(e, buttonApi, dataTable, node, config) {
                var visible = [];
                dataTable.columns().every(function(index) {
                    if (this.visible()) visible.push(index);
                });
                Shiny.setInputValue('visible_columns_age4', visible);
            });
        ")
    )
  })

  # output$inputData1 <- DT::renderDataTable({
  #   shiny::validate(
  #     shiny::need((!is.null(upload1())), "No data set uploaded yet.")
  #   )
  #   data <- inputData1() %>%
  #     mutate(prevalence = round(prevalence, 3))
  #   DT::datatable(data, filter = "top", options = list(scrollX = TRUE))
  # })
  # output$inputData2 <- DT::renderDataTable({
  #   shiny::validate(
  #     shiny::need((!is.null(upload2())), "No data set uploaded yet.")
  #   )
  #   data <- inputData2() %>%
  #     mutate(prevalence = round(prevalence, 3))
  #   DT::datatable(data, filter = "top", options = list(scrollX = TRUE))
  # })

  # output$inputData1_top <- DT::renderDataTable({
  #   shiny::validate(
  #     shiny::need((!is.null(upload1())), "No data set uploaded yet.")
  #   )
  #   DT::datatable(inputData1_top(), filter = "top", options = list(scrollX = TRUE))
  # })
  #
  # output$inputData2_top <- DT::renderDataTable({
  #   shiny::validate(
  #     shiny::need((!is.null(upload2())), "No data set uploaded yet.")
  #   )
  #   DT::datatable(inputData2_top(), filter = "top", options = list(scrollX = TRUE))
  # })
  #
  # output$inputData1_par1 <- DT::renderDataTable({
  #   shiny::validate(
  #     shiny::need((!is.null(upload1())), "No data set uploaded yet.")
  #   )
  #   DT::datatable(inputData1_par1(), filter = "top", options = list(scrollX = TRUE))
  # })
  #
  # output$inputData2_par1 <- DT::renderDataTable({
  #   shiny::validate(
  #     shiny::need((!is.null(upload2())), "No data set uploaded yet.")
  #   )
  #   DT::datatable(inputData2_par1(), filter = "top", options = list(scrollX = TRUE))
  # })
  #
  # output$upload_meta1 <- DT::renderDataTable({
  #   shiny::validate(
  #     shiny::need((!is.null(upload_meta1())), "No data set uploaded yet.")
  #   )
  #   DT::datatable(upload_meta1(), filter = "top", options = list(scrollX = TRUE))
  # })
  #
  # output$upload_chapter <- DT::renderDataTable({
  #   shiny::validate(
  #     shiny::need((!is.null(upload1())), "No data set uploaded yet.")
  #   )
  #
  #   df <- shiny::reactive({
  #     diffChapterData(upload1(), upload2(), name1, name2)
  #   })
  #
  #   data_range <- range(df()$prevalence_diff_log2, na.rm = TRUE)
  #   DT::datatable(df(), filter = "top", options = list(scrollX = TRUE,
  #                                                      pageLength = 20,
  #                                                      autoWidth = TRUE)) %>%
  #     DT::formatStyle(
  #       'log2_diff_ci',
  #       backgroundColor = DT::styleInterval(
  #         c(-0.001, 0.001),
  #         c("rgba(252, 141, 89, 0.4)", "rgba(232, 232, 232, 0.4)", "rgba(69, 117, 180, 0.4)")
  #       ),
  #       valueColumns = 'prevalence_diff_log2'
  #     )
  # })
  # output$upload_block <- DT::renderDataTable({
  #   shiny::validate(
  #     shiny::need((!is.null(upload1())), "No data set uploaded yet.")
  #   )
  #   df <- shiny::reactive({
  #     diffBlockData(upload1(), upload2(), name1, name2)
  #   })
  #   data_range <- range(df()$prevalence_diff_log2, na.rm = TRUE)
  #   DT::datatable(df(), filter = "top", options = list(scrollX = TRUE,
  #                                                      pageLength = 20,
  #                                                      autoWidth = TRUE)) %>%
  #     DT::formatStyle(
  #       'log2_diff_ci',
  #       backgroundColor = DT::styleInterval(
  #         c(-0.001, 0.001),
  #         c("rgba(252, 141, 89, 0.4)", "rgba(232, 232, 232, 0.4)", "rgba(69, 117, 180, 0.4)")
  #       ),
  #       valueColumns = 'prevalence_diff_log2'  # This tells DT which column to use for the actual values
  #     )
  # })
## Category Table 1
  output$upload_category1 <- DT::renderDataTable({
    shiny::validate(
      shiny::need((!is.null(upload1())), "No data set uploaded yet.")
    )
    df <- shiny::reactive({
      diffCategoryData(upload1(), upload2(), name1, name2)
    })
    data_range <- range(df()$prevalence_diff_log2, na.rm = TRUE)
    DT::datatable(df(), filter = "top", options = list(scrollX = TRUE,
                                                       pageLength = 20,
                                                       autoWidth = TRUE)) %>%
      DT::formatStyle(
        'log2_diff_ci',
        backgroundColor = DT::styleInterval(
          c(-0.001, 0.001),
          c("rgba(252, 141, 89, 0.4)", "rgba(232, 232, 232, 0.4)", "rgba(69, 117, 180, 0.4)")
        ),
        valueColumns = 'prevalence_diff_log2'  # This tells DT which column to use for the actual values
      )
  })
  ## getting values from filtered table:
  output$filtered_block_codes <- renderPrint({
    shiny::validate(
      shiny::need((!is.null(upload1())), "No data available.")
    )

    df <- diffBlockData(upload1(), upload2(), name1, name2)
    filtered_indices <- input$upload_block_rows_all

    if (is.null(filtered_indices)) {
      filtered_codes <- df$parent1_code
    } else {
      filtered_codes <- df[filtered_indices, ]$parent1_code
    }

    cat("Parent1 Codes (", length(filtered_codes), " results):\n")
    cat(paste(filtered_codes, collapse = ", "))
    cat("\n\nCount of filtered codes:", length(filtered_codes))
  })
  output$filtered_category_codes1 <- renderPrint({
    shiny::validate(
      shiny::need((!is.null(upload1())), "No data available.")
    )

    df <- diffCategoryData(upload1(), upload2(), name1, name2)
    filtered_indices <- input$upload_category1_rows_all

    if (is.null(filtered_indices)) {
      filtered_codes <- df$parent2_code
    } else {
      filtered_codes <- df[filtered_indices, ]$parent2_code
    }

    cat("Parent2 Codes (", length(filtered_codes), " results):\n")
    cat(paste(filtered_codes, collapse = ", "))
    cat("\n\nCount of filtered codes:", length(filtered_codes))
  })
  output$diagnosis_representation1 <- renderUI({
    shiny::validate(
      shiny::need((!is.null(upload1())), "No data available.")
    )

    df <- diffCategoryData(upload1(), upload2(), name1, name2)
    filtered_indices <- input$upload_category1_rows_all

    # Get current search and column filters
    current_search <- input$upload_category1_search
    column_filters <- input$upload_category1_search_columns

    if (is.null(filtered_indices)) {
      filtered_values <- df$prevalence_diff_log2
    } else {
      filtered_values <- df[filtered_indices, ]$prevalence_diff_log2
    }

    filtered_values <- filtered_values[!is.na(filtered_values)]

    overrepresented <- sum(filtered_values < 0)
    underrepresented <- sum(filtered_values > 0)
    neutral <- sum(filtered_values == 0)
    total <- length(filtered_values)
    total_unfiltered <- nrow(df)

    # Calculate percentages
    over_pct <- round((overrepresented / total) * 100, 1)
    under_pct <- round((underrepresented / total) * 100, 1)

    # Create detailed filter info
    filter_details <- list()

    # Global search
    if (!is.null(current_search) && current_search != "") {
      filter_details <- append(filter_details, paste("Global search: '", current_search, "'", sep = ""))
    }

    # Column-specific filters
    if (!is.null(column_filters) && length(column_filters) > 0) {
      column_names <- names(df)
      for (i in seq_along(column_filters)) {
        if (!is.null(column_filters[[i]]) && column_filters[[i]] != "") {
          col_name <- column_names[i]
          filter_value <- column_filters[[i]]
          filter_details <- append(filter_details, paste(col_name, ": '", filter_value, "'", sep = ""))
        }
      }
    }

    div(
      if (length(filter_details) > 0) {
        div(
          p(strong("Active filters:")),
          lapply(filter_details, function(x) p("• ", x, style = "margin: 2px 0; padding-left: 10px;")),
          style = "color: #0066cc; font-style: italic; margin-bottom: 10px;"
        )
      },
      if (total < total_unfiltered) {
        p("Showing ", strong(total), " of ", strong(total_unfiltered), " total diagnoses")
      },
      p("Number of diagnoses overrepresented in ", strong(name1), ": ", strong(overrepresented), " (", over_pct, "%)"),
      p("Number of diagnoses underrepresented in ", strong(name1), ": ", strong(underrepresented), " (", under_pct, "%)"),
      if (neutral > 0) {
        neutral_pct <- round((neutral / total) * 100, 1)
        p("Number of diagnoses with no difference: ", strong(neutral), " (", neutral_pct, "%)")
      },

      style = "background-color: #f8f9fa; padding: 10px; border-radius: 4px; font-family: monospace;"
    )
  })

## Category Table 2
  output$upload_category2 <- DT::renderDataTable({
    shiny::validate(
      shiny::need((!is.null(upload2())), "No data set uploaded yet.")
    )
    df <- shiny::reactive({
      diffCategoryData(upload3(), upload2(), name3, name2)
    })
    data_range <- range(df()$prevalence_diff_log2, na.rm = TRUE)
    DT::datatable(df(), filter = "top", options = list(scrollX = TRUE,
                                                       pageLength = 20,
                                                       autoWidth = TRUE)) %>%
      DT::formatStyle(
        'log2_diff_ci',
        backgroundColor = DT::styleInterval(
          c(-0.001, 0.001),
          c("rgba(252, 141, 89, 0.4)", "rgba(232, 232, 232, 0.4)", "rgba(69, 117, 180, 0.4)")
        ),
        valueColumns = 'prevalence_diff_log2'  # This tells DT which column to use for the actual values
      )
  })
  output$filtered_category_codes2 <- renderPrint({
    shiny::validate(
      shiny::need((!is.null(upload2())), "No data available.")
    )

    df <- diffCategoryData(upload3(), upload2(), name3, name2)
    filtered_indices <- input$upload_category2_rows_all

    if (is.null(filtered_indices)) {
      filtered_codes <- df$parent2_code
    } else {
      filtered_codes <- df[filtered_indices, ]$parent2_code
    }

    cat("Parent2 Codes (", length(filtered_codes), " results):\n")
    cat(paste(filtered_codes, collapse = ", "))
    cat("\n\nCount of filtered codes:", length(filtered_codes))
  })
  output$diagnosis_representation2 <- renderUI({
    shiny::validate(
      shiny::need((!is.null(upload2())), "No data available.")
    )

    df <- diffCategoryData(upload3(), upload2(), name3, name2)
    filtered_indices <- input$upload_category2_rows_all

    # Get current search and column filters
    current_search <- input$upload_category2_search
    column_filters <- input$upload_category2_search_columns

    if (is.null(filtered_indices)) {
      filtered_values <- df$prevalence_diff_log2
    } else {
      filtered_values <- df[filtered_indices, ]$prevalence_diff_log2
    }

    filtered_values <- filtered_values[!is.na(filtered_values)]

    overrepresented <- sum(filtered_values < 0)
    underrepresented <- sum(filtered_values > 0)
    neutral <- sum(filtered_values == 0)
    total <- length(filtered_values)
    total_unfiltered <- nrow(df)

    # Calculate percentages
    over_pct <- round((overrepresented / total) * 100, 1)
    under_pct <- round((underrepresented / total) * 100, 1)

    # Create detailed filter info
    filter_details <- list()

    # Global search
    if (!is.null(current_search) && current_search != "") {
      filter_details <- append(filter_details, paste("Global search: '", current_search, "'", sep = ""))
    }

    # Column-specific filters
    if (!is.null(column_filters) && length(column_filters) > 0) {
      column_names <- names(df)
      for (i in seq_along(column_filters)) {
        if (!is.null(column_filters[[i]]) && column_filters[[i]] != "") {
          col_name <- column_names[i]
          filter_value <- column_filters[[i]]
          filter_details <- append(filter_details, paste(col_name, ": '", filter_value, "'", sep = ""))
        }
      }
    }

    div(
      if (length(filter_details) > 0) {
        div(
          p(strong("Active filters:")),
          lapply(filter_details, function(x) p("• ", x, style = "margin: 2px 0; padding-left: 10px;")),
          style = "color: #0066cc; font-style: italic; margin-bottom: 10px;"
        )
      },
      if (total < total_unfiltered) {
        p("Showing ", strong(total), " of ", strong(total_unfiltered), " total diagnoses")
      },
      p("Number of diagnoses overrepresented in ", strong(name3), ": ", strong(overrepresented), " (", over_pct, "%)"),
      p("Number of diagnoses underrepresented in ", strong(name3), ": ", strong(underrepresented), " (", under_pct, "%)"),
      if (neutral > 0) {
        neutral_pct <- round((neutral / total) * 100, 1)
        p("Number of diagnoses with no difference: ", strong(neutral), " (", neutral_pct, "%)")
      },

      style = "background-color: #f8f9fa; padding: 10px; border-radius: 4px; font-family: monospace;"
    )
  })
  ## Category Table 3
  output$upload_category3 <- DT::renderDataTable({
    shiny::validate(
      shiny::need((!is.null(upload1())), "No data set uploaded yet.")
    )
    df <- shiny::reactive({
      diffCategoryData(upload4(), upload2(), name4, name2)
    })
    data_range <- range(df()$prevalence_diff_log2, na.rm = TRUE)
    DT::datatable(df(), filter = "top", options = list(scrollX = TRUE,
                                                       pageLength = 20,
                                                       autoWidth = TRUE)) %>%
      DT::formatStyle(
        'log2_diff_ci',
        backgroundColor = DT::styleInterval(
          c(-0.001, 0.001),
          c("rgba(252, 141, 89, 0.4)", "rgba(232, 232, 232, 0.4)", "rgba(69, 117, 180, 0.4)")
        ),
        valueColumns = 'prevalence_diff_log2'  # This tells DT which column to use for the actual values
      )
  })
  ## getting values from filtered table:
  output$filtered_block_codes3 <- renderPrint({
    shiny::validate(
      shiny::need((!is.null(upload1())), "No data available.")
    )

    df <- diffBlockData(upload4(), upload2(), name4, name2)
    filtered_indices <- input$upload_block_rows_all

    if (is.null(filtered_indices)) {
      filtered_codes <- df$parent1_code
    } else {
      filtered_codes <- df[filtered_indices, ]$parent1_code
    }

    cat("Parent1 Codes (", length(filtered_codes), " results):\n")
    cat(paste(filtered_codes, collapse = ", "))
    cat("\n\nCount of filtered codes:", length(filtered_codes))
  })
  output$filtered_category_codes3 <- renderPrint({
    shiny::validate(
      shiny::need((!is.null(upload1())), "No data available.")
    )

    df <- diffCategoryData(upload4(), upload2(), name4, name2)
    filtered_indices <- input$upload_category3_rows_all

    if (is.null(filtered_indices)) {
      filtered_codes <- df$parent2_code
    } else {
      filtered_codes <- df[filtered_indices, ]$parent2_code
    }

    cat("Parent2 Codes (", length(filtered_codes), " results):\n")
    cat(paste(filtered_codes, collapse = ", "))
    cat("\n\nCount of filtered codes:", length(filtered_codes))
  })
  output$diagnosis_representation3 <- renderUI({
    shiny::validate(
      shiny::need((!is.null(upload1())), "No data available.")
    )

    df <- diffCategoryData(upload4(), upload2(), name4, name2)
    filtered_indices <- input$upload_category3_rows_all

    # Get current search and column filters
    current_search <- input$upload_category3_search
    column_filters <- input$upload_category3_search_columns

    if (is.null(filtered_indices)) {
      filtered_values <- df$prevalence_diff_log2
    } else {
      filtered_values <- df[filtered_indices, ]$prevalence_diff_log2
    }

    filtered_values <- filtered_values[!is.na(filtered_values)]

    overrepresented <- sum(filtered_values < 0)
    underrepresented <- sum(filtered_values > 0)
    neutral <- sum(filtered_values == 0)
    total <- length(filtered_values)
    total_unfiltered <- nrow(df)

    # Calculate percentages
    over_pct <- round((overrepresented / total) * 100, 1)
    under_pct <- round((underrepresented / total) * 100, 1)

    # Create detailed filter info
    filter_details <- list()

    # Global search
    if (!is.null(current_search) && current_search != "") {
      filter_details <- append(filter_details, paste("Global search: '", current_search, "'", sep = ""))
    }

    # Column-specific filters
    if (!is.null(column_filters) && length(column_filters) > 0) {
      column_names <- names(df)
      for (i in seq_along(column_filters)) {
        if (!is.null(column_filters[[i]]) && column_filters[[i]] != "") {
          col_name <- column_names[i]
          filter_value <- column_filters[[i]]
          filter_details <- append(filter_details, paste(col_name, ": '", filter_value, "'", sep = ""))
        }
      }
    }

    div(
      if (length(filter_details) > 0) {
        div(
          p(strong("Active filters:")),
          lapply(filter_details, function(x) p("• ", x, style = "margin: 2px 0; padding-left: 10px;")),
          style = "color: #0066cc; font-style: italic; margin-bottom: 10px;"
        )
      },
      if (total < total_unfiltered) {
        p("Showing ", strong(total), " of ", strong(total_unfiltered), " total diagnoses")
      },
      p("Number of diagnoses overrepresented in ", strong(name4), ": ", strong(overrepresented), " (", over_pct, "%)"),
      p("Number of diagnoses underrepresented in ", strong(name4), ": ", strong(underrepresented), " (", under_pct, "%)"),
      if (neutral > 0) {
        neutral_pct <- round((neutral / total) * 100, 1)
        p("Number of diagnoses with no difference: ", strong(neutral), " (", neutral_pct, "%)")
      },

      style = "background-color: #f8f9fa; padding: 10px; border-radius: 4px; font-family: monospace;"
    )
  })
  ## Table 4
  ## Category Table 4
  output$upload_category4 <- DT::renderDataTable({
    shiny::validate(
      shiny::need((!is.null(upload2())), "No data set uploaded yet.")
    )
    df <- shiny::reactive({
      diffCategoryData(upload4(), upload3(), name4, name3)
    })
    data_range <- range(df()$prevalence_diff_log2, na.rm = TRUE)
    DT::datatable(df(), filter = "top", options = list(scrollX = TRUE,
                                                       pageLength = 20,
                                                       autoWidth = TRUE)) %>%
      DT::formatStyle(
        'log2_diff_ci',
        backgroundColor = DT::styleInterval(
          c(-0.001, 0.001),
          c("rgba(252, 141, 89, 0.4)", "rgba(232, 232, 232, 0.4)", "rgba(69, 117, 180, 0.4)")
        ),
        valueColumns = 'prevalence_diff_log2'  # This tells DT which column to use for the actual values
      )
  })

  output$filtered_parent2_codes <- renderPrint({
    shiny::validate(
      shiny::need(
        (!is.null(input$code_filter) && length(input$code_filter) > 0),
        "No codes selected."
      )
    )

    # Get the base data
    df <- upload_gender_parent2_meta1()

    # Apply same filtering as in forest2_parent2()
    df_filtered <- df %>%
      dplyr::filter(
        gender_EN %in% input$gender_filter,
        parent0_code %in% input$code_filter,
        ci_width_nat     >= input$ci_filter[1],
        ci_width_nat     <= input$ci_filter[2],
        fold_diff_reg  >= input$fold_filter[1],
        fold_diff_reg  <= input$fold_filter[2]

      )
    filtered_codes <- unique(df_filtered$parent2_code)

    df_filtered_over <- df_filtered %>%
      filter(prevalence_diff>=0)
    df_sorted_over <- df_filtered_over[order(df_filtered_over$prevalence_diff, decreasing = TRUE), ]
    filtered_codes_over <- unique(df_sorted_over$parent2_code)

    df_filtered_under <- df_filtered %>%
      filter(prevalence_diff<0)
    df_sorted_under <- df_filtered_under[order(df_filtered_under$prevalence_diff, decreasing = FALSE), ]
    filtered_codes_under <- unique(df_sorted_under$parent2_code)

    cat("Over (", length(filtered_codes_over), "):\n")
    cat(paste0(filtered_codes_over, collapse = ", "))
    cat("\nUnder (", length(filtered_codes_under), "):\n")
    cat(paste0(filtered_codes_under, collapse = ", "))
    cat("\n\nCount of filtered codes:", length(filtered_codes))
  })

  output$diagnosis_representation4 <- renderUI({
    shiny::validate(
      shiny::need((!is.null(upload2())), "No data available.")
    )

    df <- diffCategoryData(upload4(), upload3(), name4, name3)
    filtered_indices <- input$upload_category4_rows_all

    # Get current search and column filters
    current_search <- input$upload_category4_search
    column_filters <- input$upload_category4_search_columns

    if (is.null(filtered_indices)) {
      filtered_values <- df$prevalence_diff_log2
    } else {
      filtered_values <- df[filtered_indices, ]$prevalence_diff_log2
    }

    filtered_values <- filtered_values[!is.na(filtered_values)]

    overrepresented <- sum(filtered_values < 0)
    underrepresented <- sum(filtered_values > 0)
    neutral <- sum(filtered_values == 0)
    total <- length(filtered_values)
    total_unfiltered <- nrow(df)

    # Calculate percentages
    over_pct <- round((overrepresented / total) * 100, 1)
    under_pct <- round((underrepresented / total) * 100, 1)

    # Create detailed filter info
    filter_details <- list()

    # Global search
    if (!is.null(current_search) && current_search != "") {
      filter_details <- append(filter_details, paste("Global search: '", current_search, "'", sep = ""))
    }

    # Column-specific filters
    if (!is.null(column_filters) && length(column_filters) > 0) {
      column_names <- names(df)
      for (i in seq_along(column_filters)) {
        if (!is.null(column_filters[[i]]) && column_filters[[i]] != "") {
          col_name <- column_names[i]
          filter_value <- column_filters[[i]]
          filter_details <- append(filter_details, paste(col_name, ": '", filter_value, "'", sep = ""))
        }
      }
    }

    div(
      if (length(filter_details) > 0) {
        div(
          p(strong("Active filters:")),
          lapply(filter_details, function(x) p("• ", x, style = "margin: 2px 0; padding-left: 10px;")),
          style = "color: #0066cc; font-style: italic; margin-bottom: 10px;"
        )
      },
      if (total < total_unfiltered) {
        p("Showing ", strong(total), " of ", strong(total_unfiltered), " total diagnoses")
      },
      p("Number of diagnoses overrepresented in ", strong(name3), ": ", strong(overrepresented), " (", over_pct, "%)"),
      p("Number of diagnoses underrepresented in ", strong(name3), ": ", strong(underrepresented), " (", under_pct, "%)"),
      if (neutral > 0) {
        neutral_pct <- round((neutral / total) * 100, 1)
        p("Number of diagnoses with no difference: ", strong(neutral), " (", neutral_pct, "%)")
      },

      style = "background-color: #f8f9fa; padding: 10px; border-radius: 4px; font-family: monospace;"
    )
  })


} # Server end
