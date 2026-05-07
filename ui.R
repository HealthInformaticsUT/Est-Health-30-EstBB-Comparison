# ui.R — Dashboard UI definition
# Same layout as legacy app, cleaned up

# --- Header ---
header <- shinydashboard::dashboardHeader(title = "EstBB vs Population")

# --- Sidebar ---
sidebar <- shinydashboard::dashboardSidebar(
  shinydashboard::sidebarMenu(
    shinydashboard::menuItem("Plots", tabName = "Visuals", icon = shiny::icon("user")),
    shinydashboard::menuItem("Data tables - Gender", tabName = "DataTables_Gender", icon = shiny::icon("table")),
    shinydashboard::menuItem("Data tables - Age Groups", tabName = "DataTables_AgeGroups", icon = shiny::icon("table")),
    # Volcano Analysis tab hidden 2026-04-28 — restore by uncommenting this line and the tabItem block below.
    # shinydashboard::menuItem("Volcano Analysis", tabName = "VolcanoTab", icon = shiny::icon("chart-line")),
    shinydashboard::menuItem("How to use", tabName = "HowTo", icon = shiny::icon("circle-question")),
    shinydashboard::menuItem("About the Study", tabName = "AboutStudy", icon = shiny::icon("info-circle")),
    shinydashboard::menuItem("Contacts", tabName = "ContactsTab", icon = shiny::icon("address-book"))
  ),
  shiny::hr(),
  shiny::br(),
  collapsed = TRUE
)

# --- Helper: comparison tab labels ---
comp_labels <- list(
  c(DATASET_NAMES$d2, DATASET_NAMES$d1),  # Meta 1
  c(DATASET_NAMES$d3, DATASET_NAMES$d1),  # Meta 2
  c(DATASET_NAMES$d4, DATASET_NAMES$d1),  # Meta 3
  c(DATASET_NAMES$d4, DATASET_NAMES$d3)   # Meta 4
)

# --- Body ---
body <- shinydashboard::dashboardBody(
  shiny::fluidRow(
    shinyjs::useShinyjs(),

    # CSS
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("
        .box.box-primary { border-top-color: #cfcfcf; }
        .box.box-solid.box-primary > .box-header { background-color: #cfcfcf; color: #333333; }
        .no-margin-box .box-body { padding: 10px; }
        .fixed-image { max-width: 100%; height: auto; display: block; margin: 0 auto; }
        .center-img { display: block; margin-right: auto; }
        .image-column { padding-left: 10px; padding-right: 10px; text-align: center; }
        .info-icon { position: absolute; top: 5px; right: 35px; z-index: 100; color: white; }
        @media (max-width: 1500px) { .responsive-col2 { width: 100% !important; } }
        @media (max-width: 768px) { .image-column { margin-bottom: 15px !important; } }
        .justified-text { text-align: justify; }
        .text-left { text-align: left; }
        .active { background-color: #ffffff !important; color: grey; }
        .inactive { background-color: #f8f9fa !important; color: black; }
        .diag-chip { display: inline-block; padding: 1px 5px; margin: 1px 2px;
                     border-radius: 3px; background-color: #f0f0f0; border: 1px solid #ccc;
                     font-size: 11px; font-weight: 500; color: #333; line-height: 1.4; }
        .diag-chip .remove-chip { cursor: pointer; margin-left: 3px; color: #999;
                                  font-weight: bold; font-size: 11px; }
        .diag-chip .remove-chip:hover { color: #c00; }
        .diag-chips-row { margin-top: 3px; line-height: 1.6; }
      "))
    ),

    shinydashboard::tabItems(

      # ====================================================================
      # TAB: VISUALS
      # ====================================================================
      shinydashboard::tabItem(
        tabName = "Visuals",
        shinydashboard::tabBox(
          width = 12, id = "tabset0",

          # --- Sub-tab: PR Distribution (3x3 histogram grid) ---
          shiny::tabPanel("PR Distribution",
            shiny::fluidRow(
              shinydashboard::box(
                width = 12,
                title = "Distribution of Prevalence Ratios Between Datasets + Burden",
                solidHeader = TRUE, status = "primary", class = "no-margin-box",
                shiny::fluidRow(
                  # Column 1: EstBB
                  shiny::column(width = 4,
                    plotly::plotlyOutput("p_histogram_bb", height = "24vh"),
                    plotly::plotlyOutput("p_death_bb", height = "24vh"),
                    plotly::plotlyOutput("p_disability_bb", height = "24vh")
                  ),
                  # Column 2: EstBB1
                  shiny::column(width = 4,
                    plotly::plotlyOutput("p_histogram_bb1", height = "24vh"),
                    plotly::plotlyOutput("p_death_bb1", height = "24vh"),
                    plotly::plotlyOutput("p_disability_bb1", height = "24vh")
                  ),
                  # Column 3: EstBB2
                  shiny::column(width = 4,
                    plotly::plotlyOutput("p_histogram_bb2", height = "24vh"),
                    plotly::plotlyOutput("p_death_bb2", height = "24vh"),
                    plotly::plotlyOutput("p_disability_bb2", height = "24vh")
                  )
                )
              )
            )
          ),

          # --- Sub-tab: PR Distribution by Gender ---
          shiny::tabPanel("PR Distribution by Gender",
            shiny::fluidRow(
              shinydashboard::box(
                width = 12,
                title = "Distribution of Prevalence Ratios in Males vs Females Between Datasets",
                solidHeader = TRUE, status = "primary", class = "no-margin-box",
                shiny::fluidRow(
                  shiny::column(width = 4,
                    plotly::plotlyOutput("p_histogram_bbM", height = "36vh"),
                    plotly::plotlyOutput("p_histogram_bbF", height = "36vh")
                  ),
                  shiny::column(width = 4,
                    plotly::plotlyOutput("p_histogram_bb1M", height = "36vh"),
                    plotly::plotlyOutput("p_histogram_bb1F", height = "36vh")
                  ),
                  shiny::column(width = 4,
                    plotly::plotlyOutput("p_histogram_bb2M", height = "36vh"),
                    plotly::plotlyOutput("p_histogram_bb2F", height = "36vh")
                  )
                )
              )
            )
          ),

          # --- Sub-tab: PR Values by Gender per Diagnosis ---
          shiny::tabPanel("PR Values by Gender per Diagnosis",
            shiny::fluidRow(
              shinydashboard::box(
                width = 12,
                title = "Values of Prevalence Ratios by Gender per Diagnosis",
                solidHeader = TRUE, status = "primary", class = "no-margin-box",

                # Controls
                shiny::fluidRow(
                  shiny::column(width = 2,
                    shiny::checkboxGroupInput("gender_filter", "Select genders:",
                      choices = c("M", "N", "Both"), selected = c("M", "N"), inline = TRUE)
                  ),
                  shiny::column(width = 2,
                    shiny::selectInput("code_filter", label = "Diagnosis:",
                      choices = c("I00-I99"), selected = c("I00-I99"), multiple = FALSE)
                  ),
                  shiny::column(width = 3,
                    shiny::sliderInput("ci_filter", "CI width:",
                      min = 0, max = 10.0, value = c(0, 0.5), step = 0.5)
                  ),
                  shiny::column(width = 3,
                    shiny::sliderInput("fold_filter", "Fold diff:",
                      min = 1, max = 10, value = c(1.25, 10), step = 0.25)
                  ),
                  shiny::column(width = 2,
                    shiny::radioButtons("scaleModeGender", label = "Scale:",
                      choices = c("Log2 PR" = "log2", "Fold Difference" = "fold"),
                      selected = "log2", inline = TRUE),
                    shiny::actionButton("resetAnalysisValues", "Reset analysis values",
                      class = "btn-sm btn-default")
                  )
                ),
                shiny::hr(),

                # Forest plots (4 comparisons in 2x2 grid)
                shiny::fluidRow(
                  shiny::column(width = 6,
                    shinycssloaders::withSpinner(ggiraph::girafeOutput("forest2_parent2"),
                      color = "#3D8FBE", type = 7, size = 0.6),
                    shiny::hr(),
                    shinycssloaders::withSpinner(ggiraph::girafeOutput("forest2_parent2_genders3"),
                      color = "#3D8FBE", type = 7, size = 0.6)
                  ),
                  shiny::column(width = 6,
                    shinycssloaders::withSpinner(ggiraph::girafeOutput("forest2_parent2_genders2"),
                      color = "#3D8FBE", type = 7, size = 0.6),
                    shiny::hr(),
                    shinycssloaders::withSpinner(ggiraph::girafeOutput("forest2_parent2_genders4"),
                      color = "#3D8FBE", type = 7, size = 0.6)
                  )
                )
              )
            )
          ),

          # --- Sub-tab: Heatmaps ---
          shiny::tabPanel("PR Values by Age and Gender",
            shiny::fluidRow(

              # Filters
              shinydashboard::box(
                title = "Add or Remove Details", width = 12,
                solidHeader = TRUE, status = "primary", class = "no-margin-box", collapsible = TRUE,
                shiny::fluidRow(
                  shiny::column(width = 3,
                    shiny::radioButtons("filterDatasets", label = "Compare:",
                      choices = sapply(COMPARISONS, `[[`, "label"),
                      selected = COMPARISONS[[1]]$label, inline = FALSE)
                  ),
                  shiny::column(width = 3,
                    shiny::selectInput("filterAge", label = "Age:", choices = NULL, selected = NULL, multiple = TRUE)
                  ),
                  shiny::column(width = 4,
                    shiny::selectInput("filterCodeGroups0", label = "Diagnosis:",
                      choices = c("I00-I99"), selected = c("I00-I99"), multiple = TRUE)
                  ),
                  shiny::column(width = 2,
                    shiny::selectInput("filterGender", label = "Gender:",
                      choices = NULL, selected = c("M", "F"), multiple = TRUE)
                  ),
                  shiny::column(width = 12,
                    shiny::radioButtons("scaleModeHeatmap", label = NULL,
                      choices = c("Log2 PR" = "log2", "Fold Difference" = "fold"),
                      selected = "log2", inline = TRUE)
                  )
                )
              ),

              # Heatmap plots
              shinydashboard::box(
                width = 12,
                title = "PR Values by Age and Gender in Diagnoses Groups",
                solidHeader = TRUE, status = "primary",
                shiny::fluidRow(
                  shiny::column(width = 6, class = "responsive-col2",
                    shinycssloaders::withSpinner(ggiraph::girafeOutput("heatmap_meta_avg", width = "auto"),
                      color = "#3D8FBE", type = 7, size = 0.6)
                  ),
                  shiny::hr(),
                  shiny::column(width = 6, class = "responsive-col2",
                    shinycssloaders::withSpinner(ggiraph::girafeOutput("heatmap_meta_alph", width = "auto"),
                      color = "#3D8FBE", type = 7, size = 0.6)
                  ),
                  shiny::hr()
                )
              ),

              # Forest + Point Diff
              shinydashboard::box(
                width = 12, solidHeader = TRUE, status = "primary",
                title = "Detailed View: Prevalence Ratios and Prevalence Values across Years",
                shiny::div(class = "info-icon",
                  shiny::tags$span("Methodology",
                    shiny::tags$i(id = "methodology", class = "glyphicon glyphicon-info-sign",
                      title = "The meta-analysis conducted in this function uses the metagen function...")
                  )
                ),
                shinycssloaders::withSpinner(ggiraph::girafeOutput("forest1"),
                  color = "#3D8FBE", type = 7, size = 0.6),
                shiny::hr(),
                shinycssloaders::withSpinner(ggiraph::girafeOutput("pointDiff1"),
                  color = "#3D8FBE", type = 7, size = 0.6),
                shiny::hr()
              )
            )
          ),

          # --- Sub-tab: Custom ---
          shiny::tabPanel("Custom",
            shiny::fluidRow(
              shinydashboard::box(
                title = "Filters", width = 12,
                solidHeader = TRUE, status = "primary", class = "no-margin-box",
                shiny::fluidRow(
                  shiny::column(width = 2,
                    shiny::radioButtons("filterDatasetsCustom", label = "Compare:",
                      choices = sapply(COMPARISONS, `[[`, "label"),
                      selected = COMPARISONS[[1]]$label, inline = FALSE)
                  ),
                  shiny::column(width = 7,
                    shiny::selectizeInput("filterDiagCustom", label = "Diagnosis category:",
                      choices = NULL, multiple = FALSE,
                      options = list(placeholder = "Search by diagnosis name or code...")),
                    shiny::fluidRow(
                      shiny::column(width = 9,
                        shiny::textInput("bulkDiagInput", label = NULL,
                          placeholder = "Paste codes: A02, C22, I10...")
                      ),
                      shiny::column(width = 3,
                        shiny::actionButton("bulkDiagAdd", "Add", class = "btn-sm",
                          style = "margin-top: 0px;"),
                        shiny::actionButton("emptyDiagCodes", "Empty", class = "btn-sm btn-default",
                          style = "margin-top: 0px; margin-left: 4px;")
                      )
                    ),
                    shiny::uiOutput("customDiagChips")
                  ),
                  shiny::column(width = 2,
                    shiny::selectInput("filterAgeCustom", label = "Age:",
                      choices = NULL, selected = NULL, multiple = TRUE)
                  ),
                  shiny::column(width = 1,
                    shiny::selectInput("filterGenderCustom", label = "Gender:",
                      choices = NULL, selected = c("M", "F"), multiple = TRUE)
                  ),
                  shiny::column(width = 12,
                    shiny::radioButtons("scaleModeCustom", label = NULL,
                      choices = c("Log2 PR" = "log2", "Fold Difference" = "fold"),
                      selected = "log2", inline = TRUE)
                  )
                )
              ),

              # Left column: overview plots
              shiny::column(width = 6,
                shinydashboard::box(
                  width = 12,
                  title = "Custom Diagnosis Heatmap",
                  solidHeader = TRUE, status = "primary",
                  shinycssloaders::withSpinner(
                    ggiraph::girafeOutput("heatmap_custom", width = "auto"),
                    color = "#3D8FBE", type = 7, size = 0.6
                  )
                ),
                shinydashboard::box(
                  width = 12,
                  title = "Prevalence Ratios by Gender per Diagnosis",
                  solidHeader = TRUE, status = "primary",
                  shinycssloaders::withSpinner(
                    ggiraph::girafeOutput("forest_custom"),
                    color = "#3D8FBE", type = 7, size = 0.6
                  )
                )
              ),

              # Right column: detail panel (click a diagnosis to inspect)
              shiny::column(width = 6,
                shinydashboard::box(
                  width = 12,
                  title = shiny::uiOutput("customDetailTitle", inline = TRUE),
                  solidHeader = TRUE, status = "primary",
                  # Yearly forest with heatmap tile on Meta row
                  shinycssloaders::withSpinner(
                    ggiraph::girafeOutput("customDetailForest"),
                    color = "#3D8FBE", type = 7, size = 0.6
                  ),
                  shiny::hr(),
                  # Point difference plot
                  shinycssloaders::withSpinner(
                    ggiraph::girafeOutput("customDetailPointDiff"),
                    color = "#3D8FBE", type = 7, size = 0.6
                  )
                )
              )
            )
          ),

          # --- Sub-tab: Demography ---
          shiny::tabPanel("Demography",
            shiny::fluidRow(
              shinydashboard::box(
                width = 12, title = "Demographic Overview",
                solidHeader = TRUE, status = "primary",
                shiny::tags$img(src = "img/Fig1-EH30-demography.png",
                  class = "fixed-image", width = "700px", height = "auto")
              )
            )
          )
        ) # tabBox end
      ), # Visuals tabItem end

      # ====================================================================
      # TAB: HOW TO USE
      # ====================================================================
      shinydashboard::tabItem(
        tabName = "HowTo",
        shinydashboard::box(
          width = 12,
          title = "How to use this tool",
          solidHeader = TRUE, status = "info", class = "no-margin-box",
          shiny::div(style = "padding: 10px;",

            shiny::tags$h4(shiny::tags$b("What this dashboard shows")),
            shiny::tags$p(
              "This dashboard lets researchers inspect prevalence-ratio differences ",
              "between the Estonian Biobank (EstBB) and its two recruitment waves ",
              "(EstBB1, EstBB2) and the Estonian general population (Est-Health-30) ",
              "across ICD-10 three-character categories, stratified by age, sex, and year."
            ),

            shiny::tags$h4(shiny::tags$b("The four comparison pairs and when to use each")),
            shiny::tags$ul(
              shiny::tags$li(shiny::tags$b("Est-Health-30 vs EstBB"),
                " - overall EstBB representativeness."),
              shiny::tags$li(shiny::tags$b("Est-Health-30 vs EstBB1"),
                " - clinic-based recruitment wave (GP network) vs general population."),
              shiny::tags$li(shiny::tags$b("Est-Health-30 vs EstBB2"),
                " - media-campaign recruitment wave vs general population."),
              shiny::tags$li(shiny::tags$b("EstBB1 vs EstBB2"),
                " - within-EstBB wave-to-wave contrast, isolating recruitment-mechanism effects.")
            ),

            shiny::tags$h4(shiny::tags$b("What is not included and why")),
            shiny::tags$ul(
              shiny::tags$li(
                shiny::tags$b("External causes (V01-Y98)"),
                " - excluded from both the analysis and this dashboard. ",
                "A data-workflow gap affecting Estonian health records between 2012 and 2018 ",
                "was identified during dashboard QA. Including this chapter would show ",
                "misleading underrepresentation that is an artefact of the gap, not a real ",
                "cohort difference."
              ),
              shiny::tags$li(
                shiny::tags$b("Records from individuals under 10 years of age at observation"),
                " - excluded; the youngest age group shown is 10-19 years. ",
                "Prevalence estimates for conditions primarily affecting young children ",
                "are therefore not available here."
              )
            ),

            shiny::tags$h4(shiny::tags$b("Downloads and full stratified data")),
            shiny::tags$p(
              "Full CSV downloads with age-, sex-, and year-stratified prevalence ratios ",
              "for all ~1,028 ICD-10 three-character conditions are available via the ",
              "Data tables tabs. Use these for downstream weighting, calibration, or ",
              "cohort-design work."
            ),

            shiny::tags$h4(shiny::tags$b("Where to go for the framework")),
            shiny::tags$p(
              "The three-step bias-assessment framework - (i) locate your diagnosis ",
              "in the heatmap, (ii) inspect the four comparison pairs to disaggregate ",
              "wave-specific mechanisms, (iii) pull the stratified CSV for modelling - ",
              "is described in the 'Practical Guidance for Researchers' subsection of ",
              "the companion manuscript. For the most recent version, search for ",
              shiny::tags$b("'Pajusalu EstBB representativeness'"),
              " on medRxiv or Google Scholar."
            )
          )
        )
      ), # HowTo tabItem end

      # ====================================================================
      # TAB: ABOUT THE STUDY
      # ====================================================================
      shinydashboard::tabItem(
        tabName = "AboutStudy",
        shinydashboard::box(
          width = 12,
          title = "Estonian Biobank vs General Population: Analysis of Diagnosis Prevalences",
          solidHeader = TRUE, status = "info", class = "no-margin-box",
          shiny::div(style = "padding: 10px;",
            shiny::tags$h3(shiny::tags$b("Abstract")),
            shiny::tags$hr(),
            shiny::fluidRow(
              shiny::column(width = 6,
                shiny::div(class = "text-justify",
                  shiny::tags$h4(shiny::tags$b("WHY")),
                  shiny::tags$p(
                    "When publishing research, it is essential to critically assess whether the study sample is representative of the target population. This study evaluates the representativeness of the Estonian Biobank (EstBB) and its two recruitment waves relative to the general Estonian population, approximated by a 30% national reference dataset (Est-Health-30). To support generalizability and informed study design, we quantify systematic differences in disease prevalence and demographics, with additional consideration of disease burden using DALY metrics to contextualize the potential impact of over- or underrepresented conditions."
                  ),
                  shiny::tags$h4(shiny::tags$b("HOW")),
                  shiny::tags$p(
                    "We analyzed diagnosis prevalence using two Estonian healthcare datasets including the Estonian Biobank (EstBB) and a representative population sample (Est-Health-30). Diagnoses were grouped by ICD-10 codes and stratified by age and gender across 2012\u20132023, with prevalence ratios computed and synthesized using meta-analysis. To ensure interpretability and robustness, we applied thresholds for fold difference magnitude and confidence interval precision, and visualized results via an interactive dashboard."
                  )
                )
              ),
              shiny::column(width = 6,
                shiny::div(class = "text-justify",
                  shiny::tags$h4(shiny::tags$b("RESULTS")),
                  shiny::tags$p(
                    "Our analysis reveals that EstBB is enriched for outpatient-managed, non-acute, and preventive care diagnoses, including dermatological, reproductive, endocrine, and mental health conditions. In contrast, severe and high-mortality diseases\u2014such as dementia, stroke sequelae, advanced cancers, and chronic respiratory failure\u2014are consistently underrepresented. Gender-specific trends indicate a higher cardiovascular burden and stronger overrepresentation of diagnoses in men, while women are more representative of the general population. The second wave of recruitment (EstBB2), characterized by simplified procedures and broad outreach, represents a healthier subset with lower prevalence of chronic disease and higher engagement in mental health and preventive care. Conversely, the first wave (EstBB1) shows a specific subcohort with a higher disease burden, particularly among males."
                  ),
                  shiny::tags$h4(shiny::tags$b("CONCLUSION")),
                  shiny::tags$p(
                    "EstBB is well-suited for genetic association studies, behavioral health research, and longitudinal tracking of chronic conditions. Its strengths include high-quality phenotype data and strong representation of traits with stable outpatient management. However, researchers must critically account for selection bias and demographic skew when modeling population-level disease burden or studying late-stage and high-mortality conditions. The accompanying dashboard enhances transparency and adaptability, allowing researchers to interrogate cohort composition and refine phenotype selection prior to analysis. This analysis supports more accurate interpretation of biobank-derived findings and strengthens the design of future studies using EstBB data."
                  )
                )
              )
            )
          )
        )
      ),

      # ====================================================================
      # TAB: DATA TABLES - GENDER
      # ====================================================================
      shinydashboard::tabItem(
        tabName = "DataTables_Gender",
        shiny::fluidRow(
          shiny::column(width = 12,
            shinydashboard::tabBox(
              width = 12, id = "gender_tabs", title = "Gender Analysis",

              # Generate tab pairs (DT + GT) for each comparison
              shiny::tabPanel(paste0(comp_labels[[1]][1], "/", comp_labels[[1]][2]),
                shiny::div(style = "margin-bottom: 10px;",
                  shiny::downloadButton("download_csv_gender_meta1", "Download CSV")),
                DT::dataTableOutput("upload_diff_genders_meta1_DT")
              ),
              shiny::tabPanel(paste0("Print (", comp_labels[[1]][1], "/", comp_labels[[1]][2], ")"),
                shiny::div(style = "margin-bottom: 10px;",
                  shiny::downloadButton("download_pdf_gender_meta1_GT", "Download PDF")),
                gt::gt_output("upload_diff_genders_meta1_GT")
              ),

              shiny::tabPanel(paste0(comp_labels[[2]][1], "/", comp_labels[[2]][2]),
                shiny::div(style = "margin-bottom: 10px;",
                  shiny::downloadButton("download_csv_gender_meta2", "Download CSV")),
                DT::dataTableOutput("upload_diff_genders_meta2_DT")
              ),
              shiny::tabPanel(paste0("Print (", comp_labels[[2]][1], "/", comp_labels[[2]][2], ")"),
                shiny::div(style = "margin-bottom: 10px;",
                  shiny::downloadButton("download_pdf_gender_meta2_GT", "Download PDF")),
                gt::gt_output("upload_diff_genders_meta2_GT")
              ),

              shiny::tabPanel(paste0(comp_labels[[3]][1], "/", comp_labels[[3]][2]),
                shiny::div(style = "margin-bottom: 10px;",
                  shiny::downloadButton("download_csv_gender_meta3", "Download CSV")),
                DT::dataTableOutput("upload_diff_genders_meta3_DT")
              ),
              shiny::tabPanel(paste0("Print (", comp_labels[[3]][1], "/", comp_labels[[3]][2], ")"),
                shiny::div(style = "margin-bottom: 10px;",
                  shiny::downloadButton("download_pdf_gender_meta3_GT", "Download PDF")),
                gt::gt_output("upload_diff_genders_meta3_GT")
              ),

              shiny::tabPanel(paste0(comp_labels[[4]][1], "/", comp_labels[[4]][2]),
                shiny::div(style = "margin-bottom: 10px;",
                  shiny::downloadButton("download_csv_gender_meta4", "Download CSV")),
                DT::dataTableOutput("upload_diff_genders_meta4_DT")
              ),
              shiny::tabPanel(paste0("Print (", comp_labels[[4]][1], "/", comp_labels[[4]][2], ")"),
                shiny::div(style = "margin-bottom: 10px;",
                  shiny::downloadButton("download_pdf_gender_meta4_GT", "Download PDF")),
                gt::gt_output("upload_diff_genders_meta4_GT")
              )
            )
          )
        )
      ),

      # ====================================================================
      # TAB: DATA TABLES - AGE GROUPS
      # ====================================================================
      shinydashboard::tabItem(
        tabName = "DataTables_AgeGroups",
        shiny::fluidRow(
          shiny::column(width = 12,
            shinydashboard::tabBox(
              width = 12, id = "age_tabs", title = "Age Group Analysis",

              shiny::tabPanel(paste0(comp_labels[[1]][1], "/", comp_labels[[1]][2]),
                shiny::div(style = "margin-bottom: 10px;",
                  shiny::downloadButton("download_csv_age_meta1", "Download CSV")),
                DT::dataTableOutput("age_meta1")
              ),
              shiny::tabPanel(paste0("Print (", comp_labels[[1]][1], "/", comp_labels[[1]][2], ")"),
                shiny::div(style = "margin-bottom: 10px;",
                  shiny::downloadButton("download_pdf_age_meta1_GT", "Download PDF")),
                gt::gt_output("age_meta1_GT")
              ),

              shiny::tabPanel(paste0(comp_labels[[2]][1], "/", comp_labels[[2]][2]),
                shiny::div(style = "margin-bottom: 10px;",
                  shiny::downloadButton("download_csv_age_meta2", "Download CSV")),
                DT::dataTableOutput("age_meta2")
              ),
              shiny::tabPanel(paste0("Print (", comp_labels[[2]][1], "/", comp_labels[[2]][2], ")"),
                shiny::div(style = "margin-bottom: 10px;",
                  shiny::downloadButton("download_pdf_age_meta2_GT", "Download PDF")),
                gt::gt_output("age_meta2_GT")
              ),

              shiny::tabPanel(paste0(comp_labels[[3]][1], "/", comp_labels[[3]][2]),
                shiny::div(style = "margin-bottom: 10px;",
                  shiny::downloadButton("download_csv_age_meta3", "Download CSV")),
                DT::dataTableOutput("age_meta3")
              ),
              shiny::tabPanel(paste0("Print (", comp_labels[[3]][1], "/", comp_labels[[3]][2], ")"),
                shiny::div(style = "margin-bottom: 10px;",
                  shiny::downloadButton("download_pdf_age_meta3_GT", "Download PDF")),
                gt::gt_output("age_meta3_GT")
              ),

              shiny::tabPanel(paste0(comp_labels[[4]][1], "/", comp_labels[[4]][2]),
                shiny::div(style = "margin-bottom: 10px;",
                  shiny::downloadButton("download_csv_age_meta4", "Download CSV")),
                DT::dataTableOutput("age_meta4")
              ),
              shiny::tabPanel(paste0("Print (", comp_labels[[4]][1], "/", comp_labels[[4]][2], ")"),
                shiny::div(style = "margin-bottom: 10px;",
                  shiny::downloadButton("download_pdf_age_meta4_GT", "Download PDF")),
                gt::gt_output("age_meta4_GT")
              )
            )
          )
        )
      ),

      # ====================================================================
      # TAB: VOLCANO ANALYSIS — HIDDEN 2026-04-28
      # Restore by uncommenting the menuItem in the sidebar (line ~13)
      # and the tabItem block below.
      # ====================================================================
      # shinydashboard::tabItem(
      #   tabName = "VolcanoTab",
      #   shiny::fluidRow(
      #
      #     # Controls
      #     shinydashboard::box(
      #       title = "Analysis Controls", width = 12,
      #       solidHeader = TRUE, status = "primary", collapsible = TRUE,
      #       shiny::fluidRow(
      #         shiny::column(width = 3,
      #           shiny::radioButtons("filterDatasetsVolc", label = "Compare:",
      #             choices = sapply(COMPARISONS, `[[`, "label"),
      #             selected = COMPARISONS[[1]]$label, inline = FALSE)
      #         ),
      #         shiny::column(width = 3,
      #           shiny::selectizeInput("p0_filter", label = "Parent 0 Code:",
      #             choices = NULL, multiple = TRUE, options = list(placeholder = 'Search codes...'))
      #         ),
      #         shiny::column(width = 3,
      #           shiny::selectizeInput("p2_filter", label = "Parent 2 Code:",
      #             choices = NULL, multiple = TRUE, options = list(placeholder = 'Search codes...'))
      #         ),
      #         shiny::column(width = 3,
      #           shiny::sliderInput("ci_filterVolc", "CI width Max:",
      #             min = 0, max = 7, value = 7, step = 0.1)
      #         )
      #       )
      #     ),
      #
      #     # Plot + Stats
      #     shinydashboard::box(
      #       title = "Diagnosis Distribution: Fold Difference vs. Precision",
      #       width = 12, solidHeader = TRUE, status = "primary",
      #       shiny::fluidRow(
      #         shiny::column(width = 8,
      #           shinycssloaders::withSpinner(
      #             plotly::plotlyOutput("volcanoPlot", height = "750px"),
      #             color = "#3D8FBE", type = 7, size = 0.6
      #           )
      #         ),
      #         shiny::column(width = 4,
      #           shiny::div(
      #             style = "background-color: #f9f9f9; padding: 20px; border-radius: 5px; border: 1px solid #ddd; height: auto;",
      #             shiny::h4("Cohort Comparison Stats",
      #               style = "margin-top: 0; color: #3D8FBE; font-weight: bold;"),
      #             shiny::fluidRow(
      #               shiny::column(width = 6,
      #                 shiny::sliderInput("fold_filterVolc", "Fold diff. Threshold:",
      #                   min = 1, max = 7, value = 1.3, step = 0.1, width = "100%")
      #               ),
      #               shiny::column(width = 6,
      #                 shiny::selectInput("p_val_filter", label = "P-value:",
      #                   choices = c("All (No Filter)" = 10, "p < 0.05" = 0.05,
      #                               "p < 0.01" = 0.01, "p < 0.001" = 0.001),
      #                   selected = 0.05, width = "100%")
      #               )
      #             ),
      #             shiny::hr(style = "border-top: 1px solid #ccc;"),
      #             shiny::fluidRow(
      #               shiny::column(width = 12, shiny::verbatimTextOutput("statsText"))
      #             ),
      #             shiny::hr(style = "border-top: 1px solid #ccc;"),
      #             shiny::fluidRow(
      #               shiny::column(width = 12,
      #                 shiny::p(style = "margin-bottom: 0;",
      #                   shiny::tags$small("Methodology: Volcano Plot maps effect size (Fold Difference) against estimation precision (CI Width).
      #                   Top-left/right quadrants represent the most statistically robust discrepancies.")
      #                 )
      #               )
      #             )
      #           )
      #         )
      #       )
      #     )
      #   )
      # ),

      # ====================================================================
      # TAB: CONTACTS & FUNDING
      # ====================================================================
      shinydashboard::tabItem(
        tabName = "ContactsTab",
        shinydashboard::box(
          width = 12,
          title = "Contact & Funding",
          solidHeader = TRUE, status = "info", class = "no-margin-box",
          shiny::div(style = "padding: 10px;",
            shiny::fluidRow(
              shiny::column(width = 6, class = "text-left",
                shiny::tags$h4(shiny::tags$b("Contact Information")),
                shiny::tags$p("Maarja Pajusalu"),
                shiny::tags$p("maarja.pajusalu@ut.ee"),
                shiny::tags$p(""),
                shiny::tags$h4(shiny::tags$b("Collaborating Institutions")),
                shiny::tags$p(""),
                shiny::tags$a(href = "https://cs.ut.ee/en", target = "_blank",
                  shiny::tags$b("University of Tartu, Institute of Computer Science")),
                shiny::tags$p(""),
                shiny::tags$a(href = "https://health-informatics.cs.ut.ee", target = "_blank",
                  shiny::tags$b("Research Group of Health Informatics")),
                shiny::tags$p(""),
                shiny::tags$img(src = "img/logos.png", width = "300px", class = "image-column")
              ),
              shiny::column(width = 6, class = "image-column",
                shiny::tags$h4(shiny::tags$b("Funding & Acknowledgments")),
                shiny::tags$p(""),
                shiny::tags$img(src = "img/funding.png", class = "fixed-image")
              )
            )
          )
        )
      )
    ) # tabItems end
  ) # fluidRow end
) # dashboardBody end

# --- Assemble UI ---
ui <- shinydashboard::dashboardPage(
  skin = "black",
  header,
  sidebar,
  body
)
