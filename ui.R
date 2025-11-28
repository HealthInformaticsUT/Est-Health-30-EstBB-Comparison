###SHINY UI - Version Est-Health-30 ### -----
# Header ------------
header <- shinydashboard::dashboardHeader(title = "EstBB vs Population")
# Sidebar ------------
sidebar <- shinydashboard::dashboardSidebar(
  shinydashboard::sidebarMenu(
    shinydashboard::menuItem("Plots", tabName = "Visuals", icon = shiny::icon("user")),
    shinydashboard::menuItem("Data tables - Gender", tabName = "DataTables_Gender", icon = shiny::icon("table")),
    shinydashboard::menuItem("Data tables - Age Groups", tabName = "DataTables_AgeGroups", icon = shiny::icon("table")),
    shinydashboard::menuItem("About the Study", tabName = "AboutStudy", icon = icon("info-circle")),
    shinydashboard::menuItem("Contacts", tabName = "ContactsTab", icon = icon("address-book"))
  ),
  hr(),
  br(),
  collapsed = TRUE
)

# Body ------------
body <- shinydashboard::dashboardBody(
  # --- Main Dashboard Body Content ---
  shiny::fluidRow(
    shinyjs::useShinyjs(),

    # --- Unified CSS Styling ---
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("
      /* Global Box Styling (Poster Look) */
      .box.box-primary {
        border-top-color: #cfcfcf; /* Primary Blue */
      }
      .box.box-solid.box-primary > .box-header {
        background-color: #cfcfcf;
        color: #333333;
      }
      .no-margin-box .box-body {
        padding: 10px;
      }
      /* Image Utilities */
      .fixed-image {
        max-width: 100%;
        height: auto;
        display: block;
        margin: 0 auto;
      }
      .center-img {
        display: block;

        margin-right: auto;
      }
      .image-column {
        padding-left: 10px;
        padding-right: 10px;
        text-align: center;
      }

      /* Info Icon Styling */
      .info-icon {
        position: absolute;
        top: 5px;
        right: 35px; /* Adjusted to not overlap with collapse button if present */
        z-index: 100;
        color: white;
      }

      /* Responsive Columns */
      @media (max-width: 1500px) {
        .responsive-col2 { width: 100% !important; }
      }
      @media (max-width: 768px) {
        .image-column { margin-bottom: 15px !important; }
      }
      .justified-text {
        text-align: justify;
      }
      .text-left {
        text-align: left;
      }
      /* Active/Inactive States */
      .active { background-color: #ffffff !important; color: grey; }
      .inactive { background-color: #f8f9fa !important; color: black; }
    "))
    ),

    shinydashboard::tabItems(

      # ==========================================================================
      # TAB ITEM 1: VISUALS (Contains TabBox)
      # ==========================================================================
      shinydashboard::tabItem(
        tabName = "Visuals",

        shinydashboard::tabBox(
          width = 12,
          id = "tabset0",

          # ----------------------------------------------------------------------
          # Sub-Tab: PRs by Datasets and DALY (The 3-column Layout)
          # ----------------------------------------------------------------------
          shiny::tabPanel("PR Distribution",
                          shiny::fluidRow(
                            shinydashboard::box(
                              width = 12,
                              title = "Distribution of Prevalence Ratios Between Datasets + Burden",
                              solidHeader = TRUE,
                              status = "primary",
                              class = "no-margin-box",

                              shiny::fluidRow(
                                # Column 1
                                shiny::column(
                                  width = 4,
                                  plotly::plotlyOutput("p_histogram_bb", height = "24vh"),
                                  plotly::plotlyOutput("p_death_bb", height = "24vh"),
                                  plotly::plotlyOutput("p_disability_bb", height = "24vh")
                                ),
                                # Column 2
                                shiny::column(
                                  width = 4,
                                  plotly::plotlyOutput("p_histogram_bb1", height = "24vh"),
                                  plotly::plotlyOutput("p_death_bb1", height = "24vh"),
                                  plotly::plotlyOutput("p_disability_bb1", height = "24vh")
                                ),
                                # Column 3
                                shiny::column(
                                  width = 4,
                                  plotly::plotlyOutput("p_histogram_bb2", height = "24vh"),
                                  plotly::plotlyOutput("p_death_bb2", height = "24vh"),
                                  plotly::plotlyOutput("p_disability_bb2", height = "24vh")
                                )
                              )
                            )
                          )
          ),
          # ----------------------------------------------------------------------
          # Sub-Tab: Distribution by Gender (M vs F)
          # ----------------------------------------------------------------------
          shiny::tabPanel("PR Distribution by Gender",
                          shiny::fluidRow(
                            shinydashboard::box(
                              width = 12,
                              title = "Distribution of Prevalence Ratios in Males vs Females Between Datasets",
                              solidHeader = TRUE,
                              status = "primary",
                              class = "no-margin-box",

                              shiny::fluidRow(
                                # Column 1: Dataset bb (M top, F bottom)
                                shiny::column(
                                  width = 4,
                                  plotly::plotlyOutput("p_histogram_bbM", height = "36vh"),
                                  plotly::plotlyOutput("p_histogram_bbF", height = "36vh")
                                ),
                                # Column 2: Dataset bb1 (M top, F bottom)
                                shiny::column(
                                  width = 4,
                                  plotly::plotlyOutput("p_histogram_bb1M", height = "36vh"),
                                  plotly::plotlyOutput("p_histogram_bb1F", height = "36vh")
                                ),
                                # Column 3: Dataset bb2 (M top, F bottom)
                                shiny::column(
                                  width = 4,
                                  plotly::plotlyOutput("p_histogram_bb2M", height = "36vh"),
                                  plotly::plotlyOutput("p_histogram_bb2F", height = "36vh")
                                )
                              )
                            )
                          )
          ),
          # ----------------------------------------------------------------------
          # Sub-Tab: PR by Gender
          # ----------------------------------------------------------------------
          shiny::tabPanel("PR Values by Gender per Diagnosis",
                          shiny::fluidRow(
                            shinydashboard::box(
                              width = 12,
                              title = "Values of Prevalence Ratios by Gender per Diagnosis",
                              solidHeader = TRUE,
                              status = "primary",
                              class = "no-margin-box",

                              # Controls
                              shiny::fluidRow(
                                shiny::column(width = 2,
                                              shiny::checkboxGroupInput("gender_filter", "Select genders:",
                                                                        choices = c("M", "N", "Both"),
                                                                        selected = c("M", "N"),
                                                                        inline = TRUE)
                                ),
                                shiny::column(width = 2,
                                              shiny::selectInput("code_filter", label= "Diagnosis:",
                                                                 choices = c("I00-I99"), selected = c("I00-I99"), multiple=FALSE)
                                ),
                                shiny::column(width = 3,
                                              shiny::sliderInput("ci_filter", "CI width:",
                                                                 min = 0, max = 10.0, value = c(0, 0.3), step = 0.1)
                                ),
                                shiny::column(width = 3,
                                              shiny::sliderInput("fold_filter", "Fold diff:",
                                                                 min = 1, max = 10, value = c(1.3, 10), step = 0.1)
                                )
                              ),

                              shiny::hr(),

                              # Plots
                              shiny::fluidRow(
                                shiny::column(width = 6,
                                              shinycssloaders::withSpinner(ggiraph::girafeOutput("forest2_parent2"),
                                                                           color = "#3D8FBE", type = 7, size=0.6),
                                              hr(),
                                              shinycssloaders::withSpinner(ggiraph::girafeOutput("forest2_parent2_genders3"),
                                                                           color = "#3D8FBE", type = 7, size=0.6)
                                ),
                                shiny::column(width = 6,
                                              shinycssloaders::withSpinner(ggiraph::girafeOutput("forest2_parent2_genders2"),
                                                                           color = "#3D8FBE", type = 7, size=0.6),
                                              hr(),
                                              shinycssloaders::withSpinner(ggiraph::girafeOutput("forest2_parent2_genders4"),
                                                                           color = "#3D8FBE", type = 7, size=0.6)
                                )
                              )
                            )
                          )
          ),

          # ----------------------------------------------------------------------
          # Sub-Tab: Heatmaps
          # ----------------------------------------------------------------------
          shiny::tabPanel("PR Values by Age and Gender",
                          shiny::fluidRow(

                            # Filter Box
                            shinydashboard::box(
                              title = "Add or Remove Details",
                              width = 12,
                              solidHeader = TRUE,
                              status = "primary",
                              class = "no-margin-box",
                              collapsible = TRUE,

                              shiny::fluidRow(
                                shiny::column(width = 3,
                                              shiny::radioButtons("filterDatasets", label= "Compare:",
                                                                  choices = c("Est-Health-30 vs EstBB", "Est-Health-30 vs EstBB1", "Est-Health-30 vs EstBB2", "EstBB1 vs EstBB2"),
                                                                  selected = "Est-Health-30 vs EstBB", inline=FALSE)
                                ),
                                shiny::column(width = 3,
                                              shiny::selectInput("filterAge", label= "Age:", choices = NULL, selected = NULL, multiple=TRUE)
                                ),
                                shiny::column(width = 4,
                                              shiny::selectInput("filterCodeGroups0", label= "Diagnosis:", choices = c("I00-I99"), selected = c("I00-I99"), multiple=TRUE)
                                ),
                                shiny::column(width = 2,
                                              shiny::selectInput("filterGender", label= "Gender:", choices = NULL, selected = c("M", "F"), multiple=TRUE)
                                )
                              )
                            ),

                            # Heatmaps
                            shinydashboard::box(
                              width = 12,
                              title = "PR Values by Age and Gender in Diagnoses Groups",
                              solidHeader = TRUE,
                              status = "primary",

                              shiny::fluidRow(
                                shiny::column(width = 6, class = "responsive-col2",
                                              shinycssloaders::withSpinner(ggiraph::girafeOutput("heatmap_meta_avg", width = "auto"), color = "#3D8FBE", type = 7, size=0.6)
                                ),
                                hr(),
                                shiny::column(width = 6, class = "responsive-col2",
                                              shinycssloaders::withSpinner(ggiraph::girafeOutput("heatmap_meta_alph", width = "auto"), color = "#3D8FBE", type = 7, size=0.6)
                                ),
                                hr()
                              )
                            ),

                            # Forest & Points
                            shinydashboard::box(
                              width = 12,
                              solidHeader = TRUE,
                              status = "primary",
                              title = "Detailed View: Prevalence Ratios and Prevalence Values across Years",

                              # Info Icon
                              shiny::div(class = "info-icon",
                                         shiny::tags$span("Methodology",
                                                          shiny::tags$i(id = "methodology", class = "glyphicon glyphicon-info-sign",
                                                                        title = "The meta-analysis conducted in this function uses the metagen function...")
                                         )
                              ),

                              shinycssloaders::withSpinner(ggiraph::girafeOutput("forest1"), color = "#3D8FBE", type = 7, size=0.6),
                              shiny::hr(),
                              shinycssloaders::withSpinner(ggiraph::girafeOutput("pointDiff1"), color = "#3D8FBE", type = 7, size=0.6),
                              shiny::hr()
                            )
                          )
          ),

          # ----------------------------------------------------------------------
          # Sub-Tab: Demography
          # ----------------------------------------------------------------------
          shiny::tabPanel("Demography",
                          shiny::fluidRow(
                            shinydashboard::box(
                              width = 12,
                              title = "Demographic Overview",
                              solidHeader = TRUE,
                              status = "primary",

                              tags$img(src = "img/Fig1-EH30-demography.png", class = "fixed-image", width = "700px", height = "auto")
                            )
                          )
          )
        )
      ),
      # ==========================================================================
      # TAB ITEM 1.5: ABOUT THE STUDY
      # ==========================================================================
      shinydashboard::tabItem(
        tabName = "AboutStudy",

        shinydashboard::box(
          width = 12,
          title = "Estonian Biobank vs General Population: Analysis of Diagnosis Prevalences",
          solidHeader = TRUE,
          status = "info",
          class = "no-margin-box",

          # Main Content Layout
          shiny::div(style = "padding: 10px;",

                     # Abstract Heading
                     tags$h3(tags$b("Abstract")),
                     tags$hr(),

                     # --- Start of Two-Column Layout ---
                     shiny::fluidRow(
                       # Column 1
                       shiny::column(width = 6,
                                     shiny::div(class = "text-justify",
                                                # WHY Section
                                                tags$h4(tags$b("WHY")),
                                                tags$p(
                                                  "When publishing research, it is essential to critically assess whether the study sample is representative of the target population. This study evaluates the representativeness of the Estonian Biobank (EstBB) and its two recruitment waves relative to the general Estonian population, approximated by a 30% national reference dataset (Est-Health-30). To support generalizability and informed study design, we quantify systematic differences in disease prevalence and demographics, with additional consideration of disease burden using DALY metrics to contextualize the potential impact of over- or underrepresented conditions."
                                                ),
                                                # HOW Section
                                                tags$h4(tags$b("HOW")),
                                                tags$p(
                                                  "We analyzed diagnosis prevalence using two Estonian healthcare datasets including the Estonian Biobank (EstBB) and a representative population sample (Est-Health-30). Diagnoses were grouped by ICD-10 codes and stratified by age and gender across 2012–2023, with prevalence ratios computed and synthesized using meta-analysis. To ensure interpretability and robustness, we applied thresholds for fold difference magnitude and confidence interval precision, and visualized results via an interactive dashboard."
                                                )
                                     )
                       ),

                       # Column 2 - MUST USE shiny::column
                       shiny::column(width = 6,
                                     shiny::div(class = "text-justify",
                                                # RESULTS Section
                                                tags$h4(tags$b("RESULTS")),
                                                tags$p(
                                                  "Our analysis reveals that EstBB is enriched for outpatient-managed, non-acute, and preventive care diagnoses, including dermatological, reproductive, endocrine, and mental health conditions. In contrast, severe and high-mortality diseases—such as dementia, stroke sequelae, advanced cancers, and chronic respiratory failure—are consistently underrepresented. Gender-specific trends indicate a higher cardiovascular burden and stronger overrepresentation of diagnoses in men, while women are more representative of the general population. The second wave of recruitment (EstBB2), characterized by simplified procedures and broad outreach, represents a healthier subset with lower prevalence of chronic disease and higher engagement in mental health and preventive care. Conversely, the first wave (EstBB1) shows a specific subcohort with a higher disease burden, particularly among males."
                                                ),
                                                # CONCLUSION Section
                                                tags$h4(tags$b("CONCLUSION")),
                                                tags$p(
                                                  "EstBB is well-suited for genetic association studies, behavioral health research, and longitudinal tracking of chronic conditions. Its strengths include high-quality phenotype data and strong representation of traits with stable outpatient management. However, researchers must critically account for selection bias and demographic skew when modeling population-level disease burden or studying late-stage and high-mortality conditions. The accompanying dashboard enhances transparency and adaptability, allowing researchers to interrogate cohort composition and refine phenotype selection prior to analysis. This analysis supports more accurate interpretation of biobank-derived findings and strengthens the design of future studies using EstBB data."
                                                )
                                     )
                       )
                     )
          )
        )
      ),
      # ==========================================================================
      # ==========================================================================
      # TAB ITEM: DATA TABLES - GENDERS
      # ==========================================================================
      shinydashboard::tabItem(
        tabName = "DataTables_Gender",

        fluidRow(
          column(
            width = 12,

            shinydashboard::tabBox(
              width = 12,
              id = "gender_tabs",
              title = "Gender Analysis",

              # Meta 1
              shiny::tabPanel(paste0(name2, "/", name1),
                              div(style = "margin-bottom: 10px;",
                                  downloadButton("download_csv_gender_meta1", "Download CSV")
                              ),
                              DT::dataTableOutput("upload_diff_genders_meta1_DT")
              ),
              shiny::tabPanel(paste0("Print (", name2, "/", name1, ")"),
                              div(style = "margin-bottom: 10px;",
                                  downloadButton("download_pdf_gender_meta1_GT", "Download PDF")
                              ),
                              gt_output("upload_diff_genders_meta1_GT")
              ),

              # Meta 2
              shiny::tabPanel(paste0(name3, "/", name1),
                              div(style = "margin-bottom: 10px;",
                                  downloadButton("download_csv_gender_meta2", "Download CSV")
                              ),
                              DT::dataTableOutput("upload_diff_genders_meta2_DT")
              ),
              shiny::tabPanel(paste0("Print (", name3, "/", name1, ")"),
                              div(style = "margin-bottom: 10px;",
                                  downloadButton("download_pdf_gender_meta2_GT", "Download PDF")
                              ),
                              gt_output("upload_diff_genders_meta2_GT")
              ),

              # Meta 3
              shiny::tabPanel(paste0(name4, "/", name1),
                              div(style = "margin-bottom: 10px;",
                                  downloadButton("download_csv_gender_meta3", "Download CSV")
                              ),
                              DT::dataTableOutput("upload_diff_genders_meta3_DT")
              ),
              shiny::tabPanel(paste0("Print (", name4, "/", name1, ")"),
                              div(style = "margin-bottom: 10px;",
                                  downloadButton("download_pdf_gender_meta3_GT", "Download PDF")
                              ),
                              gt_output("upload_diff_genders_meta3_GT")
              ),

              # Meta 4
              shiny::tabPanel(paste0(name4, "/", name3),
                              div(style = "margin-bottom: 10px;",
                                  downloadButton("download_csv_gender_meta4", "Download CSV")
                              ),
                              DT::dataTableOutput("upload_diff_genders_meta4_DT")
              ),
              shiny::tabPanel(paste0("Print (", name4, "/", name3, ")"),
                              div(style = "margin-bottom: 10px;",
                                  downloadButton("download_pdf_gender_meta4_GT", "Download PDF")
                              ),
                              gt_output("upload_diff_genders_meta4_GT")
              )
            )
          )
        )
      ),

      # ==========================================================================
      # TAB ITEM: DATA TABLES - AGE GROUPS
      # ==========================================================================
      shinydashboard::tabItem(
        tabName = "DataTables_AgeGroups",

        fluidRow(
          column(
            width = 12,

            shinydashboard::tabBox(
              width = 12,
              id = "age_tabs",
              title = "Age Group Analysis",

              # Meta 1
              shiny::tabPanel(paste0(name2, "/", name1),
                              div(style = "margin-bottom: 10px;",
                                  downloadButton("download_csv_age_meta1", "Download CSV")
                              ),
                              DT::dataTableOutput("age_meta1")
              ),
              shiny::tabPanel(paste0("Print (", name2, "/", name1, ")"),
                              div(style = "margin-bottom: 10px;",
                                  downloadButton("download_pdf_age_meta1_GT", "Download PDF")
                              ),
                              gt_output("age_meta1_GT")
              ),

              # Meta 2
              shiny::tabPanel(paste0(name3, "/", name1),
                              div(style = "margin-bottom: 10px;",
                                  downloadButton("download_csv_age_meta2", "Download CSV")
                              ),
                              DT::dataTableOutput("age_meta2")
              ),
              shiny::tabPanel(paste0("Print (", name3, "/", name1, ")"),
                              div(style = "margin-bottom: 10px;",
                                  downloadButton("download_pdf_age_meta2_GT", "Download PDF")
                              ),
                              gt_output("age_meta2_GT")
              ),

              # Meta 3
              shiny::tabPanel(paste0(name4, "/", name1),
                              div(style = "margin-bottom: 10px;",
                                  downloadButton("download_csv_age_meta3", "Download CSV")
                              ),
                              DT::dataTableOutput("age_meta3")
              ),
              shiny::tabPanel(paste0("Print (", name4, "/", name1, ")"),
                              div(style = "margin-bottom: 10px;",
                                  downloadButton("download_pdf_age_meta3_GT", "Download PDF")
                              ),
                              gt_output("age_meta3_GT")
              ),

              # Meta 4
              shiny::tabPanel(paste0(name4, "/", name3),
                              div(style = "margin-bottom: 10px;",
                                  downloadButton("download_csv_age_meta4", "Download CSV")
                              ),
                              DT::dataTableOutput("age_meta4")
              ),
              shiny::tabPanel(paste0("Print (", name4, "/", name3, ")"),
                              div(style = "margin-bottom: 10px;",
                                  downloadButton("download_pdf_age_meta4_GT", "Download PDF")
                              ),
                              gt_output("age_meta4_GT")
              )
            )
          )
        )
      ),

      # ==========================================================================
      # TAB ITEM: CONTACTS & FUNDING
      # ==========================================================================
      shinydashboard::tabItem(
        tabName = "ContactsTab", # Must match the tabName in the sidebar menuItem

        shiny::fluidRow(
          shinydashboard::box(
            width = 12,
            title = "Contact & Funding",
            solidHeader = TRUE,
            # Set status to "default" for a light-gray header color
            status = "info",
            class = "no-margin-box",

            # Content Row: Displays Logos, Funding, and Contact images/info
            shiny::fluidRow(
              # --- Contact Information Column ---
              shiny::column(width = 6, class = "text-left",
                            tags$h4(tags$b("Contact Information")),
                            tags$p("Maarja Pajusalu"),
                            tags$p("maarja.pajusalu@ut.ee"),
                            tags$p(""),
                            tags$h4(tags$b("Collaborating Institutions")),
                            tags$p(""),
                              tags$a(
                                href = "https://cs.ut.ee/en",
                                target = "_blank", # Opens link in a new tab
                                tags$b("University of Tartu, Institute of Computer Science")
                            ),
                            tags$p(""),
                            tags$a(
                                href = "https://health-informatics.cs.ut.ee",
                                target = "_blank", # Opens link in a new tab
                                tags$b("Research Group of Health Informatics")
                              ),
                            tags$p(""),
                            tags$img(src = "img/logos.png", width="300px", class = "image-column")
              ),

              # --- Funding Sources Column ---
              shiny::column(width = 6, class = "image-column",
                            tags$h4(tags$b("Funding & Acknowledgments")),
                            tags$p(""),
                            tags$img(src = "img/funding.png", class = "fixed-image")
              )
            )
          )
        )
      )
    )
  )
) #  dashboardBody end
# UI ----
ui <- shinydashboard::dashboardPage(
  skin = "black",
  header,
  sidebar,
  body
) # ui end

