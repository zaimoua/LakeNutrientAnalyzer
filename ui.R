library(shiny)
library(shinydashboard)
library(shinyFiles)
library(shinyjs)
library(plotly)
library(shinycssloaders)
library(DT)
library(markdown)
library(leaflet)
library(waiter)


# Define UI for application
ui <- dashboardPage(
  dashboardHeader(
    title = div(
      #tags$img(src = "logo.png", height = "40px", style = "vertical-align: middle; margin-right: 10px;"),
      "Lake Nutrient Analyzer"
    ),
    titleWidth = 450
  ),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Home", tabName = "main_page", icon = icon("home")),
      # Analysis Tools - all as separate menu items
      menuItem("Waterbody Explorer", tabName = "data_extraction", icon = icon("water")),
      menuItem("Data Visualization", tabName = "visualization", icon = icon("chart-bar")),
      menuItem("Trend Analysis", tabName = "trend_analysis", icon = icon("chart-area")),
      menuItem("Geospatial Analysis", tabName = "geospatial_analysis", icon = icon("map")),
      menuItem("Regression Analysis", tabName = "regression_analysis", icon = icon("chart-line")),
      # Information items - all as separate menu items
      menuItem("About", tabName = "about", icon = icon("info")),
      menuItem("Contact", tabName = "contact", icon = icon("envelope"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
      body {
        background-color: #f4f4f9;
        font-family: Arial, sans-serif;
      }
      .btn-primary {
        background-color: #0066cc;
        border-color: #005bb5;
        color: white;
        font-size: 14px;
        font-weight: bold;
        padding: 10px 15px;
      }
      .btn-primary:hover {
        background-color: #005bb5;
        border-color: #004c99;
      }
      .btn-success {
        background-color: #28a745;
        border-color: #218838;
        color: white;
        font-size: 14px;
        font-weight: bold;
        padding: 10px 15px;
      }
      .btn-success:hover {
        background-color: #218838;
        border-color: #1e7e34;
      }
      .btn-link {
        color: #007bff;
        font-size: 14px;
        padding: 10px 15px;
      }
      .help-text {
        font-size: 12px;
        color: #666666;
      }
      .leaflet-tooltip {
        background: rgba(255, 255, 255, 0.8);
        border: none;
        box-shadow: 0 3px 14px rgba(0,0,0,0.4);
      }
      /* .draft-banner {
        background-color: yellow;
        color: red;
        font-weight: bold;
        text-align: center;
        padding: 5px;
        width: 100%;
        z-index: 1000;
      } */
    ")),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/PapaParse/5.3.0/papaparse.min.js"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/xlsx/0.16.9/xlsx.full.min.js")
    ),
    tags$script(HTML("
    $(document).on('click', '#get_started', function() {
      $('a[data-value=\"data_extraction\"]').click();
    });
    $(document).on('click', '#provide_feedback', function() {
      $('a[data-value=\"contact\"]').click();
    });
    $(document).on('click', '#about_tab', function() {
      $('a[data-value=\"about\"]').click();
    });
    $(document).on('click', '#wbid_map_link', function() {
      $('a[data-value=\"geospatial_analysis\"]').click();
    });
  ")),
    
    tabItems(
      tabItem(
        tabName = "main_page",
        fluidRow(
          box(
            title = "Welcome to the Lake Nutrient Analyser",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = FALSE,
            p("The Lake Nutrient Analyser is a comprehensive tool for analyzing nutrient data, assessing impairments, and supporting Total Maximum Daily Load (TMDL) development for lakes. This application integrates data analysis, visualization, and modeling capabilities to assist in water quality management and restoration planning.")
          )
        ),
        fluidRow(
          column(
            width = 6,
            box(
              title = "Key Features",
              status = "info",
              solidHeader = TRUE,
              width = NULL,
              collapsible = TRUE,
              collapsed = FALSE,
              tags$ul(
                tags$li(tags$strong("Data Extraction and Analysis:"), " Retrieve and analyze water quality data for specific lakes, calculating annual geometric means for nutrient parameters."),
                tags$li(tags$strong("Impairment Assessment:"), " Evaluate lakes against Numeric Nutrient Criteria (NNC) to identify exceedances."),
                tags$li(tags$strong("Visualization Tools:"), " Generate time series plots, XY plots, and nutrient analysis charts to explore water quality trends and relationships."),
                tags$li(tags$strong("Trend Analysis:"), " Conduct Mann-Kendall trend tests to identify significant changes in water quality over time."),
                tags$li(tags$strong("Regression Analysis:"), " Perform regression analysis to understand relationships between nutrient parameters."),
                tags$li(tags$strong("TMDL Calculations:"), " Determine required nutrient reductions and allocate loads for TMDL development."),
                tags$li(tags$strong("Geospatial Integration:"), " Incorporate land use and watershed data for comprehensive analysis.")
              )
            )
          ),
          column(
            width = 6,
            box(
              title = "Benefits for Water Quality Managers",
              status = "success",
              solidHeader = TRUE,
              width = NULL,
              collapsible = TRUE,
              collapsed = FALSE,
              tags$ul(
                tags$li(tags$strong("Comprehensive Analysis:"), " Integrate multiple data sources and analysis techniques in one platform."),
                tags$li(tags$strong("Efficient Assessment:"), " Quickly identify impaired waters and evaluate nutrient reduction needs."),
                tags$li(tags$strong("Data-Driven Decisions:"), " Leverage advanced analytics for informed water quality management strategies."),
                tags$li(tags$strong("Flexible Exploration:"), " Easily examine various scenarios and hypothesis through interactive tools."),
                tags$li(tags$strong("Standardized Approach:"), " Ensure consistency in water quality assessment and TMDL development across lakes.")
              )
            )
          )
        ),
        fluidRow(
          box(
            title = "Getting Started",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            collapsed = FALSE,
            div(style = "padding: 10px;",
                h4("Steps for Using the Lake Nutrient Analyser"),
                tags$ol(
                  tags$li(tags$b("Select Lake(s):"), " Choose target lakes using the interactive map or WBID search."),
                  tags$li(tags$b("Extract Data:"), " Retrieve water quality data for the selected lakes and time period."),
                  tags$li(tags$b("Assess Water Quality:"), " Analyze nutrient levels against NNC and visualize trends."),
                  tags$li(tags$b("Conduct Advanced Analyses:"), " Perform trend analysis, regression modeling, and TMDL calculations as needed."),
                  tags$li(tags$b("Explore Results:"), " Interact with generated plots and tables to gain insights."),
                  tags$li(tags$b("Generate Reports:"), " Download data, analysis results, and summaries for further use.")
                ),
                p("This tool provides a streamlined workflow for comprehensive lake water quality analysis and TMDL support."),
                br(),
                div(style = "text-align: center;",
                    actionButton("get_started", "Begin Lake Analysis", icon = icon("play"), class = "btn-lg btn-success")
                )
            )
          )
        )
      ),
      tabItem(
        tabName = "data_extraction",
        fluidRow(
          box(
            title = "Instructions for Data Extraction",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            icon = icon("info-circle"),
            div(
              style = "padding: 10px;",
              p(tags$b("Step 1:"), " Select WBIDs by clicking on the map or using the search function."),
              p(tags$b("Step 2:"), " Choose parameters for nutrient analysis."),
              p(tags$b("Step 3:"), " Click 'Run' to extract data for selected WBIDs."),
              p(tags$b("Step 4:"), " Review results and download data as needed."),
              p(tags$b("Note:"), " Refer to 'Parameter Information' for details on each nutrient parameter.")
            )
          )
        ),
        fluidRow(
          column(
            width = 6,
            box(
              title = "WBID Selection Map",
              width = NULL,
              solidHeader = TRUE,
              status = "primary",
              div(
                style = "margin-bottom: 15px;",
                p("Click on lake polygons to select/deselect WBIDs.")
              ),
              leafletOutput("all_wbids_map", height = 500),
              div(
                style = "margin-top: 10px;",
                textOutput("selectedWBIDsInfo")
              )
            )
          ),
          column(
            width = 6,
            box(
              title = "Data Extraction Options",
              width = NULL,
              solidHeader = TRUE,
              status = "primary",
              selectizeInput("wbid", "Search and Select WBID(s):", 
                             choices = NULL, 
                             multiple = TRUE,
                             options = list(
                               placeholder = 'Search and select WBID(s)...',
                               onInitialize = I('function() { this.setValue(""); }'),
                               plugins = list('remove_button', 'restore_on_backspace'),
                               valueField = 'WBID',
                               labelField = 'label',
                               searchField = c('WBID', 'WATERBODY_NAME'),
                               render = I("{
                           option: function(item, escape) {
                             return '<div>' +
                               '<span class=\"wbid\">' + escape(item.WBID) + '</span> - ' +
                               '<span class=\"waterbody-name\">' + escape(item.WATERBODY_NAME) + '</span>' +
                             '</div>';
                           }
                         }")
                             )),
              selectizeInput("parameters", "Select Nutrient Parameters:", 
                             choices = c("CHLAC", "TN", "TP", "COLOR", "ALK", "COND", "TEMP", "DO", "PH", "PORTH", 
                                         "NH4", "NO3O2", "BOD", "DOSAT", "UNNH4", "PORD", "TKN", "CHLA", "TORTH"), 
                             multiple = TRUE, 
                             options = list(placeholder = "Select Parameters")),
              selectInput("lakewatch", "Include Lakewatch Data?", 
                          choices = c("Yes", "No"),
                          selected = "No"), 
              checkboxInput("period_of_record", "Use Full Period of Record", value = TRUE),
              conditionalPanel(
                condition = "!input.period_of_record",
                sliderInput("year_range", "Select Year Range:",
                            min = 1900, max = 2024, value = c(2000, 2024), step = 1, sep = "")
              ),
              textInput("station_id", "Enter Station ID (optional):", value = "", 
                        placeholder = "Optional Station ID"),
              actionButton("runButton", "Run Extraction", class = "btn-lg btn-primary"),
              tags$br(),
              tags$br(),
              uiOutput("downloadButtons"),
              tags$br(),
              textOutput("completionMessage")
            )
          )
        ),
        fluidRow(
          column(
            width = 6,
            uiOutput("wbid_info_and_selector")
          ),
          column(
            width = 6,
            uiOutput("lake_assessment_and_exceedances")
          )
        ),
        
        fluidRow(
          column(12,
                 uiOutput("exceedances_detail")
          )
        ),
        
        fluidRow(
          box(
            title = "Parameter Information",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            icon = icon("info-circle"),
            div(style = "padding: 10px;",
                tags$ul(
                  tags$li(tags$b("ALK"), ": Alkalinity (mg/l) - Measures the water's capacity to neutralize acid, important for maintaining stable pH levels."),
                  tags$li(tags$b("BOD"), ": Biochemical Oxygen Demand (mg/l) - Indicates the amount of oxygen required for microbial decomposition of organic matter in water."),
                  tags$li(tags$b("CHLA"), ": Chlorophyll a (μg/l) - A measure of the amount of algae in water. High levels indicate nutrient enrichment and potential eutrophication."),
                  tags$li(tags$b("CHLAC"), ": Corrected Chlorophyll a (μg/l) - Adjusted measure of chlorophyll a, accounting for certain interferences."),
                  tags$li(tags$b("COLOR"), ": Color (PCU) - Platinum Cobalt Units, measures the coloration of water which can affect light penetration and aquatic life."),
                  tags$li(tags$b("COND"), ": Specific Conductance (μmhos/cm) - Indicates the water's ability to conduct electricity, related to the concentration of dissolved salts."),
                  tags$li(tags$b("DO"), ": Dissolved Oxygen (mg/l) - Essential for aquatic life, low levels can indicate poor water quality."),
                  tags$li(tags$b("DOSAT"), ": Dissolved Oxygen Saturation (%) - Percentage of oxygen saturation in water, indicating the balance between oxygen supply and consumption."),
                  tags$li(tags$b("NH4"), ": Ammonium (mg/l) - A form of nitrogen that can be toxic to aquatic life at high concentrations. Indicates pollution from wastewater or manure."),
                  tags$li(tags$b("NO3O2"), ": Nitrate (NO3) + Nitrite (NO2) Nitrogen as N (mg/l) - Forms of nitrogen that are readily available for plant uptake. Elevated levels can indicate pollution."),
                  tags$li(tags$b("PORD"), ": Dissolved Organic Phosphorus (mg/l) - A measure of organic phosphorus in water."),
                  tags$li(tags$b("PORTH"), ": Total Orthophosphate (mg/l) - The amount of orthophosphate phosphorus in the water, important for understanding nutrient levels."),
                  tags$li(tags$b("TEMP"), ": Temperature (°C) - Affects chemical and biological processes in water, important for maintaining suitable conditions for aquatic life."),
                  tags$li(tags$b("TKN"), ": Total Kjeldahl Nitrogen (mg/l) - Measures the total concentration of organic nitrogen and ammonia. Used to assess the nitrogen content in water."),
                  tags$li(tags$b("TN"), ": Total Nitrogen (mg/l) - Includes all forms of nitrogen (nitrate, nitrite, ammonia, and organic nitrogen). High levels can cause excessive plant growth and degrade water quality."),
                  tags$li(tags$b("TORTH"), ": Total Orthophosphate as P (mg/l) - Measures the concentration of orthophosphate phosphorus, essential for aquatic plants but can lead to algal blooms in excess."),
                  tags$li(tags$b("TP"), ": Total Phosphorus (mg/l) - Includes all forms of phosphorus in water. High levels can lead to excessive algal growth and eutrophication."),
                  tags$li(tags$b("UNNH4"), ": Un-ionized Ammonia (mg/l) - The toxic form of ammonia, particularly harmful to aquatic life.")
                )
            )
          )
        )
      ),
      tabItem(
        tabName = "visualization",
        fluidRow(
          column(12,
                 tabBox(
                   id = "viz_tabs",
                   width = NULL,
                   tabPanel("1. Data Summary",
                            fluidRow(
                              column(3,
                                     box(
                                       title = "Data Selection",
                                       status = "primary",
                                       solidHeader = TRUE,
                                       width = NULL,
                                       uiOutput("viz_wbid_selector"),
                                       selectInput("overview_param", "Select Parameter:", choices = NULL, selected = NULL),
                                       radioButtons("data_type", "Data Type:",
                                                    choices = c("Raw Data" = "raw", "Yearly Geomeans" = "geomean"),
                                                    selected = "raw")
                                     ),
                                     box(
                                       title = "Quick Statistics",
                                       status = "info",
                                       solidHeader = TRUE,
                                       width = NULL,
                                       withSpinner(tableOutput("quick_stats"))
                                     )
                              ),
                              column(9,
                                     fluidRow(
                                       column(6,
                                              box(
                                                title = "Histogram",
                                                status = "primary",
                                                solidHeader = TRUE,
                                                width = NULL,
                                                withSpinner(plotlyOutput("param_histogram", height = "400px"))
                                              )
                                       ),
                                       column(6,
                                              box(
                                                title = "Box Plot",
                                                status = "primary",
                                                solidHeader = TRUE,
                                                width = NULL,
                                                withSpinner(plotlyOutput("param_boxplot", height = "400px"))
                                              )
                                       )
                                     )
                              )
                            ),
                            fluidRow(
                              column(12,
                                     uiOutput("comprehensive_data_display")
                              )
                            )
                   ),
                   tabPanel("2. Time Series",
                            fluidRow(
                              column(3,
                                     box(
                                       title = "Time Series Options",
                                       status = "primary",
                                       solidHeader = TRUE,
                                       width = NULL,
                                       selectizeInput("ts_wbid", "Select WBID:", choices = NULL),
                                       selectInput("ts_data_type", "Data Type:", 
                                                   choices = c("Raw Data" = "raw", "Annual Geometric Means" = "agm")),
                                       selectInput("ts_param", "Parameter:", choices = NULL),
                                       sliderInput("ts_year_range", "Year Range:",
                                                   min = 1980, max = as.integer(format(Sys.Date(), "%Y")),
                                                   value = c(2000, as.integer(format(Sys.Date(), "%Y"))),
                                                   step = 1, sep = "")
                                     )
                              ),
                              column(9,
                                     box(
                                       title = "Time Series Plot",
                                       status = "primary",
                                       solidHeader = TRUE,
                                       width = NULL,
                                       withSpinner(plotlyOutput("tsPlot", height = "600px"))
                                     )
                              )
                            )
                   ),
                   tabPanel("3. Correlation Matrix",
                            fluidRow(
                              column(3,
                                     box(
                                       title = "Correlogram Options",
                                       status = "primary",
                                       solidHeader = TRUE,
                                       width = NULL,
                                       selectizeInput("corr_wbid", "Select WBID:", choices = NULL),
                                       selectInput("corr_data_type", "Data Type:", 
                                                   choices = c("Raw Data" = "raw", "Annual Geometric Means" = "agm")),
                                       selectizeInput("correlogram_params", "Select Parameters:", 
                                                      choices = NULL, 
                                                      multiple = TRUE,
                                                      selected = c("CHLAC", "TN", "TP", "COLOR", "COND", "ALK", "DO")),
                                       selectInput("correlogram_method", "Correlation Method:", 
                                                   choices = c("Pearson" = "pearson", "Spearman" = "spearman")),
                                       
                                     ),
                                     box(
                                       title = "About Correlation Matrix",
                                       status = "info",
                                       solidHeader = TRUE,
                                       width = NULL,
                                       uiOutput("correlogram_general_explanation")
                                     )
                              ),
                              column(9,
                                     box(
                                       title = "Correlation Matrix",
                                       status = "primary",
                                       solidHeader = TRUE,
                                       width = NULL,
                                       withSpinner(plotOutput("correlogram", height = "600px")),
                                       uiOutput("correlogram_method_explanation")
                                     )
                              )
                            )
                   ),
                   tabPanel("4. Scatter Plot",
                            fluidRow(
                              column(3,
                                     box(
                                       title = "Scatter Plot Options",
                                       status = "primary",
                                       solidHeader = TRUE,
                                       width = NULL,
                                       selectizeInput("xy_wbid", "Select WBID:", choices = NULL),
                                       selectInput("xy_data_type", "Data Type:", 
                                                   choices = c("Raw Data" = "raw", "Annual Geometric Means" = "agm")),
                                       selectizeInput("x_param", "X Parameter:", choices = NULL),
                                       selectizeInput("y_param", "Y Parameter:", choices = NULL),
                                       checkboxInput("add_trend_line", "Add Trend Line", value = FALSE)
                                     )
                              ),
                              column(9,
                                     box(
                                       title = "Scatter Plot",
                                       status = "primary",
                                       solidHeader = TRUE,
                                       width = NULL,
                                       withSpinner(plotlyOutput("xyPlot", height = "600px"))
                                     )
                              )
                            )
                   ),
                   tabPanel("5. Nutrient Relationships",
                            fluidRow(
                              column(3,
                                     box(
                                       title = "Nutrient Analysis Options",
                                       status = "primary",
                                       solidHeader = TRUE,
                                       width = NULL,
                                       selectizeInput("nutrient_wbid", "Select WBID:", choices = NULL),
                                       selectInput("nutrient_data_type", "Data Type:", 
                                                   choices = c("Raw Data" = "raw", "Annual Geometric Means" = "agm")),
                                       checkboxInput("add_trend_line_nutrients", "Add Trend Lines", value = FALSE)
                                     )
                              ),
                              column(9,
                                     box(
                                       title = "Nutrient Relationship Plots",
                                       status = "primary",
                                       solidHeader = TRUE,
                                       width = NULL,
                                       fluidRow(
                                         column(6, withSpinner(plotlyOutput("nutrientPlot1", height = "300px"))),
                                         column(6, withSpinner(plotlyOutput("nutrientPlot2", height = "300px")))
                                       ),
                                       fluidRow(
                                         column(6, withSpinner(plotlyOutput("nutrientPlot3", height = "300px"))),
                                         column(6, withSpinner(plotlyOutput("nutrientPlot4", height = "300px")))
                                       )
                                     )
                              )
                            )
                   )
                 )
          )
        )
      ),
      tabItem(
        tabName = "trend_analysis",
        fluidRow(
          box(
            title = "Instructions for Trend Analysis",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            icon = icon("info-circle"),
            div(style = "padding: 10px;",
                p("1. Select the WBID and date range for the trend analysis."),
                p("2. Choose the type of trend analysis to perform: Monthly, Seasonal, or Annual."),
                p("3. Run the trend analysis and review the summary of results for all selected parameters."),
                p("4. Click on a parameter in the summary plot to view detailed results.")
            )
          )
        ),
        fluidRow(
          column(6,
                 box(
                   title = "Trend Analysis Parameters",
                   status = "primary",
                   solidHeader = TRUE,
                   width = 12,
                   selectInput("trend_wbid", "Select WBID:", choices = NULL),
                   # Replace numeric inputs with sliderInput for year range
                   sliderInput("trend_year_range", "Select Year Range:",
                               min = 1960, max = 2024, value = c(2000, 2024), step = 1, sep = ""),
                   checkboxInput("monthly_trend", "Monthly Trend", value = TRUE),
                   checkboxInput("seasonal_trend", "Seasonal Trend", value = TRUE),
                   checkboxInput("annual_trend", "Annual Trend", value = TRUE),
                   actionButton("runTrendAnalysis", "Run Trend Analysis", class = "btn-lg btn-primary"),
                   actionButton("resetTrendAnalysis", "Reset", class = "btn-lg btn-default")
                 )
          ),
          column(6,
                 box(
                   title = "Mann-Kendall Test Explanation",
                   status = "info",
                   solidHeader = TRUE,
                   width = 12,
                   div(style = "padding: 10px;",
                       p(tags$b("Mann-Kendall Test:"), " is a non-parametric test used to identify trends in time series data. It is particularly useful for detecting monotonic trends in environmental data."),
                       p(tags$b("Key Outputs:"), 
                         tags$ul(
                           tags$li(tags$b("Mann-Kendall Statistic:"), " A measure of the trend. A positive value indicates an increasing trend, while a negative value indicates a decreasing trend."),
                           tags$li(tags$b("P-value:"), " Represents the significance level of the trend. A low p-value (typically < 0.05) indicates a statistically significant trend."),
                           tags$li(tags$b("Trend Direction:"), " Indicates whether the trend is increasing, decreasing, or there is no trend.")
                         )
                       )
                   )
                 )
          )
        ),
        fluidRow(
          box(
            title = "Analysis Progress and Summary",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            uiOutput("trend_analysis_progress"),
            #verbatimTextOutput("trend_analysis_summary"),
            plotOutput("trend_summary_plot", click = "trend_summary_plot_click")
          )
        ),
        fluidRow(
          box(
            title = "Detailed Results",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            selectInput("trend_timescale", "Select Timescale:",
                        choices = c("All", "Annual", "Seasonal", "Monthly"),
                        selected = "All"),
            uiOutput("trend_detailed_results")
          )
        )
      ),
      tabItem(
        tabName = "geospatial_analysis",
        fluidRow(
          box(
            title = "Lake Group Selection for Regression Analysis",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            tabsetPanel(
              id = "geospatial_tabs",
              tabPanel("Lake Analysis",
                       fluidRow(
                         column(3,
                                box(
                                  title = "Lake Selection Tools", 
                                  status = "primary",
                                  solidHeader = TRUE,
                                  width = NULL,
                                  selectizeInput("wbid_geo", "Search for a Lake (WBID):", 
                                                 choices = NULL, 
                                                 options = list(
                                                   placeholder = 'Type to search...',
                                                   onInitialize = I('function() { this.setValue(""); }'),
                                                   valueField = 'WBID',
                                                   labelField = 'label',
                                                   searchField = c('WBID', 'WATERBODY_NAME'),
                                                   render = I("{
                      option: function(item, escape) {
                        return '<div>' +
                          '<span class=\"wbid\">' + escape(item.WBID) + '</span> - ' +
                          '<span class=\"waterbody-name\">' + escape(item.WATERBODY_NAME) + '</span>' +
                        '</div>';
                      }
                    }")
                                                 )
                                  ),
                                  div(
                                    style = "display: flex; justify-content: space-between; margin-bottom: 15px;",
                                    actionButton("submit_geo", "Find Similar Lakes", icon = icon("filter"), class = "btn-success"),
                                    actionButton("reset_geo", "Reset", icon = icon("undo"), class = "btn-warning")
                                  ),
                                  hr(),
                                  h4("Selected for Regression:", style = "margin-top: 0;"),
                                  selectizeInput("selected_wbids_for_regression", NULL,
                                                 choices = NULL, multiple = TRUE,
                                                 options = list(
                                                   placeholder = 'No lakes selected',
                                                   plugins = list('remove_button')
                                                 )
                                  ),
                                  div(style = "display: flex; justify-content: space-between;",
                                      actionButton("use_for_regression", "Analyze Group", icon = icon("chart-line"), class = "btn-success"),
                                      actionButton("zoomFlorida_geo", "Zoom Out", icon = icon("search-minus"), class = "btn-info")
                                  )
                                ),
                                
                                box(
                                  title = "How This Works",
                                  status = "info",
                                  solidHeader = TRUE,
                                  width = NULL,
                                  collapsible = TRUE,
                                  collapsed = FALSE,
                                  tags$ul(
                                    tags$li(tags$b("Select a Lake:"), " Search by WBID or name"),
                                    tags$li(tags$b("Find Similar:"), " Finds lakes of same type in same region"),
                                    tags$li(tags$b("Select on Map:"), " Click lakes to select/deselect"),
                                    tags$li(tags$b("Analyze Group:"), " Use selected lakes for regression")
                                  )
                                )
                         ),
                         column(9,
                                box(
                                  title = "Interactive Map",
                                  width = NULL,
                                  status = "primary",
                                  solidHeader = TRUE,
                                  div(
                                    style = "text-align: center; color: #666; margin-bottom: 10px;",
                                    "Click on lakes to select/deselect for regression analysis"
                                  ),
                                  leafletOutput("geospatial_map", height = 500)
                                ),
                                
                                box(
                                  title = "Selected Lakes",
                                  width = NULL,
                                  status = "success",
                                  solidHeader = TRUE,
                                  DTOutput("filtered_lakes_table")
                                )
                         )
                       ),
                       
                       fluidRow(
                         column(12,
                                uiOutput("agm_data_display")
                         )
                       )
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "regression_analysis",
        fluidRow(
          box(
            title = "Regression Model Configuration",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            fluidRow(
              column(
                width = 4,
                h4("Data Selection"),
                radioButtons(
                  "regression_type",
                  "Analysis Type:",
                  choices = c("Single WBID" = "single", "Multiple WBIDs" = "multiple"),
                  inline = TRUE
                ),
                conditionalPanel(
                  condition = "input.regression_type == 'single'",
                  selectizeInput(
                    "reg_wbid",
                    "Select WBID:",
                    choices = NULL,
                    options = list(placeholder = "Select a WBID")
                  )
                ),
                conditionalPanel(
                  condition = "input.regression_type == 'multiple'",
                  radioButtons(
                    "wbid_selection_method",
                    "WBID Selection Method:",
                    choices = c("Geospatial Selection" = "geospatial", "Manual Input" = "manual"),
                    inline = TRUE
                  ),
                  conditionalPanel(
                    condition = "input.wbid_selection_method == 'manual'",
                    selectizeInput(
                      "manual_combined_wbids",
                      "Search and Select WBIDs:",
                      choices = NULL,
                      multiple = TRUE,
                      options = list(
                        placeholder = "Type to search WBIDs",
                        onInitialize = I('function() { this.setValue(""); }')
                      )
                    )
                  ),
                  conditionalPanel(
                    condition = "input.wbid_selection_method == 'geospatial'",
                    selectizeInput(
                      "geospatial_combined_wbids",
                      "Geospatially Selected WBIDs:",
                      choices = NULL,
                      multiple = TRUE,
                      options = list(placeholder = "WBIDs from geospatial filter")
                    )
                  )
                ),
                numericInput(
                  "start_year",
                  "Start Year:",
                  value = 2000,
                  min = 1990,
                  max = as.integer(format(Sys.Date(), "%Y"))
                ),
                numericInput(
                  "end_year",
                  "End Year:",
                  value = as.integer(format(Sys.Date(), "%Y")),
                  min = 1990,
                  max = as.integer(format(Sys.Date(), "%Y"))
                )
              ),
              column(
                width = 4,
                h4("Variables"),
                selectizeInput(
                  "regression_response",
                  "Response Variable:",
                  choices = c("CHLAC", "TN", "TP"),  # Adjust based on your data
                  selected = "CHLAC",
                  options = list(placeholder = "Select response variable")
                ),
                selectizeInput(
                  "regression_explanatory1",
                  "Explanatory Variable 1:",
                  choices = c("TN", "TP"),  # Adjust based on your data
                  selected = "TN",
                  options = list(placeholder = "Select first explanatory variable")
                ),
                checkboxInput(
                  "enable_explanatory2",
                  "Add Second Explanatory Variable",
                  value = TRUE
                ),
                conditionalPanel(
                  condition = "input.enable_explanatory2",
                  selectizeInput(
                    "regression_explanatory2",
                    "Explanatory Variable 2:",
                    choices = c("TN", "TP"),  # Adjust based on your data
                    selected = "TP",
                    options = list(placeholder = "Select second explanatory variable")
                  )
                )
              ),
              column(
                width = 4,
                h4("Options & Actions"),
                checkboxInput(
                  "include_interaction",
                  "Include Interaction Term",
                  value = FALSE
                ),
                checkboxInput(
                  "log_transform",
                  "Log Transform Response",
                  value = FALSE
                ) %>% 
                  shinyBS::tipify(
                    title = "Applies log transformation to CHLAC in regression and TMDL calculations.",
                    placement = "left"
                  ),
                actionButton(
                  "runShapiroWilk",
                  "Check Normality",
                  icon = icon("check-circle"),
                  class = "btn-info btn-block",
                  style = "margin-top: 10px;"
                ),
                actionButton(
                  "runRegression",
                  "Run Regression",
                  icon = icon("play"),
                  class = "btn-success btn-block",
                  style = "margin-top: 10px;"
                )
              )
            )
          )
        ),
        fluidRow(
          tabBox(
            id = "regressionResults",
            width = 12,
            tabPanel(
              "Summary",
              fluidRow(
                column(6, tableOutput("regressionSummary") %>% withSpinner()),
                column(
                  6,
                  valueBoxOutput("r_squared_box", width = 6),
                  valueBoxOutput("adj_r_squared_box", width = 6),
                  valueBoxOutput("f_statistic_box", width = 6),
                  valueBoxOutput("p_value_box", width = 6)
                )
              ),
              fluidRow(
                column(12, plotlyOutput("regressionPlot", height = "500px") %>% withSpinner())
              )
            ),
            tabPanel(
              "Diagnostics",
              fluidRow(
                column(6, plotlyOutput("resid_vs_fitted", height = "300px") %>% withSpinner()),
                column(6, plotlyOutput("qq_plot", height = "300px") %>% withSpinner())
              ),
              fluidRow(
                column(6, plotlyOutput("scale_location", height = "300px") %>% withSpinner()),
                column(6, plotlyOutput("resid_vs_leverage", height = "300px") %>% withSpinner())
              ),
              fluidRow(
                column(4, tableOutput("vif_table") %>% withSpinner()),
                column(4, verbatimTextOutput("bp_test") %>% withSpinner()),
                column(4, verbatimTextOutput("dw_test") %>% withSpinner())
              )
            ),
            tabPanel(
              "Interpretation",
              fluidRow(
                box(title = "Model Overview", width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                    uiOutput("modelOverview")),
                box(title = "Model Performance", width = 6, status = "info", solidHeader = TRUE, collapsible = TRUE,
                    uiOutput("modelPerformance")),
                box(title = "Coefficient Interpretation", width = 6, status = "success", solidHeader = TRUE, collapsible = TRUE,
                    uiOutput("coefficientInterpretation")),
                box(title = "Assumption Checks", width = 12, status = "warning", solidHeader = TRUE, collapsible = TRUE,
                    uiOutput("normalityCheck"), uiOutput("homoscedasticityCheck"),
                    uiOutput("multicollinearityCheck"), uiOutput("autocorrelationCheck")),
                box(title = "Conclusion", width = 12, status = "danger", solidHeader = TRUE, collapsible = TRUE,
                    uiOutput("conclusionInterpretation"))
              )
            ),
            tabPanel(
              "TMDL Calculations",
              fluidRow(
                column(12, 
                       uiOutput("tmdl_instructions")
                )
              ),
              fluidRow(
                column(
                  width = 3,
                  box(
                    title = "TMDL Parameters",
                    status = "primary",
                    solidHeader = TRUE,
                    width = NULL,
                    checkboxGroupInput(
                      "impaired_nutrients",
                      "Impaired Nutrients:",
                      choices = c("Total Nitrogen (TN)" = "TN", "Total Phosphorus (TP)" = "TP"),
                      selected = "TN",
                      inline = FALSE
                    ) %>% 
                      shinyBS::tipify(
                        title = "Select nutrients to target; must be included in the regression model.",
                        placement = "right"
                      ),
                    numericInput(
                      "chlac_target",
                      "CHLAC Target (µg/L):",
                      value = 20,
                      min = 0,
                      step = 0.1
                    ) %>% 
                      shinyBS::tipify(
                        title = "Target CHLAC; must be positive. Defaults to lake criterion if blank or 0.",
                        placement = "right"
                      ),
                    radioButtons(
                      "reduction_scenario",
                      "Reduction Scenario:",
                      choices = c(
                        "Reduce Both TN and TP" = "both",
                        "Reduce TN Only" = "tn_only",
                        "Reduce TP Only" = "tp_only",
                        "Custom Reduction" = "custom"
                      ),
                      selected = "both"
                    ) %>% 
                      shinyBS::tipify(
                        title = "Choose how to prioritize nutrient reductions.",
                        placement = "right"
                      ),
                    # Conditional panel for custom reductions
                    conditionalPanel(
                      condition = "input.reduction_scenario == 'custom'",
                      numericInput(
                        "custom_tn_reduction",
                        "TN Reduction (%):",
                        value = 50,
                        min = 0,
                        max = 100,
                        step = 1
                      ),
                      numericInput(
                        "custom_tp_reduction",
                        "TP Reduction (%):",
                        value = 50,
                        min = 0,
                        max = 100,
                        step = 1
                      ) %>% 
                        shinyBS::tipify(
                          title = "Specify custom percent reductions. Warning will show if CHLAC target isn't met.",
                          placement = "right"
                        )
                    ),
                    checkboxInput(
                      "use_paleo_tp",
                      "Use Paleo TP",
                      value = FALSE
                    ) %>% 
                      shinyBS::tipify(
                        title = "Use paleolimnological TP instead of lake criterion. Must be positive if selected.",
                        placement = "right"
                      ),
                    conditionalPanel(
                      condition = "input.use_paleo_tp",
                      numericInput(
                        "tp_paleo",
                        "Paleo TP (mg/L):",
                        value = 0.03,
                        min = 0,
                        step = 0.001
                      )
                    ),
                    actionButton(
                      "calculate_tmdl",
                      "Calculate TMDL",
                      icon = icon("calculator"),
                      class = "btn-primary btn-block",
                      style = "margin-top: 15px;"
                    ) %>% 
                      shinyBS::tipify(
                        title = "Requires data extraction and regression analysis to be completed first.",
                        placement = "bottom"
                      ),
                    helpText("TMDL calculation reduces nutrients based on selected scenario to meet the CHLAC target. Targets are capped at a minimum value of 0.01 mg/L.")
                  ),
                  box(
                    title = "Lake Information",
                    status = "info",
                    solidHeader = TRUE,
                    width = NULL,
                    textOutput("lake_type_info"),
                    helpText("Based on lake color and alkalinity.")
                  )
                ),
                column(
                  width = 9,
                  fluidRow(
                    valueBoxOutput("current_tn_box", width = 4),
                    valueBoxOutput("target_tn_box", width = 4),
                    valueBoxOutput("percent_reduction_tn_box", width = 4)
                  ),
                  fluidRow(
                    valueBoxOutput("current_tp_box", width = 4),
                    valueBoxOutput("target_tp_box", width = 4),
                    valueBoxOutput("percent_reduction_tp_box", width = 4)
                  ),
                  fluidRow(
                    box(
                      title = "Regression Results",
                      status = "success",
                      solidHeader = TRUE,
                      width = 8,
                      div(
                        strong("Equation:"), 
                        style = "margin-bottom: 5px;",
                        textOutput("regression_equation"), 
                        style = "font-size: 14px; margin-bottom: 15px;"
                      ),
                      div(
                        strong("Confidence Interval:"), 
                        style = "margin-bottom: 5px;",
                        textOutput("confidence_interval")
                      )
                    ),
                    box(
                      title = "Plot Options",
                      status = "info",
                      solidHeader = TRUE,
                      width = 4,
                      checkboxInput("show_confidence", "Show Confidence Interval", value = TRUE),
                      checkboxInput("show_target_lines", "Show Target Lines", value = TRUE)
                    )
                  ),
                  fluidRow(
                    box(
                      title = "TMDL Analysis Plots",
                      status = "primary",
                      solidHeader = TRUE,
                      width = 12,
                      plotlyOutput("tmdl_plot", height = "600px") %>% withSpinner()
                    )
                  ),
                  fluidRow(
                    box(
                      title = "TMDL Summary",
                      status = "success",
                      solidHeader = TRUE,
                      width = 12,
                      uiOutput("tmdl_summary")
                    )
                  ),
                  conditionalPanel(
                    condition = "input.regression_type == 'multiple'",
                    box(
                      title = "TMDL Results Table",
                      status = "success",
                      solidHeader = TRUE,
                      width = 12,
                      div(
                        style = "width: 100%; overflow-x: auto;",
                        DTOutput("tmdl_results_table") %>% withSpinner()
                      ),
                      helpText("Detailed TMDL results for each selected WBID. Yellow highlighting indicates target values capped at minimum threshold (0.01 mg/L)."),
                      downloadButton("download_tmdl_table", "Download TMDL Results", class = "btn-info")
                    )
                  )
                )
              )
            )
          )
        )
      ),
        tabItem(
          tabName = "about",
          fluidRow(
            box(
              title = "About LakeNutrientAnalyzer",
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              
              # Introduction
              h2("Introduction"),
              tags$p("LakeNutrientAnalyzer is a powerful Shiny app designed to streamline water quality analysis, assess nutrient impairments, and support Total Maximum Daily Load (TMDL) development for Florida lakes. Built for water quality professionals, it combines data extraction, advanced statistical analysis, and interactive visualizations to provide actionable insights into lake nutrient dynamics."),
              hr(),
              
              # Key Features
              h2("Key Features"),
              tags$p("Explore the core functionalities that make this tool essential for water quality management:"),
              tags$ul(
                tags$li(icon("water"), tags$strong("Data Extraction: "), "Retrieve water quality data from the IWR66 database by Water Body ID (WBID)."),
                tags$li(icon("calculator"), tags$strong("Nutrient Analysis: "), "Compute annual geometric means and compare against Numeric Nutrient Criteria."),
                tags$li(icon("chart-bar"), tags$strong("Interactive Visualization: "), "Create time series, XY plots, and nutrient relationship charts."),
                tags$li(icon("chart-area"), tags$strong("Trend Analysis: "), "Perform Mann-Kendall tests to detect water quality trends."),
                tags$li(icon("chart-line"), tags$strong("Regression Modeling: "), "Analyze nutrient relationships with regression techniques."),
                tags$li(icon("water"), tags$strong("TMDL Support: "), "Calculate nutrient reductions and assist in load allocations."),
                tags$li(icon("map"), tags$strong("Geospatial Visualization: "), "Select and view lakes on an interactive map."),
                tags$li(icon("layer-group"), tags$strong("Multi-lake Analysis: "), "Conduct regional assessments across multiple lakes.")
              ),
              br(),
              
              # Benefits
              h2("Benefits"),
              tags$p("This app empowers users with:"),
              tags$ul(
                tags$li("Efficient workflows for nutrient impairment assessments"),
                tags$li("Data-driven insights for TMDL development"),
                tags$li("Customizable analyses tailored to lake-specific needs"),
                tags$li("Advanced statistics for deeper nutrient understanding"),
                tags$li("Enhanced visualizations for clear result communication"),
                tags$li("Automation to save time on repetitive tasks"),
                tags$li("Consistent methods across lakes and regions")
              ),
              hr(),
              
              # Development Process (Shortened)
              h2("Development Process"),
              tags$p("LakeNutrientAnalyzer was built through collaboration with water quality experts, coded in R with Shiny, Plotly, and Leaflet, and tested with real-world lake data."),
              hr(),
              
              # Version Information
              h2("Version Information"),
              tags$p("Current Version: 1.0.0"),
              tags$p("Last Updated: March 24, 2025"),
              tags$p("Source Code: ", tags$a(href="https://github.com/zaimoua/LakeNutrientAnalyzer", "GitHub Repository"))
            )
          )
        ),
        
        tabItem(
          tabName = "contact",
          fluidRow(
            box(
              title = "Contact Information",
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              p("For support, questions, or feedback about LakeNutrientAnalyzer:"),
              p("Submit them to our GitHub repository:"),
              p(a("https://github.com/zaimoua/LakeNutrientAnalyzer/issues", 
                  href = "https://github.com/zaimoua/LakeNutrientAnalyzer/issues", 
                  target = "_blank")),
              hr(),
              p("When submitting issues, please include:"),
              tags$ul(
                tags$li("A detailed description of the issue or request"),
                tags$li("Steps to reproduce any problems (if applicable)"),
                tags$li("Information about your operating system and browser"),
                tags$li("Screenshots if possible")
              )
            )
          )
        )
      ),
    tags$footer(
      class = "footer",
      p("© 2025 LakeNutrientAnalyzer, Version 1.0.0")
    )
  )
)
