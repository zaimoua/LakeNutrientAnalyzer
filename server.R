library(shiny)
library(openxlsx)
library(shinyFiles)
library(RSQLite)
library(data.table)
library(ggplot2)
library(plotly)
library(tidyverse)
library(zoo)
library(Kendall)
library(broom)
library(doParallel)
library(logging)
library(sf)
library(DBI)
library(readr)
library(zip)
library(nortest)
library(shinyjs)
library(shinydashboard)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(DT)
library(lmtest)
library(car)
library(tidyr)
library(scales)
library(futile.logger)
library(stringr)
library(rmapshaper)
library(promises)
library(future)
library(corrplot)
library(geojsonio)


source('R/labels.R')
source('R/data_extraction.R')
source('R/determine_lake_type.R')
source('R/calculate_exceedances.R')
source('R/plot_XY.R')
source('R/nutrient_XY.R')
source('R/plot_timeseries.R')
source('R/regression_analysis.R')
source('R/trend_analysis.R')
source('R/interpret_regression.R')
source('R/helper_functions.R')


prepare_wbid_choices <- function(wbids, waterbody_info) {
  choices <- waterbody_info %>%
    filter(WBID %in% wbids) %>%
    mutate(label = paste(WBID, "-", WATERBODY_NAME)) %>%
    select(WBID, label)
  
  setNames(choices$WBID, choices$label)
}


# Define the path to the SQLite database with version handling
sql_database_locator <- function(version = NULL) {
  # If no version is specified, try to find the latest version
  if (is.null(version)) {
    # Look for all IWR database files in the data directory
    db_files <- list.files("data", pattern = "IWR\\d+_database\\.sqlite$", full.names = TRUE)
    
    if (length(db_files) == 0) {
      # Default to IWR66 if no database files are found
      return("data/IWR66_database.sqlite")
    } else {
      # Extract version numbers and find the latest
      versions <- as.numeric(gsub(".*IWR(\\d+)_database\\.sqlite$", "\\1", db_files))
      latest_file <- db_files[which.max(versions)]
      return(latest_file)
    }
  } else {
    # Use the specified version
    return(sprintf("data/IWR%s_database.sqlite", version))
  }
}

flog.threshold(ERROR)  # Set logging threshold to ERROR to suppress info messages

server <- function(input, output, session) {
  options(warn = -1)  # Suppress warnings temporarily
  
  # Load the CSV files with updated paths
  waterbody_info <- read.csv("data/WaterbodyID_Table.csv", stringsAsFactors = FALSE)
  lake_class <- read.csv("data/Lake_class.csv", stringsAsFactors = FALSE)
  lake_wbid_region <- read.csv("data/lake_wbid_region.csv", stringsAsFactors = FALSE)
  
  # Load the GeoJSON files
  lake_wbid_geojson_path <- "maps/lake_wbid/lake_wbid.geojson"
  region_geojson_path <- "maps/lake_region/lake_region.geojson"
  lake_flowlines <- "maps/lake_flowline/NHD_flowline_lakes.geojson"
  
  # Load highly simplified GeoJSON files
  lake_wbid_geojson <- load_geojson(lake_wbid_geojson_path, keep = 0.1)
  region_geojson <- load_geojson(region_geojson_path, keep = 0.5)
  
  # Check if GeoJSON was loaded correctly
  if (is.null(lake_wbid_geojson)) {
    showNotification("Failed to load lake WBID GeoJSON. The app may not function correctly.", type = "error")
  }
  
  # Ensure WBID columns are character type
  lake_wbid_geojson$WBID <- as.character(lake_wbid_geojson$WBID)
  waterbody_info$WBID <- as.character(waterbody_info$WBID)
  
  # Prepare WBID data for selectize input
  wbid_data <- reactive({
    req(waterbody_info, lake_wbid_geojson)
    waterbody_info %>%
      filter(WBID %in% lake_wbid_geojson$WBID) %>%
      mutate(label = paste(WBID, "-", WATERBODY_NAME)) %>%
      select(WBID, WATERBODY_NAME, label)
  })
  
  # Update WBID choices for all select inputs
  observe({
    choices <- setNames(wbid_data()$WBID, wbid_data()$label)
    updateSelectInput(session, "viz_wbid", choices = choices)
    updateSelectInput(session, "ts_wbid", choices = choices)
    updateSelectInput(session, "xy_wbid", choices = choices)
    updateSelectInput(session, "nutrient_wbid", choices = choices)
    updateSelectInput(session, "trend_wbid", choices = choices)
    updateSelectInput(session, "reg_wbid", choices = choices)
  })
  
  # Reactive value for selected WBIDs
  selected_wbids <- reactiveVal(character(0))
  
  # Update WBID choices
  observe({
    choices <- wbid_data()
    updateSelectizeInput(session, "wbid", 
                         choices = choices,
                         selected = selected_wbids(),
                         server = TRUE)
  })
  
  # Render initial map
  output$all_wbids_map <- renderLeaflet({
    req(lake_wbid_geojson, waterbody_info)
    
    # Merge GeoJSON data with waterbody_info to get WATERBODY_NAME
    lake_data <- merge(lake_wbid_geojson, waterbody_info, by = "WBID", all.x = TRUE)
    
    create_wbid_map(lake_data)
  })
  
  # Handle map click events for WBID selection
  observeEvent(input$all_wbids_map_shape_click, {
    clicked_wbid <- input$all_wbids_map_shape_click$id
    
    if (!is.null(clicked_wbid)) {
      current_selection <- selected_wbids()
      current_selection <- handle_map_click(clicked_wbid, current_selection)
      
      selected_wbids(current_selection)
      updateSelectizeInput(session, "wbid", selected = current_selection)
      
      lake_data <- merge(lake_wbid_geojson, waterbody_info, by = "WBID", all.x = TRUE)
      leafletProxy("all_wbids_map") %>% update_map_selection(current_selection, lake_data)
    }
  })
  
  # Update selected_wbids when input$wbid changes
  observeEvent(input$wbid, {
    new_selection <- input$wbid
    selected_wbids(new_selection)
    
    lake_data <- merge(lake_wbid_geojson, waterbody_info, by = "WBID", all.x = TRUE)
    leafletProxy("all_wbids_map") %>% update_map_selection(new_selection, lake_data)
  })
  
  # Display selected WBIDs info
  output$selectedWBIDsInfo <- renderText({
    paste("Selected WBIDs:", paste(selected_wbids(), collapse = ", "))
  })
  
  # Search functionality
  observeEvent(input$searchButton, {
    search_term <- input$searchTerm
    search_results <- perform_search(search_term, waterbody_info, lake_wbid_geojson)
    
    output$searchResults <- renderUI({
      display_search_results(search_results)
    })
    
    update_wbid_selection(session, search_results)
    leafletProxy("all_wbids_map") %>% update_map_highlight(search_results, lake_wbid_geojson)
  })
  
  # Reset map
  observeEvent(input$resetMap, {
    selected_wbids(character(0))
    updateSelectizeInput(session, "wbid", selected = character(0))
    leafletProxy("all_wbids_map") %>% reset_map()
  })
  
  # Zoom to Florida
  observeEvent(input$zoomFlorida, {
    leafletProxy("all_wbids_map") %>% zoom_to_florida()
  })
  
  
  # Reactive values
  results <- reactiveVal(NULL)
  shapiro_results <- reactiveVal(NULL)
  output_file <- reactiveVal(NULL)
  params <- reactiveVal(NULL)
  trend_results <- reactiveVal(NULL)
  selected_params <- reactiveVal(NULL)
  selected_detail_param <- reactiveVal(NULL)
  regression_results <- reactiveVal(NULL)
  tmdl_results <- reactiveVal(NULL)
  filtered_wbids_combined <- reactiveVal(NULL)
  filtered_data <- reactiveVal(list(wbids = NULL, regions = NULL))
  global_selected_wbid <- reactiveVal(NULL)
  
  # Reactive value for selected WBIDs
  selected_wbids <- reactiveVal(character(0))
  
  
  # reset state when WBID changes
  observeEvent(input$wbid, {
    # Reset all analysis results when a new WBID is selected
    shapiro_results(NULL)
    trend_results(NULL)
    regression_results(NULL)
    tmdl_results(NULL)
    selected_detail_param(NULL)
  }, ignoreInit = TRUE)
  
  
  # Observe changes in search input
  observeEvent(input$searchInput, {
    if (nchar(input$searchInput) == 0) {
      # Reset selected WBIDs
      selected_wbids(character(0))
      # Clear the map
      leafletProxy("all_wbids_map") %>% clearShapes()
    }
  }, ignoreInit = TRUE)
  
  # Update selected_wbids when input$wbid changes
  observeEvent(input$wbid, {
    new_selection <- input$wbid
    selected_wbids(new_selection)
    
    lake_data <- merge(lake_wbid_geojson, waterbody_info, by = "WBID", all.x = TRUE)
    leafletProxy("all_wbids_map") %>% update_map_selection(new_selection, lake_data)
  })
  
  
  
  # Define criteria 
  criteria <- list(
    "1" = list("CHLAC" = 20, "TP" = 0.05, "TN" = 1.27),
    "2" = list("CHLAC" = 20, "TP" = 0.03, "TN" = 1.05),
    "3" = list("CHLAC" = 6, "TP" = 0.01, "TN" = 0.51)
  )
  
  # Lake type calculation
  lake_type <- reactive({
    req(results(), global_selected_wbid())
    res <- results()
    wbid_data <- res$rawdata[res$rawdata$wbid == global_selected_wbid(), ]
    
    if ("COLOR" %in% names(wbid_data) && "ALK" %in% names(wbid_data)) {
      color_data <- wbid_data$COLOR
      alkalinity_data <- wbid_data$ALK
      
      if (length(color_data) > 0 && length(alkalinity_data) > 0) {
        color_geomean <- exp(mean(log(color_data), na.rm = TRUE))
        alkalinity_geomean <- exp(mean(log(alkalinity_data), na.rm = TRUE))
        
        if (color_geomean > 40) {
          1
        } else if (color_geomean <= 40 && alkalinity_geomean > 20) {
          2
        } else {
          3
        }
      } else {
        NA
      }
    } else {
      NA
    }
  })
  
  # Define the nutrient parameters
  nutrient_params <- c("CHLAC", "TN", "TP", "COLOR", "ALK", "COND", "TEMP", "DO", "PH", "PORTH", 
                       "NH4", "NO3O2", "BOD", "DOSAT", "UNNH4", "PORD", "TKN", "CHLA", "TORTH")
  
  # Update parameter choices
  observe({
    updateSelectizeInput(session, "parameters", choices = nutrient_params, selected = head(nutrient_params, 11))
    updateSelectizeInput(session, "x_param", choices = nutrient_params)
    updateSelectizeInput(session, "y_param", choices = nutrient_params)
    updateSelectizeInput(session, "ts_param", choices = nutrient_params)
    updateSelectInput(session, "trend_parameter", choices = nutrient_params)
    updateSelectizeInput(session, "regression_response", choices = nutrient_params, selected = "CHLAC")
    updateSelectizeInput(session, "regression_explanatory1", choices = nutrient_params, selected = "TN")
    updateSelectizeInput(session, "regression_explanatory2", choices = nutrient_params, selected = "TP")
  })
  
  # If you're using a reactive value for params, update it like this:
  observe({
    params(nutrient_params)
  })
  
  # Observe the selection of WBIDs and parameters in the data extraction tab and update the reactive values
  observeEvent(input$runButton, {
    withProgress(message = 'Extracting data...', value = 0, {
      tryCatch({
        # Store the selected WBIDs and parameters
        selected_wbids(input$wbid)
        selected_params(input$parameters)
        
        WBIDs <- input$wbid
        user_params <- input$parameters
        PARAM <- unique(c(user_params, "CHLAC", "TP", "TN", "COLOR", "ALK"))
        LAKEWATCH <- input$lakewatch == "Yes"
        year_range <- if (input$period_of_record) NULL else input$year_range
        station_id <- input$station_id
        IWR <- sql_database_locator()
        
        incProgress(0.2, detail = "Querying database...")
        
        res <- data_extraction(WBID = WBIDs, PARAM = PARAM, LAKEWATCH = LAKEWATCH, IWR = IWR, year_range = year_range, station_id = station_id)
        
        if (is.null(res) || nrow(res$rawdata) == 0) {
          showNotification("No data found for the specified criteria.", type = "warning")
          return(NULL)
        }
        
        incProgress(0.5, detail = "Processing results...")
        
        results(res)
        
        # Update global WBID choices
        wbid_choices <- unique(res$rawdata$wbid)
        updateSelectInput(session, "global_selected_wbid", 
                          choices = wbid_choices,
                          selected = wbid_choices[1])
        
        # Update global_selected_wbid reactive value
        global_selected_wbid(wbid_choices[1])
        
        incProgress(0.7, detail = "Updating UI elements...")
        
        # Update parameter choices for data overview
        all_params <- setdiff(names(res$rawdata), c("wbid", "sta", "year", "month", "day", "time"))
        updateSelectInput(session, "overview_param", 
                          choices = all_params,
                          selected = ifelse("CHLAC" %in% all_params, "CHLAC", all_params[1]))
        
        # Update choices for other inputs that depend on the extracted data
        updateSelectizeInput(session, "ts_wbid", choices = wbid_choices, selected = wbid_choices[1], server = TRUE)
        updateSelectInput(session, "xy_wbid", choices = wbid_choices, selected = wbid_choices[1])
        updateSelectInput(session, "nutrient_wbid", choices = wbid_choices, selected = wbid_choices[1])
        updateSelectInput(session, "reg_wbid", choices = wbid_choices, selected = wbid_choices[1])
        updateSelectInput(session, "trend_wbid", choices = wbid_choices, selected = wbid_choices[1])
        
        updateSelectizeInput(session, "ts_param", choices = all_params, selected = all_params[1], server = TRUE)
        updateSelectizeInput(session, "x_param", choices = all_params, selected = all_params[1], server = TRUE)
        updateSelectizeInput(session, "y_param", choices = all_params, selected = ifelse(length(all_params) > 1, all_params[2], all_params[1]), server = TRUE)
        
        incProgress(0.9, detail = "Preparing output...")
        
        output$completionMessage <- renderText({
          paste("Data extraction completed for WBIDs:", paste(WBIDs, collapse = ", "), 
                ". Select a WBID to display and analyze its data.")
        })
        
        incProgress(1, detail = "Completed!")
        showNotification("Data extraction completed successfully.", type = "message")
        
      }, error = function(e) {
        showNotification(paste("Error in data extraction:", e$message), type = "error")
        results(NULL)
      })
    })
  })
  
  # Update global_selected_wbid when the input changes
  observeEvent(input$global_selected_wbid, {
    global_selected_wbid(input$global_selected_wbid)
  })
  
  
  # Combined function for lake type and exceedances
  calculate_lake_type_and_exceedances <- function(data, criteria) {
    lake_type_result <- determine_lake_type(data)
    
    if (is.null(lake_type_result)) {
      return(NULL)
    }
    
    lake_type <- switch(lake_type_result$lake_type,
                        "> 40 Platinum Cobalt Units" = "1",
                        "≤ 40 Platinum Cobalt Units and > 20 mg/L CaCO3" = "2",
                        "≤ 40 Platinum Cobalt Units and ≤ 20 mg/L CaCO3" = "3",
                        "Unknown")
    
    if (lake_type == "Unknown") {
      return(list(lake_type = lake_type_result, exceedances = NULL))
    }
    
    exceedances <- calculate_exceedances(data, lake_type, criteria)
    
    return(list(lake_type = lake_type_result, exceedances = exceedances))
  }
  
  # Lake Type reactive
  lake_type <- reactive({
    req(results(), global_selected_wbid())
    res <- results()
    wbid_data <- res$rawdata[res$rawdata$wbid == global_selected_wbid(), ]
    
    if ("COLOR" %in% names(wbid_data) && "ALK" %in% names(wbid_data)) {
      color_data <- wbid_data$COLOR
      alkalinity_data <- wbid_data$ALK
      
      if (length(color_data) > 0 && length(alkalinity_data) > 0) {
        color_geomean <- exp(mean(log(color_data), na.rm = TRUE))
        alkalinity_geomean <- exp(mean(log(alkalinity_data), na.rm = TRUE))
        
        if (color_geomean > 40) {
          1
        } else if (color_geomean <= 40 && alkalinity_geomean > 20) {
          2
        } else {
          3
        }
      } else {
        NA
      }
    } else {
      NA
    }
  })
  
  # Waterbody Information and WBID Selector
  output$wbid_info_and_selector <- renderUI({
    req(results())
    res <- results()
    
    box(
      title = "Waterbody Information",
      status = "primary", 
      solidHeader = TRUE,
      width = 12,
      tagList(
        selectInput("global_selected_wbid", "Choose WBID to Display:", 
                    choices = unique(res$rawdata$wbid), 
                    selected = global_selected_wbid()),
        uiOutput("wbid_info_content")
      )
    )
  })
  
  # Waterbody Information Content
  output$wbid_info_content <- renderUI({
    req(global_selected_wbid())
    wbid_info <- waterbody_info[waterbody_info$WBID == global_selected_wbid(), ]
    
    if (nrow(wbid_info) == 0) {
      return(tags$p("No information available for this WBID."))
    }
    
    # Helper function to safely display values
    safe_display <- function(value, default = "N/A") {
      if (is.null(value) || is.na(value) || value == "") {
        return(default)
      } else {
        return(as.character(value))
      }
    }
    
    tagList(
      h4(paste("WBID:", safe_display(wbid_info$WBID))),
      tags$div(
        class = "row",
        tags$div(
          class = "col-md-6",
          if ("WATERBODY_NAME" %in% names(wbid_info)) 
            tags$p(tags$b("Waterbody Name:"), safe_display(wbid_info$WATERBODY_NAME)),
          if ("WATER_TYPE" %in% names(wbid_info)) 
            tags$p(tags$b("Water Type:"), safe_display(wbid_info$WATER_TYPE)),
          if ("CLASS" %in% names(wbid_info)) 
            tags$p(tags$b("Class:"), safe_display(wbid_info$CLASS))
        ),
        tags$div(
          class = "col-md-6",
          if ("GROUP_NAME" %in% names(wbid_info)) 
            tags$p(tags$b("Group Name:"), safe_display(wbid_info$GROUP_NAME)),
          if ("WATERBODY_SIZE" %in% names(wbid_info) && "UNITS" %in% names(wbid_info)) 
            tags$p(tags$b("Waterbody Size:"), 
                   paste(safe_display(wbid_info$WATERBODY_SIZE), 
                         safe_display(wbid_info$UNITS))),
          if ("COUNTY" %in% names(wbid_info)) 
            tags$p(tags$b("County:"), safe_display(wbid_info$COUNTY))
        )
      )
    )
  })
  
  
  # Lake Assessment Type
  output$lake_assessment_and_exceedances <- renderUI({
    req(results(), global_selected_wbid(), lake_type())
    current_lake_type <- lake_type()
    
    if (is.na(current_lake_type)) {
      return(box(
        title = "Lake Assessment Type",
        status = "success",
        solidHeader = TRUE,
        width = 12,
        p("Unable to determine lake type. Please check color and alkalinity data.")
      ))
    }
    
    lake_type_desc <- c(
      "1" = "Color > 40 Platinum Cobalt Units",
      "2" = "Color ≤ 40 Platinum Cobalt Units and Alkalinity > 20 mg/L CaCO3",
      "3" = "Color ≤ 40 Platinum Cobalt Units and Alkalinity ≤ 20 mg/L CaCO3"
    )[as.character(current_lake_type)]
    
    box(
      title = "Lake Assessment Type",
      status = "success",
      solidHeader = TRUE,
      width = 12,
      tagList(
        p(tags$b(paste("Lake Type:", current_lake_type, "-", lake_type_desc))),
        h5("Lake Type Details:"),
        tableOutput("lake_type_table")
      )
    )
  })
  
  # Exceedances Detail UI
  output$exceedances_detail <- renderUI({
    req(results(), global_selected_wbid(), lake_type())
    res <- results()
    current_lake_type <- lake_type()
    
    if (is.na(current_lake_type)) {
      return(NULL)
    }
    
    lake_criteria <- criteria[[as.character(current_lake_type)]]
    
    box(
      title = "Exceedances Detail",
      status = "warning",
      solidHeader = TRUE,
      width = 12,  # Adjusted to match the width of other boxes
      tagList(
        h4(tags$strong("Numeric Nutrient Criteria (NNC):")),
        tags$ul(
          tags$li(paste("Chlorophyll a (CHLAC) Criterion:", lake_criteria$CHLAC, "µg/L")),
          tags$li(paste("Total Phosphorus (TP) Criterion:", lake_criteria$TP, "mg/L")),
          tags$li(paste("Total Nitrogen (TN) Criterion:", lake_criteria$TN, "mg/L"))
        ),
        h4(tags$strong("Exceedances Summary:")),
        tableOutput("exceedances_table"),
        h4(tags$strong("Yearly Geomeans:")),
        DTOutput("yearly_geomeans_table")
      )
    )
  })
  
  
  # Lake Type Table
  output$lake_type_table <- renderTable({
    req(results(), global_selected_wbid(), lake_type())
    res <- results()
    wbid_data <- res$rawdata[res$rawdata$wbid == global_selected_wbid(), ]
    
    lake_type_result <- determine_lake_type(wbid_data)
    
    if (!is.null(lake_type_result)) {
      data.frame(
        Metric = c("Color Geomean", "Alkalinity Geomean"),
        Value = c(round(lake_type_result$color, 2), 
                  round(lake_type_result$alkalinity, 2))
      )
    } else {
      data.frame(Message = "Unable to determine lake type.")
    }
  })
  
  # Exceedances Detail UI
  output$exceedances_detail <- renderUI({
    req(results(), global_selected_wbid(), lake_type())
    res <- results()
    current_lake_type <- lake_type()
    
    if (is.na(current_lake_type)) {
      return(NULL)
    }
    
    lake_criteria <- criteria[[as.character(current_lake_type)]]
    
    box(
      title = "Exceedances Detail",
      status = "warning",
      solidHeader = TRUE,
      width = 12,
      fluidRow(
        column(
          width = 6,  # Left column for NNC criteria and Exceedances summary
          h4(tags$strong("Numeric Nutrient Criteria (NNC) - Annual Geometric Means:")),
          tags$table(
            class = "table table-bordered",
            tags$thead(
              tags$tr(
                tags$th("Lake Type", style = "font-weight: bold;"),
                tags$th("Long Term Geometric Mean Lake Color and Alkalinity"),
                tags$th("Chlorophyll a (CHLAC)", "µg/L"),
                tags$th("Total Phosphorus (TP)", "mg/L"),
                tags$th("Total Nitrogen (TN)", "mg/L")
              )
            ),
            tags$tbody(
              tags$tr(
                tags$td("1"),
                tags$td("> 40 Platinum Cobalt Units"),
                tags$td("20"),
                tags$td("0.05"),
                tags$td("1.27")
              ),
              tags$tr(
                tags$td("2"),
                tags$td("≤ 40 Platinum Cobalt Units, > 20 mg/L CaCO3"),
                tags$td("20"),
                tags$td("0.03"),
                tags$td("1.05")
              ),
              tags$tr(
                tags$td("3"),
                tags$td("≤ 40 Platinum Cobalt Units, ≤ 20 mg/L CaCO3"),
                tags$td("6"),
                tags$td("0.01"),
                tags$td("0.51")
              )
            )
          ),
          h4(tags$strong("Exceedances Summary:")),
          tableOutput("exceedances_table")
        ),
        column(
          width = 6,  # Right column for Yearly Geomeans
          h4(tags$strong("Yearly Geomeans:")),
          DTOutput("yearly_geomeans_table"),
          p(tags$em("Note: Values highlighted in red indicate exceedances of the criteria."))
        )
      )
    )
  })
  
  
  
  
  # Exceedances Table
  output$exceedances_table <- renderTable({
    req(results(), global_selected_wbid(), lake_type())
    res <- results()
    wbid_data <- res$rawdata[res$rawdata$wbid == global_selected_wbid(), ]
    current_lake_type <- lake_type()
    
    if (!is.na(current_lake_type)) {
      lake_type_and_exceedances <- calculate_lake_type_and_exceedances(wbid_data, criteria)
      
      if (!is.null(lake_type_and_exceedances) && !is.null(lake_type_and_exceedances$exceedances)) {
        lake_type_and_exceedances$exceedances %>%
          group_by(parameter) %>%
          summarize(
            Total_Samples = n(),
            Exceedances = sum(exceedance == "Exceeds"),
            Exceedance_Rate = sprintf("%.1f%%", Exceedances / Total_Samples * 100)
          )
      } else {
        data.frame(Message = "No exceedance data available.")
      }
    } else {
      data.frame(Message = "Insufficient data to calculate exceedances.")
    }
  })
  
  # Yearly Geomeans Table
  output$yearly_geomeans_table <- renderDT({
    req(results(), global_selected_wbid(), lake_type())
    res <- results()
    current_lake_type <- lake_type()
    
    # Extract geomeans data for the selected WBID
    geomeans_data <- res$geomeans[res$geomeans$wbid == global_selected_wbid(), ]
    
    # If there's no data, display an empty table
    if (nrow(geomeans_data) == 0) {
      return(datatable(data.frame(Message = "No data available."), options = list(dom = 't')))
    }
    
    # Select relevant columns: Year, CHLAC, TN, TP
    yearly_data <- geomeans_data %>%
      select(year, CHLAC, TN, TP) %>%
      mutate(
        CHLAC = round(CHLAC, 2),
        TN = round(TN, 2),
        TP = round(TP, 2)
      )
    
    # Render the table with better layout options
    datatable(yearly_data, 
              options = list(
                pageLength = 100, 
                scrollX = TRUE, 
                scrollY = "400px",
                columnDefs = list(
                  list(width = '60px', targets = c(0, 1, 2, 3))  # Adjust column widths
                ),
                dom = 't',  # Remove search and pagination
                autoWidth = TRUE
              ), 
              rownames = FALSE
    ) %>%
      formatStyle(
        'CHLAC',
        backgroundColor = styleInterval(criteria[[as.character(current_lake_type)]]$CHLAC, c('white', 'red'))
      ) %>%
      formatStyle(
        'TN',
        backgroundColor = styleInterval(criteria[[as.character(current_lake_type)]]$TN, c('white', 'red'))
      ) %>%
      formatStyle(
        'TP',
        backgroundColor = styleInterval(criteria[[as.character(current_lake_type)]]$TP, c('white', 'red'))
      )
  })
  
  # Debug output to verify data structure
  output$debug_data <- renderPrint({
    req(results(), global_selected_wbid(), lake_type())
    res <- results()
    geomeans_data <- res$geomeans[res$geomeans$wbid == global_selected_wbid(), ]
    
    if (nrow(geomeans_data) == 0) {
      print("No data available.")
    } else {
      # Display the first few rows to check alignment
      print(head(geomeans_data))
    }
  })
  
  verbatimTextOutput("debug_data")
  
  
  
  # WNAS Information
  output$wnas_info <- renderUI({
    req(global_selected_wbid())
    wnas_info_filtered <- wnas_info[wnas_info$WBID == global_selected_wbid(), ]
    
    if (nrow(wnas_info_filtered) == 0) {
      return(tags$p("No Waters Not Attaining Standards (WNAS) information available for this WBID."))
    }
    
    box(
      title = "Waters Not Attaining Standards",
      status = "danger",  # Kept as 'danger' to differentiate from Exceedances Detail
      solidHeader = TRUE,
      width = 12,
      DTOutput("wnas_table")
    )
  })
  
  # WNAS Table
  output$wnas_table <- renderDT({
    req(global_selected_wbid())
    wnas_data <- wnas_info[wnas_info$WBID == global_selected_wbid(), 
                           c("PARAMETER_IWR", "IR_ASSESSMENT_CATEGORY", "ASSESSMENT_STATUS", "COMMENTS")]
    
    datatable(wnas_data, 
              options = list(
                pageLength = 5, 
                scrollX = TRUE,
                scrollY = "200px"
              ))
  })
  
  # Update download handlers
  output$downloadSelectedData <- downloadHandler(
    filename = function() {
      paste0("Extracted_Data_WBID_", global_selected_wbid(), "_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      req(results(), global_selected_wbid())
      res <- results()
      
      selected_wbid <- global_selected_wbid()
      
      wb <- createWorkbook()
      
      # Raw Data sheet
      addWorksheet(wb, "Raw Data")
      raw_data <- res$rawdata[res$rawdata$wbid == selected_wbid,]
      writeData(wb, sheet = "Raw Data", x = raw_data)
      
      # Yearly Geomeans sheet
      addWorksheet(wb, "Yearly Geomeans")
      geomean_data <- res$geomeans[res$geomeans$wbid == selected_wbid,]
      writeData(wb, sheet = "Yearly Geomeans", x = geomean_data)
      
      # Summary sheet
      addWorksheet(wb, "Summary")
      summary_data <- res$summary[res$summary$wbid == selected_wbid,]
      writeData(wb, sheet = "Summary", x = summary_data)
      
      # Lake Type and Exceedances sheet
      addWorksheet(wb, "Lake Type and Exceedances")
      lake_type_and_exceedances <- calculate_lake_type_and_exceedances(raw_data, criteria)
      
      if (!is.null(lake_type_and_exceedances)) {
        lake_type_data <- data.frame(
          Lake_Type = lake_type_and_exceedances$lake_type$lake_type,
          Color = lake_type_and_exceedances$lake_type$color,
          Alkalinity = lake_type_and_exceedances$lake_type$alkalinity
        )
        writeData(wb, sheet = "Lake Type and Exceedances", x = lake_type_data, startRow = 1)
        
        if (!is.null(lake_type_and_exceedances$exceedances)) {
          writeData(wb, sheet = "Lake Type and Exceedances", x = lake_type_and_exceedances$exceedances, startRow = 5)
        } else {
          writeData(wb, sheet = "Lake Type and Exceedances", x = "No exceedance data available", startRow = 5)
        }
      } else {
        writeData(wb, sheet = "Lake Type and Exceedances", x = "Unable to determine lake type or calculate exceedances")
      }
      
      # Save the workbook
      saveWorkbook(wb, file, overwrite = TRUE)
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
  
  # Update the downloadAllData handler similarly
  output$downloadAllData <- downloadHandler(
    filename = function() {
      paste0("Extracted_Data_All_WBIDs_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      req(results())
      res <- results()
      
      wb <- createWorkbook()
      
      for (wbid in unique(res$rawdata$wbid)) {
        # ... (previous code for other sheets)
        
        # Lake Type and Exceedances sheet
        sheet_name <- paste("Lake Type -", wbid)
        addWorksheet(wb, sheet_name)
        
        raw_data <- res$rawdata[res$rawdata$wbid == wbid,]
        lake_type_and_exceedances <- calculate_lake_type_and_exceedances(raw_data, criteria)
        
        if (!is.null(lake_type_and_exceedances)) {
          lake_type_data <- data.frame(
            Lake_Type = lake_type_and_exceedances$lake_type$lake_type,
            Color = lake_type_and_exceedances$lake_type$color,
            Alkalinity = lake_type_and_exceedances$lake_type$alkalinity
          )
          writeData(wb, sheet = sheet_name, x = lake_type_data, startRow = 1)
          
          if (!is.null(lake_type_and_exceedances$exceedances)) {
            writeData(wb, sheet = sheet_name, x = lake_type_and_exceedances$exceedances, startRow = 5)
          } else {
            writeData(wb, sheet = sheet_name, x = "No exceedance data available", startRow = 5)
          }
        } else {
          writeData(wb, sheet = sheet_name, x = "Unable to determine lake type or calculate exceedances")
        }
      }
      
      # Save the workbook
      saveWorkbook(wb, file, overwrite = TRUE)
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
  
  
  ####################################################################################################################################################################################
  # Visualization
  ##################################################################################################################################################################################
  
  
  # Add stats function (keep as it is)
  add_stats <- function(df) {
    numeric_cols <- sapply(df, is.numeric)
    stats_df <- df %>%
      summarise(across(where(is.numeric), 
                       list(Mean = ~mean(., na.rm = TRUE),
                            Median = ~median(., na.rm = TRUE),
                            Min = ~min(., na.rm = TRUE),
                            Max = ~max(., na.rm = TRUE),
                            SD = ~sd(., na.rm = TRUE)))) %>%
      pivot_longer(everything(), names_to = c(".value", "Statistic"), names_sep = "_") %>%
      mutate(Statistic = factor(Statistic, levels = c("Mean", "Median", "Min", "Max", "SD")))
    
    bind_rows(df, stats_df)
  }
  
  # Histogram Plot
  output$param_histogram <- renderPlotly({
    req(results(), input$global_selected_wbid, input$overview_param, input$data_type)
    res <- results()
    
    tryCatch({
      if (input$data_type == "raw") {
        data <- res$rawdata[res$rawdata$wbid == input$global_selected_wbid, ]
      } else {
        data <- res$geomeans[res$geomeans$wbid == input$global_selected_wbid, ]
      }
      
      param_data <- data[[input$overview_param]]
      
      if (length(param_data) == 0 || all(is.na(param_data))) {
        return(plot_ly() %>% 
                 add_annotations(
                   text = "No data available for the selected parameter and data type",
                   showarrow = FALSE,
                   font = list(size = 16)
                 ))
      }
      
      # Create histogram
      plot_ly(x = ~param_data, type = "histogram", 
              marker = list(color = "rgba(0, 0, 255, 0.6)"),
              name = "Histogram") %>%
        layout(
          title = paste("Histogram of", input$overview_param),
          xaxis = list(title = input$overview_param),
          yaxis = list(title = "Frequency")
        )
    }, error = function(e) {
      message("Error in param_histogram: ", e$message)
      return(plot_ly() %>% 
               add_annotations(
                 text = paste("Error:", e$message),
                 showarrow = FALSE,
                 font = list(size = 16)
               ))
    })
  })
  
  # Box Plot
  output$param_boxplot <- renderPlotly({
    req(results(), input$global_selected_wbid, input$overview_param, input$data_type)
    res <- results()
    
    tryCatch({
      if (input$data_type == "raw") {
        data <- res$rawdata[res$rawdata$wbid == input$global_selected_wbid, ]
      } else {
        data <- res$geomeans[res$geomeans$wbid == input$global_selected_wbid, ]
      }
      
      param_data <- data[[input$overview_param]]
      
      if (length(param_data) == 0 || all(is.na(param_data))) {
        return(plot_ly() %>% 
                 add_annotations(
                   text = "No data available for the selected parameter and data type",
                   showarrow = FALSE,
                   font = list(size = 16)
                 ))
      }
      
      # Create box plot
      plot_ly(y = ~param_data, type = "box", 
              marker = list(color = "rgba(255, 0, 0, 0.6)"), 
              boxmean = TRUE, boxpoints = "outliers",
              name = "Box Plot") %>%
        layout(
          title = paste("Box Plot of", input$overview_param),
          yaxis = list(title = input$overview_param)
        )
    }, error = function(e) {
      message("Error in param_boxplot: ", e$message)
      return(plot_ly() %>% 
               add_annotations(
                 text = paste("Error:", e$message),
                 showarrow = FALSE,
                 font = list(size = 16)
               ))
    })
  })
  
  # Quick Statistics (keep as it was)
  output$quick_stats <- renderTable({
    req(results(), input$global_selected_wbid, input$overview_param, input$data_type)
    res <- results()
    
    tryCatch({
      if (input$data_type == "raw") {
        data <- res$rawdata[res$rawdata$wbid == input$global_selected_wbid, ]
      } else {
        data <- res$geomeans[res$geomeans$wbid == input$global_selected_wbid, ]
      }
      
      param_data <- data[[input$overview_param]]
      
      if (length(param_data) == 0 || all(is.na(param_data))) {
        return(data.frame(Message = "No data available for the selected parameter and data type."))
      }
      
      data.frame(
        Statistic = c("Count", "Mean", "Median", "Min", "Max", "SD"),
        Value = c(
          length(param_data),
          mean(param_data, na.rm = TRUE),
          median(param_data, na.rm = TRUE),
          min(param_data, na.rm = TRUE),
          max(param_data, na.rm = TRUE),
          sd(param_data, na.rm = TRUE)
        )
      ) %>% 
        mutate(Value = round(Value, 3))
    }, error = function(e) {
      message("Error in quick_stats: ", e$message)
      return(data.frame(Message = paste("Error:", e$message)))
    })
  })
  
  # Comprehensive Data Display
  output$comprehensive_data_display <- renderUI({
    req(results(), global_selected_wbid())
    
    # Get the list of all available parameters and exclude essential columns
    all_params <- setdiff(names(results()$rawdata), c("wbid", "year", "month", "day", "time"))
    
    tagList(
      # UI for selecting nutrient parameters to display
      checkboxGroupInput("selected_params", "Select Nutrient Parameters to Display:", 
                         choices = all_params, 
                         selected = all_params, 
                         inline = TRUE),
      
      # UI for selecting optional columns (month, day, time)
      checkboxGroupInput("optional_columns", "Select Optional Columns to Display:", 
                         choices = c("Month" = "month", "Day" = "day", "Time" = "time"),
                         selected = c("month", "day", "time"),  # Default selection
                         inline = TRUE),
      
      # Tab box for data display
      tabBox(
        title = "Comprehensive Data Display",
        width = 12,
        tabPanel("Raw Data", DTOutput("raw_data_table", height = "500px")),  # Set height for scroll
        tabPanel("Yearly Geomeans", DTOutput("geomeans_data_table", height = "500px"))  # Set height for scroll
      )
    )
  })
  
  
  
  
  
  # Function to create formatted datatable
  create_formatted_datatable <- function(data, title, wbid) {
    datatable(
      data,
      caption = htmltools::tags$caption(
        style = 'caption-side: top; text-align: center; color: black; font-size: 120%; font-weight: bold;',
        title
      ),
      options = list(
        pageLength = 100,  # Show 100 rows by default
        lengthMenu = list(c(10, 25, 50, 100, -1), c('10', '25', '50', '100', 'All')),  # Allow users to select the number of rows to display
        scrollX = TRUE,  # Enable horizontal scrolling
        scrollY = "400px",  # Enable vertical scrolling
        dom = 'Blfrtip',  # Include pagination controls
        buttons = list(
          list(extend = 'copy', exportOptions = list(modifier = list(page = 'all'))),
          list(extend = 'csv', filename = paste0(title, "_WBID_", wbid),
               exportOptions = list(modifier = list(page = 'all'))),
          list(extend = 'excel', filename = paste0(title, "_WBID_", wbid),
               exportOptions = list(modifier = list(page = 'all'))),
          list(extend = 'pdf', filename = paste0(title, "_WBID_", wbid),
               exportOptions = list(modifier = list(page = 'all'))),
          list(extend = 'print', exportOptions = list(modifier = list(page = 'all')))
        ),
        processing = TRUE,
        scrollCollapse = TRUE  # Collapse the table height if there are fewer rows
      ),
      extensions = 'Buttons',
      filter = 'top',
      rownames = FALSE
    ) %>%
      formatStyle(
        columns = names(data),
        backgroundColor = styleEqual(c("Mean", "Median", "Min", "Max", "SD"), 
                                     rep("#f2f2f2", 5))
      ) %>%
      formatRound(columns = sapply(data, is.numeric), digits = 2)  # Ensure only 2 digits after decimal
  }
  
  
  # Convert year, month, day to integer and round parameter values
  process_data <- function(data) {
    # Check if the columns exist and convert them to integers
    if ("year" %in% names(data)) {
      data$year <- as.integer(data$year)  # Ensure year is displayed without commas
    }
    if ("month" %in% names(data)) {
      data$month <- as.integer(data$month)
    }
    if ("day" %in% names(data)) {
      data$day <- as.integer(data$day)
    }
    
    # Identify numeric columns that are not year, month, or day
    numeric_cols <- which(sapply(data, is.numeric))
    numeric_cols <- setdiff(numeric_cols, which(names(data) %in% c("year", "month", "day")))
    
    # Round numeric columns to 2 decimal places
    data[, (numeric_cols) := lapply(.SD, round, 2), .SDcols = numeric_cols]
    
    return(data)
  }
  
  
  
  # Render individual datatables
  output$raw_data_table <- renderDT({
    req(results(), global_selected_wbid(), input$selected_params)
    res <- results()
    wbid <- global_selected_wbid()
    
    tryCatch({
      # Filter data for the selected WBID
      raw_data <- res$rawdata[res$rawdata$wbid == wbid, ]
      
      # Convert year, month, day, time to integers only if they exist in the dataset
      if ("year" %in% names(raw_data)) raw_data$year <- as.integer(raw_data$year)
      if ("month" %in% names(raw_data)) raw_data$month <- as.integer(raw_data$month)
      if ("day" %in% names(raw_data)) raw_data$day <- as.integer(raw_data$day)
      if ("time" %in% names(raw_data)) raw_data$time <- as.integer(raw_data$time)
      
      # Round COND column if it exists
      if ("COND" %in% names(raw_data)) {
        raw_data$COND <- round(raw_data$COND, 0)
      }
      
      # Always include essential columns and append selected nutrient parameters
      essential_columns <- c("wbid", "year")
      
      # Add optional columns (month, day, time) if selected by the user
      optional_columns <- input$optional_columns
      selected_columns <- c(essential_columns, optional_columns, input$selected_params)
      
      # Check if each selected column exists in the data
      selected_columns <- selected_columns[selected_columns %in% names(raw_data)]
      
      # If no columns are left after filtering, show a message
      if (length(selected_columns) == 0) {
        return(DT::datatable(data.frame(Message = "No columns selected or available.")))
      }
      
      # Subset data based on existing selected columns
      filtered_data <- raw_data[, ..selected_columns]
      
      # Create formatted datatable with uniform font style
      create_formatted_datatable(filtered_data, "Raw Data", wbid) %>%
        formatStyle(columns = names(filtered_data), `font-family` = 'Arial', textAlign = 'center') %>%
        formatRound(columns = intersect(c("year", "month", "day", "time", "COND"), names(filtered_data)), digits = 0, mark = "")  # Apply to relevant columns if they exist
    }, error = function(e) {
      showNotification(paste("Error in rendering raw data table:", e$message), type = "error")
      return(NULL)
    })
  }, server = TRUE)
  
  
  
  output$geomeans_data_table <- renderDT({
    req(results(), global_selected_wbid(), input$selected_params)
    res <- results()
    wbid <- global_selected_wbid()
    
    tryCatch({
      # Filter data for the selected WBID
      geomeans_data <- res$geomeans[res$geomeans$wbid == wbid, ]
      
      # Convert year to integer only if it exists
      if ("year" %in% names(geomeans_data)) geomeans_data$year <- as.integer(geomeans_data$year)
      
      # Round COND column if it exists
      if ("COND" %in% names(geomeans_data)) {
        geomeans_data$COND <- round(geomeans_data$COND, 0)
      }
      
      # Always include essential columns and append selected nutrient parameters
      essential_columns <- c("wbid", "year")
      
      # Add optional columns (month, day, time) if selected by the user
      optional_columns <- input$optional_columns
      selected_columns <- c(essential_columns, optional_columns, input$selected_params)
      
      # Check if each selected column exists in the data
      selected_columns <- selected_columns[selected_columns %in% names(geomeans_data)]
      
      # If no columns are left after filtering, show a message
      if (length(selected_columns) == 0) {
        return(DT::datatable(data.frame(Message = "No columns selected or available.")))
      }
      
      # Subset data based on existing selected columns
      filtered_data <- geomeans_data[, ..selected_columns]
      
      # Create formatted datatable with uniform font style
      create_formatted_datatable(filtered_data, "Yearly Geomeans", wbid) %>%
        formatStyle(columns = names(filtered_data), `font-family` = 'Arial', textAlign = 'center') %>%
        formatRound(columns = intersect(c("year", "COND"), names(filtered_data)), digits = 0, mark = "")  # Apply to relevant columns if they exist
    }, error = function(e) {
      showNotification(paste("Error in rendering geomeans data table:", e$message), type = "error")
      return(NULL)
    })
  }, server = TRUE)
  
  
  
  
  

  # Time Series Plot
  observe({
    req(results(), input$ts_wbid, input$ts_param, input$ts_data_type, input$ts_year_range)
    res <- results()
    selected_wbid <- input$ts_wbid
    param <- input$ts_param
    data_type <- input$ts_data_type
    
    # Ensure res, selected_wbid, and param are valid
    if (is.null(res) || length(selected_wbid) == 0 || is.null(param)) {
      return(NULL)
    }
    
    # Retrieve the appropriate dataset
    if (data_type == "raw") {
      data <- res$rawdata[res$rawdata$wbid == selected_wbid, ]
      data$date <- as.Date(paste(data$year, data$month, data$day, sep = "-"))
      time_var <- "date"
    } else {
      data <- res$geomeans[res$geomeans$wbid == selected_wbid, ]
      time_var <- "year"
    }
    
    # Ensure the year column is numeric and remove NA values
    data$year <- as.numeric(data$year)
    data <- data[!is.na(data$year), ]
    
    # Filter data based on the selected year range
    filtered_data <- data[data$year >= input$ts_year_range[1] & data$year <= input$ts_year_range[2], ]
    
    # Proceed with plotting if there is data after filtering
    if (nrow(filtered_data) > 0 && !all(is.na(filtered_data[[param]]))) {
      output$tsPlot <- renderPlotly({
        plot_title <- paste("Time Series Plot -", param, "- WBID:", selected_wbid)
        create_timeseries_plot(filtered_data, time_var, param, param_labels, title = plot_title, param_name = param)
      })
    } else {
      # If data is invalid, return an empty plot with a message
      output$tsPlot <- renderPlotly({
        plot_ly() %>%
          add_annotations(
            text = "No valid data available for the selected parameter and WBID",
            showarrow = FALSE,
            font = list(size = 16)
          )
      })
    }
  })
  
  
  
  # Update UI elements for correlogram
  observe({
    req(results())
    res <- results()
    available_wbids <- unique(res$rawdata$wbid)
    available_params <- setdiff(names(res$rawdata), c("wbid", "sta", "year", "month", "day", "time"))
    
    wbid_choices <- prepare_wbid_choices(available_wbids, waterbody_info)
    
    updateSelectizeInput(session, "corr_wbid", choices = wbid_choices, server = TRUE)
    
    # Define default parameters
    default_params <- c("CHLAC", "TN", "TP", "COLOR", "COND", "ALK", "DO")
    
    # Select parameters that are available in the data
    selected_params <- intersect(default_params, available_params)
    
    # If none of the default parameters are available, fall back to the first 5 available parameters
    if (length(selected_params) == 0) {
      selected_params <- available_params[1:min(5, length(available_params))]
    }
    
    updateSelectizeInput(session, "correlogram_params", 
                         choices = available_params,
                         selected = selected_params,
                         server = TRUE)
  })
  
  # Correlogram logic
  # In server.R around line 1331, modify the correlogram observer:
  observe({
    req(results(), input$corr_wbid, input$correlogram_params, input$corr_data_type, input$correlogram_method)
    res <- results()
    selected_wbid <- input$corr_wbid
    selected_params <- input$correlogram_params
    data_type <- input$corr_data_type
    cor_method <- input$correlogram_method
    
    if (data_type == "raw") {
      data <- res$rawdata[res$rawdata$wbid == selected_wbid, ]
    } else {
      data <- res$geomeans[res$geomeans$wbid == selected_wbid, ]
    }
    
    # Convert to data.table if it's not already
    if (!is.data.table(data)) {
      data <- as.data.table(data)
    }
    
    # Select only the specified parameters that exist in the data
    valid_params <- intersect(selected_params, names(data))
    
    # Add validation to prevent the error
    if (length(valid_params) < 2) {
      output$correlogram <- renderPlot({
        plot.new()
        text(0.5, 0.5, "Insufficient data for correlation analysis. Please select at least two parameters with valid data.", cex = 1.2)
      })
      return(NULL)
    }
    
    data <- data[, ..valid_params]
    
    # Remove any non-numeric columns and check again
    numeric_cols <- sapply(data, is.numeric)
    if (sum(numeric_cols) < 2) {
      output$correlogram <- renderPlot({
        plot.new()
        text(0.5, 0.5, "Insufficient numeric data for correlation analysis", cex = 1.2)
      })
      return(NULL)
    }
    
    data <- data[, numeric_cols, with = FALSE]
    
    # Remove rows with all NA values
    data <- na.omit(data)
    
    # Final check before calculating correlation
    if (ncol(data) < 2 || nrow(data) < 3) {
      output$correlogram <- renderPlot({
        plot.new()
        text(0.5, 0.5, "Insufficient data for correlation analysis after removing missing values", cex = 1.2)
      })
      return(NULL)
    }
    
    # Calculate correlation matrix based on selected method
    tryCatch({
      cor_matrix <- cor(data, method = cor_method, use = "pairwise.complete.obs")
      
      output$correlogram <- renderPlot({
        corrplot(cor_matrix, method = "circle", type = "upper", order = "hclust", 
                 tl.col = "black", tl.srt = 45, addCoef.col = "black", 
                 number.cex = 0.7, tl.cex = 0.7, title = paste(cor_method, "Correlation"),
                 mar = c(0,0,1,0))
      })
    }, error = function(e) {
      output$correlogram <- renderPlot({
        plot.new()
        text(0.5, 0.5, paste("Error in correlation calculation:", e$message), cex = 1.2)
      })
    })
  })
  
  # General explanation for correlation matrix
  output$correlogram_general_explanation <- renderUI({
    HTML("
    <p>The correlation matrix provides a visual representation of the relationships between selected parameters.</p>
    <ul>
      <li>Blue circles indicate positive correlations</li>
      <li>Red circles indicate negative correlations</li>
      <li>The size of the circle represents the strength of the correlation</li>
      <li>Perfect correlations (1 or -1) are represented by full circles</li>
      <li>No correlation (0) is represented by a blank space</li>
    </ul>
    <p>The default selection includes key parameters for water quality analysis, but you can modify this selection to explore other relationships.</p>
  ")
  })
  
  # Method-specific explanation
  output$correlogram_method_explanation <- renderUI({
    req(input$correlogram_method)
    if(input$correlogram_method == "pearson") {
      HTML("
      <h4>Pearson Correlation</h4>
      <p>The Pearson correlation measures the strength and direction of linear relationships between pairs of continuous variables.</p>
      <ul>
        <li>Assumes variables are normally distributed</li>
        <li>Sensitive to outliers</li>
        <li>Best for linear relationships</li>
      
    ")
    } else {
      HTML("
      <h4>Spearman Correlation</h4>
      <p>The Spearman correlation measures the strength and direction of monotonic relationships between pairs of variables.</p>
      <ul>
        <li>Does not assume normal distribution</li>
        <li>Less sensitive to outliers</li>
        <li>Can detect non-linear relationships if they are monotonic</li>
      
    ")
    }
  })
  
  
  # XY Plot 
  observe({
    req(results(), input$xy_wbid, input$x_param, input$y_param, input$xy_data_type)
    res <- results()
    selected_wbid <- input$xy_wbid
    x_param <- input$x_param
    y_param <- input$y_param
    data_type <- input$xy_data_type
    add_trend_line <- input$add_trend_line
    
    if (data_type == "raw") {
      data <- res$rawdata[res$rawdata$wbid == selected_wbid, ]
    } else {
      data <- res$geomeans[res$geomeans$wbid == selected_wbid, ]
    }
    
    if (!(x_param %in% names(data)) || !(y_param %in% names(data))) {
      showNotification("Selected parameters are not available in the data", type = "warning")
      return(NULL)
    }
    
    plot <- ggplot(data, aes(x = !!sym(x_param), y = !!sym(y_param))) +
      geom_point(alpha = 0.7) +
      labs(title = paste("XY Plot -", x_param, "vs", y_param, "- WBID:", selected_wbid),
           x = param_labels[[x_param]], y = param_labels[[y_param]]) +
      theme_minimal()
    
    if (add_trend_line) {
      plot <- plot + geom_smooth(method = "lm", se = FALSE, color = "red")
    }
    
    output$xyPlot <- renderPlotly({
      ggplotly(plot)
    })
  })
  
  
  # Nutrient Analysis Plots
  observe({
    req(results(), input$nutrient_wbid)
    res <- results()
    data_type <- input$nutrient_data_type
    add_trend_line <- input$add_trend_line_nutrients
    nutrient_pairs <- list(
      c("TN", "CHLAC"),
      c("TP", "CHLAC"),
      c("TN", "TP"),
      c("COLOR", "CHLAC")
    )
    
    for (i in seq_along(nutrient_pairs)) {
      local({
        plot_id <- paste0("nutrientPlot", i)
        x_param <- nutrient_pairs[[i]][1]
        y_param <- nutrient_pairs[[i]][2]
        
        output[[plot_id]] <- renderPlotly({
          if (is.null(res)) {
            showNotification("No data available for plotting", type = "warning")
            return(NULL)
          }
          
          tryCatch({
            if (data_type == "raw") {
              data <- res$rawdata[res$rawdata$wbid == input$nutrient_wbid, ]
            } else {
              data <- res$geomeans[res$geomeans$wbid == input$nutrient_wbid, ]
            }
            
            if (!(x_param %in% names(data)) || !(y_param %in% names(data))) {
              showNotification("Selected parameters are not available in the data", type = "warning")
              return(NULL)
            }
            
            plot <- ggplot(data, aes(x = !!sym(x_param), y = !!sym(y_param))) +
              geom_point(alpha = 0.7) +
              labs(title = paste(x_param, "vs", y_param),
                   x = param_labels[[x_param]], y = param_labels[[y_param]]) +
              theme_minimal()
            
            if (add_trend_line) {
              plot <- plot + geom_smooth(method = "lm", se = FALSE, color = "red")
            }
            
            ggplotly(plot) %>% 
              layout(margin = list(t = 40, r = 20, b = 40, l = 60))
          }, error = function(e) {
            showNotification(paste("Error in creating plot:", e$message), type = "error")
            cat("Error details:", e$message, "\n")
            print(str(data))
            NULL
          })
        })
      })
    }
  })
  
  
  # WBID Selection for Visualization
  output$viz_wbid_selector <- renderUI({
    req(results())
    wbids <- unique(results()$rawdata$wbid)
    choices <- prepare_wbid_choices(wbids, waterbody_info)
    selectInput("global_selected_wbid", "Select WBID for Visualization:", 
                choices = choices, 
                selected = global_selected_wbid())
  })
  
  # Update global_selected_wbid when viz_wbid changes
  observeEvent(input$global_selected_wbid, {
    global_selected_wbid(input$global_selected_wbid)
  })
  
  # Observe changes in results and update UI elements accordingly
  observe({
    req(results())
    res <- results()
    
    all_params <- setdiff(names(res$rawdata), c("wbid", "sta", "year", "month", "day", "time"))
    
    updateSelectInput(session, "overview_param", 
                      choices = all_params,
                      selected = ifelse("CHLAC" %in% all_params, "CHLAC", all_params[1]))
  })
  
  # Update UI elements based on results
  observe({
    req(results())
    res <- results()
    available_wbids <- unique(res$rawdata$wbid)
    available_params <- setdiff(names(res$rawdata), c("wbid", "sta", "year", "month", "day", "time"))
    
    wbid_choices <- prepare_wbid_choices(available_wbids, waterbody_info)
    
    updateSelectInput(session, "ts_wbid", choices = wbid_choices)
    updateSelectInput(session, "xy_wbid", choices = wbid_choices)
    updateSelectInput(session, "nutrient_wbid", choices = wbid_choices)
    updateSelectInput(session, "reg_wbid", choices = wbid_choices)
    updateSelectInput(session, "trend_wbid", choices = wbid_choices)
    
    updateSelectizeInput(session, "x_param", choices = available_params, selected = available_params[1], server = TRUE)
    updateSelectizeInput(session, "y_param", choices = available_params, selected = ifelse(length(available_params) > 1, available_params[2], available_params[1]), server = TRUE)
    updateSelectizeInput(session, "ts_param", choices = available_params, selected = available_params[1], server = TRUE)
    
    # Updated to reflect default selections for regression
    updateSelectizeInput(session, "regression_response", choices = available_params, selected = "CHLAC", server = TRUE)
    updateSelectizeInput(session, "regression_explanatory1", choices = available_params, selected = "TN", server = TRUE)
    updateSelectizeInput(session, "regression_explanatory2", choices = available_params, selected = "TP", server = TRUE)
    
    updateSelectInput(session, "trend_parameter", choices = available_params)
  })
  
  
  
####################################################################################################################################################################################
# Geospatial Analysis
####################################################################################################################################################################################

# Create a waiter loading screen
w <- Waiter$new(id = "geospatial_map", html = spin_fading_circles())

# Load lake flowlines
lake_flowlines <- load_lake_flowlines("maps/lake_flowline/NHD_flowline_lakes.geojson")

# Initialize reactive values
geo_filtered_data <- reactiveVal(list(wbids = NULL, regions = NULL, flowlines = NULL))
selected_wbids_for_regression <- reactiveVal(character(0))
selected_wbid <- reactiveVal(NULL)
selected_wbid_for_agm <- reactiveVal(NULL)

# Initialize the map
output$geospatial_map <- renderLeaflet({
  leaflet() %>%
    addProviderTiles(providers$Esri.WorldTopoMap) %>%
    setView(lng = -81.5158, lat = 27.6648, zoom = 6)  # Centered on Florida
})

# Prepare WBID data for selectize input
observe({
  req(waterbody_info, lake_wbid_geojson)
  choices <- waterbody_info %>%
    filter(WBID %in% lake_wbid_geojson$WBID) %>%
    mutate(label = paste(WBID, "-", WATERBODY_NAME)) %>%
    select(WBID, WATERBODY_NAME, label)
  
  updateSelectizeInput(session, "wbid_geo", choices = choices, server = TRUE)
})

# Update the selected WBID when the input changes
observeEvent(input$wbid_geo, {
  new_wbid <- input$wbid_geo
  if (!is.null(new_wbid) && new_wbid != "") {
    current_selection <- selected_wbids_for_regression()
    if (!(new_wbid %in% current_selection)) {
      current_selection <- c(current_selection, new_wbid)
      selected_wbids_for_regression(current_selection)
      updateSelectizeInput(session, "selected_wbids_for_regression", selected = current_selection)
    }
    selected_wbid(new_wbid)
  }
  update_geospatial_map()
})

# Filter WBIDs based on selected WBID
observeEvent(input$submit_geo, {
  req(input$wbid_geo)
  
  w$show()  # Show loading screen
  
  withProgress(message = 'Filtering lakes...', value = 0, {
    input_wbid <- input$wbid_geo
    
    input_class <- lake_class %>% filter(WBID == input_wbid) %>% pull(CLASS)
    if (length(input_class) == 0) {
      showNotification("The selected WBID does not exist in the lake class dataset.", type = "error")
      w$hide()
      return(NULL)
    }
    
    input_wbid_region <- lake_wbid_region %>% filter(WBID == input_wbid)
    if (nrow(input_wbid_region) == 0) {
      showNotification("The selected WBID does not exist in the lake WBID region dataset.", type = "error")
      w$hide()
      return(NULL)
    }
    
    incProgress(0.2)
    
    input_regions <- input_wbid_region$REGION
    wbids_in_regions <- lake_wbid_region %>% filter(REGION %in% input_regions)
    filtered_wbids <- wbids_in_regions %>%
      inner_join(lake_class %>% filter(CLASS == input_class), by = "WBID")
    
    if (nrow(filtered_wbids) == 0) {
      showNotification("No WBIDs found for the specified CLASS in the filtered regions.", type = "error")
      geo_filtered_data(list(wbids = NULL, regions = NULL, flowlines = NULL))
      w$hide()
      return(NULL)
    }
    
    incProgress(0.4)
    
    filtered_wbid_geom <- lake_wbid_geojson[lake_wbid_geojson$WBID %in% filtered_wbids$WBID, ]
    filtered_region_geom <- region_geojson[region_geojson$REGION %in% input_regions, ]
    
    incProgress(0.6)
    
    filtered_flowlines <- NULL
    if (!is.null(lake_flowlines) && inherits(lake_flowlines, "sf") && nrow(lake_flowlines) > 0) {
      tryCatch({
        filtered_flowlines <- st_intersection(lake_flowlines, st_union(filtered_region_geom))
      }, error = function(e) {
        showNotification("Error processing flowlines data.", type = "warning")
      })
    } else {
      showNotification("Flowlines data not available or empty.", type = "warning")
    }
    
    incProgress(0.8)
    
    geo_filtered_data(list(wbids = filtered_wbid_geom, regions = filtered_region_geom, flowlines = filtered_flowlines))
    
    update_geospatial_map()
    
    updateSelectizeInput(session, "selected_wbids_for_regression", 
                         choices = setNames(filtered_wbid_geom$WBID, 
                                            paste(filtered_wbid_geom$WBID, "-", filtered_wbid_geom$WATERBODY_)),
                         selected = character(0))
    
    incProgress(1)
  })
  
  w$hide()  # Hide loading screen
  showNotification("Lake filtering completed successfully", type = "message")
})

# Update geospatial map
update_geospatial_map <- function() {
  data <- geo_filtered_data()
  selected <- selected_wbid()
  current_selection <- selected_wbids_for_regression()
  
  map <- leafletProxy("geospatial_map") %>%
    clearGroup("Regions") %>%
    clearGroup("WBIDs") %>%
    clearGroup("Flowlines") %>%
    clearGroup("SelectedWBIDs") %>%
    clearControls()  # This will clear the existing legend
  
  if (!is.null(data$regions) && nrow(data$regions) > 0) {
    map <- map %>%
      addPolygons(data = data$regions, color = "green", weight = 1, fillOpacity = 0.5, group = "Regions",
                  popup = ~paste("Region:", REGION))
  }
  
  if (!is.null(data$wbids) && nrow(data$wbids) > 0) {
    map <- map %>%
      addPolygons(data = data$wbids, color = "blue", weight = 1, fillOpacity = 0.5, group = "WBIDs",
                  layerId = ~WBID,
                  label = ~paste(WBID, "-", WATERBODY_),
                  labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                              textsize = "15px", direction = "auto"),
                  popup = ~paste("WBID:", WBID, "<br>Waterbody Name:", WATERBODY_, "<br>Class:", CLASS))
  }
  
  if (!is.null(data$flowlines) && nrow(data$flowlines) > 0) {
    map <- map %>%
      addPolylines(data = data$flowlines, color = "red", weight = 2, opacity = 0.8, group = "Flowlines",
                   popup = ~paste("Flowline ID:", ifelse("COMID" %in% names(.), COMID, "N/A")))
  }
  
  if (length(current_selection) > 0 && !is.null(data$wbids)) {
    selected_data <- data$wbids[data$wbids$WBID %in% current_selection, ]
    if (nrow(selected_data) > 0) {
      map <- map %>%
        addPolygons(data = selected_data, 
                    color = "orange", weight = 2, fillOpacity = 0.7, group = "SelectedWBIDs")
    }
  }
  
  # Add layers control and legend only if there's data to show
  if (!is.null(data$regions) || !is.null(data$wbids) || !is.null(data$flowlines) || length(current_selection) > 0) {
    map <- map %>%
      addLayersControl(
        overlayGroups = c("Regions", "WBIDs", "Flowlines", "SelectedWBIDs"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addLegend(position = "bottomright", 
                colors = c("green", "blue", "red", "orange"), 
                labels = c("Lake Regions", "Filtered WBIDs", "Flowlines", "Selected WBIDs"), 
                title = "Legend")
  }
  
  update_map_extent()
}

# Handle map click events for WBID selection
observeEvent(input$geospatial_map_shape_click, {
  click <- input$geospatial_map_shape_click
  if (!is.null(click$id)) {
    clicked_wbid <- click$id
    current_selection <- selected_wbids_for_regression()
    
    if (clicked_wbid %in% current_selection) {
      current_selection <- setdiff(current_selection, clicked_wbid)
    } else {
      current_selection <- c(current_selection, clicked_wbid)
    }
    
    selected_wbids_for_regression(current_selection)
    updateSelectizeInput(session, "selected_wbids_for_regression", selected = current_selection)
    
    # Update the entire map to ensure proper legend update
    update_geospatial_map()
  }
})

# Update selected WBIDs when input changes
observeEvent(input$selected_wbids_for_regression, {
  selected_wbids_for_regression(input$selected_wbids_for_regression)
  update_geospatial_map()
})

# Render filtered WBIDs table (now showing only selected WBIDs for regression)
output$filtered_lakes_table <- renderDT({
  data <- geo_filtered_data()$wbids
  selected <- selected_wbids_for_regression()
  
  if (is.null(data) || length(selected) == 0) {
    return(datatable(data.frame(Message = "No WBIDs selected for regression.")))
  }
  
  selected_data <- data[data$WBID %in% selected, ]
  
  if (nrow(selected_data) == 0) {
    return(datatable(data.frame(Message = "No data available for the selected WBIDs.")))
  }
  
  available_columns <- intersect(c("WBID", "WATERBODY_", "CLASS", "REGION"), names(selected_data))
  
  datatable(selected_data %>% 
              st_drop_geometry() %>% 
              select(all_of(available_columns)),
            options = list(pageLength = 10, scrollX = TRUE),
            selection = 'single')  # Make the table rows selectable
})

# Create a reactive value to store the selected WBID for AGM data
selected_wbid_for_agm <- reactiveVal(NULL)

# Update the selected WBID when a row in the filtered lakes table is clicked
observeEvent(input$filtered_lakes_table_rows_selected, {
  selected_row <- input$filtered_lakes_table_rows_selected
  if (length(selected_row) > 0) {
    selected_data <- geo_filtered_data()$wbids[geo_filtered_data()$wbids$WBID %in% selected_wbids_for_regression(), ]
    selected_wbid <- selected_data$WBID[selected_row]
    selected_wbid_for_agm(selected_wbid)
  }
})

# Create the AGM data display UI
output$agm_data_display <- renderUI({
  req(selected_wbid_for_agm())
  
  # Here you would fetch the AGM data for the selected WBID
  # For this example, I'll assume you have a function get_agm_data(wbid) that returns the data
  agm_data <- get_agm_data(selected_wbid_for_agm())
  
  # Get the list of all available parameters and exclude essential columns
  all_params <- setdiff(names(agm_data), c("wbid", "year", "month", "day", "time"))
  
  tagList(
    h3(paste("AGM Data for WBID:", selected_wbid_for_agm())),
    
    # UI for selecting nutrient parameters to display
    checkboxGroupInput("selected_params", "Select Nutrient Parameters to Display:", 
                       choices = all_params, 
                       selected = all_params, 
                       inline = TRUE),
    
    # UI for selecting optional columns (month, day, time)
    checkboxGroupInput("optional_columns", "Select Optional Columns to Display:", 
                       choices = c("Month" = "month", "Day" = "day", "Time" = "time"),
                       selected = c("month", "day", "time"),  # Default selection
                       inline = TRUE),
    
    # Tab box for data display
    tabBox(
      title = "AGM Data Display",
      width = 12,
      tabPanel("Raw Data", DTOutput("agm_raw_data_table", height = "500px")),  # Set height for scroll
      tabPanel("Yearly Geomeans", DTOutput("agm_geomeans_data_table", height = "500px"))  # Set height for scroll
    )
  )
})

# Render the AGM raw data table
output$agm_raw_data_table <- renderDT({
  req(selected_wbid_for_agm())
  
  agm_data <- get_agm_data(selected_wbid_for_agm())
  
  # Filter columns based on user selection
  selected_columns <- c("wbid", "year", input$selected_params, input$optional_columns)
  filtered_data <- agm_data[, selected_columns]
  
  datatable(filtered_data, options = list(pageLength = 10, scrollX = TRUE))
})


# Render the AGM yearly geomeans table 
output$agm_geomeans_data_table <- renderDT({
  req(selected_wbid_for_agm())
  
  agm_data <- get_agm_data(selected_wbid_for_agm())
  
  # Calculate yearly geomeans for selected parameters
  geomeans_data <- agm_data %>%
    group_by(wbid, year) %>%
    summarise(across(all_of(input$selected_params), ~exp(mean(log(.x), na.rm = TRUE))), .groups = "drop")
  
  datatable(geomeans_data, options = list(pageLength = 10, scrollX = TRUE))
})

# Reset geospatial analysis
observeEvent(input$reset_geo, {
  # Reset reactive values
  geo_filtered_data(list(wbids = NULL, regions = NULL, flowlines = NULL))
  selected_wbids_for_regression(character(0))
  selected_wbid(NULL)
  selected_wbid_for_agm(NULL)
  
  # Reset input controls
  updateSelectizeInput(session, "wbid_geo", selected = character(0))
  updateSelectizeInput(session, "selected_wbids_for_regression", choices = NULL, selected = character(0))
  
  # Reset geospatial map
  leafletProxy("geospatial_map") %>%
    clearShapes() %>%
    clearControls() %>%
    setView(lng = -81.5158, lat = 27.6648, zoom = 6)
  
  # Reset filtered lakes table
  output$filtered_lakes_table <- renderDT({
    datatable(data.frame(Message = "No lakes selected for regression."))
  })
  
  # Reset AGM data display
  output$agm_data_display <- renderUI({
    NULL
  })
  
  # Show notification
  showNotification("Geospatial analysis has been reset.", type = "message")
})

# Zoom to Florida for geospatial analysis
observeEvent(input$zoomFlorida_geo, {
  leafletProxy("geospatial_map") %>% zoom_to_florida()
})

# Use selected WBIDs for combined regression
observeEvent(input$use_for_regression, {
  selected_wbids <- selected_wbids_for_regression()
  if (length(selected_wbids) > 0) {
    updateSelectizeInput(session, "combined_wbids", selected = selected_wbids)
    updateTabItems(session, "sidebar", selected = "regression_analysis")
    showNotification("Selected WBIDs have been added to the combined regression analysis.", type = "message")
  } else {
    showNotification("Please select at least one WBID for regression analysis.", type = "warning")
  }
})

# Function to update map extent based on visible data
update_map_extent <- function() {
  data <- geo_filtered_data()
  
  all_geometries <- list()
  
  if (!is.null(data$wbids) && nrow(data$wbids) > 0) {
    all_geometries <- c(all_geometries, list(st_geometry(data$wbids)))
  }
  if (!is.null(data$regions) && nrow(data$regions) > 0) {
    all_geometries <- c(all_geometries, list(st_geometry(data$regions)))
  }
  
  if (length(all_geometries) > 0) {
    combined_geometries <- do.call(c, all_geometries)
    if (length(combined_geometries) > 0) {
      bbox <- st_bbox(combined_geometries)
      leafletProxy("geospatial_map") %>% fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])
    }
  }
}

# Zoom to Florida function
zoom_to_florida <- function(map) {
  map %>% setView(lng = -81.5158, lat = 27.6648, zoom = 6)
}

# Function to get AGM data (placeholder - replace with your actual data retrieval logic)
get_agm_data <- function(wbid) {
  # This is a placeholder function. Replace this with your actual data retrieval logic.
  # For now, it returns a sample dataset
  data.frame(
    wbid = rep(wbid, 100),
    year = rep(2010:2019, each = 10),
    month = rep(1:12, 10)[1:100],
    day = sample(1:28, 100, replace = TRUE),
    time = format(as.POSIXct(paste(Sys.Date(), sample(seq(0, 86399, 60), 100, replace = TRUE))), "%H:%M:%S"),
    parameter1 = rnorm(100, mean = 10, sd = 2),
    parameter2 = rnorm(100, mean = 5, sd = 1),
    parameter3 = rnorm(100, mean = 15, sd = 3)
  )
}
  



####################################################################################################################################################################################
# Regression analysis
####################################################################################################################################################################################

# Comprehensive Regression Analysis Server Logic

# Update manual WBID choices
observe({
  req(waterbody_info)
  choices <- setNames(waterbody_info$WBID, paste(waterbody_info$WBID, "-", waterbody_info$WATERBODY_NAME))
  updateSelectizeInput(session, "manual_combined_wbids", choices = choices, server = TRUE)
})

# Update geospatial WBID choices when geo_filtered_data changes
observe({
  req(geo_filtered_data())
  filtered_wbids <- geo_filtered_data()$wbids
  if (!is.null(filtered_wbids)) {
    choices <- setNames(filtered_wbids$WBID, paste(filtered_wbids$WBID, "-", filtered_wbids$WATERBODY_))
    updateSelectizeInput(session, "geospatial_combined_wbids", choices = choices, selected = selected_wbids_for_regression())
  }
})

# Update selected WBIDs based on selection method
observe({
  if (input$regression_type == "multiple") {
    if (input$wbid_selection_method == "geospatial") {
      selected_wbids_for_regression(input$geospatial_combined_wbids)
    } else {
      selected_wbids_for_regression(input$manual_combined_wbids)
    }
  } else {
    selected_wbids_for_regression(input$reg_wbid)
  }
})

# Shapiro-Wilk normality test
observeEvent(input$runShapiroWilk, {
  req(input$regression_response, input$regression_explanatory1)
  withProgress(message = 'Running normality test', value = 0, {
    data <- if (input$regression_type == "single") {
      results()$geomeans %>%
        filter(wbid == input$reg_wbid, year >= input$start_year, year <= input$end_year) %>%
        select(!!input$regression_response, !!input$regression_explanatory1, 
               if (input$enable_explanatory2) !!input$regression_explanatory2 else NULL)
    } else {
      prepare_multiple_wbid_data(
        selected_wbids = selected_wbids_for_regression(),
        start_year = input$start_year,
        end_year = input$end_year,
        response = input$regression_response,
        explanatory1 = input$regression_explanatory1,
        explanatory2 = if (input$enable_explanatory2) input$regression_explanatory2 else NULL,
        IWR_path = sql_database_locator()
      )
    }
    
    normality_table <- shapiro_wilk_test(data, data_type = "agm")
    shapiro_results(normality_table)
    
    output$normality_table <- renderTable({
      normality_table %>%
        select(Parameter, n, p_value, normality) %>%
        rename(
          "Sample Size" = n,
          "P-Value" = p_value,
          "Normality Assessment" = normality
        )
    })
    
    non_normal_vars <- normality_table %>% filter(normality == "Non-normal") %>% pull(Parameter)
    if (length(non_normal_vars) > 0) {
      notification_message <- paste("The following variables may not be normally distributed:",
                                    paste(non_normal_vars, collapse = ", "),
                                    ". Consider log transformation.")
    } else {
      notification_message <- "All variables appear to be normally distributed (p > 0.05)."
    }
    showNotification(notification_message, type = "message")
    incProgress(1)
  })
})

# Run Regression
observeEvent(input$runRegression, {
  req(input$regression_response, input$regression_explanatory1)
  if (input$regression_type == "multiple" && length(selected_wbids_for_regression()) < 2) {
    showNotification("Please select at least two WBIDs for multiple WBID analysis.", type = "warning")
    return()
  }
  
  withProgress(message = 'Running regression analysis', value = 0, {
    tryCatch({
      # Prepare data
      data <- if (input$regression_type == "single") {
        prepare_single_wbid_data(
          data = results()$geomeans,
          wbid = input$reg_wbid,
          start_year = input$start_year,
          end_year = input$end_year,
          response = input$regression_response,
          explanatory1 = input$regression_explanatory1,
          explanatory2 = if (input$enable_explanatory2) input$regression_explanatory2 else NULL
        )
      } else {
        prepare_multiple_wbid_data(
          selected_wbids = selected_wbids_for_regression(),
          start_year = input$start_year,
          end_year = input$end_year,
          response = input$regression_response,
          explanatory1 = input$regression_explanatory1,
          explanatory2 = if (input$enable_explanatory2) input$regression_explanatory2 else NULL,
          IWR_path = sql_database_locator()
        )
      }
      
      #Debugging: Print data summary
      print("Data summary:")
      print(summary(data))
      print("Data types:")
      print(sapply(data, class))
      print("Data info:")
      print(sapply(data, function(x) {
        if (is.numeric(x)) {
          c(min = min(x, na.rm = TRUE), 
            max = max(x, na.rm = TRUE), 
            na_count = sum(is.na(x)))
        } else if (is.factor(x)) {
          c(levels = paste(levels(x), collapse = ", "), 
            na_count = sum(is.na(x)))
        } else {
          c(class = class(x)[1], 
            na_count = sum(is.na(x)))
        }
      }))
      
      # Perform regression
      reg_results <- if (input$regression_type == "single") {
        perform_single_regression(
          data = data,
          response = input$regression_response,
          explanatory1 = input$regression_explanatory1,
          explanatory2 = if (input$enable_explanatory2) input$regression_explanatory2 else NULL,
          include_interaction = input$include_interaction,
          log_transform = input$log_transform
        )
      } else {
        perform_multiple_regression(
          data = data,
          response = input$regression_response,
          explanatory1 = input$regression_explanatory1,
          explanatory2 = if (input$enable_explanatory2) input$regression_explanatory2 else NULL,
          include_interaction = input$include_interaction,
          log_transform = input$log_transform
        )
      }
      
      # Perform assumption checks
      assumption_checks <- list()
      
      # Normality check (Shapiro-Wilk test on residuals)
      assumption_checks$normality <- shapiro.test(residuals(reg_results$model))
      
      # Homoscedasticity check (Breusch-Pagan test)
      assumption_checks$homoscedasticity <- bptest(reg_results$model)
      
      # Multicollinearity check (VIF)
      if (length(coef(reg_results$model)) > 2) {  # Only applicable for multiple predictors
        assumption_checks$multicollinearity <- vif(reg_results$model)
      } else {
        assumption_checks$multicollinearity <- "Not applicable for single predictor models"
      }
      
      # Autocorrelation check (Durbin-Watson test)
      assumption_checks$autocorrelation <- dwtest(reg_results$model)
      
      # Add assumption checks to reg_results
      reg_results$assumption_checks <- assumption_checks
      
      regression_results(reg_results)
      
      output$regressionSummary <- renderTable({
        reg_results$summary %>%
          mutate(
            p.value = format.pval(p.value, digits = 3),
            estimate = format(estimate, digits = 3, scientific = FALSE),
            std.error = format(std.error, digits = 3, scientific = FALSE)
          )
      })
      
      output$r_squared_box <- renderValueBox({
        valueBox(
          round(reg_results$glance$r.squared, 3),
          "R-squared",
          icon = icon("chart-line"),
          color = "blue"
        )
      })
      
      output$adj_r_squared_box <- renderValueBox({
        valueBox(
          round(reg_results$glance$adj.r.squared, 3),
          "Adjusted R-squared",
          icon = icon("chart-line"),
          color = "blue"
        )
      })
      
      output$f_statistic_box <- renderValueBox({
        valueBox(
          round(reg_results$glance$statistic, 2),
          "F-statistic",
          icon = icon("calculator"),
          color = "green"
        )
      })
      
      output$p_value_box <- renderValueBox({
        valueBox(
          format.pval(reg_results$glance$p.value, digits = 3),
          "p-value",
          icon = icon("check"),
          color = if (reg_results$glance$p.value < 0.05) "green" else "yellow"
        )
      })
      
      output$regressionPlot <- renderPlotly({
        req(regression_results())
        reg_results <- regression_results()
        tryCatch({
          if (input$regression_type == "single") {
            plot <- create_single_regression_plot(
              data = reg_results$data,
              response = input$regression_response,
              explanatory1 = input$regression_explanatory1,
              explanatory2 = if (input$enable_explanatory2) input$regression_explanatory2 else NULL,
              param_labels = param_labels,
              include_interaction = as.logical(input$include_interaction)
            )
          } else {
            plot <- create_multiple_regression_plot(
              data = reg_results$data,
              response = input$regression_response,
              explanatory1 = input$regression_explanatory1,
              explanatory2 = if (input$enable_explanatory2) input$regression_explanatory2 else NULL,
              param_labels = param_labels,
              include_interaction = as.logical(input$include_interaction),
              use_size_for_explanatory2 = as.logical(input$use_size_for_explanatory2)
            )
          }
          ggplotly(plot)
        }, error = function(e) {
          showNotification(paste("Error in creating regression plot:", e$message), type = "error")
          plot_ly() %>% add_annotations(
            text = paste("Error in creating regression plot:", e$message),
            showarrow = FALSE,
            font = list(size = 15)
          )
        })
      })
      
      # Diagnostic plots
      diagnostic_plots <- create_diagnostic_plots(reg_results$model, multiple_wbids = input$regression_type == "multiple")
      
      output$resid_vs_fitted <- renderPlotly({ ggplotly(diagnostic_plots$resid_vs_fitted) })
      output$qq_plot <- renderPlotly({ ggplotly(diagnostic_plots$qq_plot) })
      output$scale_location <- renderPlotly({ ggplotly(diagnostic_plots$scale_location) })
      output$resid_vs_leverage <- renderPlotly({ ggplotly(diagnostic_plots$resid_vs_leverage) })
      
      # Additional diagnostic tests
      output$vif_table <- renderTable({
        if (is.null(reg_results$assumption_checks$multicollinearity) || 
            is.character(reg_results$assumption_checks$multicollinearity)) {
          data.frame(Message = reg_results$assumption_checks$multicollinearity)
        } else {
          data.frame(Variable = names(reg_results$assumption_checks$multicollinearity), 
                     VIF = reg_results$assumption_checks$multicollinearity)
        }
      })
      
      output$bp_test <- renderPrint({ reg_results$assumption_checks$homoscedasticity })
      output$dw_test <- renderPrint({ reg_results$assumption_checks$autocorrelation })
      
      # Generate and render interpretation
      interpretation_results <- interpret_regression_results(reg_results, shapiro_results())
      
      # Debugging: Print interpretation results
      print("Interpretation Results:")
      print(str(interpretation_results))
      
      output$modelOverview <- renderUI({ interpretation_results$modelOverview })
      output$modelPerformance <- renderUI({ interpretation_results$modelPerformance })
      output$coefficientInterpretation <- renderUI({ 
        tagList(interpretation_results$coefficientInterpretation)
      })
      output$normalityCheck <- renderUI({ interpretation_results$normalityCheck })
      output$homoscedasticityCheck <- renderUI({ interpretation_results$homoscedasticityCheck })
      output$multicollinearityCheck <- renderUI({ interpretation_results$multicollinearityCheck })
      output$autocorrelationCheck <- renderUI({ interpretation_results$autocorrelationCheck })
      output$conclusionInterpretation <- renderUI({ interpretation_results$conclusionInterpretation })
      
      # Add a debug output for interpretation
      output$debugInterpretation <- renderPrint({
        str(interpretation_results)
      })
      
      showNotification("Regression analysis completed successfully.", type = "message")
    }, error = function(e) {
      showNotification(paste("Error in regression analysis:", e$message), type = "error")
      print(paste("Error in regression analysis:", e$message))
      print("Error traceback:")
      print(traceback())
    })
  })
})

# Update impaired nutrient choices based on regression results
observe({
  req(regression_results())
  coefficients <- regression_results()$summary$term
  nutrients_in_regression <- c("TN", "TP")[c("TN", "TP") %in% coefficients]
  updateCheckboxGroupInput(session, "impaired_nutrients",
                           choices = nutrients_in_regression,
                           selected = nutrients_in_regression[1])
})

# Define the null-coalesce operator if not already defined
`%||%` <- function(a, b) if (is.null(a)) b else a

# Helper function to safely display values
safe_display <- function(value, default = "N/A", digits = 3) {
  if (is.null(value) || is.na(value)) {
    return(default)
  } else {
    return(round(as.numeric(value), digits))
  }
}

# Helper function to calculate TMDL for a single dataset
calculate_tmdl_single <- function(data, reg_results, chlac_target, impaired_nutrients, 
                                  lake_criteria, use_paleo_tp, tp_paleo, 
                                  reduction_scenario = "both", 
                                  custom_tn_reduction = 0, custom_tp_reduction = 0) {
  # Validate inputs
  if (is.null(data) || nrow(data) == 0) {
    stop("No data available for TMDL calculation")
  }
  
  if (is.null(reg_results) || is.null(reg_results$summary)) {
    stop("Regression results not available for TMDL calculation")
  }
  
  if (length(impaired_nutrients) == 0) {
    stop("No impaired nutrients selected")
  }
  
  # Enhanced error handling: Check if impaired nutrients are in the model
  coefficients <- reg_results$summary
  tn_coeff <- ifelse("TN" %in% coefficients$term, 
                     as.numeric(coefficients$estimate[coefficients$term == "TN"]), 0)
  tp_coeff <- ifelse("TP" %in% coefficients$term, 
                     as.numeric(coefficients$estimate[coefficients$term == "TP"]), 0)
  if (("TN" %in% impaired_nutrients && tn_coeff == 0) || 
      ("TP" %in% impaired_nutrients && tp_coeff == 0)) {
    stop("Selected impaired nutrient(s) not in regression model.")
  }
  
  # Print diagnostics
  cat("TMDL calculation inputs:\n")
  cat("- Data rows:", nrow(data), "\n")
  cat("- Impaired nutrients:", paste(impaired_nutrients, collapse=", "), "\n")
  cat("- CHLAC target:", chlac_target, "\n")
  cat("- Lake criteria:", paste(names(lake_criteria), lake_criteria, sep="=", collapse=", "), "\n")
  cat("- Reduction scenario:", reduction_scenario, "\n")
  
  # Ensure numeric columns
  data$TN <- as.numeric(data$TN)
  data$TP <- as.numeric(data$TP)
  data$CHLAC <- as.numeric(data$CHLAC)
  
  # Check for NA values
  if (all(is.na(data$TN)) || all(is.na(data$TP)) || all(is.na(data$CHLAC))) {
    stop("All values for TN, TP, or CHLAC are NA in the data")
  }
  
  # Extract coefficients safely
  intercept <- ifelse("(Intercept)" %in% coefficients$term, 
                      as.numeric(coefficients$estimate[coefficients$term == "(Intercept)"]), 0)
  
  # Handle empty coefficient values
  if (length(tn_coeff) == 0) tn_coeff <- 0
  if (length(tp_coeff) == 0) tp_coeff <- 0
  if (length(intercept) == 0) intercept <- 0
  
  cat("Extracted coefficients:\n")
  cat("- TN coefficient:", tn_coeff, "\n")
  cat("- TP coefficient:", tp_coeff, "\n")
  cat("- Intercept:", intercept, "\n")
  
  if (all(c(tn_coeff, tp_coeff) == 0)) {
    stop("No valid TN or TP coefficients found in regression results.")
  }
  
  # Get current concentrations
  current_tn <- max(data$TN, na.rm = TRUE)
  current_tp <- max(data$TP, na.rm = TRUE)
  cat("Current TN:", current_tn, "Current TP:", current_tp, "\n")
  
  # Calculate target concentrations based on reduction scenario
  target_conc <- list()
  max_agm <- list()
  percent_reduction <- list()
  capped <- list(TN = FALSE, TP = FALSE)  # Track if values were capped
  min_conc <- 0.01  # Minimum concentration in mg/L
  
  if (reduction_scenario == "both" && length(impaired_nutrients) == 2 && tn_coeff != 0 && tp_coeff != 0) {
    # Both TN and TP impaired - reduce proportionally
    cat("Calculating for both TN and TP - proportional reduction\n")
    ratio <- tn_coeff / tp_coeff
    
    x <- (tn_coeff * current_tn + tp_coeff * current_tp + intercept - chlac_target) / 
      (tn_coeff * ratio + tp_coeff)
    
    target_conc$TN <- current_tn - x * ratio
    target_conc$TP <- current_tp - x
    
    cat("Target TN:", target_conc$TN, "Target TP:", target_conc$TP, "\n")
  } else if (reduction_scenario == "tn_only" || (reduction_scenario == "both" && 
                                                 ("TN" %in% impaired_nutrients && !("TP" %in% impaired_nutrients)))) {
    # Only TN impaired or tn_only scenario
    cat("Calculating for TN only\n")
    tp_value <- ifelse(use_paleo_tp, as.numeric(tp_paleo), as.numeric(lake_criteria$TP))
    cat("Using TP value:", tp_value, "\n")
    
    target_conc$TN <- (chlac_target - intercept - (tp_coeff * tp_value)) / tn_coeff
    target_conc$TP <- tp_value
    cat("Target TN:", target_conc$TN, "Target TP:", target_conc$TP, "\n")
  } else if (reduction_scenario == "tp_only" || (reduction_scenario == "both" && 
                                                 ("TP" %in% impaired_nutrients && !("TN" %in% impaired_nutrients)))) {
    # Only TP impaired or tp_only scenario
    cat("Calculating for TP only\n")
    tn_value <- as.numeric(lake_criteria$TN)
    target_conc$TP <- (chlac_target - intercept - (tn_coeff * tn_value)) / tp_coeff
    target_conc$TN <- tn_value
    cat("Target TN:", target_conc$TN, "Target TP:", target_conc$TP, "\n")
  } else if (reduction_scenario == "custom") {
    # Apply user-specified custom reductions
    cat("Calculating with custom reductions\n")
    target_conc$TN <- current_tn * (1 - custom_tn_reduction / 100)
    target_conc$TP <- current_tp * (1 - custom_tp_reduction / 100)
    
    # Check if CHLAC target is met with custom reductions
    predicted_chlac <- intercept + (tn_coeff * target_conc$TN) + (tp_coeff * target_conc$TP)
    cat("Custom reductions result in predicted CHLAC:", predicted_chlac, "\n")
    if (predicted_chlac > chlac_target) {
      warning("Custom reductions do not meet the CHLAC target of ", chlac_target, 
              ". Predicted CHLAC is ", round(predicted_chlac, 2), ".")
    }
  }
  
  # Cap minimum values to ensure physically realistic results
  if (!is.null(target_conc$TN)) {
    if (target_conc$TN < min_conc) {
      cat("TN target capped at minimum value:", min_conc, "\n")
      target_conc$TN <- min_conc
      capped$TN <- TRUE
    }
  }
  if (!is.null(target_conc$TP)) {
    if (target_conc$TP < min_conc) {
      cat("TP target capped at minimum value:", min_conc, "\n")
      target_conc$TP <- min_conc
      capped$TP <- TRUE
    }
  }
  
  # Calculate max AGM and percent reduction
  for (nutrient in c("TN", "TP")) {
    max_agm[[nutrient]] <- max(data[[nutrient]], na.rm = TRUE)
    if (!is.null(target_conc[[nutrient]])) {
      percent_reduction[[nutrient]] <- ((max_agm[[nutrient]] - target_conc[[nutrient]]) / 
                                          max_agm[[nutrient]]) * 100
      # Cap percent reduction at 0-100%
      percent_reduction[[nutrient]] <- max(0, min(100, percent_reduction[[nutrient]]))
    } else {
      warning("Target concentration for ", nutrient, " is NULL")
    }
  }
  
  # Build regression equation
  equation <- "CHLAC = "
  if (tn_coeff != 0) equation <- paste0(equation, sprintf("%.3f * TN", tn_coeff))
  if (tp_coeff != 0) {
    equation <- paste0(equation, ifelse(tn_coeff != 0, " + ", ""), sprintf("%.3f * TP", tp_coeff))
  }
  equation <- paste0(equation, sprintf(" + %.3f", intercept))
  
  # Prediction interval
  newdata <- data.frame(
    TN = ifelse(!is.null(target_conc$TN), target_conc$TN, lake_criteria$TN),
    TP = ifelse(!is.null(target_conc$TP), target_conc$TP, 
                ifelse(use_paleo_tp, tp_paleo, lake_criteria$TP))
  )
  
  cat("Prediction data:\n")
  print(newdata)
  
  pred <- tryCatch({
    predict(reg_results$model, newdata = newdata, interval = "prediction", level = 0.95)
  }, error = function(e) {
    warning("Prediction failed: ", e$message)
    data.frame(fit = NA, lwr = NA, upr = NA)
  })
  
  if (all(is.na(pred))) {
    warning("Prediction returned all NA values")
  } else {
    cat("Prediction results:\n")
    print(pred)
  }
  
  # Create final result
  result_list <- list(
    target_conc = target_conc,
    max_agm = max_agm,
    percent_reduction = percent_reduction,
    equation = equation,
    prediction_interval = c(pred[1, "lwr"], pred[1, "upr"]),
    predicted_chlac = pred[1, "fit"],
    capped = capped,
    reduction_scenario = reduction_scenario
  )
  
  return(result_list)
}

# Main TMDL calculation logic
observeEvent(input$calculate_tmdl, {
  req(regression_results(), results(), lake_type())
  
  reg_results <- regression_results()
  data <- results()$geomeans
  current_lake_type <- lake_type()
  
  # Validate lake type
  if (is.na(current_lake_type)) {
    showNotification("Unable to determine lake type. Check color and alkalinity data.", 
                     type = "error", duration = 10)
    return()
  }
  
  # Define criteria based on lake type
  criteria <- list(
    "1" = list(CHLAC = 20, TP = 0.05, TN = 1.27),
    "2" = list(CHLAC = 20, TP = 0.03, TN = 1.05),
    "3" = list(CHLAC = 6, TP = 0.01, TN = 0.51)
  )[[as.character(current_lake_type)]]
  
  # Validate criteria
  if (is.null(criteria)) {
    showNotification(paste("Invalid lake type:", current_lake_type), type = "error", duration = 10)
    return()
  }
  
  chlac_target <- as.numeric(ifelse(is.null(input$chlac_target) || input$chlac_target == 0,
                                    criteria$CHLAC, input$chlac_target))
  
  # Input validation
  if (chlac_target <= 0) {
    showNotification("CHLAC target must be positive.", type = "error", duration = 10)
    return()
  }
  if (input$use_paleo_tp && (is.null(input$tp_paleo) || input$tp_paleo <= 0)) {
    showNotification("Paleo TP must be positive when selected.", type = "error", duration = 10)
    return()
  }
  if (is.null(input$impaired_nutrients) || length(input$impaired_nutrients) == 0) {
    showNotification("Please select at least one impaired nutrient.", type = "error", duration = 10)
    return()
  }
  
  # Execute TMDL calculation
  tryCatch({
    if (input$regression_type == "single") {
      # IMPORTANT: Apply the same year filter used in regression
      filtered_data <- data %>% 
        filter(wbid == input$reg_wbid, 
               year >= input$start_year, 
               year <= input$end_year)
      
      # Validate data
      if (nrow(filtered_data) == 0) {
        showNotification("No data available for the selected WBID and year range.", type = "error", duration = 10)
        return()
      }
      
      # Calculate TMDL (passing the filtered data)
      tmdl <- calculate_tmdl_single(
        data = filtered_data,
        reg_results = reg_results,
        chlac_target = chlac_target,
        impaired_nutrients = input$impaired_nutrients,
        lake_criteria = criteria,
        use_paleo_tp = input$use_paleo_tp,
        tp_paleo = as.numeric(input$tp_paleo),
        reduction_scenario = input$reduction_scenario,
        custom_tn_reduction = input$custom_tn_reduction,
        custom_tp_reduction = input$custom_tp_reduction
      )
      
      # Add lake type and criteria to results
      tmdl$lake_type <- current_lake_type
      tmdl$criteria <- criteria
      tmdl_results(tmdl)
      
    } else {
      # Multiple WBIDs handling - filter for the same year range
      selected_wbids <- selected_wbids_for_regression()
      
      # Validate WBIDs
      if (length(selected_wbids) == 0) {
        showNotification("No WBIDs selected for analysis.", type = "error", duration = 10)
        return()
      }
      
      # Track warnings
      any_capped <- FALSE
      any_custom_warnings <- FALSE
      
      # Process each WBID - with year filtering
      tmdl_results_list <- lapply(selected_wbids, function(wbid) {
        # Apply the same year filter for each WBID
        wbid_data <- data %>% 
          dplyr::filter(wbid == !!wbid,
                        year >= input$start_year, 
                        year <= input$end_year)
        
        if (nrow(wbid_data) == 0) {
          return(NULL)
        }
        
        # Calculate TMDL for this WBID
        tmdl <- calculate_tmdl_single(
          data = wbid_data,
          reg_results = reg_results,
          chlac_target = chlac_target,
          impaired_nutrients = input$impaired_nutrients,
          lake_criteria = criteria,
          use_paleo_tp = input$use_paleo_tp,
          tp_paleo = as.numeric(input$tp_paleo),
          reduction_scenario = input$reduction_scenario,
          custom_tn_reduction = input$custom_tn_reduction,
          custom_tp_reduction = input$custom_tp_reduction
        )
        
        # Track warnings
        if (any(unlist(tmdl$capped))) {
          any_capped <<- TRUE
        }
        
        if (input$reduction_scenario == "custom") {
          predicted_chlac <- tmdl$predicted_chlac
          if (!is.na(predicted_chlac) && predicted_chlac > chlac_target) {
            any_custom_warnings <<- TRUE
          }
        }
        
        # Format results
        list(
          WBID = wbid,
          Current_TN = safe_display(max(wbid_data$TN, na.rm = TRUE), "N/A", 3),
          Current_TP = safe_display(max(wbid_data$TP, na.rm = TRUE), "N/A", 3),
          Target_TN = safe_display(tmdl$target_conc$TN, "N/A", 3),
          Target_TP = safe_display(tmdl$target_conc$TP, "N/A", 3),
          Percent_Reduction_TN = safe_display(tmdl$percent_reduction$TN, "N/A", 1),
          Percent_Reduction_TP = safe_display(tmdl$percent_reduction$TP, "N/A", 1),
          Predicted_CHLAC = safe_display(tmdl$predicted_chlac, "N/A", 2),
          TN_Capped = tmdl$capped$TN,
          TP_Capped = tmdl$capped$TP
        )
      })
      
      # Filter valid results
      valid_results <- Filter(Negate(is.null), tmdl_results_list)
      
      if (length(valid_results) == 0) {
        showNotification("No valid data for the selected WBIDs and year range.", type = "error", duration = 10)
        return()
      }
      
      # Store results
      tmdl_results(list(
        results = valid_results,
        lake_type = current_lake_type,
        criteria = criteria,
        reduction_scenario = input$reduction_scenario
      ))
      
      # Show warnings
      if (any_capped) {
        showNotification("Some target concentrations were capped at minimum values.", 
                         type = "warning", duration = 10)
      }
      
      if (any_custom_warnings) {
        showNotification("Custom reductions do not meet CHLAC target for one or more WBIDs.", 
                         type = "warning", duration = 10)
      }
    }
    
    showNotification("TMDL calculation completed successfully.", type = "message")
  }, error = function(e) {
    showNotification(paste("TMDL calculation failed:", e$message), type = "error", duration = 10)
  })
})

# New outputs for UI enhancements
output$current_tn_box <- renderValueBox({
  req(tmdl_results())
  tmdl <- tmdl_results()
  value <- if (input$regression_type == "single") {
    safe_display(tmdl$max_agm$TN)
  } else {
    "See Table"
  }
  valueBox(value, "Current TN (mg/L)", icon = icon("water"), color = "aqua")
})

output$current_tp_box <- renderValueBox({
  req(tmdl_results())
  tmdl <- tmdl_results()
  value <- if (input$regression_type == "single") {
    safe_display(tmdl$max_agm$TP)
  } else {
    "See Table"
  }
  valueBox(value, "Current TP (mg/L)", icon = icon("water"), color = "teal")
})

# Target TN Box
output$target_tn_box <- renderValueBox({
  req(tmdl_results())
  tmdl <- tmdl_results()
  
  value <- if (input$regression_type == "single") {
    safe_display(tmdl$target_conc$TN)
  } else {
    "See Table"
  }
  
  valueBox(
    value = ifelse(is.na(value), "N/A", value),
    subtitle = "Target TN (mg/L)",
    icon = icon("bullseye"),
    color = if (is.na(value) && input$regression_type == "single") "red" else "blue",
    width = NULL
  )
})

# Target TP Box
output$target_tp_box <- renderValueBox({
  req(tmdl_results())
  tmdl <- tmdl_results()
  
  value <- if (input$regression_type == "single") {
    safe_display(tmdl$target_conc$TP)
  } else {
    "See Table"
  }
  
  valueBox(
    value = ifelse(is.na(value), "N/A", value),
    subtitle = "Target TP (mg/L)",
    icon = icon("bullseye"),
    color = if (is.na(value) && input$regression_type == "single") "red" else "green",
    width = NULL
  )
})

# Percent Reduction TN Box
output$percent_reduction_tn_box <- renderValueBox({
  req(tmdl_results())
  tmdl <- tmdl_results()
  
  value <- if (input$regression_type == "single") {
    reduction <- tmdl$percent_reduction$TN
    if (is.null(reduction) || is.na(reduction)) {
      "N/A"
    } else {
      paste0(round(reduction, 1), "%")
    }
  } else {
    "See Table"
  }
  
  valueBox(
    value = value,
    subtitle = "TN Reduction Needed",
    icon = icon("percent"),
    color = if (value == "N/A" && input$regression_type == "single") "red" else "purple",
    width = NULL
  )
})

# Percent Reduction TP Box
output$percent_reduction_tp_box <- renderValueBox({
  req(tmdl_results())
  tmdl <- tmdl_results()
  
  value <- if (input$regression_type == "single") {
    reduction <- tmdl$percent_reduction$TP
    if (is.null(reduction) || is.na(reduction)) {
      "N/A"
    } else {
      paste0(round(reduction, 1), "%")
    }
  } else {
    "See Table"
  }
  
  valueBox(
    value = value,
    subtitle = "TP Reduction Needed",
    icon = icon("percent"),
    color = if (value == "N/A" && input$regression_type == "single") "red" else "orange",
    width = NULL
  )
})

output$regression_equation <- renderText({
  req(tmdl_results())
  tmdl <- tmdl_results()
  if (input$regression_type == "single") {
    paste("Regression Equation:", tmdl$equation)
  } else {
    "Regression Equation: Varies by WBID (see regression results)"
  }
})

output$confidence_interval <- renderText({
  req(tmdl_results())
  tmdl <- tmdl_results()
  if (input$regression_type == "single") {
    ci <- tmdl$prediction_interval
    if (is.null(ci) || any(is.na(ci))) {
      "95% Prediction Interval for CHLAC: Not available"
    } else {
      paste0("95% Prediction Interval for CHLAC: [", round(ci[1], 2), ", ", round(ci[2], 2), "] µg/L")
    }
  } else {
    "Prediction Intervals: See Table"
  }
})

output$lake_type_info <- renderText({
  req(tmdl_results())
  tmdl <- tmdl_results()
  paste("Lake Type:", tmdl$lake_type,
        "- CHLAC Criterion:", tmdl$criteria$CHLAC, "µg/L,",
        "TP Criterion:", tmdl$criteria$TP, "mg/L,",
        "TN Criterion:", tmdl$criteria$TN, "mg/L")
})

output$tmdl_summary <- renderUI({
  req(tmdl_results())
  tmdl <- tmdl_results()
  chlac_target <- tmdl$criteria$CHLAC %||% input$chlac_target
  
  if (input$regression_type == "single") {
    scenario_text <- paste0("<p><strong>Reduction Scenario:</strong> ", 
                            switch(input$reduction_scenario,
                                   "both" = "Proportional reduction of both TN and TP",
                                   "tn_only" = "Reduce TN only (TP fixed at criterion)",
                                   "tp_only" = "Reduce TP only (TN fixed at criterion)",
                                   "custom" = paste0("Custom reduction: TN by ", input$custom_tn_reduction, 
                                                     "%, TP by ", input$custom_tp_reduction, "%"),
                                   "Unknown"),
                            "</p>")
    
    current_max_text <- "<p><strong>Current Maximum AGM:</strong>"
    for (nutrient in c("TN", "TP")) {
      if (!is.null(tmdl$max_agm[[nutrient]])) {
        current_max_text <- paste0(current_max_text, "<br>", nutrient, ": ", 
                                   safe_display(tmdl$max_agm[[nutrient]], "N/A", 3), " mg/L")
      }
    }
    current_max_text <- paste0(current_max_text, "</p>")
    
    target_conc_text <- "<p><strong>Target Concentrations:</strong>"
    if (length(tmdl$target_conc) > 0) {
      target_conc_text <- paste0(target_conc_text, "<br>",
                                 paste(names(tmdl$target_conc), ":", 
                                       sapply(tmdl$target_conc, safe_display), 
                                       "mg/L", collapse = "<br>"))
    }
    target_conc_text <- paste0(target_conc_text, "</p>")
    
    percent_reduction_text <- "<p><strong>Percent Reductions Needed:</strong>"
    if (length(tmdl$percent_reduction) > 0) {
      for (nutrient in names(tmdl$percent_reduction)) {
        if (!is.null(tmdl$percent_reduction[[nutrient]]) && !is.na(tmdl$percent_reduction[[nutrient]])) {
          percent_reduction_text <- paste0(percent_reduction_text, "<br>", nutrient, ": ", 
                                           safe_display(tmdl$percent_reduction[[nutrient]], "N/A", 1), "%")
        }
      }
    }
    percent_reduction_text <- paste0(percent_reduction_text, "</p>")
    
    interpretation_text <- paste0("<p><strong>Interpretation:</strong><br>",
                                  "To achieve the target CHLAC of ", 
                                  round(chlac_target, 1), " µg/L")
    
    if (input$reduction_scenario == "custom") {
      interpretation_text <- paste0(interpretation_text, ", the custom reductions result in:")
    } else {
      interpretation_text <- paste0(interpretation_text, ", reduce:")
    }
    
    # Loop correctly through nutrients for interpretation text
    for (nutrient in names(tmdl$percent_reduction)) {
      if (!is.null(tmdl$percent_reduction[[nutrient]]) && !is.na(tmdl$percent_reduction[[nutrient]])) {
        interpretation_text <- paste0(interpretation_text, "<br>- ", nutrient, " from ", 
                                      safe_display(tmdl$max_agm[[nutrient]], "N/A", 3), " mg/L to ", 
                                      safe_display(tmdl$target_conc[[nutrient]], "N/A", 3), " mg/L (", 
                                      safe_display(tmdl$percent_reduction[[nutrient]], "N/A", 1), "%).")
      }
    }
    interpretation_text <- paste0(interpretation_text, "</p>")
    
    prediction_text <- paste0("<p><strong>Prediction Interval:</strong><br>",
                              "At these levels, the 95% prediction interval for CHLAC is [", 
                              round(tmdl$prediction_interval[1], 2), ", ", 
                              round(tmdl$prediction_interval[2], 2), "] µg/L.</p>")
    
    # Add warning text if any targets were capped
    warning_text <- ""
    if (!is.null(tmdl$capped) && any(unlist(tmdl$capped))) {
      capped_nutrients <- names(tmdl$capped)[unlist(tmdl$capped)]
      warning_text <- paste0("<p style='color:red'><strong>Warning:</strong> Target concentration(s) for ",
                             paste(capped_nutrients, collapse=" and "),
                             " were capped at 0.01 mg/L to maintain physically realistic values. ",
                             "This may indicate the regression model predicts unrealistically low targets.",
                             "</p>")
    }
    
    # Add warning for custom scenarios if targets not met
    custom_warning <- ""
    if (input$reduction_scenario == "custom" && !is.null(tmdl$predicted_chlac) && 
        !is.na(tmdl$predicted_chlac) && tmdl$predicted_chlac > chlac_target) {
      custom_warning <- paste0("<p style='color:orange'><strong>Note:</strong> The custom reduction scenario ",
                               "results in a predicted CHLAC of ", round(tmdl$predicted_chlac, 2), " µg/L, ",
                               "which does not meet the target of ", round(chlac_target, 1), " µg/L.</p>")
    }
    
    HTML(paste("<h4>TMDL Analysis Results:</h4>", 
               scenario_text,
               current_max_text, 
               target_conc_text, 
               percent_reduction_text, 
               interpretation_text, 
               prediction_text,
               warning_text,
               custom_warning))
  } else {
    # For multiple WBIDs, just show a summary message
    scenario_text <- paste0("<p><strong>Reduction Scenario:</strong> ", 
                            switch(input$reduction_scenario,
                                   "both" = "Proportional reduction of both TN and TP",
                                   "tn_only" = "Reduce TN only (TP fixed at criterion)",
                                   "tp_only" = "Reduce TP only (TN fixed at criterion)",
                                   "custom" = paste0("Custom reduction: TN by ", input$custom_tn_reduction, 
                                                     "%, TP by ", input$custom_tp_reduction, "%"),
                                   "Unknown"),
                            "</p>")
    
    HTML(paste("<h4>TMDL Analysis Results (Multiple WBIDs):</h4>",
               scenario_text,
               "<p>See the TMDL Results Table below for detailed target concentrations and percent reductions for each WBID.</p>"))
  }
})



# Improved TMDL plot
output$tmdl_plot <- renderPlotly({
  req(tmdl_results(), regression_results())
  
  # Get regression data and results
  reg_results <- regression_results()
  tmdl <- tmdl_results()
  
  # Use the filtered data from regression (already filtered by WBID, year range, etc.)
  plot_data <- reg_results$data
  
  # Debug output to verify data
  cat("TMDL plot data points:", nrow(plot_data), "\n")
  if ("year" %in% colnames(plot_data)) {
    cat("Year range:", min(plot_data$year, na.rm = TRUE), "to", max(plot_data$year, na.rm = TRUE), "\n")
  } else {
    cat("Year column not found in plot_data\n")
  }
  
  # Get  Validate plot_data
  if (is.null(plot_data) || nrow(plot_data) == 0) {
    return(plot_ly() %>%
             add_annotations(
               text = "No data available for plotting.",
               showarrow = FALSE,
               font = list(size = 16)
             ))
  }
  
  # Get the CHLAC target value
  chlac_target_value <- if (!is.null(input$chlac_target) && input$chlac_target > 0) {
    input$chlac_target
  } else if (!is.null(tmdl$criteria) && !is.null(tmdl$criteria$CHLAC)) {
    tmdl$criteria$CHLAC
  } else {
    NA
  }
  
  # Generate plots for each impaired nutrient
  plots <- lapply(input$impaired_nutrients, function(nutrient) {
    # Verify the nutrient column exists in the data
    if (!nutrient %in% colnames(plot_data)) {
      return(plot_ly() %>%
               add_annotations(
                 text = paste("Nutrient column", nutrient, "not found in regression data"),
                 showarrow = FALSE,
                 font = list(size = 16)
               ))
    }
    
    # Check for valid data
    if (all(is.na(plot_data[[nutrient]])) || all(is.na(plot_data$CHLAC))) {
      return(plot_ly() %>%
               add_annotations(
                 text = paste("No valid data for", nutrient, "or CHLAC"),
                 showarrow = FALSE,
                 font = list(size = 16)
               ))
    }
    
    # Get the target concentration if available (single WBID mode only)
    target_value <- if (input$regression_type == "single" && !is.null(tmdl$target_conc)) {
      tmdl$target_conc[[nutrient]] %||% NA
    } else {
      NA
    }
    
    # Create base ggplot
    p <- ggplot(plot_data, aes_string(x = nutrient, y = "CHLAC")) +
      geom_point(alpha = 0.6, size = 2) +
      geom_smooth(method = "lm", formula = y ~ x, se = input$show_confidence, color = "blue") +
      labs(
        title = paste("TMDL Analysis:", nutrient, "vs CHLAC"),
        subtitle = if ("year" %in% colnames(plot_data)) {
          paste("Years:", min(plot_data$year, na.rm = TRUE), "to", max(plot_data$year, na.rm = TRUE))
        } else {
          "All Years"
        },
        x = paste(nutrient, "(mg/L)"),
        y = "CHLAC (µg/L)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 12)
      )
    
    # Add WBID coloring for multiple WBIDs
    if (input$regression_type == "multiple" && "wbid" %in% colnames(plot_data)) {
      p <- p + aes(color = factor(wbid)) +
        labs(color = "WBID")
    }
    
    # Add target lines if requested
    if (input$show_target_lines) {
      if (!is.na(chlac_target_value)) {
        p <- p + 
          geom_hline(yintercept = chlac_target_value, linetype = "dashed", color = "red") +
          annotate("text", 
                   x = min(plot_data[[nutrient]], na.rm = TRUE), 
                   y = chlac_target_value, 
                   label = sprintf("CHLAC Target: %.1f", chlac_target_value),
                   hjust = 0, vjust = -0.5, color = "red", size = 4)
      }
      
      if (!is.na(target_value)) {
        max_y <- max(plot_data$CHLAC, na.rm = TRUE)
        p <- p + 
          geom_vline(xintercept = target_value, linetype = "dashed", color = "blue") +
          annotate("text", 
                   x = target_value, 
                   y = max_y * 0.95, 
                   label = sprintf("Target %s: %.3f", nutrient, target_value),
                   hjust = 1, vjust = 1, color = "blue", size = 4)
      }
    }
    
    # Convert to plotly with improved layout
    ggplotly(p, tooltip = c(nutrient, "CHLAC", if (input$regression_type == "multiple") "wbid" else NULL)) %>%
      layout(
        margin = list(t = 80, r = 20, b = 60, l = 60),
        hovermode = "closest",
        legend = list(orientation = "h", y = -0.2, x = 0.5, xanchor = "center")
      )
  })
  
  # Handle case where no plots are generated
  if (length(plots) == 0) {
    return(plot_ly() %>%
             add_annotations(
               text = "No impaired nutrients selected for plotting",
               showarrow = FALSE,
               font = list(size = 16)
             ))
  }
  
  # Combine plots into a subplot
  subplot(plots, nrows = 1, shareY = TRUE, titleX = TRUE) %>%
    layout(
      height = 600,
      showlegend = (input$regression_type == "multiple"),
      title = list(
        text = "TMDL Analysis Plots",
        y = 0.99,
        x = 0.5,
        xanchor = "center",
        font = list(size = 16)
      )
    )
})

# Add TMDL Results Table for Multiple WBIDs
output$tmdl_results_table <- renderDT({
  req(tmdl_results())
  if (input$regression_type == "multiple") {
    if (is.null(tmdl_results()$results) || length(tmdl_results()$results) == 0) {
      return(datatable(data.frame(Message = "No valid TMDL results available."), options = list(dom = 't')))
    }
    
    # Convert list of results to data frame
    results_df <- do.call(rbind, lapply(tmdl_results()$results, function(result) {
      # Convert each result to a data frame with consistent columns
      as.data.frame(result)
    }))
    
    # Clean up columns - hide the capped flags which are internal only
    if ("TN_Capped" %in% names(results_df)) {
      results_df$TN_Capped <- NULL
    }
    if ("TP_Capped" %in% names(results_df)) {
      results_df$TP_Capped <- NULL
    }
    
    # Rename columns to make them more readable and compact
    colnames(results_df) <- gsub("Current_", "Curr_", colnames(results_df))
    colnames(results_df) <- gsub("Percent_Reduction_", "Red_", colnames(results_df))
    colnames(results_df) <- gsub("Predicted_", "Pred_", colnames(results_df))
    
    datatable(
      results_df,
      options = list(
        pageLength = 10, 
        scrollX = TRUE,  # Enable horizontal scrolling
        autoWidth = FALSE,  # Don't try to auto-size columns
        columnDefs = list(
          list(width = '60px', targets = 0),  # WBID column
          list(width = '70px', targets = c(1, 2, 3, 4)),  # Current and Target columns
          list(width = '70px', targets = c(5, 6, 7))  # Reduction and Predicted columns
        )
      ),
      rownames = FALSE
    ) %>%
      formatStyle(
        'Target_TN',
        target = 'row',
        backgroundColor = styleEqual("0.01", "lightyellow")
      ) %>%
      formatStyle(
        'Target_TP',
        target = 'row',
        backgroundColor = styleEqual("0.01", "lightyellow")
      )
  }
})

# Download handler for multiple WBIDs
output$download_tmdl_table <- downloadHandler(
  filename = function() { paste("tmdl_results_", Sys.Date(), ".csv", sep = "") },
  content = function(file) {
    req(tmdl_results())
    if (input$regression_type == "multiple") {
      results_df <- do.call(rbind, lapply(tmdl_results()$results, as.data.frame))
      write.csv(results_df, file, row.names = FALSE)
    }
  }
)


# Update impaired nutrient choices based on regression results
observe({
  req(regression_results())
  coefficients <- regression_results()$summary$term
  nutrients_in_regression <- c("TN", "TP")[c("TN", "TP") %in% coefficients]
  updateCheckboxGroupInput(session, "impaired_nutrients",
                           choices = nutrients_in_regression,
                           selected = nutrients_in_regression[1])
})

# Define the null-coalesce operator if not already defined
`%||%` <- function(a, b) if (is.null(a)) b else a

# Helper function to safely display values
safe_display <- function(value, default = "N/A", digits = 3) {
  if (is.null(value) || is.na(value)) {
    return(default)
  } else {
    return(round(as.numeric(value), digits))
  }
}

# Helper function to calculate TMDL for a single dataset
calculate_tmdl_single <- function(data, reg_results, chlac_target, impaired_nutrients, 
                                  lake_criteria, use_paleo_tp, tp_paleo, 
                                  reduction_scenario = "both", 
                                  custom_tn_reduction = 0, custom_tp_reduction = 0) {
  # Validate inputs
  if (is.null(data) || nrow(data) == 0) {
    stop("No data available for TMDL calculation")
  }
  
  if (is.null(reg_results) || is.null(reg_results$summary)) {
    stop("Regression results not available for TMDL calculation")
  }
  
  if (length(impaired_nutrients) == 0) {
    stop("No impaired nutrients selected")
  }
  
  # Enhanced error handling: Check if impaired nutrients are in the model
  coefficients <- reg_results$summary
  tn_coeff <- ifelse("TN" %in% coefficients$term, 
                     as.numeric(coefficients$estimate[coefficients$term == "TN"]), 0)
  tp_coeff <- ifelse("TP" %in% coefficients$term, 
                     as.numeric(coefficients$estimate[coefficients$term == "TP"]), 0)
  if (("TN" %in% impaired_nutrients && tn_coeff == 0) || 
      ("TP" %in% impaired_nutrients && tp_coeff == 0)) {
    stop("Selected impaired nutrient(s) not in regression model.")
  }
  
  # Print diagnostics
  cat("TMDL calculation inputs:\n")
  cat("- Data rows:", nrow(data), "\n")
  cat("- Impaired nutrients:", paste(impaired_nutrients, collapse=", "), "\n")
  cat("- CHLAC target:", chlac_target, "\n")
  cat("- Lake criteria:", paste(names(lake_criteria), lake_criteria, sep="=", collapse=", "), "\n")
  cat("- Reduction scenario:", reduction_scenario, "\n")
  
  # Ensure numeric columns
  data$TN <- as.numeric(data$TN)
  data$TP <- as.numeric(data$TP)
  data$CHLAC <- as.numeric(data$CHLAC)
  
  # Check for NA values
  if (all(is.na(data$TN)) || all(is.na(data$TP)) || all(is.na(data$CHLAC))) {
    stop("All values for TN, TP, or CHLAC are NA in the data")
  }
  
  # Extract coefficients safely
  intercept <- ifelse("(Intercept)" %in% coefficients$term, 
                      as.numeric(coefficients$estimate[coefficients$term == "(Intercept)"]), 0)
  
  # Handle empty coefficient values
  if (length(tn_coeff) == 0) tn_coeff <- 0
  if (length(tp_coeff) == 0) tp_coeff <- 0
  if (length(intercept) == 0) intercept <- 0
  
  cat("Extracted coefficients:\n")
  cat("- TN coefficient:", tn_coeff, "\n")
  cat("- TP coefficient:", tp_coeff, "\n")
  cat("- Intercept:", intercept, "\n")
  
  if (all(c(tn_coeff, tp_coeff) == 0)) {
    stop("No valid TN or TP coefficients found in regression results.")
  }
  
  # Get current concentrations
  current_tn <- max(data$TN, na.rm = TRUE)
  current_tp <- max(data$TP, na.rm = TRUE)
  cat("Current TN:", current_tn, "Current TP:", current_tp, "\n")
  
  # Calculate target concentrations based on reduction scenario
  target_conc <- list()
  max_agm <- list()
  percent_reduction <- list()
  capped <- list(TN = FALSE, TP = FALSE)  # Track if values were capped
  min_conc <- 0.01  # Minimum concentration in mg/L
  
  if (reduction_scenario == "both" && length(impaired_nutrients) == 2 && tn_coeff != 0 && tp_coeff != 0) {
    # Both TN and TP impaired - reduce proportionally
    cat("Calculating for both TN and TP - proportional reduction\n")
    ratio <- tn_coeff / tp_coeff
    
    x <- (tn_coeff * current_tn + tp_coeff * current_tp + intercept - chlac_target) / 
      (tn_coeff * ratio + tp_coeff)
    
    target_conc$TN <- current_tn - x * ratio
    target_conc$TP <- current_tp - x
    
    cat("Target TN:", target_conc$TN, "Target TP:", target_conc$TP, "\n")
  } else if (reduction_scenario == "tn_only" || (reduction_scenario == "both" && 
                                                 ("TN" %in% impaired_nutrients && !("TP" %in% impaired_nutrients)))) {
    # Only TN impaired or tn_only scenario
    cat("Calculating for TN only\n")
    tp_value <- ifelse(use_paleo_tp, as.numeric(tp_paleo), as.numeric(lake_criteria$TP))
    cat("Using TP value:", tp_value, "\n")
    
    target_conc$TN <- (chlac_target - intercept - (tp_coeff * tp_value)) / tn_coeff
    target_conc$TP <- tp_value
    cat("Target TN:", target_conc$TN, "Target TP:", target_conc$TP, "\n")
  } else if (reduction_scenario == "tp_only" || (reduction_scenario == "both" && 
                                                 ("TP" %in% impaired_nutrients && !("TN" %in% impaired_nutrients)))) {
    # Only TP impaired or tp_only scenario
    cat("Calculating for TP only\n")
    tn_value <- as.numeric(lake_criteria$TN)
    target_conc$TP <- (chlac_target - intercept - (tn_coeff * tn_value)) / tp_coeff
    target_conc$TN <- tn_value
    cat("Target TN:", target_conc$TN, "Target TP:", target_conc$TP, "\n")
  } else if (reduction_scenario == "custom") {
    # Apply user-specified custom reductions
    cat("Calculating with custom reductions\n")
    target_conc$TN <- current_tn * (1 - custom_tn_reduction / 100)
    target_conc$TP <- current_tp * (1 - custom_tp_reduction / 100)
    
    # Check if CHLAC target is met with custom reductions
    predicted_chlac <- intercept + (tn_coeff * target_conc$TN) + (tp_coeff * target_conc$TP)
    cat("Custom reductions result in predicted CHLAC:", predicted_chlac, "\n")
    if (predicted_chlac > chlac_target) {
      warning("Custom reductions do not meet the CHLAC target of ", chlac_target, 
              ". Predicted CHLAC is ", round(predicted_chlac, 2), ".")
    }
  }
  
  # Cap minimum values to ensure physically realistic results
  if (!is.null(target_conc$TN)) {
    if (target_conc$TN < min_conc) {
      cat("TN target capped at minimum value:", min_conc, "\n")
      target_conc$TN <- min_conc
      capped$TN <- TRUE
    }
  }
  if (!is.null(target_conc$TP)) {
    if (target_conc$TP < min_conc) {
      cat("TP target capped at minimum value:", min_conc, "\n")
      target_conc$TP <- min_conc
      capped$TP <- TRUE
    }
  }
  
  # Calculate max AGM and percent reduction
  for (nutrient in c("TN", "TP")) {
    max_agm[[nutrient]] <- max(data[[nutrient]], na.rm = TRUE)
    if (!is.null(target_conc[[nutrient]])) {
      percent_reduction[[nutrient]] <- ((max_agm[[nutrient]] - target_conc[[nutrient]]) / 
                                          max_agm[[nutrient]]) * 100
      # Cap percent reduction at 0-100%
      percent_reduction[[nutrient]] <- max(0, min(100, percent_reduction[[nutrient]]))
    } else {
      warning("Target concentration for ", nutrient, " is NULL")
    }
  }
  
  # Build regression equation
  equation <- "CHLAC = "
  if (tn_coeff != 0) equation <- paste0(equation, sprintf("%.3f * TN", tn_coeff))
  if (tp_coeff != 0) {
    equation <- paste0(equation, ifelse(tn_coeff != 0, " + ", ""), sprintf("%.3f * TP", tp_coeff))
  }
  equation <- paste0(equation, sprintf(" + %.3f", intercept))
  
  # Prediction interval
  newdata <- data.frame(
    TN = ifelse(!is.null(target_conc$TN), target_conc$TN, lake_criteria$TN),
    TP = ifelse(!is.null(target_conc$TP), target_conc$TP, 
                ifelse(use_paleo_tp, tp_paleo, lake_criteria$TP))
  )
  
  cat("Prediction data:\n")
  print(newdata)
  
  pred <- tryCatch({
    predict(reg_results$model, newdata = newdata, interval = "prediction", level = 0.95)
  }, error = function(e) {
    warning("Prediction failed: ", e$message)
    data.frame(fit = NA, lwr = NA, upr = NA)
  })
  
  if (all(is.na(pred))) {
    warning("Prediction returned all NA values")
  } else {
    cat("Prediction results:\n")
    print(pred)
  }
  
  # Create final result
  result_list <- list(
    target_conc = target_conc,
    max_agm = max_agm,
    percent_reduction = percent_reduction,
    equation = equation,
    prediction_interval = c(pred[1, "lwr"], pred[1, "upr"]),
    predicted_chlac = pred[1, "fit"],
    capped = capped,
    reduction_scenario = reduction_scenario
  )
  
  return(result_list)
}



# Instructions for TMDL tab
output$tmdl_instructions <- renderUI({
  if (is.null(results()) || is.null(results()$geomeans) || nrow(results()$geomeans) == 0) {
    div(
      class = "alert alert-info",
      icon("info-circle"), 
      strong("Data Required:"), 
      " Before calculating TMDL, please complete these steps:",
      tags$ol(
        tags$li("Extract water quality data in the", tags$b("Data Extraction"), "tab"),
        tags$li("Run a regression analysis in the", tags$b("Regression Analysis"), "tab"),
        tags$li("Then return here to calculate TMDL values")
      )
    )
  } else if (is.null(regression_results())) {
    div(
      class = "alert alert-warning",
      icon("exclamation-triangle"), 
      strong("Regression Required:"), 
      " Please run a regression analysis in the Regression Analysis tab before calculating TMDL."
    )
  } else {
    # When all prerequisites are met, show a success message
    div(
      class = "alert alert-success",
      icon("check-circle"), 
      strong("Ready for TMDL Calculation:"), 
      " Data and regression model are available. You can now calculate TMDL values."
    )
  }
})




# Main TMDL calculation logic
observeEvent(input$calculate_tmdl, {
  # Check prerequisites
  if (is.null(results()) || is.null(results()$geomeans) || nrow(results()$geomeans) == 0) {
    showNotification("Please extract data in the Data Extraction tab before calculating TMDL.", 
                     type = "warning", duration = 10)
    return()
  }
  
  if (is.null(regression_results())) {
    showNotification("Please run a regression analysis in the Regression Analysis tab before calculating TMDL.", 
                     type = "warning", duration = 10)
    return()
  }
  
  # Main TMDL calculation logic
  req(regression_results(), results(), lake_type())
  
  reg_results <- regression_results()
  data <- results()$geomeans
  current_lake_type <- lake_type()
  
  # Validate lake type
  if (is.na(current_lake_type)) {
    showNotification("Unable to determine lake type. Check color and alkalinity data.", 
                     type = "error", duration = 10)
    return()
  }
  
  # Define criteria based on lake type
  criteria <- list(
    "1" = list(CHLAC = 20, TP = 0.05, TN = 1.27),
    "2" = list(CHLAC = 20, TP = 0.03, TN = 1.05),
    "3" = list(CHLAC = 6, TP = 0.01, TN = 0.51)
  )[[as.character(current_lake_type)]]
  
  if (is.null(criteria)) {
    showNotification(paste("Invalid lake type:", current_lake_type), type = "error", duration = 10)
    return()
  }
  
  chlac_target <- as.numeric(ifelse(is.null(input$chlac_target) || input$chlac_target == 0,
                                    criteria$CHLAC, input$chlac_target))
  
  # Input validation
  if (chlac_target <= 0) {
    showNotification("CHLAC target must be positive.", type = "error", duration = 10)
    return()
  }
  if (input$use_paleo_tp && (is.null(input$tp_paleo) || input$tp_paleo <= 0)) {
    showNotification("Paleo TP must be positive when selected.", type = "error", duration = 10)
    return()
  }
  if (is.null(input$impaired_nutrients) || length(input$impaired_nutrients) == 0) {
    showNotification("Please select at least one impaired nutrient.", type = "error", duration = 10)
    return()
  }
  
  # Log start of TMDL calculation
  cat("Starting TMDL calculation\n")
  cat("Year range:", input$start_year, "to", input$end_year, "\n")
  
  # Execute TMDL calculation
  tryCatch({
    if (input$regression_type == "single") {
      # Apply year filter consistent with regression
      filtered_data <- data %>% 
        filter(wbid == input$reg_wbid, 
               year >= input$start_year, 
               year <= input$end_year)
      
      # Validate data
      if (nrow(filtered_data) == 0) {
        showNotification("No data available for the selected WBID and year range.", type = "error", duration = 10)
        return()
      }
      
      cat("Single WBID calculation for:", input$reg_wbid, "\n")
      cat("Number of data rows after filtering:", nrow(filtered_data), "\n")
      
      # Calculate TMDL
      tmdl <- calculate_tmdl_single(
        data = filtered_data,
        reg_results = reg_results,
        chlac_target = chlac_target,
        impaired_nutrients = input$impaired_nutrients,
        lake_criteria = criteria,
        use_paleo_tp = input$use_paleo_tp,
        tp_paleo = as.numeric(input$tp_paleo),
        reduction_scenario = input$reduction_scenario,
        custom_tn_reduction = input$custom_tn_reduction,
        custom_tp_reduction = input$custom_tp_reduction
      )
      
      # Add lake type and criteria to results
      tmdl$lake_type <- current_lake_type
      tmdl$criteria <- criteria
      tmdl_results(tmdl)
      
      # Show warnings
      if (any(unlist(tmdl$capped))) {
        showNotification("Some target concentrations were capped at minimum values.", 
                         type = "warning", duration = 10)
      }
      if (input$reduction_scenario == "custom" && !is.na(tmdl$predicted_chlac) && 
          tmdl$predicted_chlac > chlac_target) {
        showNotification(paste0("Custom reductions do not meet CHLAC target of ", 
                                round(chlac_target, 2), ". Predicted CHLAC is ", 
                                round(tmdl$predicted_chlac, 2), "."), 
                         type = "warning", duration = 10)
      }
      
    } else {
      # Multiple WBIDs handling
      selected_wbids <- selected_wbids_for_regression()
      
      if (length(selected_wbids) == 0) {
        showNotification("No WBIDs selected for analysis.", type = "error", duration = 10)
        return()
      }
      
      any_capped <- FALSE
      any_custom_warnings <- FALSE
      
      tmdl_results_list <- lapply(selected_wbids, function(wbid) {
        # Apply year filter consistent with regression
        wbid_data <- data %>% 
          dplyr::filter(wbid == !!wbid,
                        year >= input$start_year, 
                        year <= input$end_year)
        
        if (nrow(wbid_data) == 0) {
          cat("No data for WBID:", wbid, "in year range\n")
          return(NULL)
        }
        
        # Calculate TMDL for this WBID
        tmdl <- calculate_tmdl_single(
          data = wbid_data,
          reg_results = reg_results,
          chlac_target = chlac_target,
          impaired_nutrients = input$impaired_nutrients,
          lake_criteria = criteria,
          use_paleo_tp = input$use_paleo_tp,
          tp_paleo = as.numeric(input$tp_paleo),
          reduction_scenario = input$reduction_scenario,
          custom_tn_reduction = input$custom_tn_reduction,
          custom_tp_reduction = input$custom_tp_reduction
        )
        
        # Track warnings
        if (any(unlist(tmdl$capped))) {
          any_capped <<- TRUE
        }
        if (input$reduction_scenario == "custom" && !is.na(tmdl$predicted_chlac) && 
            tmdl$predicted_chlac > chlac_target) {
          any_custom_warnings <<- TRUE
        }
        
        # Format results
        list(
          WBID = wbid,
          Current_TN = safe_display(max(wbid_data$TN, na.rm = TRUE), "N/A", 3),
          Current_TP = safe_display(max(wbid_data$TP, na.rm = TRUE), "N/A", 3),
          Target_TN = safe_display(tmdl$target_conc$TN, "N/A", 3),
          Target_TP = safe_display(tmdl$target_conc$TP, "N/A", 3),
          Percent_Reduction_TN = safe_display(tmdl$percent_reduction$TN, "N/A", 1),
          Percent_Reduction_TP = safe_display(tmdl$percent_reduction$TP, "N/A", 1),
          Predicted_CHLAC = safe_display(tmdl$predicted_chlac, "N/A", 2),
          TN_Capped = tmdl$capped$TN,
          TP_Capped = tmdl$capped$TP
        )
      })
      
      valid_results <- Filter(Negate(is.null), tmdl_results_list)
      
      if (length(valid_results) == 0) {
        showNotification("No valid data for the selected WBIDs and year range.", type = "error", duration = 10)
        return()
      }
      
      tmdl_results(list(
        results = valid_results,
        lake_type = current_lake_type,
        criteria = criteria,
        reduction_scenario = input$reduction_scenario
      ))
      
      if (any_capped) {
        showNotification("Some target concentrations were capped at minimum values.", 
                         type = "warning", duration = 10)
      }
      if (any_custom_warnings) {
        showNotification("Custom reductions do not meet CHLAC target for one or more WBIDs.", 
                         type = "warning", duration = 10)
      }
    }
    
    showNotification("TMDL calculation completed successfully.", type = "message")
  }, error = function(e) {
    showNotification(paste("TMDL calculation failed:", e$message), type = "error", duration = 10)
    cat("TMDL Error:", e$message, "\n")
  })
})


# Enable/disable the TMDL calculation button
observe({
  if (is.null(results()) || is.null(results()$geomeans) || nrow(results()$geomeans) == 0 || 
      is.null(regression_results())) {
    # Disable button when data or regression results aren't available
    shinyjs::disable("calculate_tmdl")
  } else {
    # Enable button when all prerequisites are met
    shinyjs::enable("calculate_tmdl")
  }
})


# New outputs for UI enhancements
output$current_tn_box <- renderValueBox({
  req(tmdl_results())
  tmdl <- tmdl_results()
  value <- if (input$regression_type == "single") {
    safe_display(tmdl$max_agm$TN)
  } else {
    "See Table"
  }
  valueBox(value, "Current TN (mg/L)", icon = icon("water"), color = "aqua")
})

output$current_tp_box <- renderValueBox({
  req(tmdl_results())
  tmdl <- tmdl_results()
  value <- if (input$regression_type == "single") {
    safe_display(tmdl$max_agm$TP)
  } else {
    "See Table"
  }
  valueBox(value, "Current TP (mg/L)", icon = icon("water"), color = "teal")
})

# Target TN Box
output$target_tn_box <- renderValueBox({
  req(tmdl_results())
  tmdl <- tmdl_results()
  
  value <- if (input$regression_type == "single") {
    safe_display(tmdl$target_conc$TN)
  } else {
    "See Table"
  }
  
  valueBox(
    value = ifelse(is.na(value), "N/A", value),
    subtitle = "Target TN (mg/L)",
    icon = icon("bullseye"),
    color = if (is.na(value) && input$regression_type == "single") "red" else "blue",
    width = NULL
  )
})

# Target TP Box
output$target_tp_box <- renderValueBox({
  req(tmdl_results())
  tmdl <- tmdl_results()
  
  value <- if (input$regression_type == "single") {
    safe_display(tmdl$target_conc$TP)
  } else {
    "See Table"
  }
  
  valueBox(
    value = ifelse(is.na(value), "N/A", value),
    subtitle = "Target TP (mg/L)",
    icon = icon("bullseye"),
    color = if (is.na(value) && input$regression_type == "single") "red" else "green",
    width = NULL
  )
})

# Percent Reduction TN Box
output$percent_reduction_tn_box <- renderValueBox({
  req(tmdl_results())
  tmdl <- tmdl_results()
  
  value <- if (input$regression_type == "single") {
    reduction <- tmdl$percent_reduction$TN
    if (is.null(reduction) || is.na(reduction)) {
      "N/A"
    } else {
      paste0(round(reduction, 1), "%")
    }
  } else {
    "See Table"
  }
  
  valueBox(
    value = value,
    subtitle = "TN Reduction Needed",
    icon = icon("percent"),
    color = if (value == "N/A" && input$regression_type == "single") "red" else "purple",
    width = NULL
  )
})

# Percent Reduction TP Box
output$percent_reduction_tp_box <- renderValueBox({
  req(tmdl_results())
  tmdl <- tmdl_results()
  
  value <- if (input$regression_type == "single") {
    reduction <- tmdl$percent_reduction$TP
    if (is.null(reduction) || is.na(reduction)) {
      "N/A"
    } else {
      paste0(round(reduction, 1), "%")
    }
  } else {
    "See Table"
  }
  
  valueBox(
    value = value,
    subtitle = "TP Reduction Needed",
    icon = icon("percent"),
    color = if (value == "N/A" && input$regression_type == "single") "red" else "orange",
    width = NULL
  )
})

output$regression_equation <- renderText({
  req(tmdl_results())
  tmdl <- tmdl_results()
  if (input$regression_type == "single") {
    paste("Regression Equation:", tmdl$equation)
  } else {
    "Regression Equation: Varies by WBID (see regression results)"
  }
})

output$confidence_interval <- renderText({
  req(tmdl_results())
  tmdl <- tmdl_results()
  if (input$regression_type == "single") {
    ci <- tmdl$prediction_interval
    if (is.null(ci) || any(is.na(ci))) {
      "95% Prediction Interval for CHLAC: Not available"
    } else {
      paste0("95% Prediction Interval for CHLAC: [", round(ci[1], 2), ", ", round(ci[2], 2), "] µg/L")
    }
  } else {
    "Prediction Intervals: See Table"
  }
})

output$lake_type_info <- renderText({
  req(tmdl_results())
  tmdl <- tmdl_results()
  paste("Lake Type:", tmdl$lake_type,
        "- CHLAC Criterion:", tmdl$criteria$CHLAC, "µg/L,",
        "TP Criterion:", tmdl$criteria$TP, "mg/L,",
        "TN Criterion:", tmdl$criteria$TN, "mg/L")
})

output$tmdl_summary <- renderUI({
  req(tmdl_results())
  tmdl <- tmdl_results()
  chlac_target <- tmdl$criteria$CHLAC %||% input$chlac_target
  
  if (input$regression_type == "single") {
    scenario_text <- paste0("<p><strong>Reduction Scenario:</strong> ", 
                            switch(input$reduction_scenario,
                                   "both" = "Proportional reduction of both TN and TP",
                                   "tn_only" = "Reduce TN only (TP fixed at criterion)",
                                   "tp_only" = "Reduce TP only (TN fixed at criterion)",
                                   "custom" = paste0("Custom reduction: TN by ", input$custom_tn_reduction, 
                                                     "%, TP by ", input$custom_tp_reduction, "%"),
                                   "Unknown"),
                            "</p>")
    
    current_max_text <- "<p><strong>Current Maximum AGM:</strong>"
    for (nutrient in c("TN", "TP")) {
      if (!is.null(tmdl$max_agm[[nutrient]])) {
        current_max_text <- paste0(current_max_text, "<br>", nutrient, ": ", 
                                   safe_display(tmdl$max_agm[[nutrient]], "N/A", 3), " mg/L")
      }
    }
    current_max_text <- paste0(current_max_text, "</p>")
    
    target_conc_text <- "<p><strong>Target Concentrations:</strong>"
    if (length(tmdl$target_conc) > 0) {
      target_conc_text <- paste0(target_conc_text, "<br>",
                                 paste(names(tmdl$target_conc), ":", 
                                       sapply(tmdl$target_conc, safe_display), 
                                       "mg/L", collapse = "<br>"))
    }
    target_conc_text <- paste0(target_conc_text, "</p>")
    
    percent_reduction_text <- "<p><strong>Percent Reductions Needed:</strong>"
    if (length(tmdl$percent_reduction) > 0) {
      for (nutrient in names(tmdl$percent_reduction)) {
        if (!is.null(tmdl$percent_reduction[[nutrient]]) && !is.na(tmdl$percent_reduction[[nutrient]])) {
          percent_reduction_text <- paste0(percent_reduction_text, "<br>", nutrient, ": ", 
                                           safe_display(tmdl$percent_reduction[[nutrient]], "N/A", 1), "%")
        }
      }
    }
    percent_reduction_text <- paste0(percent_reduction_text, "</p>")
    
    interpretation_text <- paste0("<p><strong>Interpretation:</strong><br>",
                                  "To achieve the target CHLAC of ", 
                                  round(chlac_target, 1), " µg/L")
    
    if (input$reduction_scenario == "custom") {
      interpretation_text <- paste0(interpretation_text, ", the custom reductions result in:")
    } else {
      interpretation_text <- paste0(interpretation_text, ", reduce:")
    }
    
    for (nutrient in names(tmdl$percent_reduction)) {
      if (!is.null(tmdl$percent_reduction[[nutrient]]) && !is.na(tmdl$percent_reduction[[nutrient]])) {
        interpretation_text <- paste0(interpretation_text, "<br>- ", nutrient, " from ", 
                                      safe_display(tmdl$max_agm[[nutrient]], "N/A", 3), " mg/L to ", 
                                      safe_display(tmdl$target_conc[[nutrient]], "N/A", 3), " mg/L (", 
                                      safe_display(tmdl$percent_reduction[[nutrient]], "N/A", 1), "%).")
      }
    }
    interpretation_text <- paste0(interpretation_text, "</p>")
    
    prediction_text <- paste0("<p><strong>Prediction Interval:</strong><br>",
                              "At these levels, the 95% prediction interval for CHLAC is [", 
                              round(tmdl$prediction_interval[1], 2), ", ", 
                              round(tmdl$prediction_interval[2], 2), "] µg/L.</p>")
    
    # Add warning text if any targets were capped
    warning_text <- ""
    if (!is.null(tmdl$capped) && any(unlist(tmdl$capped))) {
      capped_nutrients <- names(tmdl$capped)[unlist(tmdl$capped)]
      warning_text <- paste0("<p style='color:red'><strong>Warning:</strong> Target concentration(s) for ",
                             paste(capped_nutrients, collapse=" and "),
                             " were capped at 0.01 mg/L to maintain physically realistic values. ",
                             "This may indicate the regression model predicts unrealistically low targets.",
                             "</p>")
    }
    
    # Add warning for custom scenarios if targets not met
    custom_warning <- ""
    if (input$reduction_scenario == "custom" && !is.null(tmdl$predicted_chlac) && 
        !is.na(tmdl$predicted_chlac) && tmdl$predicted_chlac > chlac_target) {
      custom_warning <- paste0("<p style='color:orange'><strong>Note:</strong> The custom reduction scenario ",
                               "results in a predicted CHLAC of ", round(tmdl$predicted_chlac, 2), " µg/L, ",
                               "which does not meet the target of ", round(chlac_target, 1), " µg/L.</p>")
    }
    
    HTML(paste("<h4>TMDL Analysis Results:</h4>", 
               scenario_text,
               current_max_text, 
               target_conc_text, 
               percent_reduction_text, 
               interpretation_text, 
               prediction_text,
               warning_text,
               custom_warning))
  } else {
    # For multiple WBIDs, just show a summary message
    scenario_text <- paste0("<p><strong>Reduction Scenario:</strong> ", 
                            switch(input$reduction_scenario,
                                   "both" = "Proportional reduction of both TN and TP",
                                   "tn_only" = "Reduce TN only (TP fixed at criterion)",
                                   "tp_only" = "Reduce TP only (TN fixed at criterion)",
                                   "custom" = paste0("Custom reduction: TN by ", input$custom_tn_reduction, 
                                                     "%, TP by ", input$custom_tp_reduction, "%"),
                                   "Unknown"),
                            "</p>")
    
    HTML(paste("<h4>TMDL Analysis Results (Multiple WBIDs):</h4>",
               scenario_text,
               "<p>See the TMDL Results Table below for detailed target concentrations and percent reductions for each WBID.</p>"))
  }
})

# Improved TMDL plot
output$tmdl_plot <- renderPlotly({
  req(tmdl_results(), regression_results(), results())
  data <- results()$geomeans
  tmdl <- tmdl_results()
  
  plots <- lapply(input$impaired_nutrients, function(nutrient) {
    if (input$regression_type == "single") {
      target_value <- tmdl$target_conc[[nutrient]] %||% NA
      plot_data <- data %>% filter(wbid == input$reg_wbid)
    } else {
      target_value <- NA  # No single target for multiple WBIDs
      plot_data <- data %>% filter(wbid %in% selected_wbids_for_regression())
    }
    
    # Check if we have the required data
    if (!all(c(nutrient, "CHLAC") %in% colnames(plot_data))) {
      return(plot_ly() %>% 
               add_annotations(
                 text = paste("Missing required columns:", paste(setdiff(c(nutrient, "CHLAC"), colnames(plot_data)), collapse=", ")),
                 showarrow = FALSE,
                 font = list(size = 16)
               ))
    }
    
    # Check if we have valid numeric data
    if (all(is.na(plot_data[[nutrient]])) || all(is.na(plot_data[["CHLAC"]]))) {
      return(plot_ly() %>% 
               add_annotations(
                 text = paste("No valid data for", nutrient, "or CHLAC"),
                 showarrow = FALSE,
                 font = list(size = 16)
               ))
    }
    
    # Get CHLAC target for plotting
    chlac_target_value <- if(!is.null(input$chlac_target) && input$chlac_target > 0) {
      input$chlac_target
    } else if(!is.null(tmdl$criteria) && !is.null(tmdl$criteria$CHLAC)) {
      tmdl$criteria$CHLAC
    } else {
      NA
    }
    
    p <- ggplot(plot_data, aes_string(x = nutrient, y = "CHLAC")) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "lm", formula = y ~ x, se = input$show_confidence, color = "blue") +
      labs(title = paste("TMDL Analysis for", nutrient),
           x = paste(nutrient, "(mg/L)"), y = "CHLAC (µg/L)") +
      theme_minimal()
    
    if (input$show_target_lines) {
      # Add horizontal line for CHLAC target if available
      if (!is.na(chlac_target_value)) {
        p <- p + geom_hline(yintercept = chlac_target_value, linetype = "dashed", color = "red")
      }
      
      # Add vertical line for nutrient target if available
      if (!is.na(target_value)) {
        max_y <- max(plot_data$CHLAC, na.rm = TRUE)
        p <- p + 
          geom_vline(xintercept = target_value, linetype = "dashed", color = "blue") +
          annotate("text", x = target_value, y = max_y, 
                   label = sprintf("Target %s: %.3f", nutrient, target_value), 
                   vjust = -1, color = "blue")
      }
    }
    
    ggplotly(p)
  })
  
  if (length(plots) == 0) {
    return(plot_ly() %>% 
             add_annotations(
               text = "No impaired nutrients selected for plotting",
               showarrow = FALSE,
               font = list(size = 16)
             ))
  }
  
  subplot(plots, nrows = 1, shareY = TRUE, titleX = TRUE) %>%
    layout(height = 600, showlegend = FALSE)
})

# Add TMDL Results Table for Multiple WBIDs
output$tmdl_results_table <- renderDT({
  cat("Running tmdl_results_table renderer\n")
  
  # Check if tmdl_results exists
  if (is.null(tmdl_results())) {
    cat("tmdl_results is NULL\n")
    return(datatable(data.frame(Message = "No TMDL results available yet."), options = list(dom = 't')))
  }
  
  cat("tmdl_results structure:", class(tmdl_results()), "\n")
  
  if (input$regression_type == "multiple") {
    if (is.null(tmdl_results()$results) || length(tmdl_results()$results) == 0) {
      cat("tmdl_results()$results is NULL or empty\n")
      return(datatable(data.frame(Message = "No valid TMDL results available."), options = list(dom = 't')))
    }
    
    cat("Number of TMDL results:", length(tmdl_results()$results), "\n")
    
    # Convert list of results to data frame
    results_df <- do.call(rbind, lapply(tmdl_results()$results, function(result) {
      if (is.null(result)) return(NULL)
      as.data.frame(result)
    }))
    
    cat("Results dataframe dimensions:", paste(dim(results_df), collapse="x"), "\n")
    
    return(datatable(
      results_df,
      options = list(pageLength = 10, autoWidth = TRUE),
      rownames = FALSE
    ) %>%
      formatStyle(
        'Target_TN',
        target = 'row',
        backgroundColor = styleEqual("0.01", "lightyellow")
      ) %>%
      formatStyle(
        'Target_TP',
        target = 'row',
        backgroundColor = styleEqual("0.01", "lightyellow")
      ))
  } else {
    # For single WBID, you might want to show a simple summary
    cat("Single WBID TMDL calculation\n")
    return(datatable(data.frame(Message = "TMDL calculation completed for single WBID, check summary."), 
                     options = list(dom = 't')))
  }
})


output$debug_tmdl <- renderPrint({
  cat("Regression type:", input$regression_type, "\n")
  cat("TMDL results available:", !is.null(tmdl_results()), "\n")
  
  if (!is.null(tmdl_results())) {
    cat("Results structure:\n")
    str(tmdl_results())
  }
})

# Add this to your UI in the TMDL Calculations tab
verbatimTextOutput("debug_tmdl")

# Download handler for multiple WBIDs
output$download_tmdl_table <- downloadHandler(
  filename = function() { paste("tmdl_results_", Sys.Date(), ".csv", sep = "") },
  content = function(file) {
    req(tmdl_results())
    if (input$regression_type == "multiple") {
      results_df <- do.call(rbind, lapply(tmdl_results()$results, as.data.frame))
      write.csv(results_df, file, row.names = FALSE)
    }
  }
)
  
  
  ####################################################################################################################################################################################
  #   Trend Analysis
  ##################################################################################################################################################################################
  
  # Helper functions
  update_trend_ui_elements <- function() {
    req(results())
    available_params <- unique(results()$rawdata$mastercode)
    available_wbids <- unique(results()$rawdata$wbid)
    wbid_choices <- prepare_wbid_choices(available_wbids, waterbody_info)
    
    updateSelectInput(session, "trend_wbid", choices = wbid_choices)
    
    year_range <- range(results()$rawdata$year, na.rm = TRUE)
    updateSliderInput(session, "trend_year_range", 
                      min = year_range[1], 
                      max = year_range[2], 
                      value = c(year_range[1], year_range[2]))
  }
  
  # Update the prepare_trend_data function
  prepare_trend_data <- function() {
    req(input$trend_year_range, input$trend_wbid, results())
    
    raw_data <- results()$rawdata
    filtered_data <- raw_data %>%
      filter(wbid == input$trend_wbid,
             year >= input$trend_year_range[1],
             year <= input$trend_year_range[2])
    
    # Ensure that parameter columns are present
    param_columns <- setdiff(names(filtered_data), c("wbid", "year", "month", "day", "time"))
    
    if (length(param_columns) == 0) {
      stop("No parameter columns found in the data for trend analysis.")
    }
    
    list(rawdata = filtered_data, geomeans = results()$geomeans)
  }
  
  
  run_analysis <- function(trend_data) {
    tryCatch({
      param_columns <- setdiff(names(trend_data$rawdata), c("wbid", "year", "month", "day", "time"))
      
      flog.info(paste("Parameters to analyze:", paste(param_columns, collapse = ", ")))
      
      results <- data.frame(Parameter = character(),
                            Timescale = character(),
                            Trend = character(),
                            P_value = numeric(),
                            Slope = numeric(),
                            stringsAsFactors = FALSE)
      
      # Iterate through each parameter column
      for (param in param_columns) {
        #print(paste("Processing parameter:", param))
        
        # Convert the specific parameter column to numeric
        trend_data$rawdata[[param]] <- as.numeric(trend_data$rawdata[[param]])
        
        # Check if there's valid data for the parameter
        param_data <- trend_data$rawdata %>%
          select(wbid, year, month, day, result = !!sym(param)) %>%
          filter(!is.na(result))
        
        #print(paste("Number of valid data points for", param, ":", nrow(param_data)))
        
        if (nrow(param_data) < 3) {
          print(paste("Skipping invalid parameter:", param, "- insufficient data"))
          next
        }
        
        # Aggregate data to daily values
        daily_data <- param_data %>%
          group_by(wbid, year, month, day) %>%
          summarize(result = mean(result, na.rm = TRUE), .groups = 'drop')
        
        #print(paste("Number of daily aggregated data points for", param, ":", nrow(daily_data)))
        
        if (nrow(daily_data) == 0) {
          print(paste("No valid data for parameter:", param, "after aggregation"))
          next
        }
        
        # Run trend analysis for the current parameter
        #print(paste("Running trend analysis for", param))
        trend_res <- run_trend_analysis(
          extracted_data = list(rawdata = daily_data, geomeans = trend_data$geomeans),
          start_yr = min(daily_data$year),
          trend_sel = TRUE,
          monthly_sel = input$monthly_trend,
          seasonal_sel = input$seasonal_trend,
          annual_sel = input$annual_trend,
          year_trend = "Calendar year"
        )
        
        if (!is.null(trend_res) && nrow(trend_res) > 0) {
          #print(paste("Valid trend results obtained for", param))
          #print(str(trend_res))
          trend_res$Parameter <- param
          results <- rbind(results, trend_res)
        } else {
          #print(paste("No valid results for parameter:", param))
        }
      }
      
      if (nrow(results) == 0) {
        print("No valid results found for any parameter")
        stop("No valid results found for the provided inputs.")
      }
      
      #print("Final results summary:")
      #print(str(results))
      
      results
    }, error = function(e) {
      flog.error(paste("Error in run_analysis:", e$message))
      showNotification(paste("Error in running trend analysis:", e$message), type = "error")
      return(NULL)
    })
  }
  
  
  # Observers
  observe({
    update_trend_ui_elements()
  })
  
  
  
  observeEvent(input$runTrendAnalysis, {
    withProgress(message = 'Running trend analysis...', value = 0, {
      trend_data <- prepare_trend_data()
      
      #print("Prepared trend data:")
      #print(str(trend_data))
      
      incProgress(0.3, detail = "Processing data...")
      
      trend_res <- run_analysis(trend_data)
      
      #print("Trend analysis results:")
      #print(str(trend_res))
      
      incProgress(0.6, detail = "Analyzing trends...")
      
      if (is.null(trend_res) || nrow(trend_res) == 0) {
        showNotification("No valid results found for the provided inputs.", type = "warning")
        trend_results(NULL)
        return()
      }
      
      trend_results(trend_res)
      selected_detail_param(NULL)
      
      incProgress(1, detail = "Completed!")
      showNotification("Trend analysis completed successfully.", type = "message")
    })
  })
  
  observeEvent(input$resetTrendAnalysis, {
    update_trend_ui_elements()
    trend_results(NULL)
    selected_detail_param(NULL)
  })
  
  # some input validation
  observe({
    req(input$trend_start_year, input$trend_end_year)
    if (input$trend_start_year > input$trend_end_year) {
      updateNumericInput(session, "trend_end_year", value = input$trend_start_year)
    }
  })
  
  # Outputs
  output$trend_summary_plot <- renderPlot({
    res <- trend_results()
    req(res)
    
    trend_summary <- res %>%
      mutate(TrendCategory = factor(Trend, levels = c("Increasing", "No significant trend", "Decreasing"))) %>%
      group_by(Parameter, Timescale, TrendCategory) %>%
      summarize(Count = n(), .groups = "drop") %>%
      group_by(Parameter, Timescale) %>%
      mutate(Proportion = Count / sum(Count)) %>%
      ungroup()
    
    ggplot(trend_summary, aes(x = Timescale, y = Proportion, fill = TrendCategory)) +
      geom_col(position = "stack") +
      scale_fill_manual(values = c("Increasing" = "lightgreen", "No significant trend" = "lightgrey", "Decreasing" = "lightpink")) +
      labs(title = "Trend Summary for All Parameters", 
           x = "Timescale", 
           y = "Proportion of Trends",
           fill = "Trend Category") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      facet_wrap(~Parameter, scales = "free_y") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1))
  })
  
  # Update the observeEvent for plot clicks
  observeEvent(input$trend_summary_plot_click, {
    click <- input$trend_summary_plot_click
    res <- trend_results()
    
    if (!is.null(click) && !is.null(res)) {
      # Get unique parameters
      parameters <- unique(res$Parameter)
      
      # Calculate the number of rows and columns in the facet
      n_params <- length(parameters)
      n_cols <- ceiling(sqrt(n_params))
      n_rows <- ceiling(n_params / n_cols)
      
      # Calculate which facet was clicked
      facet_col <- ceiling(click$x / (1/n_cols))
      facet_row <- ceiling((1 - click$y) / (1/n_rows))
      facet_number <- (facet_row - 1) * n_cols + facet_col
      
      if (facet_number <= n_params) {
        clicked_parameter <- parameters[facet_number]
        selected_detail_param(clicked_parameter)
        print(paste("Clicked parameter:", clicked_parameter))  # Debug print
      }
    }
  })
  
  output$trend_analysis_summary <- renderText({
    res <- trend_results()
    req(res)
    
    summary_text <- paste("Trend Analysis Summary:\n\n")
    
    for (param in unique(res$Parameter)) {
      param_summary <- res %>% 
        filter(Parameter == param) %>%
        summarize(
          Total = n(),
          Increasing = sum(Trend == "Increasing"),
          Decreasing = sum(Trend == "Decreasing"),
          No_Trend = sum(Trend == "No significant trend")
        )
      
      summary_text <- paste0(summary_text, 
                             param, ":\n",
                             "  Total trends analyzed: ", param_summary$Total, "\n",
                             "  Increasing trends: ", param_summary$Increasing, " (", round(param_summary$Increasing / param_summary$Total * 100, 1), "%)\n",
                             "  Decreasing trends: ", param_summary$Decreasing, " (", round(param_summary$Decreasing / param_summary$Total * 100, 1), "%)\n",
                             "  No significant trends: ", param_summary$No_Trend, " (", round(param_summary$No_Trend / param_summary$Total * 100, 1), "%)\n\n"
      )
    }
    
    summary_text
  })
  
  output$trend_analysis_progress <- renderUI({
    res <- trend_results()
    req(res)
    
    div(
      p("Analysis completed successfully."),
      tags$ul(
        tags$li(paste("Parameters analyzed:", paste(unique(res$Parameter), collapse = ", "))),
        tags$li(paste("Time periods:", paste(unique(res$Timescale), collapse = ", ")))
      )
    )
  })
  
  observeEvent(input$trend_summary_plot_click, {
    click_data <- input$trend_summary_plot_click
    if (!is.null(click_data) && !is.null(click_data$panelvar1)) {
      clicked_parameter <- click_data$panelvar1
      selected_detail_param(clicked_parameter)
    }
  })
  
  output$trend_detailed_results <- renderUI({
    res <- trend_results()
    req(res)
    
    selected_param <- selected_detail_param()
    
    if (is.null(selected_param)) {
      return(p("Click on a parameter in the summary plot to view detailed results."))
    }
    
    param_res <- res %>% filter(Parameter == selected_param)
    
    tagList(
      h4(paste("Detailed Results for", selected_param)),
      plotlyOutput("trend_param_timeseries_plot"),
      DTOutput("trend_param_results_table")
    )
  })
  
  # Update the trend_param_timeseries_plot output
  output$trend_param_timeseries_plot <- renderPlotly({
    selected_param <- selected_detail_param()
    req(selected_param)
    trend_data <- prepare_trend_data()
    
    # Error checking
    if (is.null(trend_data) || nrow(trend_data$rawdata) == 0) {
      return(plotly_empty())
    }
    
    param_data <- trend_data$rawdata %>%
      select(wbid, year, month, day, !!sym(selected_param)) %>%
      rename(result = !!sym(selected_param)) %>%
      filter(!is.na(result)) %>%
      mutate(
        result = as.numeric(result),
        date = as.Date(paste(year, month, day, sep = "-"))
      ) %>%
      group_by(wbid, date) %>%
      summarize(result = mean(result, na.rm = TRUE), .groups = 'drop')
    
    p <- ggplot(param_data, aes(x = date, y = result, group = wbid, color = wbid)) +
      geom_line(alpha = 0.7) +
      geom_smooth(method = "loess", se = FALSE, linetype = "dashed", color = "black") +
      labs(title = paste("Time Series Plot for", selected_param),
           x = "Date", y = "Result", color = "WBID") +
      theme_minimal() +
      theme(legend.position = "none")  # Hide legend if there are too many WBIDs
    
    ggplotly(p) %>% 
      layout(hovermode = "closest") %>%
      config(displayModeBar = FALSE)
  })
  
  # Add this function to check trend results
  check_trend_results <- function(res) {
    if (is.null(res) || nrow(res) == 0) {
      warning("Trend results are empty or NULL.")
      return(FALSE)
    }
    if (!"Slope" %in% names(res)) {
      warning("Slope column is missing from trend results.")
      return(FALSE)
    }
    if (all(is.na(res$Slope))) {
      warning("All Slope values are NA.")
      return(FALSE)
    }
    return(TRUE)
  }
  
  # Update the trend_param_results_table output
  # Update the trend_param_results_table output
  output$trend_param_results_table <- renderDT({
    selected_param <- selected_detail_param()
    selected_timescale <- input$trend_timescale
    req(selected_param, selected_timescale)
    res <- trend_results()
    
    flog.debug("Rendering trend results table")
    flog.debug(paste("Selected parameter:", selected_param))
    flog.debug(paste("Selected timescale:", selected_timescale))
    
    if (is.null(res) || nrow(res) == 0) {
      flog.warn("No trend results available")
      return(datatable(data.frame(Message = "No trend results available."), options = list(dom = 't')))
    }
    
    param_res <- res %>% 
      filter(Parameter == selected_param) %>%
      mutate(
        P_value = round(as.numeric(P_value), 4),
        Slope = round(as.numeric(Slope), 6)
      )
    
    # Filter based on selected timescale
    if (selected_timescale != "All") {
      param_res <- param_res %>%
        filter(case_when(
          selected_timescale == "Annual" ~ Timescale == "Annual",
          selected_timescale == "Seasonal" ~ str_detect(Timescale, "Seasonal"),
          selected_timescale == "Monthly" ~ str_detect(Timescale, "Monthly"),
          TRUE ~ TRUE
        ))
    }
    
    param_res <- param_res %>%
      select(Timescale, Trend, P_value, Slope, everything())  # Reorder columns
    
    datatable(param_res, 
              options = list(pageLength = 25, scrollX = TRUE),
              rownames = FALSE) %>%
      formatStyle('Trend',
                  backgroundColor = styleEqual(
                    c("Increasing", "Decreasing", "No significant trend"),
                    c('lightgreen', 'lightpink', 'lightgrey')
                  )) %>%
      formatStyle('Slope',
                  backgroundColor = styleInterval(c(-0.001, 0.001), c('lightpink', 'lightgrey', 'lightgreen')),
                  fontWeight = 'bold')
  })
  
 
  options(warn = 0)  # Reset warning level
   
}
