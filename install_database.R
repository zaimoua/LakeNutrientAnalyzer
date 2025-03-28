# iwr_access_to_sqlite.R - Utility for converting IWR Access databases to SQLite format
# This script converts four IWR Access databases to SQLite format
# and filters data to include only lakes and relevant nutrient parameters

library(shiny)
library(DBI)
library(RSQLite)
library(tools)  # For file_path_sans_ext
library(data.table)  # For efficient data manipulation

# Check if RODBC is installed
if (!requireNamespace("RODBC", quietly = TRUE)) {
  message("Installing RODBC package...")
  install.packages("RODBC")
  library(RODBC)
} else {
  library(RODBC)
}

# Set maximum upload size to 5 GB
options(shiny.maxRequestSize = 5000 * 1024^2)

# UI for the app
ui <- fluidPage(
  titlePanel("IWR Access to SQLite Converter"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("accessFiles", "Select Access Database Files (.accdb)", 
                multiple = TRUE, accept = c(".accdb")),
      textInput("sqliteOutput", "SQLite Output Path", 
                value = file.path("data", "IWR66_database.sqlite")),
      checkboxInput("filterData", "Filter data (lakes and nutrients only)", value = TRUE),
      actionButton("processButton", "Process Database", 
                   class = "btn-primary", style = "margin-top: 10px;")
    ),
    
    mainPanel(
      h3("Database Processing Log:"),
      verbatimTextOutput("processingLog"),
      
      h3("Instructions:"),
      tags$ol(
        tags$li("Download the IWR Access database files from ", 
                tags$a(href = "https://publicfiles.dep.state.fl.us/dear/IWR/", 
                       "Florida DEP Website", target = "_blank")),
        tags$li("Click 'Choose Files' to select all four .accdb files (rawDataDB1.accdb to rawDataDB4.accdb)"),
        tags$li("Verify the output path (default is 'data/IWR66_database.sqlite')"),
        tags$li("Check 'Filter data' if you want to include only lakes and nutrient parameters"),
        tags$li("Make sure WaterbodyID_Table.csv exists in the 'data' folder for filtering"),
        tags$li("Click 'Process Database' to start the conversion"),
        tags$li("Wait for the process to complete (this may take several minutes)")
      ),
      
      tags$div(
        style = "margin-top: 20px; padding: 15px; background-color: #f8f9fa; border-radius: 5px;",
        tags$h4("Technical Notes:"),
        tags$ul(
          tags$li("Requires RODBC to connect to Access databases"),
          tags$li("Use 32-bit R for 32-bit Access databases; 64-bit R for 64-bit"),
          tags$li("Ensure write permissions for the output directory"),
          tags$li("Existing SQLite files at the output path will be overwritten"),
          tags$li("Processing may take 5-10 minutes depending on system speed")
        )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Create data directory if it doesn't exist
  dir.create("data", showWarnings = FALSE)
  
  # Log messages
  logs <- reactiveVal("")
  
  # Add to log
  addLog <- function(message) {
    current <- logs()
    logs(paste0(current, message, "\n"))
  }
  
  # Process button
  observeEvent(input$processButton, {
    req(input$accessFiles)
    
    # Validate that all files are .accdb
    if (!all(grepl("\\.accdb$", input$accessFiles$name, ignore.case = TRUE))) {
      addLog("Error: All uploaded files must be .accdb files")
      showNotification("Please upload only .accdb files", type = "error")
      return()
    }
    
    # Check for waterbody file if filtering is enabled
    if (input$filterData) {
      waterbody_file_path <- file.path("data", "WaterbodyID_Table.csv")
      if (!file.exists(waterbody_file_path)) {
        addLog("Error: WaterbodyID_Table.csv not found in data folder")
        showNotification("WaterbodyID_Table.csv must exist in data folder for filtering", type = "error")
        return()
      }
    }
    
    # Define nutrient parameters for filtering
    nutrient_params <- c("CHLAC", "TN", "TP", "COLOR", "ALK", "COND", "TEMP", "DO", "PORTH", 
                         "NH4", "NO3O2", "BOD", "DOSAT", "UNNH4", "PORD", "TKN", "CHLA", "TORTH")
    
    # Load lake WBIDs if filtering is enabled
    lake_wbids <- NULL
    if (input$filterData) {
      tryCatch({
        waterbody_type <- fread(waterbody_file_path)
        
        # Add debugging information about the loaded file
        addLog(paste("Waterbody file columns:", paste(names(waterbody_type), collapse=", ")))
        addLog(paste("First few water types:", paste(head(waterbody_type$WATER_TYPE), collapse=", ")))
        
        # Check if columns exist and extract lake WBIDs, being case-insensitive
        if ("WATER_TYPE" %in% names(waterbody_type) && "WBID" %in% names(waterbody_type)) {
          # Case-insensitive matching for "LAKE"
          lake_rows <- grepl("^lake$", waterbody_type$WATER_TYPE, ignore.case = TRUE)
          lake_wbids <- waterbody_type$WBID[lake_rows]
          addLog(paste("Loaded", length(lake_wbids), "lake WBIDs for filtering"))
          addLog(paste("First few lake WBIDs:", paste(head(lake_wbids), collapse=", ")))
        } else {
          addLog("Error: Expected columns 'WATER_TYPE' and 'WBID' not found in waterbody file")
          addLog(paste("Found columns:", paste(names(waterbody_type), collapse=", ")))
          showNotification("Waterbody file format incorrect", type = "error")
          return()
        }
      }, error = function(e) {
        addLog(paste("Error loading waterbody file:", e$message))
        showNotification("Error loading waterbody file", type = "error")
        return()
      })
    }
    
    addLog("Starting database processing...")
    
    # Create mapping of file names to table names
    table_mapping <- list(
      "rawDataDB1.accdb" = "rawData1",
      "rawDataDB2.accdb" = "rawData2",
      "rawDataDB3.accdb" = "rawData3",
      "rawDataDB4.accdb" = "rawData4"
    )
    
    withProgress(message = 'Processing database', value = 0, {
      # Create SQLite database
      output_sqlite_path <- input$sqliteOutput
      if (file.exists(output_sqlite_path)) {
        file.remove(output_sqlite_path)
        addLog("Existing SQLite file removed")
      }
      con_sqlite <- DBI::dbConnect(RSQLite::SQLite(), output_sqlite_path)
      addLog("SQLite database created")
      
      # Create RawData table
      DBI::dbExecute(con_sqlite, "CREATE TABLE RawData(
        wbid text,
        STA text,
        year integer,
        month integer,
        day integer,
        time integer,
        depth float,
        param integer,
        mastercode text,
        result float,
        rcode text,
        xcode text,
        lab integer,
        mdl text,
        pql text,
        dunits text,
        ds text,
        season integer,
        hurricaneflag integer,
        week2 integer,
        planperiod text,
        verperiod text,
        epa text
      )")
      addLog("RawData table created")
      
      # Process each file
      num_files <- nrow(input$accessFiles)
      for (i in 1:num_files) {
        file_path <- input$accessFiles$datapath[i]
        file_name <- input$accessFiles$name[i]
        
        # Get table name from mapping
        table_name <- table_mapping[[file_name]]
        if (is.null(table_name)) {
          addLog(paste("No table mapping found for", file_name))
          next
        }
        
        addLog(paste("Processing file:", file_name, "with table:", table_name))
        
        tryCatch({
          # Connect to Access database
          con_access <- RODBC::odbcConnectAccess2007(file_path)
          # Extract table
          table_data <- RODBC::sqlFetch(con_access, table_name)
          # Close connection
          RODBC::odbcClose(con_access)
          # Convert column names to lowercase
          names(table_data) <- tolower(names(table_data))
          
          # Apply filtering if enabled
          original_rows <- nrow(table_data)
          if (input$filterData && !is.null(lake_wbids) && length(lake_wbids) > 0) {
            # Add debugging for the first few rows before filtering
            addLog(paste("Sample mastercode values before filtering:", 
                         paste(head(unique(table_data$mastercode)), collapse=", ")))
            
            # Convert table_data to data.table for faster filtering
            dt <- as.data.table(table_data)
            
            # Log counts before filtering
            addLog(paste("Rows before filtering:", nrow(dt)))
            
            # Filter for nutrient parameters (case-insensitive)
            nutrient_matches <- dt$mastercode %in% nutrient_params
            addLog(paste("Rows matching nutrient params:", sum(nutrient_matches)))
            
            # Filter for lake WBIDs (matching strings, case-insensitive if needed)
            lake_matches <- dt$wbid %in% lake_wbids
            addLog(paste("Rows matching lake WBIDs:", sum(lake_matches)))
            
            # Apply both filters
            dt <- dt[nutrient_matches & lake_matches]
            
            # Convert back to data.frame
            table_data <- as.data.frame(dt)
            
            filtered_rows <- nrow(table_data)
            addLog(paste("  Filtered from", original_rows, "to", filtered_rows, "rows",
                         "(", round(filtered_rows/original_rows*100, 2), "% kept)"))
          }
          
          # Write to SQLite
          DBI::dbWriteTable(con_sqlite, "RawData", table_data, append = TRUE)
          addLog(paste("  Successfully processed", nrow(table_data), "rows from", table_name))
        }, error = function(e) {
          addLog(paste("  Error processing", table_name, ":", e$message))
        })
        
        # Update progress
        incProgress(1 / num_files, detail = paste("Processed", i, "of", num_files, "files"))
      }
      
      # Create index
      addLog("Creating index (this may take a while)...")
      DBI::dbExecute(con_sqlite, "CREATE INDEX search_speed ON RawData (wbid, sta, year, month, day, mastercode, result)")
      addLog("Index created")
      
      # Close SQLite connection
      DBI::dbDisconnect(con_sqlite)
      addLog("Database processing complete!")
    })
  })
  
  # Display logs
  output$processingLog <- renderText({
    logs()
  })
}

# Run the Shiny app
shinyApp(ui, server)