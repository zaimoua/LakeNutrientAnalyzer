library(RSQLite)
library(data.table)
library(logging)
library(digest)

# Set up logging
basicConfig()

# Global cache
cache <- new.env(parent = emptyenv())

# Helper functions
create_where_clause <- function(column, values) {
  values <- sapply(values, function(x) paste0("'", x, "'"))
  clause <- paste(column, "IN (", paste(values, collapse = ", "), ")", sep = " ")
  return(clause)
}

YC <- function(OBJECT) {
  loginfo("Calculating yearly counts.")
  tryCatch({
    mdat <- setDT(copy(OBJECT))
    id_vars <- c("wbid", "year", "month", "day", "time")
    measure_vars <- setdiff(names(mdat), id_vars)
    mdat <- melt(mdat, id.vars = id_vars, measure.vars = measure_vars, variable.factor = FALSE)
    mdat <- mdat[complete.cases(mdat)]
    mdat.ag <- dcast(mdat[, .(value = .N), by = .(year, variable)], year ~ variable)
    setnames(mdat.ag, "year", "Year")
    return(mdat.ag)
  }, error = function(e) {
    logerror(paste("Error in YC function:", e$message))
    return(NULL)
  })
}

GSC <- function(OBJECT, TYPE="") {
  loginfo("Calculating geometric mean sample count.")
  tryCatch({
    mdat <- setDT(copy(OBJECT))
    mdat[, season := ifelse(month > 4 & month <= 10, 1, 0)]
    id_vars <- c("wbid", "year", "month", "day", "time", "season")
    measure_vars <- setdiff(names(mdat), id_vars)
    mdat <- melt(mdat, id.vars = id_vars, measure.vars = measure_vars, variable.factor = FALSE)
    mdat <- mdat[complete.cases(mdat)]
    mdat[, value := as.numeric(season)]
    mdat.ag <- dcast(mdat[, .(value = sum(value)), by = .(year, variable)], year ~ variable)
    setnames(mdat.ag, "year", "Year")
    return(mdat.ag)
  }, error = function(e) {
    logerror(paste("Error in GSC function:", e$message))
    return(NULL)
  })
}

NGSC <- function(OBJECT, TYPE="") {
  loginfo("Calculating non-geometric mean sample count.")
  tryCatch({
    mdat <- setDT(copy(OBJECT))
    mdat[, season := ifelse(month > 4 & month <= 10, 1, 0)]
    id_vars <- c("wbid", "year", "month", "day", "time", "season")
    measure_vars <- setdiff(names(mdat), id_vars)
    mdat <- melt(mdat, id.vars = id_vars, measure.vars = measure_vars, variable.factor = FALSE)
    mdat <- mdat[complete.cases(mdat)]
    mdat[, value := as.numeric(season)]
    mdat.ag <- dcast(mdat[, .(value = sum(value)), by = .(year, variable)], year ~ variable)
    setnames(mdat.ag, "year", "Year")
    return(mdat.ag)
  }, error = function(e) {
    logerror(paste("Error in NGSC function:", e$message))
    return(NULL)
  })
}

# Helper function to replace #NUM! with NA in data frame
replace_num_errors <- function(df) {
  df[] <- lapply(df, function(col) {
    if (is.character(col)) {
      col[col == "#NUM!"] <- NA
    }
    return(col)
  })
  return(df)
}

data_extraction <- function(WBID = NULL, STATIONS = NULL, PARAM = NULL, LAKEWATCH = FALSE, IWR = NULL, year_range = NULL, station_id = NULL) {
  loginfo("Starting data extraction process.")
  
  tryCatch({
    # Input validation
    if (is.null(WBID)) stop("WBID cannot be NULL.")
    if (!file.exists(IWR)) stop("IWR database file does not exist.")
    if (!is.null(year_range) && (length(year_range) != 2 || !all(is.numeric(year_range)))) 
      stop("Invalid year range: Must be a numeric vector of length 2.")
    
    # Create cache key
    cache_key <- digest(list(WBID, STATIONS, PARAM, LAKEWATCH, IWR, year_range, station_id))
    
    # Check cache
    if (exists(cache_key, envir = cache)) {
      loginfo("Returning cached data.")
      return(get(cache_key, envir = cache))
    }
    
    loginfo("Creating SQL where clauses.")
    WBID.SQL <- create_where_clause("wbid", WBID)
    PARAM.SQL <- if (!is.null(PARAM) && length(PARAM) > 0) create_where_clause("mastercode", PARAM) else NULL
    YEAR.SQL <- if (!is.null(year_range) && length(year_range) == 2 && all(!is.na(year_range))) 
      paste("year BETWEEN", year_range[1], "AND", year_range[2]) else NULL
    STATIONS.SQL <- if (!is.null(station_id) && station_id != "") create_where_clause("sta", station_id) else NULL
    
    loginfo("Constructing SQL query.")
    # Modified to include STA in the query
    SQL.String <- paste("SELECT wbid, year, month, day, time, STA, mastercode, result FROM RawData WHERE", WBID.SQL)
    if (!is.null(PARAM.SQL)) SQL.String <- paste(SQL.String, "AND", PARAM.SQL)
    if (!is.null(YEAR.SQL)) SQL.String <- paste(SQL.String, "AND", YEAR.SQL)
    if (!is.null(STATIONS.SQL)) SQL.String <- paste(SQL.String, "AND", STATIONS.SQL)
    
    loginfo("Connecting to the database.")
    con <- dbConnect(SQLite(), IWR)
    on.exit(dbDisconnect(con), add = TRUE)
    
    loginfo("Executing SQL query.")
    data.raw <- dbGetQuery(con, SQL.String)
    
    loginfo("Checking for empty data.")
    if (nrow(data.raw) == 0) stop("No data found for the specified query.")
    
    loginfo("Converting raw data to data.table.")
    data.raw <- setDT(data.raw)
    
    # Add LAKEWATCH filtering here
    if (!LAKEWATCH) {
      data.raw <- data.raw[!grepl("21FLKWAT", STA)]
      if (nrow(data.raw) == 0) stop("No non-LAKEWATCH data available")
    }
    
    loginfo("Processing data.")
    data.raw[, `:=`(
      year = as.numeric(year),
      month = as.numeric(month),
      day = as.numeric(day),
      time = as.character(time),
      result = as.numeric(result)
    )]
    
    loginfo("Replacing #NUM! errors with NA.")
    data.raw <- replace_num_errors(data.raw)
    
    loginfo("Reshaping raw data to wide format.")
    id_vars <- c("wbid", "year", "month", "day", "time")
    data.raw.wide <- dcast(data.raw, as.formula(paste(paste(id_vars, collapse = " + "), "~ mastercode")), 
                           value.var = "result", fun.aggregate = mean)
    
    loginfo("Calculating yearly geometric means.")
    # Calculate geometric means after LAKEWATCH filtering
    data.geomean <- data.raw[, .(geomean = exp(mean(log(result[result > 0]), na.rm = TRUE))), 
                             by = .(wbid, year, mastercode)]
    
    loginfo("Pivoting geometric means to wide format.")
    data.geomean.wide <- dcast(data.geomean, wbid + year ~ mastercode, value.var = "geomean")
    
    loginfo("Calculating summary statistics.")
    data.sum <- data.raw[, .(
      Min = min(result, na.rm = TRUE),
      `1st Qu.` = quantile(result, 0.25, na.rm = TRUE),
      Median = median(result, na.rm = TRUE),
      Mean = mean(result, na.rm = TRUE),
      `3rd Qu.` = quantile(result, 0.75, na.rm = TRUE),
      Max = max(result, na.rm = TRUE)
    ), by = .(wbid, mastercode)]
    
    loginfo("Preparing output.")
    Out.list <- list(
      "WBID" = WBID,
      "summary" = data.sum,
      "geomeans" = data.geomean.wide,
      "rawdata" = data.raw.wide
    )
    
    # Cache the result
    assign(cache_key, Out.list, envir = cache)
    
    return(Out.list)
    
  }, error = function(e) {
    logerror(paste("Error in data extraction:", e$message))
    return(NULL)
  })
}