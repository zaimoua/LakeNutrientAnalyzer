determine_lake_type <- function(data) {
  tryCatch({
    if (!all(c("COLOR", "ALK") %in% names(data))) {
      stop("COLOR or ALK data is missing")
    }
    
    color <- mean(data$COLOR, na.rm = TRUE)
    alkalinity <- mean(data$ALK, na.rm = TRUE)
    
    if (is.na(color) | is.na(alkalinity)) {
      stop("Color or alkalinity data is invalid or all NA")
    }
    
    lake_type <- if (color > 40) {
      "> 40 Platinum Cobalt Units"
    } else if (color <= 40 & alkalinity > 20) {
      "≤ 40 Platinum Cobalt Units and > 20 mg/L CaCO3"
    } else if (color <= 40 & alkalinity <= 20) {
      "≤ 40 Platinum Cobalt Units and ≤ 20 mg/L CaCO3"
    } else {
      "Unknown"
    }
    
    return(list(
      lake_type = lake_type,
      color = color,
      alkalinity = alkalinity
    ))
  }, error = function(e) {
    print(paste("Error in determine_lake_type:", e$message))
    return(NULL)
  })
}