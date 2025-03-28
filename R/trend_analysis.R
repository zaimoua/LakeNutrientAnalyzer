run_trend_analysis <- function(extracted_data, start_yr, trend_sel, monthly_sel, seasonal_sel, annual_sel, year_trend) {
  rawdata <- extracted_data$rawdata
  
  # Filter data based on start year
  rawdata <- rawdata[rawdata$year >= start_yr, ]
  
  # Initialize results dataframe
  results <- data.frame(Parameter = character(),
                        Timescale = character(),
                        Trend = character(),
                        P_value = numeric(),
                        Slope = numeric(),
                        stringsAsFactors = FALSE)
  
  # Function to calculate Sen's slope
  calculate_sens_slope <- function(x, y) {
    n <- length(x)
    slopes <- numeric()
    for (i in 1:(n-1)) {
      for (j in (i+1):n) {
        slope <- (y[j] - y[i]) / (x[j] - x[i])
        slopes <- c(slopes, slope)
      }
    }
    median(slopes, na.rm = TRUE)
  }
  
  
  # Function to run Mann-Kendall test
  run_mk_test <- function(data, parameter, timescale) {
    print(paste("Running MK test for", parameter, "on", timescale))
    print(paste("Data length:", length(data)))
    print(paste("Data summary:", toString(summary(data))))
    
    if (length(data) < 3 || all(is.na(data))) {
      print("Insufficient data")
      return(data.frame(Parameter = parameter,
                        Timescale = timescale,
                        Trend = "Insufficient data",
                        P_value = NA,
                        Slope = NA,
                        stringsAsFactors = FALSE))
    }
    
    mk_result <- tryCatch({
      Kendall::MannKendall(data)
    }, error = function(e) {
      print(paste("Error in MannKendall:", e$message))
      return(NULL)
    })
    
    if (is.null(mk_result)) {
      print("MannKendall returned NULL")
      return(data.frame(Parameter = parameter,
                        Timescale = timescale,
                        Trend = "Error in analysis",
                        P_value = NA,
                        Slope = NA,
                        stringsAsFactors = FALSE))
    }
    
    trend <- ifelse(mk_result$sl < 0.05, 
                    ifelse(mk_result$tau > 0, "Increasing", "Decreasing"), 
                    "No significant trend")
    
    # Calculate Sen's slope
    x <- 1:length(data)
    slope <- tryCatch({
      calculate_sens_slope(x, data)
    }, error = function(e) {
      print(paste("Error in slope calculation:", e$message))
      NA
    })
    
    print(paste("Calculated slope:", slope))
    
    return(data.frame(Parameter = parameter,
                      Timescale = timescale,
                      Trend = trend,
                      P_value = mk_result$sl,
                      Slope = slope,
                      stringsAsFactors = FALSE))
  }
  
  # Run analysis for each parameter
  for (param in setdiff(names(rawdata), c("wbid", "year", "month", "day"))) {
    param_data <- rawdata[, c("year", "month", param)]
    names(param_data)[3] <- "result"
    
    # Remove rows with NA results
    param_data <- param_data[!is.na(param_data$result), ]
    
    if (nrow(param_data) == 0) {
      results <- rbind(results, data.frame(Parameter = param,
                                           Timescale = "All",
                                           Trend = "No data",
                                           P_value = NA,
                                           Slope = NA,
                                           stringsAsFactors = FALSE))
      next
    }
    
    # Annual trend
    if (annual_sel) {
      annual_data <- aggregate(result ~ year, data = param_data, FUN = mean, na.rm = TRUE)
      results <- rbind(results, run_mk_test(annual_data$result, param, "Annual"))
    }
    
    # Seasonal trend
    if (seasonal_sel) {
      param_data$season <- cut(param_data$month, breaks = c(0,3,6,9,12), 
                               labels = c("Winter", "Spring", "Summer", "Fall"))
      seasonal_data <- aggregate(result ~ year + season, data = param_data, FUN = mean, na.rm = TRUE)
      for (season in unique(seasonal_data$season)) {
        season_data <- seasonal_data[seasonal_data$season == season, ]
        results <- rbind(results, run_mk_test(season_data$result, param, paste("Seasonal -", season)))
      }
    }
    
    # Monthly trend
    if (monthly_sel) {
      monthly_data <- aggregate(result ~ year + month, data = param_data, FUN = mean, na.rm = TRUE)
      for (month in 1:12) {
        month_data <- monthly_data[monthly_data$month == month, ]
        results <- rbind(results, run_mk_test(month_data$result, param, paste("Monthly -", month.abb[month])))
      }
    }
  }
  
  return(results)
}