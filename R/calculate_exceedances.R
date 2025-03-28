calculate_exceedances <- function(data, lake_type, criteria) {
  tryCatch({
    print("Input data:")
    print(head(data))
    print("Lake type:")
    print(lake_type)
    print("Criteria:")
    print(criteria)
    
    if (is.null(criteria) || length(criteria) == 0) {
      stop("Criteria object is null or empty")
    }
    
    if (!(as.character(lake_type) %in% names(criteria))) {
      stop(paste("Invalid lake_type:", lake_type))
    }
    
    required_params <- c("CHLAC", "TP", "TN")
    available_params <- intersect(required_params, names(data))
    missing_params <- setdiff(required_params, available_params)
    if (length(missing_params) > 0) {
      warning(paste("Missing parameters:", paste(missing_params, collapse = ", ")))
    }
    
    if (length(available_params) == 0) {
      stop("No required parameters (CHLAC, TP, TN) found in the data")
    }
    
    data_filtered <- data %>%
      select(year, all_of(available_params)) %>%
      group_by(year) %>%
      summarise(across(all_of(available_params), ~exp(mean(log(.), na.rm = TRUE)), .names = "geo_mean_{.col}")) %>%
      pivot_longer(cols = starts_with("geo_mean_"), names_to = "parameter", values_to = "geo_mean") %>%
      mutate(parameter = sub("geo_mean_", "", parameter))
    
    print("Data after filtering and summarising:")
    print(data_filtered)
    
    if (nrow(data_filtered) == 0) {
      stop("No data available for exceedance calculation after filtering and summarizing")
    }
    
    print("Criteria for this lake type:")
    print(criteria[[as.character(lake_type)]])
    
    exceedances <- data_filtered %>%
      mutate(
        exceedance = case_when(
          parameter == "CHLAC" & geo_mean > criteria[[as.character(lake_type)]][["CHLAC"]] ~ "Exceeds",
          parameter == "TP" & geo_mean > criteria[[as.character(lake_type)]][["TP"]] ~ "Exceeds",
          parameter == "TN" & geo_mean > criteria[[as.character(lake_type)]][["TN"]] ~ "Exceeds",
          TRUE ~ "Meets"
        )
      )
    
    print("Exceedances:")
    print(exceedances)
    
    return(exceedances)
  }, error = function(e) {
    print(paste("Error in calculate_exceedances:", e$message))
    return(NULL)
  })
}


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