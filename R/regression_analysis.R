library(ggplot2)
library(data.table)
library(plotly)
library(dplyr)
library(broom)
library(car)
library(lmtest)
library(caret)
library(tidyr)

# Single WBID Regression Functions

prepare_single_wbid_data <- function(data, wbid, start_year, end_year, response, explanatory1, explanatory2 = NULL) {
  data %>%
    filter(wbid == !!wbid, year >= start_year, year <= end_year) %>%
    select(year, wbid, !!response, !!explanatory1, !!explanatory2) %>%
    mutate(across(c(!!response, !!explanatory1, !!explanatory2), as.numeric))
}

perform_single_regression <- function(data, response, explanatory1, explanatory2 = NULL, include_interaction = FALSE, log_transform = FALSE) {
  if (log_transform) {
    data <- data %>% mutate(across(c(!!response, !!explanatory1, !!explanatory2), log))
  }
  
  formula <- if (is.null(explanatory2)) {
    as.formula(paste(response, "~", explanatory1))
  } else if (include_interaction) {
    as.formula(paste(response, "~", explanatory1, "*", explanatory2))
  } else {
    as.formula(paste(response, "~", explanatory1, "+", explanatory2))
  }
  
  model <- lm(formula, data = data)
  
  list(
    model = model,
    summary = tidy(model),
    glance = glance(model),
    data = data
  )
}


# Multiple WBID Regression Functions

prepare_multiple_wbid_data <- function(selected_wbids, start_year, end_year, response, explanatory1, explanatory2 = NULL, IWR_path) {
  all_data <- lapply(selected_wbids, function(wbid) {
    wbid_data <- tryCatch({
      data_extraction(WBID = wbid, PARAM = c(response, explanatory1, explanatory2), IWR = IWR_path)
    }, error = function(e) {
      message(paste("Error extracting data for WBID", wbid, ":", e$message))
      return(NULL)
    })
    
    if (!is.null(wbid_data) && !is.null(wbid_data$geomeans)) {
      wbid_data$geomeans %>%
        mutate(wbid = wbid) %>%
        filter(year >= start_year, year <= end_year) %>%
        select(year, wbid, !!response, !!explanatory1, !!explanatory2)
    } else {
      NULL
    }
  })
  
  combined_data <- bind_rows(all_data)
  
  if (nrow(combined_data) == 0) {
    stop("No data available for regression analysis.")
  }
  
  combined_data %>%
    mutate(wbid = factor(wbid),
           across(c(!!response, !!explanatory1, !!explanatory2), as.numeric))
}

perform_multiple_regression <- function(data, response, explanatory1, explanatory2 = NULL, include_interaction = FALSE, log_transform = FALSE) {
  # Function to safely log transform
  safe_log <- function(x) {
    # Add a small constant to avoid log(0)
    min_non_zero <- min(x[x > 0], na.rm = TRUE)
    constant <- ifelse(min_non_zero < 1, min_non_zero / 2, 0.1)
    log(x + constant)
  }
  
  if (log_transform) {
    # Apply safe log transformation
    data[[response]] <- safe_log(data[[response]])
    data[[explanatory1]] <- safe_log(data[[explanatory1]])
    if (!is.null(explanatory2)) {
      data[[explanatory2]] <- safe_log(data[[explanatory2]])
    }
  }
  
  # Construct formula
  formula <- as.formula(paste(response, "~", explanatory1, "+ factor(wbid)"))
  if (!is.null(explanatory2)) {
    if (include_interaction) {
      formula <- update(formula, as.formula(paste(". ~ . *", explanatory2)))
    } else {
      formula <- update(formula, as.formula(paste(". ~ . +", explanatory2)))
    }
  }
  
  # Fit model
  model <- lm(formula, data = data)
  
  list(
    model = model,
    summary = tidy(model),
    glance = glance(model),
    data = data
  )
}


# New Helper function to format full regression summary
format_full_regression_summary <- function(full_summary) {
  summary_df <- as.data.frame(full_summary)
  summary_df$Coefficient <- rownames(summary_df)
  summary_df <- summary_df %>%
    select(Coefficient, Estimate, `Std. Error`, `t value`, `Pr(>|t|)`) %>%
    mutate(
      Estimate = format(Estimate, digits = 3, scientific = FALSE),
      `Std. Error` = format(`Std. Error`, digits = 3, scientific = FALSE),
      `t value` = format(`t value`, digits = 3, scientific = FALSE),
      `Pr(>|t|)` = format.pval(`Pr(>|t|)`, digits = 3)
    )
  return(summary_df)
}

# Function to create regression plots
# Single WBID Regression Plot Function
create_single_regression_plot <- function(data, response, explanatory1, explanatory2 = NULL, param_labels, include_interaction = FALSE) {
  response_label <- param_labels[[response]] %||% response
  explanatory1_label <- param_labels[[explanatory1]] %||% explanatory1
  
  plot <- ggplot(data, aes(x = .data[[explanatory1]], y = .data[[response]])) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "red") +
    labs(x = explanatory1_label, y = response_label, 
         title = "Single WBID Regression Plot",
         subtitle = paste("Response:", response, "vs Explanatory:", explanatory1)) +
    theme_minimal(base_size = 15)
  
  if (!is.null(explanatory2)) {
    explanatory2_label <- param_labels[[explanatory2]] %||% explanatory2
    plot <- plot +
      aes(color = .data[[explanatory2]]) +
      labs(color = explanatory2_label)
    
    if (include_interaction) {
      plot <- plot + geom_smooth(method = "lm", formula = y ~ x * z, se = TRUE, aes(color = .data[[explanatory2]]))
    }
  }
  
  return(plot)
}

# Multiple WBID Regression Plot Function
create_multiple_regression_plot <- function(data, response, explanatory1, explanatory2 = NULL, param_labels, include_interaction = FALSE, use_size_for_explanatory2 = FALSE) {
  # Check if all required variables are present in the data
  required_vars <- c(response, explanatory1, "wbid")
  if (!is.null(explanatory2)) required_vars <- c(required_vars, explanatory2)
  
  missing_vars <- setdiff(required_vars, names(data))
  if (length(missing_vars) > 0) {
    stop(paste("The following variables are missing from the data:", paste(missing_vars, collapse = ", ")))
  }
  
  # Create the base plot
  p <- ggplot(data, aes_string(x = explanatory1, y = response)) +
    geom_point(aes(color = wbid)) +
    geom_smooth(method = "lm", se = TRUE, color = "black", 
                formula = if(!is.null(explanatory2) && isTRUE(include_interaction)) y ~ x * z else y ~ x) +
    labs(x = param_labels[[explanatory1]] %||% explanatory1,
         y = param_labels[[response]] %||% response,
         title = paste("Combined Regression Plot:", response, "vs", explanatory1),
         subtitle = "Single regression line for all WBIDs") +
    theme_minimal() +
    theme(legend.position = "right")
  
  # Add second explanatory variable if provided and option is selected
  if (!is.null(explanatory2) && isTRUE(use_size_for_explanatory2)) {
    p <- p + aes_string(size = explanatory2) +
      labs(size = param_labels[[explanatory2]] %||% explanatory2)
  } else if (!is.null(explanatory2)) {
    # If explanatory2 is provided but not used for size, add it to the subtitle
    p <- p + labs(subtitle = paste(p$labels$subtitle, "\nSecond explanatory variable:", explanatory2))
  }
  
  return(p)
}


# Function to create diagnostic plots
create_diagnostic_plots <- function(model, multiple_wbids = FALSE) {
  augmented_data <- augment(model)
  
  # Add WBID as a factor if it's a multiple WBID analysis
  if (multiple_wbids && "wbid" %in% names(augmented_data)) {
    augmented_data$wbid <- factor(augmented_data$wbid)
  }
  
  base_plot <- function(p) {
    p + theme_minimal(base_size = 15) +
      (if (multiple_wbids && "wbid" %in% names(augmented_data)) 
        scale_color_viridis_d(option = "D", begin = 0.3, end = 0.9) 
       else 
         scale_color_manual(values = "black")
      ) +
      theme(legend.position = "none")
  }
  
  plot_list <- list(
    resid_vs_fitted = base_plot(
      ggplot(augmented_data, aes(x = .fitted, y = .resid, color = if (multiple_wbids && "wbid" %in% names(augmented_data)) wbid else NULL)) +
        geom_point(alpha = 0.7) +
        geom_smooth(se = FALSE, method = "loess", formula = y ~ x, color = "red") +
        geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
        labs(x = "Fitted values", y = "Residuals", 
             title = "Residuals vs Fitted",
             subtitle = if (multiple_wbids && "wbid" %in% names(augmented_data)) "Colors represent different WBIDs" else NULL)
    ),
    
    qq_plot = base_plot(
      ggplot(augmented_data, aes(sample = .std.resid, color = if (multiple_wbids && "wbid" %in% names(augmented_data)) wbid else NULL)) +
        stat_qq(alpha = 0.7) +
        stat_qq_line() +
        labs(x = "Theoretical Quantiles", y = "Standardized Residuals", 
             title = "Normal Q-Q Plot",
             subtitle = if (multiple_wbids && "wbid" %in% names(augmented_data)) "Colors represent different WBIDs" else NULL)
    ),
    
    scale_location = base_plot(
      ggplot(augmented_data, aes(x = .fitted, y = sqrt(abs(.std.resid)), color = if (multiple_wbids && "wbid" %in% names(augmented_data)) wbid else NULL)) +
        geom_point(alpha = 0.7) +
        geom_smooth(se = FALSE, method = "loess", formula = y ~ x, color = "red") +
        labs(x = "Fitted values", y = "√|Standardized Residuals|", 
             title = "Scale-Location",
             subtitle = if (multiple_wbids && "wbid" %in% names(augmented_data)) "Colors represent different WBIDs" else NULL)
    ),
    
    resid_vs_leverage = base_plot(
      ggplot(augmented_data, aes(x = .hat, y = .std.resid, color = if (multiple_wbids && "wbid" %in% names(augmented_data)) wbid else NULL)) +
        geom_point(alpha = 0.7) +
        geom_smooth(se = FALSE, method = "loess", formula = y ~ x, color = "red") +
        labs(x = "Leverage", y = "Standardized Residuals", 
             title = "Residuals vs Leverage",
             subtitle = if (multiple_wbids && "wbid" %in% names(augmented_data)) "Colors represent different WBIDs" else NULL)
    )
  )
  
  return(plot_list)
}

# Function to interpret regression results
interpret_regression <- function(reg_results) {
  model_summary <- summary(reg_results$model)
  
  # Model fit interpretation
  r_squared <- model_summary$r.squared
  adj_r_squared <- model_summary$adj.r.squared
  f_statistic <- model_summary$fstatistic
  p_value <- pf(f_statistic[1], f_statistic[2], f_statistic[3], lower.tail = FALSE)
  
  fit_interpretation <- paste(
    "The model explains", round(r_squared * 100, 2), "% of the variance in the response variable.",
    "The adjusted R-squared is", round(adj_r_squared, 3), ".",
    "The overall model is", ifelse(p_value < 0.05, "statistically significant", "not statistically significant"),
    "(F-statistic:", round(f_statistic[1], 2), "on", f_statistic[2], "and", f_statistic[3], "DF, p-value:", format.pval(p_value), ")."
  )
  
  # Coefficient interpretation
  coef_interpretation <- lapply(rownames(model_summary$coefficients), function(var) {
    coef <- model_summary$coefficients[var, "Estimate"]
    p_val <- model_summary$coefficients[var, "Pr(>|t|)"]
    sig <- if(p_val < 0.001) "highly significant" else if(p_val < 0.01) "very significant" else if(p_val < 0.05) "significant" else "not significant"
    paste(var, "has a coefficient of", round(coef, 3), "and is", sig, "(p-value:", format.pval(p_val), ").")
  })
  
  # Assumption checks
  assumptions <- list(
    normality = "Check the Q-Q plot for normality of residuals.",
    homoscedasticity = "Examine the Scale-Location plot for constant variance of residuals.",
    independence = ifelse(is.null(reg_results$dw_test), 
                          "Unable to perform Durbin-Watson test.", 
                          paste("Durbin-Watson test statistic:", round(reg_results$dw_test$dw, 3),
                                ". A value close to 2 suggests no autocorrelation."))
  )
  
  if (!is.null(reg_results$vif)) {
    assumptions$multicollinearity <- paste("VIF values:", 
                                           paste(names(reg_results$vif), round(reg_results$vif, 2), sep = ": ", collapse = ", "), 
                                           ". VIF > 5 may indicate multicollinearity.")
  }
  
  # Cross-validation results
  cv_interpretation <- if (!is.null(reg_results$cv_result)) {
    paste("Cross-validation RMSE:", round(reg_results$cv_result$results$RMSE, 3),
          "R-squared:", round(reg_results$cv_result$results$Rsquared, 3))
  } else {
    "Cross-validation could not be performed."
  }
  
  list(
    fit = fit_interpretation,
    coefficients = coef_interpretation,
    assumptions = assumptions,
    cross_validation = cv_interpretation
  )
}

# Main function to run regression analysis
run_regression_analysis <- function(data, response, explanatory1, explanatory2 = NULL, include_interaction = FALSE, 
                                    polynomial_degree = 1, log_transform = FALSE, multiple_wbids = FALSE, param_labels) {
  
  reg_results <- if (multiple_wbids) {
    perform_multiple_regression(data, response, explanatory1, explanatory2, 
                                include_interaction, log_transform)
  } else {
    perform_single_regression(data, response, explanatory1, explanatory2, 
                              include_interaction, log_transform)
  }
  
  # Create regression plot
  plot <- if (multiple_wbids) {
    create_multiple_regression_plot(reg_results$data, response, explanatory1, explanatory2, 
                                    param_labels, include_interaction = include_interaction)
  } else {
    create_single_regression_plot(reg_results$data, response, explanatory1, explanatory2, 
                                  param_labels, include_interaction = include_interaction)
  }
  
  # Create diagnostic plots
  diagnostic_plots <- create_diagnostic_plots(reg_results$model, multiple_wbids = multiple_wbids)
  
  # Interpret results
  interpretation <- interpret_regression(reg_results)
  
  # Perform Shapiro-Wilk test
  shapiro_results <- shapiro_wilk_test(reg_results$data, data_type = "agm")
  
  # Return all results
  return(list(
    regression_results = reg_results,
    plot = plot,
    diagnostic_plots = diagnostic_plots,
    interpretation = interpretation,
    shapiro_results = shapiro_results,
    full_summary = format_full_regression_summary(summary(reg_results$model))
  ))
}

# Helper function for Shapiro-Wilk test
shapiro_wilk_test <- function(data, data_type = "raw") {
  if (data_type == "raw") {
    # For raw data
    shapiro_results <- data %>%
      group_by(mastercode) %>%
      summarise(
        n = n(),
        p_value = if (n() > 3) shapiro.test(result)$p.value else NA_real_,
        normality = case_when(
          is.na(p_value) ~ "Insufficient data",
          p_value > 0.05 ~ "Normal",
          TRUE ~ "Non-normal"
        ),
        .groups = "drop"
      ) %>%
      rename(Parameter = mastercode) %>%
      mutate(p_value = format(p_value, scientific = TRUE, digits = 4))
  } else {
    # For geometric means or other data types
    numeric_columns <- data %>% select_if(is.numeric) %>% names()
    
    shapiro_results <- data %>%
      select(all_of(numeric_columns)) %>%
      pivot_longer(cols = everything(), names_to = "Parameter", values_to = "value") %>%
      group_by(Parameter) %>%
      summarise(
        n = n(),
        p_value = if (n() > 3) shapiro.test(value)$p.value else NA_real_,
        normality = case_when(
          is.na(p_value) ~ "Insufficient data",
          p_value > 0.05 ~ "Normal",
          TRUE ~ "Non-normal"
        ),
        .groups = "drop"
      ) %>%
      mutate(p_value = format(p_value, scientific = TRUE, digits = 4))
  }
  
  return(shapiro_results)
}

# Helper function to format regression summary
format_regression_summary <- function(reg_results) {
  summary_table <- reg_results$summary %>%
    mutate(
      p.value = format.pval(p.value, digits = 3),
      estimate = format(estimate, digits = 3, scientific = FALSE),
      std.error = format(std.error, digits = 3, scientific = FALSE)
    )
  
  return(summary_table)
}

# Helper function to create summary boxes
create_summary_boxes <- function(reg_results) {
  list(
    r_squared = list(
      value = round(reg_results$glance$r.squared, 3),
      title = "R-squared",
      icon = "chart-line",
      color = "blue"
    ),
    adj_r_squared = list(
      value = round(reg_results$glance$adj.r.squared, 3),
      title = "Adjusted R-squared",
      icon = "chart-line",
      color = "blue"
    ),
    f_statistic = list(
      value = round(reg_results$glance$statistic, 2),
      title = "F-statistic",
      icon = "calculator",
      color = "green"
    ),
    p_value = list(
      value = format.pval(reg_results$glance$p.value, digits = 3),
      title = "p-value",
      icon = "check",
      color = if (reg_results$glance$p.value < 0.05) "green" else "yellow"
    )
  )
}

# Helper function to format VIF table
format_vif_table <- function(reg_results) {
  if (is.null(reg_results$vif)) {
    data.frame(Message = "VIF not applicable for single predictor models")
  } else {
    data.frame(Variable = names(reg_results$vif), VIF = reg_results$vif)
  }
}

# Helper function to create interpretation HTML
create_interpretation_html <- function(interpretation) {
  HTML(paste(
    "<h4>Regression Analysis Interpretation</h4>",
    "<p><strong>Model Overview:</strong><br>", interpretation$fit, "</p>",
    "<p><strong>Coefficient Interpretation:</strong><br>", 
    paste(interpretation$coefficients, collapse = "<br>"), "</p>",
    "<p><strong>Assumption Checks:</strong><br>",
    "Normality: ", interpretation$assumptions$normality, "<br>",
    "Homoscedasticity: ", interpretation$assumptions$homoscedasticity, "<br>",
    "Independence: ", interpretation$assumptions$independence, "<br>",
    ifelse(!is.null(interpretation$assumptions$multicollinearity),
           paste("Multicollinearity: ", interpretation$assumptions$multicollinearity), ""),
    "</p>",
    "<p><strong>Cross-validation:</strong><br>", interpretation$cross_validation, "</p>"
  ))
}