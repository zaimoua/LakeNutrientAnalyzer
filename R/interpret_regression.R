interpret_regression_results <- function(reg_results, shapiro_results) {
  if (is.null(reg_results)) {
    return(list(
      modelOverview = "No regression results available. Please run the regression analysis first.",
      modelPerformance = NULL,
      coefficientInterpretation = NULL,
      normalityCheck = NULL,
      homoscedasticityCheck = NULL,
      multicollinearityCheck = NULL,
      autocorrelationCheck = NULL,
      conclusionInterpretation = NULL
    ))
  }
  
  # Extract relevant information
  r_squared <- reg_results$glance$r.squared
  adj_r_squared <- reg_results$glance$adj.r.squared
  f_statistic <- reg_results$glance$statistic
  p_value <- reg_results$glance$p.value
  coefficients <- reg_results$summary
  
  # Model Overview
  modelOverview <- HTML(paste0(
    "<p>The regression model uses ", length(coefficients$term) - 1, " predictor variable(s) to explain the variation in the response variable.</p>",
    "<p>The model's F-statistic is ", round(f_statistic, 2), 
    " with a p-value of ", format.pval(p_value, digits = 3), ".</p>"
  ))
  
  # Model Performance
  modelPerformance <- HTML(paste0(
    "<p>R-squared: ", round(r_squared, 3), 
    "<br>Adjusted R-squared: ", round(adj_r_squared, 3), "</p>",
    "<p>The model explains approximately ", round(r_squared * 100, 1), 
    "% of the variance in the response variable.</p>",
    "<p>The model is ", ifelse(p_value < 0.05, "statistically significant", "not statistically significant"), 
    " at the 0.05 level.</p>"
  ))
  
  # Coefficient Interpretation
  coefficientInterpretation <- lapply(1:nrow(coefficients), function(i) {
    coef <- coefficients[i, ]
    if (coef$term != "(Intercept)") {
      HTML(paste0(
        "<h4>", coef$term, "</h4>",
        "<p>Coefficient: ", round(coef$estimate, 3),
        "<br>p-value: ", format.pval(coef$p.value, digits = 3),
        "<br>Interpretation: ", 
        ifelse(coef$p.value < 0.05,
               paste0("For each one-unit increase in ", coef$term, ", we expect the response variable to change by ", 
                      round(coef$estimate, 3), " units, holding other variables constant."),
               paste0(coef$term, " does not have a statistically significant effect on the response variable at the 0.05 level.")
        ),
        "</p>"
      ))
    }
  })
  
  # Normality Check
  normalityCheck <- if (!is.null(reg_results$assumption_checks$normality)) {
    normality_test <- reg_results$assumption_checks$normality
    HTML(paste0(
      "<p>Shapiro-Wilk test for normality of residuals:</p>",
      "<ul>",
      "<li>W-statistic: ", round(normality_test$statistic, 3), "</li>",
      "<li>p-value: ", format.pval(normality_test$p.value, digits = 3), "</li>",
      "<li>Interpretation: ", 
      ifelse(normality_test$p.value > 0.05,
             "The residuals appear to be normally distributed (p > 0.05).",
             "There is evidence that the residuals are not normally distributed (p <= 0.05). Consider transforming variables or using robust regression techniques."),
      "</li>",
      "</ul>"
    ))
  } else {
    "Normality test results are not available."
  }
  
  # Homoscedasticity Check
  homoscedasticityCheck <- if (!is.null(reg_results$assumption_checks$homoscedasticity)) {
    bp_test <- reg_results$assumption_checks$homoscedasticity
    HTML(paste0(
      "<p>Breusch-Pagan test for homoscedasticity:</p>",
      "<ul>",
      "<li>BP-statistic: ", round(bp_test$statistic, 3), "</li>",
      "<li>p-value: ", format.pval(bp_test$p.value, digits = 3), "</li>",
      "<li>Interpretation: ", 
      ifelse(bp_test$p.value > 0.05,
             "There is no significant evidence of heteroscedasticity (p > 0.05).",
             "There is evidence of heteroscedasticity (p <= 0.05). Consider using weighted least squares or robust standard errors."),
      "</li>",
      "</ul>"
    ))
  } else {
    "Homoscedasticity test results are not available."
  }
  
  # Multicollinearity Check
  multicollinearityCheck <- if (is.character(reg_results$assumption_checks$multicollinearity)) {
    reg_results$assumption_checks$multicollinearity
  } else if (!is.null(reg_results$assumption_checks$multicollinearity)) {
    vif_values <- reg_results$assumption_checks$multicollinearity
    HTML(paste0(
      "<p>Variance Inflation Factors (VIF):</p>",
      "<ul>",
      paste0("<li>", names(vif_values), ": ", round(vif_values, 2), "</li>", collapse = ""),
      "</ul>",
      "<p>Interpretation: ", 
      ifelse(any(vif_values > 5),
             "Some variables have a VIF greater than 5, indicating potential multicollinearity issues.",
             "All VIF values are below 5, suggesting that multicollinearity is not a major concern."),
      "</p>"
    ))
  } else {
    "Multicollinearity check is not applicable for this model."
  }
  
  # Autocorrelation Check
  autocorrelationCheck <- if (!is.null(reg_results$assumption_checks$autocorrelation)) {
    dw_test <- reg_results$assumption_checks$autocorrelation
    HTML(paste0(
      "<p>Durbin-Watson test for autocorrelation:</p>",
      "<ul>",
      "<li>DW-statistic: ", round(dw_test$statistic, 3), "</li>",
      "<li>p-value: ", format.pval(dw_test$p.value, digits = 3), "</li>",
      "<li>Interpretation: ", 
      ifelse(dw_test$p.value > 0.05,
             "There is no significant evidence of autocorrelation in the residuals (p > 0.05).",
             "There is evidence of autocorrelation in the residuals (p <= 0.05). Consider using time series models or adding lagged variables."),
      "</li>",
      "</ul>"
    ))
  } else {
    "Autocorrelation test results are not available."
  }
  
  # Conclusion
  conclusionInterpretation <- HTML(paste0(
    "<p>Based on the analysis above:</p>",
    "<ul>",
    "<li>The model ", ifelse(p_value < 0.05, "is", "is not"), " statistically significant.</li>",
    "<li>It explains ", round(r_squared * 100, 1), "% of the variance in the response variable.</li>",
    ifelse(p_value < 0.05 && r_squared > 0.3,
           "<li>The model appears to be a good fit for the data.</li>",
           "<li>The model might be improved by including additional relevant predictors or considering non-linear relationships.</li>"
    ),
    "</ul>",
    "<p>Remember to consider the practical significance of these results in addition to statistical significance, 
    and be cautious about making causal inferences from observational data.</p>"
  ))
  
  list(
    modelOverview = HTML(modelOverview),
    modelPerformance = HTML(modelPerformance),
    coefficientInterpretation = lapply(coefficientInterpretation, HTML),
    normalityCheck = normalityCheck,
    homoscedasticityCheck = homoscedasticityCheck,
    multicollinearityCheck = multicollinearityCheck,
    autocorrelationCheck = autocorrelationCheck,
    conclusionInterpretation = HTML(conclusionInterpretation)
  )
}