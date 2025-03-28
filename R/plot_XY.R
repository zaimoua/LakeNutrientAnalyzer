library(ggplot2)

create_xy_plot <- function(data, x_var, y_var, param_labels, plot_type = "scatter", point_color = "blue", 
                           title = NULL, subtitle = NULL, add_trend_line = FALSE, trend_line_type = "lm") {
  # Check if the columns exist in the data
  if (!(x_var %in% names(data)) | !(y_var %in% names(data))) {
    stop(paste("Specified columns do not exist in the data. Available columns:", paste(names(data), collapse = ", ")))
  }
  
  # Ensure data types are correct
  data[[x_var]] <- as.numeric(data[[x_var]])
  data[[y_var]] <- as.numeric(data[[y_var]])
  
  # Remove rows with NA values
  data <- data[!is.na(data[[x_var]]) & !is.na(data[[y_var]]), ]
  
  if (nrow(data) == 0) {
    stop("No valid data points after removing NA values")
  }
  
  # Get labels for x and y axes
  x_label <- param_labels[[x_var]] %||% x_var
  y_label <- param_labels[[y_var]] %||% y_var
  
  # Create the plot base
  plot <- ggplot(data, aes_string(x = x_var, y = y_var)) +
    labs(x = x_label, y = y_label) +
    theme_minimal(base_size = 12) +  # Reduced base font size
    theme(
      axis.title.x = element_text(face = "bold", size = 10),  # Reduced axis title size
      axis.title.y = element_text(face = "bold", size = 10),
      axis.text = element_text(size = 8),  # Reduced axis text size
      panel.grid.major = element_line(color = "gray", size = 0.5),
      panel.grid.minor = element_line(color = "gray", size = 0.25),
      legend.position = "none",  # Remove legend
      plot.margin = margin(5, 5, 5, 5)  # Reduce plot margins
    )
  
  # Add title only if provided
  if (!is.null(title)) {
    plot <- plot + ggtitle(title)
  }
  
  # Add plot type
  if (plot_type == "scatter") {
    plot <- plot + geom_point(color = point_color, size = 2)  # Reduced point size
    if (add_trend_line) {
      plot <- plot + geom_smooth(method = trend_line_type, se = FALSE, linetype = "dashed", color = "red", size = 0.5)  # Reduced line size
    }
  } else if (plot_type == "line") {
    plot <- plot + geom_line(color = point_color, size = 0.5) +  # Reduced line size
      geom_point(color = point_color, size = 2)  # Reduced point size
  }
  
  return(plot)
}