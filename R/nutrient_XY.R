library(ggplot2)
library(ggpubr)

create_nutrient_xy_plot <- function(data, x_var, y_var, param_labels, data_type = "raw", point_color = "black", add_trend_line = FALSE) {
  # Check if the columns exist in the data
  if (!(x_var %in% names(data)) || !(y_var %in% names(data))) {
    stop(paste("Specified columns do not exist in the data. Available columns:", paste(names(data), collapse = ", ")))
  }
  
  # Ensure data types are correct
  data[[x_var]] <- as.numeric(as.character(data[[x_var]]))
  data[[y_var]] <- as.numeric(as.character(data[[y_var]]))
  
  # Remove rows with NA values
  data <- data[complete.cases(data[c(x_var, y_var)]), ]
  
  if (nrow(data) == 0) {
    stop("No valid data points after removing NA values")
  }
  
  # Get labels for x and y axes
  x_label <- param_labels[[x_var]] %||% x_var
  y_label <- param_labels[[y_var]] %||% y_var
  
  # Create the plot base
  plot <- ggplot(data, aes_string(x = x_var, y = y_var)) +
    labs(x = x_label, y = y_label) +
    theme_minimal(base_size = 15) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      axis.title.x = element_text(face = "bold", size = 14),
      axis.title.y = element_text(face = "bold", size = 14),
      axis.text = element_text(size = 12),
      panel.grid.major = element_line(color = "gray", size = 0.5),
      panel.grid.minor = element_line(color = "gray", size = 0.25),
      legend.position = "bottom",
      legend.title = element_blank()
    )
  
  # Add points (smaller size for raw data)
  point_size <- if(data_type == "raw") 1.5 else 3
  plot <- plot + geom_point(color = point_color, size = point_size, alpha = 0.7)
  
  # Add simple trend line if requested
  if (add_trend_line) {
    plot <- plot + 
      geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed")
  }
  
  # Add plot title
  plot <- plot + ggtitle(paste(y_label, "vs", x_label))
  
  return(plot)
}