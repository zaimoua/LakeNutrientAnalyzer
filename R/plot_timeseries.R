library(ggplot2)
library(plotly)
library(scales)

create_timeseries_plot <- function(data, time_var, y_var, param_labels,
                                   title = "Time Series Plot", subtitle = NULL,
                                   point_size = 1, line_size = 0.5,
                                   param_name = "") {
  if (!(time_var %in% names(data)) || !(y_var %in% names(data))) {
    stop(paste("Required variables are not present in the data."))
  }
  
  y_label <- ifelse(param_name != "", 
                    paste(param_labels[[param_name]] %||% param_name, paste0("(", param_name, ")")),
                    param_labels[[y_var]] %||% y_var)
  
  plot <- ggplot(data, aes_string(x = time_var, y = y_var)) +
    geom_line(size = line_size, color = "#1f77b4") + 
    geom_point(size = point_size, color = "#1f77b4", alpha = 0.7) +
    labs(title = title, subtitle = subtitle, y = y_label) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 10),
      axis.title = element_text(face = "bold", size = 12),
      axis.text = element_text(size = 10),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major = element_line(color = "gray90", size = 0.3),
      panel.grid.minor = element_line(color = "gray95", size = 0.15),
      legend.position = "bottom",
      legend.title = element_blank()
    )
  
  # Adjust x-axis based on whether we're using dates or years
  if (inherits(data[[time_var]], "Date")) {
    date_range <- range(data[[time_var]], na.rm = TRUE)
    years_diff <- as.numeric(difftime(date_range[2], date_range[1], units = "days")) / 365
    
    if (years_diff > 10) {
      date_breaks <- "2 years"
      date_labels <- "%Y"
    } else if (years_diff > 5) {
      date_breaks <- "1 year"
      date_labels <- "%Y"
    } else if (years_diff > 2) {
      date_breaks <- "6 months"
      date_labels <- "%b %Y"
    } else {
      date_breaks <- "3 months"
      date_labels <- "%b %Y"
    }
    
    plot <- plot + 
      scale_x_date(date_breaks = date_breaks, date_labels = date_labels) +
      labs(x = "Date")
  } else {
    plot <- plot + 
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      labs(x = "Year")
  }
  
  # Improve y-axis formatting
  plot <- plot +
    scale_y_continuous(labels = scales::label_number(accuracy = 0.01))
  
  # Convert to plotly for interactivity
  plotly_plot <- ggplotly(plot, tooltip = c("x", "y")) %>%
    layout(hovermode = "closest") %>%
    config(displayModeBar = FALSE)
  
  return(plotly_plot)
}