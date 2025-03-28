# Install required packages for Florida Lake Nutrient Analyser
packages <- c(
  "shiny", "shinydashboard", "shinyFiles", "shinyjs", "plotly", "shinycssloaders",
  "DT", "markdown", "leaflet", "waiter", "openxlsx", "RSQLite", "data.table",
  "ggplot2", "tidyverse", "zoo", "Kendall", "broom", "doParallel", "logging",
  "sf", "DBI", "readr", "zip", "nortest", "leaflet.extras", "dplyr", "lmtest",
  "car", "tidyr", "scales", "futile.logger", "stringr", "rmapshaper", "promises",
  "future", "corrplot", "geojsonio"
)

# Install packages if not already installed
installed <- rownames(installed.packages())
for (pkg in packages) {
  if (!pkg %in% installed) {
    install.packages(pkg)
  }
}

# Verify installation
cat("Checking installed packages:\n")
for (pkg in packages) {
  if (require(pkg, character.only = TRUE)) {
    cat(sprintf("%s is installed and loaded.\n", pkg))
  } else {
    cat(sprintf("Warning: %s failed to load.\n", pkg))
  }
}