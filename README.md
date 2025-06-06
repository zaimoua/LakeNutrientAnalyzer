# Florida Lake Nutrient Analyzer

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)


A comprehensive Shiny app designed to analyze nutrient data, assess impairments, and support Total Maximum Daily Load (TMDL) development for Florida lakes. This tool integrates data extraction, statistical analysis, visualization, and geospatial features to assist water quality managers and researchers.

![Dashboard Screenshot](www/dashboard_screenshot.png)

## Features

- **Data Extraction**: Retrieve water quality data by WBID with automated annual geometric mean calculations
- **Impairment Assessment**: Automatically classify lakes and evaluate against Numeric Nutrient Criteria
- **Visualization**: Generate interactive time series, scatter plots, correlation matrices, and nutrient charts
- **Trend Analysis**: Perform Mann-Kendall tests to identify significant water quality trends
- **Geospatial Analysis**: Select and analyze lakes using interactive Leaflet maps
- **Regression Analysis**: Model nutrient relationships and calculate TMDL targets
- **Export Capabilities**: Download data, analysis results, and visualizations in multiple formats

## Demo

Try the [online demo](https://tmdl.shinyapps.io/LakeNutrientAnalyzer/) to see the app in action.

## Prerequisites

- **R**: Version 4.0.0 or higher recommended
- **R Packages**: Install all required packages by running:
  ```r
  source("install_packages.R")
  ```
- **Internet Connection**: Required for loading external JavaScript libraries (PapaParse, xlsx) via CDN

## Installation

1. **Get the Repository**:
   
   **Option A**: Download ZIP file:
   - Click the green "Code" button
   - Select "Download ZIP"
   - Extract the ZIP file to your desired location

   **Option B**: Clone using Git:
   ```
   git clone https://github.com/zaimoua/LakeNutrientAnalyzer.git
   cd LakeNutrientAnalyzer
   ```

2. **Set Working Directory** in R or RStudio:
   ```
   setwd("path/to/LakeNutrientAnalyzer")
   ```

3. **Install Dependencies**:
   ```
   source("install_packages.R")
   ```

## Database Setup

The app requires the Florida DEP Impaired Waters Rule (IWR) database in SQLite format. The app is designed to work with the latest available IWR run.

### Automated Processing

1. **Download the IWR database files** from the [Florida DEP website](https://publicfiles.dep.state.fl.us/dear/IWR/)
   - Navigate to the most recent IWR run folder (e.g., IWR66, IWR67, etc.)
   - Download the Access database files (.mdb or .accdb format)

2. **Run the database conversion script:**
   ```r
   source("install_database.R")
   ```

3. **Follow the conversion process:**
   - The script will prompt you to select the downloaded Access files
   - It will automatically convert them to SQLite format
   - The resulting database will be named according to the IWR run (e.g., `IWR66_database.sqlite`)

4. **Ensure required files are in place:**
   - Verify the `WaterbodyID_Table.csv` file is in the `data/` folder
   - This file is used for waterbody filtering and identification

### Database Updates

As Florida DEP releases new IWR runs, simply:
1. Download the latest IWR database files
2. Re-run the `install_database.R` script
3. Update any hardcoded references to the database filename in your local configuration

## Running the App

Launch the app locally by running the `app.R` script:
```r
# In R or RStudio
source("app.R")
```

## Data Requirements

The app relies on specific data files in `data/` and `maps/` folders:

### Included Files
- `data/WaterbodyID_Table.csv`
- `data/Lake_class.csv`
- `data/lake_wbid_region.csv`
- `maps/lake_wbid/lake_wbid.geojson`
- `maps/lake_region/lake_region.geojson`
- `maps/lake_flowline/NHD_flowline_lakes.geojson`

### External File
- `data/IWR66_database.sqlite` (see Database Setup)

## Project Structure

- `ui.R`: Defines the user interface using shinydashboard
- `server.R`: Contains the server logic for data processing and analysis
- `app.R`: Initializes and runs the application
- `install_database.R`: Utility for converting IWR Access databases to SQLite format
- `R/`: Contains helper scripts:
  - `labels.R`: Parameter labels and descriptions
  - `data_extraction.R`: Functions for retrieving data from database
  - `determine_lake_type.R`: Lake classification logic
  - `calculate_exceedances.R`: Numeric nutrient criteria assessment
  - `plot_*.R`: Visualization functions
  - And other utility scripts
- `data/`: CSV files and SQLite database
- `maps/`: GeoJSON files for spatial visualization
- `install_packages.R`: Script to install required R packages
- `.gitignore`: Excludes unnecessary files from version control

## Usage Guide

1. **Select Lake(s)**:
   - Use the interactive map or WBID search in the "Waterbody Explorer" tab
   - Click on waterbodies or search by WBID/name

2. **Extract Data**:
   - Select parameters of interest
   - Choose data period
   - Click "Run Extraction"

3. **Analyze Data**:
   - View lake information and assessment results
   - Explore data through visualizations
   - Perform trend analysis
   - Conduct regression modeling
   - Calculate TMDL targets

4. **Export Results**:
   - Download data tables
   - Export visualizations
   - Generate analysis reports

## Dependencies

Installed via `install_packages.R`:

### Core Shiny
- shiny, shinydashboard, shinyFiles, shinyjs, shinycssloaders

### Visualization
- plotly, ggplot2, leaflet, leaflet.extras, corrplot

### Data Handling
- RSQLite, data.table, tidyverse, dplyr, tidyr, readr, openxlsx, DBI, sf, geojsonio, stringr

### Statistical Analysis
- zoo, Kendall, broom, lmtest, car, nortest, scales

### Utilities
- DT, markdown, waiter, zip, doParallel, logging, futile.logger, rmapshaper, promises, future

## Troubleshooting

- **Missing Data**: Verify all files are in place, especially the SQLite database
- **Package Errors**: Run `install_packages.R` and check the console for any errors
- **Path Errors**: Confirm your working directory is set to the project root
- **Visualization Issues**: Ensure you have an internet connection for external libraries
- **Memory Errors**: Close other applications to free up RAM or increase R's memory allocation

## Contributing

Contributions are welcome via GitHub issues or pull requests, as detailed in [CONTRIBUTING.md](https://github.com/zaimoua/LakeNutrientAnalyzer/blob/main/CONTRIBUTING.md). Users can contact the author for support or feedback.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Disclaimer

This app was developed by Zaim Ouazzani as a personal project, independent of any affiliation with the Florida Department of Environmental Protection (FDEP). It is not an official FDEP product, nor does it represent the agency’s views, tools, or endorsement. The app uses publicly available Impaired Waters Rule (IWR) data from the Florida DEP, in compliance with the agency’s data usage policy. For official FDEP tools or data, contact the agency at https://floridadep.gov.

## Acknowledgments

- Data from Florida DEP IWR database
- Built with Shiny and R community packages

---

Version: 1.0.0 (Last Updated: May 27, 2025)
