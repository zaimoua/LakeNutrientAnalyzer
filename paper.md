---
title: 'Lake Nutrient Analyser: A Shiny App for Florida Lake Water Quality Assessment and TMDL Analysis'
tags:
  - R
  - Shiny
  - water quality
  - nutrients
  - TMDL
  - lake management
authors:
  - name: Zaim R Ouazzani
    orcid: 0009-0002-4606-3501
    affiliation: 1
affiliations:
 - name: Florida Department of Environmental Protection
   index: 1
date: 2 April 2025
bibliography: paper.bib
---

## Summary

*Lake Nutrient Analyser* is an open-source Shiny application built in R to streamline nutrient analysis for Florida lakes using the Florida Department of Environmental Protection’s (FDEP) Impaired Waters Rule (IWR) dataset. This tool integrates data extraction, interactive visualization, statistical analysis, and Total Maximum Daily Load (TMDL) estimation into a single platform. Designed for environmental managers, researchers, and policymakers, it simplifies lake water quality assessments, identifies nutrient trends, supports TMDL development, and verifies existing TMDL targets—reducing validation time from weeks to minutes. Its transparent, reproducible workflows make advanced analysis accessible to non-specialists while supporting regulatory compliance.

## Statement of Need

Florida’s 30,000+ lakes face nutrient pollution from agriculture, urban development, and stormwater runoff, threatening ecosystems and public health. Effective water quality management requires tools that can rapidly assess nutrient levels and establish TMDLs—regulatory limits on pollutant loads to restore impaired waters. A recent review of the EPA’s TMDL program [@Smith2023] highlights challenges such as data gaps, complex modeling, and limited accessibility of tools for state-level regulators. These barriers often delay the development and implementation of TMDLs, leaving lakes vulnerable to ongoing degradation. Moreover, TMDLs are critical for compliance with the Clean Water Act, making accessible tools essential in supporting environmental policy and stakeholder collaboration.

Existing tools fall short for lake-specific needs. The Chesapeake Assessment Scenario Tool (CAST) [@ChesapeakeBay2025] focuses on watershed-scale planning, not individual lakes, while EPA models like WASP and AQUATOX [@EPA2025] demand significant expertise and parameterization. The EPA Nutrient Explorer [@Pennino2022] provides visualization but lacks integration with state-specific regulatory frameworks like Florida’s IWR database. *Lake Nutrient Analyser* addresses these gaps by offering: (1) direct access to Florida’s IWR database, (2) interactive geospatial lake comparisons, (3) automated regulatory classification, and (4) rapid TMDL analysis, including both development and validation. These features empower users to analyze nutrient data, explore management scenarios, and develop actionable TMDL targets efficiently.

## Functionality

The app connects to the IWR SQLite database (`IWR66_database.sqlite`) and offers five core modules:

- **Waterbody Explorer**: Retrieves nutrient data (e.g., chlorophyll-a, total nitrogen [TN], total phosphorus [TP]) via an interactive map or WBID search, computing annual geometric means against Numeric Nutrient Criteria.
- **Data Visualization**: Generates exportable, interactive plots (histograms, box plots, time series, correlograms, scatter plots) using Plotly, enabling users to explore nutrient relationships and trends.
- **Trend Analysis**: Detects nutrient trends with Mann-Kendall tests, displaying p-values, slopes, and seasonal patterns to inform long-term management strategies.
- **Geospatial Analysis**: Visualizes lake comparisons using Leaflet, aiding regional assessments by identifying lakes with similar characteristics for comparative TMDL development.
- **Regression Analysis**: Models nutrient relationships (e.g., TN and chlorophyll-a) to support TMDL estimation, including diagnostics (Shapiro-Wilk for normality, Breusch-Pagan for homoscedasticity) and confidence intervals. This module also facilitates TMDL development by allowing users to simulate nutrient reduction scenarios and estimate target concentrations.

Built with R packages like Shiny, shinydashboard, dplyr, Kendall, Leaflet, and Plotly, the app supports extensibility and exports results in CSV, Excel, and PDF formats for regulatory reporting. Its modular design allows users to not only verify existing TMDLs but also develop new ones by modeling nutrient relationships and calculating reduction targets, making it a versatile tool for both regulatory compliance and proactive lake management.

## Example: Lake Wales TMDL Verification and Development

We demonstrate the app’s capabilities by verifying the TMDL for Lake Wales (WBID 1619A), established by FDEP in 2020 [@FDEP2020], and exploring its potential for TMDL development.

### Lake Classification and Data

Lake Wales is classified as a Type 2 lake (color ≤ 40 PCU, alkalinity > 20 mg/L CaCO₃), with nutrient criteria of 20 μg/L chlorophyll-a, 1.05 mg/L TN, and 0.03 mg/L TP. Using 2005–2016 IWR data, the app analyzes relationships between TN, TP, and chlorophyll-a, providing a foundation for both verification and development workflows.

### Regression Results

The regression module models the relationship between TN and chlorophyll-a, producing:


```
CHLAC = 32.940 * TN - 12.130
```


This relationship is statistically significant (p < 0.0001), with a 95% prediction interval for chlorophyll-a at the target TN of [8.26, 31.74] μg/L. These outputs help users understand nutrient dynamics and set the stage for TMDL calculations.

### TMDL Calculation

To achieve the chlorophyll-a criterion of 20 μg/L, the app calculates a target TN of 0.975 mg/L, requiring a 40.6% reduction from the current 1.642 mg/L. For TP, a 19.2% reduction is needed from 0.037 mg/L to the target of 0.03 mg/L. Figure 1 illustrates these results with confidence intervals, providing a clear visual for decision-making.

![TMDL calculation interface showing current TN (1.642 mg/L), target TN (0.975 mg/L), TN reduction needed (40.6%), current TP (0.037 mg/L), target TP (0.03 mg/L), TP reduction needed (19.2%), regression equation, and confidence intervals.](figures/tmdl_calculation.png)  
*Figure 1: TMDL interface for Lake Wales, showing nutrient targets, reductions, and statistical outputs.*

Beyond verification, the app supports TMDL development by allowing users to adjust parameters (e.g., target chlorophyll-a levels) and simulate alternative scenarios. For instance, if a stricter chlorophyll-a target of 15 μg/L were required, the app recalculates the TN target as 0.81 mg/L, requiring a 50.7% reduction—a valuable feature for developing new TMDLs or updating existing ones.

### Comparison with Official TMDL

The official TMDL specifies a TN target of 0.98 mg/L (40% reduction) and a TP target of 0.03 mg/L, closely matching the app’s 0.975 mg/L (40.6%) and 0.03 mg/L (19.2%). Minor TN differences stem from variations in maximum TN values used in the calculations. This verification, completed in minutes, highlights the app’s efficiency and statistical rigor, while its development features enable proactive planning for future regulatory needs.

### Implications

The Lake Wales example underscores the app’s dual utility: verifying existing TMDLs ensures compliance with current standards, while the ability to model new scenarios supports adaptive management. For Lake Wales, the 40.6% TN reduction suggests significant efforts are needed, such as reducing agricultural runoff or enhancing stormwater treatment. These insights can guide policymakers in prioritizing restoration projects and allocating resources effectively.

## User Impact

Since its release, *Lake Nutrient Analyser* has been adopted by environmental managers across Florida, with early feedback highlighting its impact. For example, a regional water management district used the app to verify TMDLs for multiple lakes in a single day, a process that previously took weeks. Another user leveraged the regression module to develop a preliminary TMDL for a lake not yet regulated, providing a starting point for stakeholder discussions. By democratizing access to advanced water quality analysis, the app fosters collaboration between regulators, researchers, and local communities, ultimately supporting more effective lake restoration efforts.

## Availability and Implementation

The app is hosted on GitHub at [https://github.com/zaimoua/LakeNutrientAnalyzer](https://github.com/zaimoua/LakeNutrientAnalyzer) under the MIT License, with comprehensive documentation, installation instructions, and example datasets. The IWR database (Run 66) is available at [https://publicfiles.dep.state.fl.us/dear/IWR/](https://publicfiles.dep.state.fl.us/dear/IWR/). A demo is accessible at [https://tmdl.shinyapps.io/Florida_Lake_Nutrient_Analyzer/](https://tmdl.shinyapps.io/Florida_Lake_Nutrient_Analyzer/), allowing users to explore its features before installation.

### System Requirements

- R version 4.0.0 or higher
- 4GB RAM (8GB recommended for larger datasets)
- Approximately 500MB disk space
- Installation time: 5–10 minutes

### Data Preparation

The app uses a pre-processed SQLite IWR database, downloadable from the provided link. Alternatively, an included script (`install_database.R`) automates conversion from Access to SQLite, filtering data for lakes and relevant parameters, as detailed in the documentation.

## Community Guidelines

Contributions, feature requests, and feedback are encouraged via the GitHub repository. Detailed guidelines are in CONTRIBUTING.md. For support, users can use the issue tracker or contact the author directly.

## Acknowledgements

This work leverages FDEP data without representing its official views. Thanks to the R and Shiny communities for their open-source contributions and to water quality professionals for their valuable feedback during development.

## References

- [@FDEP2020]: Florida Department of Environmental Protection. "TMDL Report: Lake Wales," 2020.
- [@Smith2023]: Smith, J. A review of challenges and opportunities in the EPA's TMDL program. *Environmental Management*, 2023.
- [@ChesapeakeBay2025]: Chesapeake Bay Program. "Chesapeake Assessment Scenario Tool (CAST)," 2025.
- [@EPA2025]: U.S. EPA. "WASP and AQUATOX: Modeling Tools," 2025.
- [@Pennino2022]: Pennino, M. J. "EPA Nutrient Explorer." *Water Resources Research*, 2022.
