---
title: 'Florida Lake Nutrient Analyzer: A Shiny App for Water Quality Assessment and TMDL Development'
tags:
  - R
  - Shiny
  - water quality
  - nutrient analysis
  - geospatial analysis
  - TMDL
authors:
  - name: Zaim Ouazzani
    orcid: 0009-0002-4606-3501
    affiliation: 1
affiliations:
  - name: Independent Researcher
    index: 1
date: 04 June 2025
bibliography: paper.bib
---

# Summary

The *Florida Lake Nutrient Analyzer* is an open-source Shiny application built in R to streamline nutrient analysis, impairment assessment, and Total Maximum Daily Load (TMDL) development for Florida lakes using the Florida Department of Environmental Protection’s (FDEP) publicly available Impaired Waters Rule (IWR) dataset. Designed for water quality managers, environmental researchers, and policymakers, it integrates data extraction, interactive visualization, statistical analysis, and geospatial tools into a user-friendly platform. The app automates tasks such as retrieving water quality data by Waterbody Identification (WBID), computing annual geometric means, evaluating compliance with Florida’s Numeric Nutrient Criteria, and modeling nutrient relationships for TMDL targets, reducing analysis time from weeks to minutes. Its transparent, reproducible workflows enhance accessibility for non-specialists while supporting regulatory compliance.

# Statement of Need

Nutrient pollution in Florida’s 30,000+ lakes, driven by agriculture, urban development, and stormwater runoff, threatens ecosystems and public health. Effective management requires tools to rapidly assess nutrient levels and develop TMDLs, regulatory limits on pollutant loads to restore impaired waters [@Smith2023]. However, challenges like data complexity, fragmented workflows, and limited tool accessibility often delay TMDL implementation [@FDEP2020]. Existing tools, such as the Chesapeake Assessment Scenario Tool (CAST) [@ChesapeakeBay2025], focus on watershed-scale modeling, while EPA’s Water Quality Analysis Simulation Program (WASP) [@EPA2025] requires significant expertise. The EPA Nutrient Explorer [@Pennino2022] offers visualization but lacks integration with Florida’s IWR database and regulatory frameworks. 

The *Florida Lake Nutrient Analyzer* addresses these gaps by providing:
1. Direct access to Florida’s IWR database for lake-specific nutrient data.
2. Automated classification and impairment analysis aligned with Florida’s Numeric Nutrient Criteria.
3. Interactive geospatial visualization for regional lake comparisons.
4. Rapid TMDL development and verification through regression modeling and scenario analysis.

This app empowers users to efficiently analyze nutrient trends, verify existing TMDLs, and develop new ones, fostering data-driven lake management and regulatory compliance.

# Functionality

The app connects to the IWR Run 66 SQLite database and offers five core modules:
- **Waterbody Explorer**: Retrieves nutrient data (e.g., chlorophyll-a, total nitrogen [TN], total phosphorus [TP]) via WBID search or interactive Leaflet maps, computing annual geometric means for Numeric Nutrient Criteria assessment.
- **Data Visualization**: Generates exportable plots (time series, scatter plots, correlograms) using `plotly` and `ggplot2` to explore nutrient trends and relationships.
- **Trend Analysis**: Applies Mann-Kendall tests (`Kendall` package) to detect significant nutrient trends, providing p-values and slopes for long-term insights.
- **Geospatial Analysis**: Visualizes lake comparisons using `leaflet`, enabling regional assessments for TMDL planning.
- **Regression Analysis**: Models nutrient relationships (e.g., TN vs. chlorophyll-a) with diagnostics (`broom`, `lmtest`) and calculates TMDL targets, supporting scenario analysis for nutrient reductions.

Built with R packages including `shiny`, `shinydashboard`, `dplyr`, `RSQLite`, `leaflet`, and `plotly`, the app supports extensibility and exports results in CSV, Excel, PNG, PDF, or HTML formats for regulatory reporting.

# Example: Lake Wales TMDL Analysis

To demonstrate, we analyze Lake Wales (WBID 1619A), with an established TMDL [@FDEP2020]. Classified as a Type 2 lake (color ≤ 40 PCU, alkalinity > 20 mg/L CaCO₃), it has nutrient criteria of 20 μg/L chlorophyll-a, 1.05 mg/L TN, and 0.03 mg/L TP. Using IWR data (2005–2016), the app’s regression module models TN vs. chlorophyll-a, yielding:


```
CHLAC = 32.940 * TN - 12.130
```

This model (p < 0.0001) predicts chlorophyll-a at the target TN (0.975 mg/L) within [8.26, 31.74] μg/L (95% prediction interval). To meet the 20 μg/L chlorophyll-a criterion, the app calculates a 40.6% TN reduction (from 1.642 mg/L to 0.975 mg/L) and a 19.2% TP reduction (from 0.037 mg/L to 0.03 mg/L). These align closely with FDEP’s TMDL (0.98 mg/L TN, 40% reduction; 0.03 mg/L TP) [@FDEP2020], validating the app’s accuracy. The app also supports TMDL development by simulating stricter scenarios (e.g., 15 μg/L chlorophyll-a requires a 50.7% TN reduction to 0.81 mg/L).

*Figure 1: Regression results for Lake Wales, showing the relationship between total nitrogen (TN) and chlorophyll-a with the fitted model and prediction intervals.* 
![Dashboard Screenshot](figures/regression_plot.png)

This example highlights the app’s efficiency in verifying TMDLs and its flexibility for developing new targets, aiding adaptive lake management.

# User Impact

The *Florida Lake Nutrient Analyser* enables rapid, data-driven decisions. For example, water managers can verify TMDLs for multiple lakes in hours, compared to weeks with traditional methods. Researchers can model nutrient scenarios to inform restoration strategies, such as reducing agricultural runoff. By providing open-source access to advanced analysis, the app supports collaboration among regulators, scientists, and communities, enhancing lake restoration efforts.

# Availability and Implementation

The app is hosted on GitHub at [https://github.com/zaimoua/LakeNutrientAnalyzer](https://github.com/zaimoua/LakeNutrientAnalyzer) under the MIT License. A demo is available at [https://tmdl.shinyapps.io/LakeNutrientAnalyzer/](https://tmdl.shinyapps.io/LakeNutrientAnalyzer/). The IWR Run 66 database is downloadable from [https://publicfiles.dep.state.fl.us/dear/IWR/](https://publicfiles.dep.state.fl.us/dear/IWR/). Installation instructions, data preparation scripts (`install_database.R`), and troubleshooting guides are in the README.

**System Requirements**:
- R version 4.0.0 or higher
- 4GB RAM (8GB recommended)
- ~500MB disk space
- Installation time: 5–10 minutes

# Disclaimer

This app was developed by Zaim Ouazzani as a personal project, independent of any affiliation with the Florida Department of Environmental Protection (FDEP). It is not an official FDEP product, nor does it represent the agency’s views, tools, or endorsement. The app uses publicly available Impaired Waters Rule (IWR) data from the FDEP. For official FDEP tools or data, contact the agency at [https://floridadep.gov](https://floridadep.gov).

# Community Guidelines

Contributions are welcome via GitHub issues or pull requests, as detailed in [CONTRIBUTING.md](https://github.com/zaimoua/LakeNutrientAnalyzer/blob/main/CONTRIBUTING.md). Users can contact the author for support or feedback.

# Acknowledgements

Thanks to the FDEP for providing public IWR data and to the R and Shiny communities for their open-source packages. This work was conducted independently as a personal project.

# References

- [@FDEP2020]: Florida Department of Environmental Protection. "TMDL Report: Lake Wales," 2020.
- [@Smith2023]: Smith, J. A review of challenges and opportunities in the EPA's TMDL program. *Environmental Management*, 2023.
- [@ChesapeakeBay2025]: Chesapeake Bay Program. "Chesapeake Assessment Scenario Tool (CAST)," 2025.
- [@EPA2025]: U.S. EPA. "WASP and AQUATOX: Modeling Tools," 2025.
- [@Pennino2022]: Pennino, M. J. "EPA Nutrient Explorer." *Water Resources Research*, 2022.
