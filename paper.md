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

*Lake Nutrient Analyser* is an open-source Shiny application built in R to streamline nutrient analysis for Florida lakes using the Florida Department of Environmental Protection’s (FDEP) Impaired Waters Rule (IWR) dataset. This tool integrates data extraction, interactive visualization, statistical analysis, and TMDL estimation into a single platform. Designed for environmental managers, researchers, and policymakers, it simplifies lake water quality assessments, identifies nutrient trends, and verifies TMDL targets—reducing validation time from weeks to minutes. Its transparent, reproducible workflows make advanced analysis accessible to non-specialists while supporting regulatory compliance.

## Statement of Need

Florida’s 30,000+ lakes face nutrient pollution from agriculture and urban growth, necessitating efficient tools for water quality management. A recent review of the EPA’s TMDL program [@Smith2023] notes challenges like data gaps and complex modeling, underscoring the demand for user-friendly solutions. Existing tools like the Chesapeake Assessment Scenario Tool (CAST) [@ChesapeakeBay2025] focus on watersheds, not lakes, while EPA models such as WASP and AQUATOX [@EPA2025] require significant expertise. The EPA Nutrient Explorer [@Pennino2022] offers visualization but lacks state-specific regulatory integration.

*Lake Nutrient Analyser* fills these gaps by providing: (1) direct access to Florida’s IWR database, (2) interactive geospatial lake comparisons, (3) automated regulatory classification, and (4) rapid TMDL validation. These features empower users to analyze nutrient data and explore management scenarios efficiently.

## Functionality

The app connects to the IWR SQLite database (`IWR66_database.sqlite`) and offers five core modules:

- **Waterbody Explorer**: Retrieves nutrient data (e.g., chlorophyll-a, total nitrogen [TN], total phosphorus [TP]) via an interactive map or WBID search, computing annual geometric means against Numeric Nutrient Criteria.
- **Data Visualization**: Generates exportable, interactive plots (histograms, box plots, time series, etc.) using Plotly.
- **Trend Analysis**: Detects nutrient trends with Mann-Kendall tests, displaying p-values and slopes.
- **Geospatial Analysis**: Visualizes lake comparisons using Leaflet, aiding regional assessments.
- **Regression Analysis**: Models nutrient relationships for TMDL estimation, including diagnostics (e.g., Shapiro-Wilk) and confidence intervals.

Built with R packages like Shiny, dplyr, Kendall, Leaflet, and Plotly, the app supports extensibility and exports results in CSV, Excel, and PDF formats.

## Example: Lake Wales TMDL Verification

We showcase the app’s capabilities by verifying the TMDL for Lake Wales (WBID 1619A), set by FDEP in 2020 [@FDEP2020].

### Lake Classification and Data

Lake Wales is classified as a Type 2 lake (color ≤ 40 PCU, alkalinity > 20 mg/L CaCO₃), with nutrient criteria of 20 μg/L chlorophyll-a, 1.05 mg/L TN, and 0.03 mg/L TP. Using 2005–2016 IWR data, the app analyzes TN and chlorophyll-a relationships.

### Regression Results

The regression module produces:

```
CHLAC = 32.940 * TN - 12.130
```

This relationship is significant (p < 0.0001), with a 95% prediction interval for chlorophyll-a at the target TN of [8.26, 31.74] μg/L.

### TMDL Calculation

To achieve 20 μg/L chlorophyll-a, the app calculates a target TN of 0.975 mg/L, requiring a 40.6% reduction from the current 1.642 mg/L. For TP, a 19.2% reduction is needed from 0.037 mg/L to 0.03 mg/L. Figure 1 illustrates these results with confidence intervals.

![TMDL calculation interface showing current TN (1.642 mg/L), target TN (0.975 mg/L), TN reduction needed (40.6%), current TP (0.037 mg/L), target TP (0.03 mg/L), TP reduction needed (19.2%), regression equation, and confidence intervals.](figures/tmdl_calculation.png)  
*Figure 1: TMDL interface for Lake Wales, showing nutrient targets, reductions, and statistical outputs.*

### Comparison with Official TMDL

The official TMDL specifies a TN target of 0.98 mg/L (40% reduction) and TP target of 0.03 mg/L, closely matching the app’s 0.975 mg/L (40.6%) and 0.03 mg/L (19.2%). Minor TN differences arise from maximum value variations. Completed in minutes, this verification demonstrates the app’s speed and added statistical rigor.

## Availability and Implementation

The app is hosted on GitHub at [https://github.com/zaimoua/LakeNutrientAnalyzer](https://github.com/zaimoua/LakeNutrientAnalyzer) under the MIT License, with documentation and example datasets. The IWR database (Run 66) is available at [https://publicfiles.dep.state.fl.us/dear/IWR/](https://publicfiles.dep.state.fl.us/dear/IWR/). A demo runs at [https://tmdl.shinyapps.io/Florida_Lake_Nutrient_Analyzer/](https://tmdl.shinyapps.io/Florida_Lake_Nutrient_Analyzer/).

### System Requirements

- R version 4.0.0+
- 4GB RAM (8GB recommended)
- ~500MB disk space
- Installation: 5–10 minutes

### Data Preparation

The app uses a pre-processed SQLite IWR database or a script (`install_database.R`) to convert Access files, detailed in the documentation.

## Community Guidelines

Contributions and feedback are encouraged via GitHub. See CONTRIBUTING.md for details. Support is available through the issue tracker.

## Acknowledgements

This work leverages FDEP data without representing its official views. Gratitude to the R community and water quality professionals for their input.

## References

- [@FDEP2020]: Florida Department of Environmental Protection. "TMDL Report: Lake Wales," 2020.
- [@Smith2023]: Smith, J. A review of challenges and opportunities in the EPA's TMDL program. *Environmental Management*, 2023.
- [@ChesapeakeBay2025]: Chesapeake Bay Program. "Chesapeake Assessment Scenario Tool (CAST)," 2025.
- [@EPA2025]: U.S. EPA. "WASP and AQUATOX: Modeling Tools," 2025.
- [@Pennino2022]: Pennino, M. J. "EPA Nutrient Explorer." *Water Resources Research*, 2022.
```

---
