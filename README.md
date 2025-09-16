
# Yelp Case Study

This repository contains R scripts, Shiny apps, and processed data used to explore restaurant availability and customer satisfaction across ten U.S. metro areas.  
The analysis combines Yelp business data with U.S. Census population data to examine:

- **Residents per Quick Service Restaurant (QSR)**
- **Customer Satisfaction** (average Yelp star ratings)
- **Food type distributions** across metros

## Project Overview

The project is delivered through three Shiny apps, two figures and Tableau-ready datasets:

- **Metro Map App**: Interactive mapping of residents per QSR and satisfaction by ZIP code.  
- **Food Charts App**: Charts showing customer satisfaction and QSR counts by food type.  
- **Final Correlation App**: Visualizes correlations between satisfaction and residents per QSR.  

## Repository Structure
```
Yelp_Case_Study/
│
├── data/
│ ├── raw/ # Raw inputs (ignored in Git)
│ │ ├── boise_zip.xlsx
│ │ ├── food_type_by_metro.xlsx
│ │ ├── ideal_plot_data.xlsx
│ │ ├── indianapolis_zip.xlsx
│ │ ├── nashville_zip.xlsx
│ │ ├── new_orleans_zip.xlsx
│ │ ├── philadelphia_zip.xlsx
│ │ ├── reno_zip.xlsx
│ │ ├── santa_barbara_zip.xlsx
│ │ ├── st_louis_zip.xlsx
│ │ ├── tampa_zip.xlsx
│ │ ├── tucson_zip.xlsx
│ │ └── Sheet_9_data.xlsx
│ │
│ └── processed/ # Clean/processed data
│ ├── metro_areas_for_map.xlsx
│ └── viz_data.xlsx
│
├── final_correlation_app/ # Shiny app for correlation analysis
│ ├── app.R
│ └── data2.xlsx
│
├── food_charts_app/ # Shiny app for food-type analysis
│ ├── app.R
│ └── viz_data.xlsx
│
├── metro_map_app/ # Shiny app for metro-level mapping
│ ├── app.R
│ ├── map_data_prepped.xlsx
│ └── zctas20.rds (ignored in Git)
│
├── outputs/ # Reserved for exports (ignored in Git)
│
├── Data_Clean_and_Prep_Google_Analytics_...R # Main data-prep script
├── map_data_prepped.xlsx # Copy of processed map data
├── .gitignore
├── .Rhistory
├── .RData
├── Yelp_Case_Study.Rproj # RStudio project file
└── README.md
```

## Data

- **Excluded from Git**:  
  - `data/raw/` (raw inputs, per-metro ZIP files, etc.)  
  - `outputs/` (temporary outputs/exports)  
  - `metro_map_app/zctas20.rds` (large shapefile)  
  - `archive (17)/` and other duplicates  

- **Included**:  
  - Minimal processed datasets under `/data/processed`  
  - App-specific inputs (`data2.xlsx`, `viz_data.xlsx`, `map_data_prepped.xlsx`)  

These inclusions ensure the Shiny apps run without requiring the raw data.

## How to Run

Clone this repository and open the R project:

```r
# In RStudio, set working directory to project root
shiny::runApp("final_correlation_app")
shiny::runApp("food_charts_app")
shiny::runApp("metro_map_app")
```
Each app will open interactively in your browser.

Reproducibility
This repo can use renv to lock package versions. To restore the environment:

```r
Copy code
install.packages("renv")
renv::restore()
```
Screenshots (optional)
You can add screenshots of your apps here later to make the repo more visual.

Metro Map App
Screenshot coming soon

<!-- ![Metro Map App Screenshot](docs/images/metro_map.png) -->
Food Charts App
Screenshot coming soon

<!-- ![Food Charts App Screenshot](docs/images/food_charts.png) -->
Final Correlation App
Screenshot coming soon

<!-- ![Final Correlation App Screenshot](docs/images/final_correlation.png) -->

