# Traffic Crash Data Analysis & Predictive Modeling

## Overview
This project analyzes traffic crash data by cleaning and merging multiple datasets and building predictive models to understand factors associated with driver fault.

The workflow includes data preprocessing, feature engineering, exploratory analysis, and decision tree modeling.

---

## Data

This project uses publicly available traffic crash datasets containing information on accidents, vehicles, and drivers.

The analysis integrates multiple data sources, including:
- Accident-level data (crash type, location, weather conditions)
- Vehicle-level data (damage severity, speed limits, alcohol involvement)
- Person-level data (driver age, gender, seating position)
- Violation data (used to determine fault)

These datasets were merged using shared identifiers (e.g., crash ID and vehicle ID) to construct a unified dataset.

The datasets required substantial cleaning due to inconsistent coding schemes and missing values across sources.

**Note:** Raw data files are not included in this repository due to size constraints. To run the analysis, place the relevant CSV files in a `/data` directory and update file paths as needed.

---

## Methods

- Cleaned and merged multiple relational datasets using `dplyr`
- Engineered features including:
  - Damage severity (categorical and numeric)
  - Speed group categories
  - Vehicle country of origin
  - Driver fault classification
- Performed exploratory data analysis using proportional summaries and visualizations
- Built decision tree models using `ctree` to predict driver fault
- Evaluated model performance using training/testing splits

---

## Key Findings

- Alcohol involvement and speed are strong predictors of driver fault
- Feature engineering significantly improved interpretability of results
- Decision tree models provided clear insights into key contributing factors

---

## Tools & Technologies

- **Language:** R  
- **Libraries:** tidyverse, dplyr, ggplot2, partykit  

---

## How to Run

1. Clone or download this repository  
2. Place required CSV datasets in the `/data` folder  
3. Update file paths in the script if needed  
4. Run: traffic_crash_analysis.R

---

## Future Improvements

- Implement additional models (e.g., logistic regression, random forest)
- Perform cross-validation for more robust evaluation
- Improve feature engineering and handling of missing data
- Add interactive visualizations

---

## Author

Felicia Selbst  
Wellesley College — Data Science Major, Chemistry Minor  

- LinkedIn: https://www.linkedin.com/in/feliciaselbst  
- GitHub: https://github.com/fs105-jpg
