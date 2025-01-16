# CSAS-Challenge-2025
# MLB Injury Analysis Project

This project analyzes MLB player performance data in the context of injuries, focusing on swing speed trends, injury impacts, and predictive modeling. Below are instructions for running the project scripts in the correct order.

---

## **1. Data Cleaning and Scraping**
**File**: `Data CleaningScraping.R.r`

- **Purpose**: Fetches player performance data from the MLB API and prepares it for analysis.
- **Actions**:
  - Retrieves player statistics such as swing speed, fielding, and batting metrics.
  - Cleans and organizes the data for downstream analysis.

---

## **2. Fangraphs Injury Data Scrape**
**File**: `Fangraphs Scrape.R`

- **Purpose**: Scrapes Fangraphs' injury report data from the following URL:  
  [https://www.fangraphs.com/roster-resource/injury-report?season=2024&timeframe=all&groupby=all](https://www.fangraphs.com/roster-resource/injury-report?season=2024&timeframe=all&groupby=all)
- **Actions**:
  - Extracts player injury data for the 2024 MLB season.
  - Links injury data to the cleaned player data from `data_cleaning_and_scraping.R`.
  - Saves the compiled IL data to a CSV file.

---

## **3. Modeling and Visualization**
**File**: `Swing Speed Modeling.R`

- **Purpose**: Analyzes the processed data, generates visualizations, and fits statistical models.
- **Actions**:
  - Creates rolling averages for swing speed and length to explore injury effects.
  - Fits regression models to predict injury-related absences and performance impacts.
  - Visualizes key findings, such as changes in swing speed and injury trends.
- **How to Run**:
 
---

## **Workflow**
1. Start with `Data CleaningScraping.R.r` to fetch and preprocess MLB data.
2. Run `Fangraphs Scrape.R` to retrieve injury data and integrate it into the analysis.
3. Execute `Swing Speed Modeling.R` to perform modeling and generate visual insights.

This workflow ensures a structured and efficient approach to analyzing MLB injury data. If you encounter any issues, verify that scripts are run in the correct order and that all required dependencies are installed.
