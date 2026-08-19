# Titanic Data Cleaning & Preliminary Analysis (R)

Week 1 task for the Virtual R Data Analyst Internship: cleaning, preprocessing,
and exploratory analysis of the Titanic passenger dataset using base R.

## Contents
- `analysis.R` — full cleaning + EDA script (missing value handling, outlier
  detection, normalization, categorical encoding, correlation analysis, plots)
- `titanic.csv` — raw input dataset
- `titanic_cleaned.csv` — cleaned/encoded output dataset
- `output_log.txt` — captured console output (str/summary/stats)
- `plots/` — generated visualizations (PNG)

## How to run
```
Rscript analysis.R
```

## Summary of steps
1. Loaded data with `read.csv()`, inspected with `str()` / `summary()`.
2. Handled missing values: median imputation (Age), mode imputation (Embarked),
   binary flag instead of drop (Cabin, 77% missing).
3. Detected outliers via IQR method on Fare/Age; winsorized Fare at the 99th
   percentile instead of deleting rows.
4. Min-max normalized Age and Fare into `Age_Norm` / `Fare_Norm`.
5. Encoded categoricals: label encoding (Sex), one-hot encoding (Embarked),
   factor labeling (Pclass, Survived).
6. Explored survival rate by sex/class, correlation matrix, and 5 visualizations.
