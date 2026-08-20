# Titanic Data Visualization (Week 2 - R Internship)

Continuing from Week 1's cleaned Titanic dataset, this week focuses on
visualization and insight communication using ggplot2.

## Contents
- `visualize.R` - script that generates all 7 charts
- `titanic_cleaned.csv` - cleaned dataset carried over from Week 1
- `week2_output_log.txt` - console output (summary stats used in the report)
- `plots/` - 7 exported PNG charts

## How to run
```
Rscript visualize.R
```

## Charts included
1. Survival count by class (bar)
2. Survival rate by sex (bar, %)
3. Age distribution split by survival (histogram)
4. Age vs Fare scatter, colored by survival
5. Fare by class (boxplot)
6. Survival rate across age groups (line chart)
7. Class mix by embarkation port (stacked bar)

## Key takeaway
Sex (74.2% vs 18.9% survival) and class (63% vs 24% survival) were the two
strongest drivers of survival, with children under 10 showing the highest
survival rate of any age group (~59%).
