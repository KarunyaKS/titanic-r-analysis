# Titanic Statistical Analysis & Predictive Modeling (Week 3 - R Internship)

Week 3 of the R internship: hypothesis testing + logistic regression
classification model to predict Titanic passenger survival.

## Contents
- `model.R` - full script (hypothesis tests, train/test split, 10-fold CV
  logistic regression, evaluation, diagnostics)
- `titanic_cleaned.csv` - cleaned dataset carried over from Week 1
- `week3_output_log.txt` - console output (test results, model summary, confusion matrix)
- `plots/` - Q-Q plot, confusion matrix, ROC curve, residual diagnostics, variable importance

## How to run
```
Rscript model.R
```
Requires: caret, pROC (both used for CV, evaluation metrics, and ROC/AUC)

## Summary
- Chi-square tests confirmed Sex and Pclass are both significantly associated
  with survival (p < 2.2e-16 for both).
- Logistic regression (10-fold CV) trained on Pclass, Sex, Age, Fare, SibSp,
  Parch, Embarked.
- Test set performance: 82.5% accuracy, AUC = 0.866, sensitivity 66.2%,
  specificity 92.7%.
- Sex was by far the strongest predictor, followed by Pclass and Age.
