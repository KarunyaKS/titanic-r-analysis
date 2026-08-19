# ==========================================================
# Week 1 Task: Data Cleaning and Preliminary Analysis with R
# Dataset: Titanic Passenger Data
# ==========================================================

df <- read.csv("titanic.csv", stringsAsFactors = FALSE)

sink("output_log.txt", split = TRUE)

cat("========== 1. STRUCTURE OF RAW DATA ==========\n")
str(df)

cat("\n========== 2. SUMMARY OF RAW DATA ==========\n")
summary(df)

cat("\n========== 3. MISSING VALUE COUNT PER COLUMN ==========\n")
missing_counts <- colSums(is.na(df) | df == "")
print(missing_counts)

cat("\n========== 4. MISSING VALUE HANDLING ==========\n")

# Age: numeric, ~19.9% missing -> impute with median (robust to skew/outliers)
age_median <- median(df$Age, na.rm = TRUE)
df$Age[is.na(df$Age)] <- age_median
cat("Age missing values imputed with median:", age_median, "\n")

# Embarked: categorical, 2 missing -> impute with mode
embarked_mode <- names(sort(table(df$Embarked[df$Embarked != ""]), decreasing = TRUE))[1]
df$Embarked[df$Embarked == ""] <- embarked_mode
cat("Embarked missing values imputed with mode:", embarked_mode, "\n")

# Cabin: ~77% missing -> too sparse to impute meaningfully.
# Convert to a binary indicator instead of dropping the signal entirely.
df$Cabin_Known <- ifelse(df$Cabin == "" | is.na(df$Cabin), 0, 1)
df$Cabin <- NULL
cat("Cabin column dropped (77% missing); replaced with binary 'Cabin_Known' flag.\n")

cat("\nMissing values remaining after cleaning:\n")
print(colSums(is.na(df) | df == ""))

cat("\n========== 5. OUTLIER DETECTION (Fare, Age) ==========\n")
detect_outliers <- function(x) {
  q1 <- quantile(x, 0.25); q3 <- quantile(x, 0.75); iqr <- q3 - q1
  lower <- q1 - 1.5 * iqr; upper <- q3 + 1.5 * iqr
  sum(x < lower | x > upper)
}
cat("Number of Fare outliers (IQR method):", detect_outliers(df$Fare), "\n")
cat("Number of Age outliers (IQR method):", detect_outliers(df$Age), "\n")
cat("Fare summary:\n"); print(summary(df$Fare))

# Cap extreme Fare outliers at the 99th percentile (winsorizing) rather than deleting rows
fare_cap <- quantile(df$Fare, 0.99, na.rm = TRUE)
df$Fare_Capped <- ifelse(df$Fare > fare_cap, fare_cap, df$Fare)
cat("Fare capped at 99th percentile:", fare_cap, "\n")

cat("\n========== 6. NORMALIZATION ==========\n")
# Min-max normalize Age and Fare_Capped into new columns (0-1 scale)
minmax <- function(x) (x - min(x)) / (max(x) - min(x))
df$Age_Norm  <- minmax(df$Age)
df$Fare_Norm <- minmax(df$Fare_Capped)
cat("Age and Fare min-max normalized into Age_Norm / Fare_Norm (range 0-1).\n")
cat("Age_Norm summary:\n"); print(summary(df$Age_Norm))
cat("Fare_Norm summary:\n"); print(summary(df$Fare_Norm))

cat("\n========== 7. ENCODING CATEGORICAL VARIABLES ==========\n")
# Label encoding for Sex
df$Sex_Encoded <- ifelse(df$Sex == "male", 1, 0)
# One-hot encoding for Embarked
df$Embarked_S <- ifelse(df$Embarked == "S", 1, 0)
df$Embarked_C <- ifelse(df$Embarked == "C", 1, 0)
df$Embarked_Q <- ifelse(df$Embarked == "Q", 1, 0)
# Factor conversion for Pclass and Survived (for correct EDA plotting)
df$Pclass   <- factor(df$Pclass, levels = c(1,2,3), labels = c("1st","2nd","3rd"))
df$Survived_Label <- factor(df$Survived, levels = c(0,1), labels = c("No","Yes"))
cat("Sex label-encoded (Sex_Encoded); Embarked one-hot encoded (Embarked_S/C/Q);\n")
cat("Pclass and Survived converted to labeled factors.\n")

cat("\n========== 8. CLEANED DATA STRUCTURE ==========\n")
str(df)

cat("\n========== 9. DESCRIPTIVE STATISTICS (CLEANED) ==========\n")
summary(df[, c("Age","Fare_Capped","SibSp","Parch")])

cat("\n========== 10. SURVIVAL RATE BY GROUP ==========\n")
cat("Overall survival rate:\n")
print(round(prop.table(table(df$Survived_Label)), 3))
cat("\nSurvival rate by Sex:\n")
print(round(prop.table(table(df$Sex, df$Survived_Label), margin = 1), 3))
cat("\nSurvival rate by Pclass:\n")
print(round(prop.table(table(df$Pclass, df$Survived_Label), margin = 1), 3))

cat("\n========== 11. CORRELATION MATRIX (numeric variables) ==========\n")
num_vars <- df[, c("Survived","Age","Fare_Capped","SibSp","Parch","Sex_Encoded")]
corr_matrix <- round(cor(num_vars, use = "complete.obs"), 2)
print(corr_matrix)

sink()

# Save the cleaned dataset
write.csv(df, "titanic_cleaned.csv", row.names = FALSE)

# ==========================================================
# VISUALIZATIONS
# ==========================================================

png("plot1_age_distribution.png", width = 800, height = 600)
hist(df$Age, breaks = 30, col = "#4C72B0", border = "white",
     main = "Distribution of Passenger Age (After Median Imputation)",
     xlab = "Age", ylab = "Frequency")
abline(v = age_median, col = "red", lwd = 2, lty = 2)
legend("topright", legend = paste("Median =", age_median), col = "red", lty = 2, lwd = 2, bty = "n")
dev.off()

png("plot2_fare_boxplot.png", width = 800, height = 600)
boxplot(Fare ~ Pclass, data = df, col = c("#55A868","#C44E52","#8172B2"),
        main = "Fare Distribution by Passenger Class (Outliers Visible)",
        xlab = "Passenger Class", ylab = "Fare")
dev.off()

png("plot3_survival_by_sex.png", width = 800, height = 600)
counts <- table(df$Sex, df$Survived_Label)
barplot(counts, beside = TRUE, col = c("#4C72B0","#DD8452"),
        legend.text = rownames(counts), args.legend = list(x = "topright"),
        main = "Survival Count by Sex", xlab = "Survived", ylab = "Count")
dev.off()

png("plot4_survival_by_class.png", width = 800, height = 600)
counts2 <- table(df$Pclass, df$Survived_Label)
barplot(counts2, beside = TRUE, col = c("#55A868","#C44E52","#8172B2"),
        legend.text = rownames(counts2), args.legend = list(x = "topright"),
        main = "Survival Count by Passenger Class", xlab = "Survived", ylab = "Count")
dev.off()

png("plot5_correlation_heatmap.png", width = 700, height = 700)
heatmap(as.matrix(corr_matrix), Rowv = NA, Colv = NA, col = heat.colors(20),
        scale = "none", margins = c(10,10),
        main = "Correlation Heatmap (Numeric Variables)")
dev.off()

cat("All plots and logs generated successfully.\n")
