# ============================================================
# Week 3 Task - Statistical Analysis and Predictive Modeling
# Continuing with the Titanic dataset
# ============================================================

library(caret)
library(pROC)

set.seed(42)  # reproducibility

df <- read.csv("titanic_cleaned.csv", stringsAsFactors = FALSE)
df$Pclass <- factor(df$Pclass, levels = c("1st","2nd","3rd"))
df$Sex <- factor(df$Sex, levels = c("male","female"))
df$Embarked <- factor(df$Embarked)
df$Survived <- as.integer(df$Survived)

sink("week3_output_log.txt", split = TRUE)

cat("========== 1. NORMALITY TEST (Shapiro-Wilk) on Age & Fare ==========\n")
# shapiro.test max sample 5000, we have 891 so fine
age_shapiro <- shapiro.test(df$Age)
fare_shapiro <- shapiro.test(df$Fare_Capped)
print(age_shapiro)
print(fare_shapiro)
cat("Interpretation: p-value < 0.05 for both => reject normality => Age and Fare\n")
cat("are NOT normally distributed (expected, Fare is right-skewed, Age has an\n")
cat("imputation spike). This is why non-parametric/robust checks are also used below.\n")

cat("\n========== 2. HYPOTHESIS TEST 1: Does Sex affect survival? (Chi-square) ==========\n")
cat("H0: Survival is independent of Sex\nH1: Survival depends on Sex\n")
tab_sex <- table(df$Sex, df$Survived)
chi_sex <- chisq.test(tab_sex)
print(chi_sex)
cat("p-value is far below 0.05 => reject H0 => Sex is significantly associated with survival.\n")

cat("\n========== 3. HYPOTHESIS TEST 2: Does Pclass affect survival? (Chi-square) ==========\n")
cat("H0: Survival is independent of Pclass\nH1: Survival depends on Pclass\n")
tab_class <- table(df$Pclass, df$Survived)
chi_class <- chisq.test(tab_class)
print(chi_class)
cat("p-value is far below 0.05 => reject H0 => Passenger class is significantly associated with survival.\n")

cat("\n========== 4. HYPOTHESIS TEST 3: Do survivors and non-survivors differ in Age? (t-test) ==========\n")
cat("H0: Mean age of survivors = mean age of non-survivors\nH1: Means differ\n")
t_age <- t.test(Age ~ Survived, data = df)
print(t_age)
cat("p-value ~", round(t_age$p.value, 4), "-> ", ifelse(t_age$p.value < 0.05, "significant difference in age", "no significant difference"), "\n")

cat("\n========== 5. HYPOTHESIS TEST 4: Do survivors and non-survivors differ in Fare? (t-test) ==========\n")
cat("H0: Mean fare of survivors = mean fare of non-survivors\nH1: Means differ\n")
t_fare <- t.test(Fare_Capped ~ Survived, data = df)
print(t_fare)
cat("p-value ~", format.pval(t_fare$p.value, digits=4), "-> ", ifelse(t_fare$p.value < 0.05, "significant difference in fare", "no significant difference"), "\n")

cat("\n========== 6. CORRELATION CHECK (numeric predictors) ==========\n")
num_df <- df[, c("Survived","Age","Fare_Capped","SibSp","Parch")]
print(round(cor(num_df), 2))

cat("\n========== 7. TRAIN/TEST SPLIT (80/20, stratified) ==========\n")
df$Survived_f <- factor(ifelse(df$Survived == 1, "Yes", "No"), levels = c("No","Yes"))
train_idx <- createDataPartition(df$Survived_f, p = 0.8, list = FALSE)
train <- df[train_idx, ]
test  <- df[-train_idx, ]
cat("Train rows:", nrow(train), " Test rows:", nrow(test), "\n")
cat("Train survival rate:", round(mean(train$Survived),3), " Test survival rate:", round(mean(test$Survived),3), "\n")

cat("\n========== 8. MODEL BUILDING: Logistic Regression with 10-fold Cross-Validation ==========\n")
ctrl <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary, savePredictions = TRUE)

model <- train(Survived_f ~ Pclass + Sex + Age + Fare_Capped + SibSp + Parch + Embarked,
               data = train, method = "glm", family = "binomial",
               trControl = ctrl, metric = "ROC")

print(model)
cat("\nFinal model coefficients:\n")
print(summary(model$finalModel)$coefficients)

cat("\n========== 9. TEST SET EVALUATION ==========\n")
pred_prob <- predict(model, newdata = test, type = "prob")[, "Yes"]
pred_class <- factor(ifelse(pred_prob > 0.5, "Yes", "No"), levels = c("No","Yes"))

cm <- confusionMatrix(pred_class, test$Survived_f, positive = "Yes")
print(cm)

roc_obj <- roc(response = test$Survived_f, predictor = pred_prob, levels = c("No","Yes"))
cat("\nTest set AUC:", round(auc(roc_obj), 4), "\n")

cat("\n========== 10. VARIABLE IMPORTANCE ==========\n")
print(varImp(model))

sink()

# ============================================================
# DIAGNOSTIC PLOTS
# ============================================================

png("d1_age_qqplot.png", width = 700, height = 600)
qqnorm(df$Age, main = "Q-Q Plot: Age (Normality Check)")
qqline(df$Age, col = "red", lwd = 2)
dev.off()

png("d2_confusion_matrix.png", width = 750, height = 600)
par(mar = c(2, 9, 6, 2))
cm_table <- as.data.frame(cm$table)
plot(1, type="n", xlim=c(0,2), ylim=c(0,2), axes=FALSE, xlab="", ylab="")
title(main = "Confusion Matrix - Test Set", line = 4, cex.main = 1.4)
labels <- c("TN","FN","FP","TP")
vals <- c(cm$table[1,1], cm$table[1,2], cm$table[2,1], cm$table[2,2])
positions <- list(c(0.5,1.5), c(1.5,1.5), c(0.5,0.5), c(1.5,0.5))
cols <- c("#8FBF8F","#E8A0A0","#E8A0A0","#8FBF8F")
for (i in 1:4) {
  rect(positions[[i]][1]-0.5, positions[[i]][2]-0.5, positions[[i]][1]+0.5, positions[[i]][2]+0.5, col = cols[i], border="white")
  text(positions[[i]][1], positions[[i]][2], paste0(labels[i], "\n", vals[i]), cex = 1.6)
}
mtext(c("Actual: No","Actual: Yes"), side = 3, at = c(0.5,1.5), line = 1, cex = 1.05)
mtext(c("Pred: No","Pred: Yes"), side = 2, at = c(1.5,0.5), line = 1, las = 1, cex = 1.05)
dev.off()

png("d3_roc_curve.png", width = 650, height = 600)
plot(roc_obj, main = paste0("ROC Curve (AUC = ", round(auc(roc_obj),3), ")"),
     col = "#2E4A6B", lwd = 2.5)
dev.off()

png("d4_residuals.png", width = 800, height = 600)
par(mfrow = c(1,2))
final_glm <- model$finalModel
plot(final_glm$fitted.values, residuals(final_glm, type = "deviance"),
     pch = 20, col = rgb(0.2,0.3,0.5,0.4),
     main = "Deviance Residuals vs Fitted", xlab = "Fitted values", ylab = "Deviance residuals")
abline(h = 0, col = "red", lty = 2)
plot(final_glm, which = 4, main = "Cook's Distance", caption = "")
dev.off()

png("d5_varimp.png", width = 700, height = 600)
vi <- varImp(model)$importance
vi$Variable <- rownames(vi)
vi <- vi[order(vi$Overall),]
par(mar = c(5,10,4,2))
barplot(vi$Overall, names.arg = vi$Variable, horiz = TRUE, las = 1,
        col = "#4C72B0", main = "Variable Importance (Logistic Regression)",
        xlab = "Importance Score")
dev.off()

cat("Week 3 script completed - all logs and plots generated.\n")
