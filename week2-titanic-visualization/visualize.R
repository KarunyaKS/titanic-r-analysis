# ============================================================
# Week 2 Task - Data Visualization and Insight Communication
# Continuing with the Titanic dataset from Week 1
# ============================================================

library(ggplot2)

df <- read.csv("titanic_cleaned.csv", stringsAsFactors = FALSE)

# keep factor labels consistent for plotting
df$Pclass <- factor(df$Pclass, levels = c("1st","2nd","3rd"))
df$Survived_Label <- factor(df$Survived_Label, levels = c("No","Yes"))
df$Sex <- factor(df$Sex, levels = c("male","female"))

theme_set(theme_minimal(base_size = 13))
my_colors <- c("#2E4A6B", "#DD8452", "#55A868", "#C44E52")

sink("week2_output_log.txt", split = TRUE)
cat("Rows:", nrow(df), " Cols:", ncol(df), "\n")
cat("Survival rate overall:", round(mean(df$Survived)*100,1), "%\n")
cat("\nSurvival by class:\n")
print(round(prop.table(table(df$Pclass, df$Survived_Label), margin = 1)*100, 1))
cat("\nSurvival by sex:\n")
print(round(prop.table(table(df$Sex, df$Survived_Label), margin = 1)*100, 1))
cat("\nAvg fare by class:\n")
print(tapply(df$Fare, df$Pclass, mean))
sink()

# ---------- Chart 1: Bar chart - Survival count by class ----------
p1 <- ggplot(df, aes(x = Pclass, fill = Survived_Label)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("No" = "#C44E52", "Yes" = "#55A868")) +
  labs(title = "Who Actually Survived? Breakdown by Ticket Class",
       x = "Passenger Class", y = "Number of Passengers", fill = "Survived") +
  theme(plot.title = element_text(face = "bold", size = 15))
ggsave("g1_survival_by_class.png", p1, width = 7.5, height = 5, dpi = 150)

# ---------- Chart 2: Bar chart - Survival rate (%) by sex ----------
sex_surv <- aggregate(Survived ~ Sex, data = df, mean)
sex_surv$Survived <- round(sex_surv$Survived * 100, 1)
p2 <- ggplot(sex_surv, aes(x = Sex, y = Survived, fill = Sex)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = paste0(Survived, "%")), vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("male" = "#2E4A6B", "female" = "#DD8452")) +
  ylim(0, 100) +
  labs(title = "Survival Rate: Male vs Female", x = "Sex", y = "Survival Rate (%)") +
  theme(legend.position = "none", plot.title = element_text(face = "bold", size = 15))
ggsave("g2_survival_rate_by_sex.png", p2, width = 6.5, height = 5, dpi = 150)

# ---------- Chart 3: Histogram - Age distribution split by survival ----------
p3 <- ggplot(df, aes(x = Age, fill = Survived_Label)) +
  geom_histogram(binwidth = 5, position = "identity", alpha = 0.55, color = "white") +
  scale_fill_manual(values = c("No" = "#C44E52", "Yes" = "#55A868")) +
  labs(title = "Age Distribution: Survivors vs Non-Survivors",
       x = "Age", y = "Number of Passengers", fill = "Survived") +
  theme(plot.title = element_text(face = "bold", size = 15))
ggsave("g3_age_histogram_survival.png", p3, width = 7.5, height = 5, dpi = 150)

# ---------- Chart 4: Scatter plot - Age vs Fare, colored by survival ----------
p4 <- ggplot(df, aes(x = Age, y = Fare, color = Survived_Label)) +
  geom_point(alpha = 0.6, size = 2) +
  scale_color_manual(values = c("No" = "#C44E52", "Yes" = "#55A868")) +
  labs(title = "Age vs Fare Paid, Colored by Survival",
       x = "Age", y = "Fare (raw, before capping)", color = "Survived") +
  theme(plot.title = element_text(face = "bold", size = 15))
ggsave("g4_scatter_age_fare.png", p4, width = 7.5, height = 5, dpi = 150)

# ---------- Chart 5: Boxplot - Fare by class (log scale) ----------
p5 <- ggplot(df, aes(x = Pclass, y = Fare_Capped, fill = Pclass)) +
  geom_boxplot() +
  scale_fill_manual(values = c("1st" = "#8172B2", "2nd" = "#55A868", "3rd" = "#C44E52")) +
  labs(title = "Fare Paid by Passenger Class (Capped)",
       x = "Passenger Class", y = "Fare") +
  theme(legend.position = "none", plot.title = element_text(face = "bold", size = 15))
ggsave("g5_boxplot_fare_class.png", p5, width = 6.5, height = 5, dpi = 150)

# ---------- Chart 6: Line chart - Survival rate by age group ----------
df$AgeGroup <- cut(df$Age, breaks = c(0,10,20,30,40,50,60,80),
                    labels = c("0-10","11-20","21-30","31-40","41-50","51-60","60+"))
age_surv <- aggregate(Survived ~ AgeGroup, data = df, mean)
age_surv$Survived <- age_surv$Survived * 100
p6 <- ggplot(age_surv, aes(x = AgeGroup, y = Survived, group = 1)) +
  geom_line(color = "#2E4A6B", linewidth = 1.2) +
  geom_point(color = "#DD8452", size = 3) +
  labs(title = "Survival Rate Trend Across Age Groups",
       x = "Age Group", y = "Survival Rate (%)") +
  theme(plot.title = element_text(face = "bold", size = 15))
ggsave("g6_line_survival_age_group.png", p6, width = 7.5, height = 5, dpi = 150)

# ---------- Chart 7: Stacked bar - Embarkation port vs class ----------
p7 <- ggplot(df, aes(x = Embarked, fill = Pclass)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("1st" = "#8172B2", "2nd" = "#55A868", "3rd" = "#C44E52")) +
  labs(title = "Passenger Class Mix by Port of Embarkation",
       x = "Port (C = Cherbourg, Q = Queenstown, S = Southampton)",
       y = "Proportion", fill = "Class") +
  theme(plot.title = element_text(face = "bold", size = 14))
ggsave("g7_embarked_class_stack.png", p7, width = 7.5, height = 5, dpi = 150)

cat("All 7 ggplot2 visualizations generated.\n")
