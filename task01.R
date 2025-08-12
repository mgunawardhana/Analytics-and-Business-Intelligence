# Clear work space and set options for clean output
rm(list = ls())

options(scipen = 999, digits = 4)

# Load required packages
library(readr)
library(nortest) # For Anderson-Darling and Lilliefors tests
library(ggplot2)
library(dplyr)
library(corrplot)
library(car)      # For VIF
library(moments)  # For skewness and kurtosis
library(tidyr)

# Helper function to format p-values for clean reporting
format.p <- function(p_value, digits = 3) {
  ifelse(p_value < 0.001, "< 0.001", round(p_value, digits))
}

setwd("D:/QGIS/ABI-CIS6008-MAR-2025-Dataset/Question-(a)/")

# Load data
uqr_data <- read_csv("UQR.csv")

# Data preprocessing: clean and convert relevant columns to numeric
numerical_cols <- c(
  "Student Enrollment", "Faculty Salary (Avg.)", "Research Funding (Million USD)",
  "Graduation Rate (%)", "Student-Faculty Ratio", "Tuition Fees (USD)",
  "Employment Rate (%)", "University Ranking Score"
)
for (col_name in numerical_cols) {
  uqr_data[[col_name]] <- suppressWarnings(as.numeric(gsub("[$,]", "", uqr_data[[col_name]])))
}

# Remove rows where the dependent variable is missing
uqr_data_clean <- uqr_data %>%
  filter(!is.na(`University Ranking Score`))

# =================================================================================
# 1. DESCRIPTIVE STATISTICS
# =================================================================================
# Function to calculate mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Create a comprehensive summary statistics table
summary_stats <- data.frame(
  Variable = character(), Mean = numeric(), Median = numeric(), Mode = numeric(),
  SD = numeric(), Skewness = numeric(), stringsAsFactors = FALSE
)

# Calculate statistics for each numerical variable
for (col in numerical_cols) {
  values <- na.omit(uqr_data_clean[[col]])
  summary_stats <- rbind(summary_stats, data.frame(
    Variable = col, Mean = mean(values), Median = median(values),
    Mode = getmode(values), SD = sd(values), Skewness = skewness(values)
  ))
}

print("--- 1. DESCRIPTIVE STATISTICS ---")
print(summary_stats)

# =================================================================================
# 1.1 VISUAL DESCRIPTIVE ANALYSIS: MODERN BOXPLOTS
# =================================================================================
print("\n--- 1.1 Generating Base R Boxplots for all Variables ---")

# Set up a 4-row, 2-column grid for the plots
par(mfrow = c(4, 2))

# Adjust the margins to prevent titles from being cut off
# Format is c(bottom, left, top, right)
par(mar = c(4, 4, 2, 1))

# Loop through each of the numerical column names
for (var in numerical_cols) {
  # Create a boxplot for the current variable
  boxplot(
    uqr_data_clean[[var]],      # The data for the plot
    main = var,                 # Sets the title of the plot
    col = "lightblue",          # Sets the fill color to light blue
    ylab = "Value",             # Label for the y-axis
    na.rm = TRUE                # Ensures missing values are ignored
  )
}

# IMPORTANT: Reset the plotting grid back to a single plot
par(mfrow = c(1, 1))

# =================================================================================
# 2. INFERENTIAL ANALYSIS: EXPLORING RELATIONSHIPS
# =================================================================================

# --- 2.1 Visual Exploration: Scatter Plots ---
print("\n--- 2.1 Generating Scatter Plots ---")
independent_vars <- setdiff(numerical_cols, "University Ranking Score")

for (var in independent_vars) {
  plot <- ggplot(uqr_data_clean, aes(x = .data[[var]], y = `University Ranking Score`)) +
    geom_point(alpha = 0.6, color = "blue") +
    geom_smooth(method = "lm", col = "red", se = FALSE) + # Add linear model trend line
    labs(
      title = paste("University Ranking Score vs.", var),
      x = var,
      y = "University Ranking Score"
    ) +
    theme_minimal()
  print(plot)
}

# --- 2.2 Normality Testing of Dependent Variable ---
print("\n--- 2.2 NORMALITY TESTS FOR UNIVERSITY RANKING SCORE ---")
urs_values <- na.omit(uqr_data_clean$`University Ranking Score`)
alpha <- 0.05

# Visual Inspection
# Histogram
ggplot(data.frame(x = urs_values), aes(x = x)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "lightblue", color = "black") +
  stat_function(fun = dnorm, args = list(mean = mean(urs_values), sd = sd(urs_values)), color = "darkblue", size = 1) +
  labs(title = "Distribution of University Ranking Score", x = "University Ranking Score", y = "Density")

# Q-Q Plot
qqnorm(urs_values, main = "Q-Q Plot of University Ranking Score"); qqline(urs_values, col = "red", lwd = 2)

# Formal Tests (as requested, three tests)
print("\nHypotheses for Normality Tests:")
print("H0: The data is normally distributed.")
print("H1: The data is NOT normally distributed.")
print(paste("Significance Level (alpha) =", alpha))

# Shapiro-Wilk Test
shapiro_result <- shapiro.test(urs_values)
print(paste("Shapiro-Wilk Test: W =", round(shapiro_result$statistic, 4), ", p-value =", format.p(shapiro_result$p.value)))

# Anderson-Darling Test
ad_result <- ad.test(urs_values)
print(paste("Anderson-Darling Test: A^2 =", round(ad_result$statistic, 4), ", p-value =", format.p(ad_result$p.value)))

# Lilliefors (Kolmogorov-Smirnov) Test
lillie_result <- lillie.test(urs_values)
print(paste("Lilliefors Test: D =", round(lillie_result$statistic, 4), ", p-value =", format.p(lillie_result$p.value)))

# =================================================================================
# 2.3 CORRELATION ANALYSIS (REVISED AND FIXED CODE)
# =================================================================================
print("\n--- 2.3 CORRELATION ANALYSIS ---")
# This line selects only the numerical columns and removes any rows with missing data
cor_data <- na.omit(uqr_data_clean[, numerical_cols]) 

# An empty data frame is created to store the final, report-ready results
final_cor_table <- data.frame()

# The code loops through each independent variable
for (var in independent_vars) {
  pearson_test <- cor.test(cor_data[[var]], cor_data$`University Ranking Score`, method = "pearson")
  spearman_test <- cor.test(cor_data[[var]], cor_data$`University Ranking Score`, method = "spearman", exact = FALSE)
  
  # --- This new logic automatically creates the interpretation text ---
  interpretation_text <- ""
  p_val <- pearson_test$p.value
  r_val <- pearson_test$estimate
  
  if (p_val < 0.05) {
    # It's statistically significant, so we determine strength and direction
    strength <- ""
    if (abs(r_val) >= 0.7) {
      strength <- "Strong"
    } else if (abs(r_val) >= 0.4) {
      strength <- "Moderate"
    } else {
      strength <- "Weak"
    }
    
    direction <- ifelse(r_val > 0, "positive", "negative")
    interpretation_text <- paste0(strength, ", ", direction, ", and statistically significant.")
    
  } else {
    # It's not statistically significant
    interpretation_text <- "Not statistically significant."
  }
  
  # We combine the results into a new row with the correct columns for your report
  final_cor_table <- rbind(final_cor_table, data.frame(
    Variable = var,
    `Pearson r` = round(r_val, 3),
    `Spearman ρ` = round(spearman_test$estimate, 3),
    `p-value` = format.p(p_val),
    Interpretation = interpretation_text,
    check.names = FALSE # This command prevents R from changing your column names
  ))
}

print("\n--- Report-Ready Correlation Table (FIXED OUTPUT) ---")
print(final_cor_table)

# =================================================================================
# 3. PREDICTIVE MODELING: REGRESSION ANALYSIS
# =================================================================================
print("\n--- 3.0 REGRESSION ANALYSIS ---")

# --- 3.2 Simple Linear Regression (using strongest predictor) ---
strongest_predictor <- "Graduation Rate (%)"
print(paste("\n--- 3.2 Simple Linear Regression using:", strongest_predictor, "---"))

simple_model_formula <- reformulate(paste0("`", strongest_predictor, "`"), response = "`University Ranking Score`")
simple_model <- lm(simple_model_formula, data = cor_data)
simple_summary <- summary(simple_model)
print(simple_summary)

# --- 3.3 Multiple Linear Regression (using significant predictors) ---
print("\n--- 3.3 Multiple Linear Regression ---")
mlr_predictors <- c("Graduation Rate (%)", "Employment Rate (%)", "Research Funding (Million USD)", "Student-Faculty Ratio")
print(paste("Predictors selected for Multiple Regression:", paste(mlr_predictors, collapse = ", ")))

formula_str <- paste("`University Ranking Score` ~", paste(paste0("`", mlr_predictors, "`"), collapse = " + "))
multiple_model <- lm(as.formula(formula_str), data = cor_data)
multiple_summary <- summary(multiple_model)
print(multiple_summary)

# Model Diagnostics: Check for Multicollinearity
print("\n--- Model Diagnostics: Variance Inflation Factors (VIF) ---")
vif_results <- vif(multiple_model)
print(vif_results)
if(any(vif_results > 5)) {
  print("Warning: VIF > 5 suggests potential multicollinearity.")
} else {
  print("All VIF values are below 5, indicating no serious multicollinearity.")
}

# Diagnostic plots
par(mfrow = c(2, 2)); plot(multiple_model); par(mfrow = c(1, 1))

# --- Final Model Equation ---
print("\n--- FINAL PREDICTION EQUATION (from Multiple Regression Model) ---")
coef_multiple <- coef(multiple_model)
equation <- paste("Predicted Score =", round(coef_multiple[1], 3))
for (i in 2:length(coef_multiple)) {
  sign <- ifelse(coef_multiple[i] >= 0, "+", "-")
  # Clean up variable names for the equation
  var_name <- gsub("`", "", names(coef_multiple)[i])
  equation <- paste(equation, sign, abs(round(coef_multiple[i], 3)), paste0("*(", var_name, ")"))
}
print(equation)

# =================================================================================
# 4. SAVE OUTPUTS TO FILES
# =================================================================================
# Save summary tables to CSV files for the report
write.csv(summary_stats, "revised_summary_statistics.csv", row.names = FALSE)
write.csv(cor_results_table, "revised_correlation_results.csv", row.names = FALSE)

print("\nANALYSIS COMPLETED SUCCESSFULLY!")
