# Get current working directory
getwd()

#read dataset
data_for_analysis <- read.csv("/Users/apple/Downloads/INRTU courses/2nd semester/Data Analysis/Homework 3/data_for_analysis.csv", header=T)
data
#Load required libraries
library(car)
library(lawstat)
library(gtsummary)
library(DataExplorer)

#Data preparation
# Convert outcome to factor
data_for_analysis$outcome <- as.factor(data_for_analysis$outcome)

# Define hormone variables
attach(data_for_analysis)
hormone_vars <- c("hormone1", "hormone2", "hormone3", "hormone4", 
                  "hormone5", "hormone6", "hormone7", "hormone8", "hormone10_generated")

# QUESTION 1: Create descriptive statistics table by group

# Create descriptive statistics table for all hormones by outcome group
summary(data_for_analysis)
# First, let's check the automatic table
tbl_summary(data_for_analysis)  # Automatic table for all variables

# Table by groups (by outcome)
tbl_summary(data_for_analysis, by = outcome)  # By groups

# Now create a customized table for hormones only
# First, determine distribution type for each hormone
check_distribution <- function(data, var) {
  # Clean data - remove NAs
  clean_data <- data[[var]][!is.na(data[[var]])]
  
  if(length(clean_data) < 3) {
    return("insufficient data")
  }
  
  # Shapiro-Wilk test
  shapiro_test <- shapiro.test(clean_data)
  
  if(shapiro_test$p.value > 0.05) {
    return("normal")  # Normally distributed - use mean (SD)
  } else {
    return("non-normal")  # Non-normally distributed - use median (IQR)
  }
}

# Check distribution for each hormone
distribution_results <- sapply(hormone_vars, function(h) {
  check_distribution(data_for_analysis, h)
})

print("Distribution types for hormones:")
print(distribution_results)

# Create customized table for hormones with appropriate statistics by group
# For normally distributed: Mean (SD)
# For non-normally distributed: Median (IQR)

hormone_table_by_group <- data_for_analysis %>%
  select(outcome, all_of(hormone_vars)) %>%
  tbl_summary(
    by = outcome,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})"
    ),
    digits = list(all_continuous() ~ 2)
  ) %>%
  modify_header(label ~ "**Hormone Variable**") %>%
  add_overall() %>%
  bold_labels()

hormone_table_by_group


# QUESTION 2: Levene's Test and Shapiro-Wilk test

# Create a function to perform both tests
normality_variance_tests <- function(data, hormone_var) {
  # Split data by outcome groups
  group0 <- data[data$outcome == "0", hormone_var]
  group1 <- data[data$outcome == "1", hormone_var]
  
  # Remove NAs
  group0 <- group0[!is.na(group0)]
  group1 <- group1[!is.na(group1)]
  
  # Shapiro-Wilk test for each group
  shapiro0 <- shapiro.test(group0)
  shapiro1 <- shapiro.test(group1)
  
  # Levene's Test
  levene_result <- leveneTest(as.formula(paste(hormone_vars, "~ outcome")), 
                              data = data)
  
  # Compile results
  results <- data.frame(
    Hormone = hormone_var,
    # Group 0 Shapiro-Wilk
    SW_Group0_W = round(shapiro0$statistic, 3),
    SW_Group0_p = round(shapiro0$p.value, 4),
    SW_Group0_Normal = ifelse(shapiro0$p.value > 0.05, "Normal", "Non-normal"),
    # Group 1 Shapiro-Wilk
    SW_Group1_W = round(shapiro1$statistic, 3),
    SW_Group1_p = round(shapiro1$p.value, 4),
    SW_Group1_Normal = ifelse(shapiro1$p.value > 0.05, "Normal", "Non-normal"),
    # Levene's Test
    Levene_F = round(levene_result$`F value`[1], 3),
    Levene_p = round(levene_result$`Pr(>F)`[1], 4),
    Levene_Result = ifelse(levene_result$`Pr(>F)`[1] > 0.05, 
                           "Equal variances", "Unequal variances")
  )
  
  return(results)
}

# Apply tests to all hormones
diagnostic_results <- do.call(rbind, lapply(hormone_vars, function(h) {
  normality_variance_tests(data_for_analysis, h)
}))

# Display results
print("QUESTION 2: NORMALITY AND VARIANCE TEST RESULTS")
print(diagnostic_results)

# Create a formatted table for the diagnostic tests
diagnostic_table <- diagnostic_results %>%
  select(Hormone, SW_Group0_Normal, SW_Group1_Normal, Levene_Result)

colnames(diagnostic_table) <- c("Hormone", 
                                "Distribution (Group 0)", 
                                "Distribution (Group 1)", 
                                "Variance Homogeneity")

print("Diagnostic Summary Table:")
print(diagnostic_table)
# From the output we can see that all 9 hormones show non-normal distribution in both groups (Shapiro-Wilk p < 0.05) but have equal variances between groups (Levene's test p > 0.05).

# Implication: Since normality is violated, parametric tests like Student's t-test are not appropriate. Use non-parametric tests such as Wilcoxon rank-sum test or Brunner-Munzel test. Given that variances are equal, the Wilcoxon test is suitable, though Brunner-Munzel remains a robust alternative.


# QUESTION 3: Q-Q Plots and histograms (all hormones)

for(hormone in hormone_vars) {
  par(mfrow = c(2, 2))
  
  # Extract data for each group
  group0_data <- data_for_analysis[data_for_analysis$outcome == "0", hormone]
  group1_data <- data_for_analysis[data_for_analysis$outcome == "1", hormone]
  
  # Remove NAs
  group0_data <- group0_data[!is.na(group0_data)]
  group1_data <- group1_data[!is.na(group1_data)]
  
  # Histogram - Group 0
  hist(group0_data, col = "lightblue", 
       main = paste(hormone, "- Group 0 Histogram"),
       xlab = "Value")
  
  # Histogram - Group 1
  hist(group1_data, col = "lightgreen", 
       main = paste(hormone, "- Group 1 Histogram"),
       xlab = "Value")
  
  # Q-Q Plot - Group 0
  qqnorm(group0_data, 
         main = paste(hormone, "- Group 0 Q-Q Plot"),
         col = "blue", pch = 19)
  qqline(group0_data, col = "red", lwd = 2)
  
  # Q-Q Plot - Group 1
  qqnorm(group1_data, 
         main = paste(hormone, "- Group 1 Q-Q Plot"),
         col = "green", pch = 19)
  qqline(group1_data, col = "red", lwd = 2)
  
  par(mfrow = c(1, 1))  # Reset
}

# QUESTION 4: Statistical tests comparison
# (Brunner-Munzel, t-test, Wilcoxon) for all hormones

# Function to perform all three tests
compare_tests <- function(data, hormone_var) {
  # Extract data by group
  group0 <- data[data$outcome == "0", hormone_var]
  group1 <- data[data$outcome == "1", hormone_var]
  
  # Remove NAs
  group0 <- group0[!is.na(group0)]
  group1 <- group1[!is.na(group1)]
  
  # t-test
  t_result <- t.test(group0, group1)
  
  # Wilcoxon test
  wilcox_result <- wilcox.test(group0, group1)
  
  # Brunner-Munzel test
  bm_result <- brunner.munzel.test(group0, group1)
  
  # Return p-values
  results <- data.frame(
    Hormone = hormone_var,
    t_test_pvalue = round(t_result$p.value, 4),
    wilcox_pvalue = round(wilcox_result$p.value, 4),
    bm_pvalue = round(bm_result$p.value, 4)
  )
  
  return(results)
}

# Apply tests to all hormones
all_comparison_results <- do.call(rbind, lapply(hormone_vars, function(h) {
  compare_tests(data_for_analysis, h)
}))

# Display results
print("QUESTION 4: STATISTICAL TEST COMPARISON")
print(all_comparison_results)  

# CONCLUSION: Which test is applicable?

# Based on the Shapiro-Wilk test results from Question 2, all hormones are 
# non-normally distributed in both groups. Therefore, the t-test is not 
# appropriate as it assumes normality.

# Between the Wilcoxon rank-sum test and Brunner-Munzel test:
# Both are non-parametric tests suitable for non-normal data
# Levene's test showed equal variances for all hormones, so Wilcoxon is sufficient
# Brunner-Munzel test is also valid and provides similar results

# Recommendation: Use Wilcoxon rank-sum test or Brunner-Munzel test since 
# the data violates normality. Given equal variances across all hormones, 
# Wilcoxon is the appropriate choice.



# QUESTION 5: Correlation heatmaps by group
install.packages("corrplot")
library(corrplot)
# Determine correlation method based on normality
determine_method <- function(data, vars) {
  # Test normality for each variable
  normality <- sapply(vars, function(v) {
    if(sum(!is.na(data[[v]])) < 3) return(FALSE)
    test <- shapiro.test(data[[v]][!is.na(data[[v]])])
    return(test$p.value > 0.05)
  })
  
  # Count normal variables
  normal_count <- sum(normality, na.rm = TRUE)
  
  cat(sprintf("• %d out of %d hormones are normally distributed\n", 
              normal_count, length(vars)))
  
  # If majority are normal, use Pearson; otherwise Spearman
  if(normal_count >= length(vars) * 0.7) {
    cat("  → Using PEARSON correlation (data is predominantly normal)\n")
    return("pearson")
  } else {
    cat("  → Using SPEARMAN correlation (data is predominantly non-normal)\n")
    return("spearman")
  }
}

# Create correlation heatmaps for each group
pdf("Correlation_Heatmaps.pdf", width = 12, height = 6)

for(group_level in c("0", "1")) {
  # Subset data
  group_data <- data_for_analysis[data_for_analysis$outcome == group_level, 
                                  hormone_vars]
  
  # Remove rows with any NAs
  group_data_clean <- na.omit(group_data)
  
  cat(sprintf("\nAnalyzing Group %s:\n", group_level))
  cat(sprintf("• Sample size after removing NAs: %d\n", nrow(group_data_clean)))
  
  # Determine correlation method
  cor_method <- determine_method(group_data_clean, hormone_vars)
  
  # Calculate correlation matrix
  cor_matrix <- cor(group_data_clean, method = cor_method)
  
  # Perform correlation significance test
  cor_test_matrix <- cor.mtest(group_data_clean, conf.level = 0.95)
  
  # Create heatmap with significance indicators
  par(mfrow = c(1, 1))
  
  corrplot(cor_matrix,
           method = "color",
           type = "upper",
           order = "hclust",
           addCoef.col = "black",
           number.cex = 0.7,
           tl.col = "black",
           tl.cex = 0.8,
           tl.srt = 45,
           p.mat = cor_test_matrix$p,
           sig.level = c(0.001, 0.01, 0.05),
           insig = "label_sig",
           pch.cex = 0.9,
           pch.col = "red",
           title = paste("Correlation Heatmap - Group", group_level,
                         "\n(Method:", cor_method, ")"),
           mar = c(0, 0, 3, 0))
  
  # Add legend for significance
  legend("bottom", 
         legend = c("* p<0.05", "** p<0.01", "*** p<0.001"),
         horiz = TRUE, 
         bty = "n",
         cex = 0.8)
}

dev.off()

# Create comparative visualization
par(mfrow = c(1, 2))

for(group_level in c("0", "1")) {
  group_data <- data_for_analysis[data_for_analysis$outcome == group_level, 
                                  hormone_vars]
  group_data_clean <- na.omit(group_data)
  cor_method <- determine_method(group_data_clean, hormone_vars)
  cor_matrix <- cor(group_data_clean, method = cor_method)
  
  corrplot(cor_matrix,
           method = "circle",
           type = "upper",
           order = "hclust",
           tl.col = "black",
           tl.cex = 0.7,
           tl.srt = 45,
           title = paste("Group", group_level),
           mar = c(0, 0, 2, 0))
}

par(mfrow = c(1, 1))


# Interpretation of Correlation Patterns

# Overall pattern:
# Both groups show predominantly weak to moderate correlations among hormones.
# No strong correlations were observed in either group.

# Key findings in Group 0:
# hormone3 and hormone4 show moderate positive correlation (r = 0.587)
# Other correlations are weak

# Key findings in Group 1:
# hormone3 and hormone4 maintain moderate positive correlation (r = 0.564)
# hormone7 and hormone10_generated correlation increased from 0.190 to 0.269
# hormone5 and hormone10_generated correlation increased from 0.011 to 0.216
# hormone7 and hormone8 correlation increased from 0.088 to 0.136

# Interpretation:
# The consistent moderate correlation between hormone3 and hormone4 in both 
# groups suggests a stable biological relationship unaffected by outcome status.

# Group 1 shows slightly stronger inter-hormone correlations overall, 
# particularly involving hormone5, hormone7, and hormone10_generated. 
# This may indicate compensatory or pathological hormonal clustering 
# associated with the outcome condition.

# However, all correlations remain weak to moderate, suggesting that 
# these hormones largely function independently rather than as a tightly 
# coordinated system in both groups.






