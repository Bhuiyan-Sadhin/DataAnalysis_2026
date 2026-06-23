# Load required libraries
library(tidyverse)
library(epitools)
library(ggplot2)
library(broom)
library(knitr)
data<- read.csv("/Users/apple/Downloads/INRTU courses/2nd semester/Data Analysis/Homework 3/data_for_analysis.csv", header =T )
data

# Check structure
str(data)

# Summary statistics
summary(data)

# Convert outcome to factor
data$outcome <- as.factor(data$outcome)

# Function to perform univariate logistic regression
run_univariate_logistic <- function(data, predictors, outcome) {
  results <- data.frame()
  
  for (var in predictors) {
    # Skip if too many NAs
    if (sum(is.na(data[[var]])) > nrow(data) * 0.5) next
    
    formula <- as.formula(paste(outcome, "~", var))
    model <- glm(formula, data = data, family = binomial())
    
    # Extract coefficients
    coef_summary <- summary(model)$coefficients
    
    # Calculate OR and CI
    OR <- exp(coef_summary[2, 1])
    CI <- exp(confint(model, var))
    p_value <- coef_summary[2, 4]
    
    results <- rbind(results, data.frame(
      Variable = var,
      OR = round(OR, 3),
      CI_Lower = round(CI[1], 3),
      CI_Upper = round(CI[2], 3),
      P_Value = round(p_value, 4),
      Significant = ifelse(p_value < 0.05, "Yes", "No")
    ))
  }
  
  return(results)
}

# Define predictors (excluding outcome and ID)
predictors <- names(data)[!names(data) %in% c("record_id", "outcome")]

# Run analysis
results <- run_univariate_logistic(data, predictors, "outcome")

# View results
print(results)
kable(results, caption = "Univariate Logistic Regression Results")

# Filter significant results (p < 0.05)
significant_results <- results %>% filter(P_Value < 0.05)
print(significant_results)

# Function for detailed analysis of one predictor
detailed_logistic <- function(data, predictor, outcome) {
  formula <- as.formula(paste(outcome, "~", predictor))
  model <- glm(formula, data = data, family = binomial())
  
  cat("\n=== Model Summary for", predictor, "===\n")
  print(summary(model))
  
  # Odds Ratio with CI
  cat("\n=== Odds Ratio ===\n")
  OR <- exp(coef(model))
  CI <- exp(confint(model))
  print(data.frame(OR = OR, CI_lower = CI[,1], CI_upper = CI[,2]))
  
  # Predictions for visualization
  data$predicted <- predict(model, type = "response")
  
  return(model)
}

# Example: Analyze factor_pcos
model_pcos <- detailed_logistic(data, "factor_pcos", "outcome")

# 1. Forest Plot for Odds Ratios
create_forest_plot <- function(results, title = "Odds Ratios for Predictors") {
  # Filter significant results and sort by OR
  plot_data <- results %>%
    filter(!is.na(OR)) %>%
    arrange(desc(OR)) %>%
    head(15)  # Top 15 predictors
  
  ggplot(plot_data, aes(x = reorder(Variable, OR), y = OR)) +
    geom_point(size = 3, color = "steelblue") +
    geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), 
                  width = 0.2, color = "steelblue") +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
    coord_flip() +
    labs(
      title = title,
      x = "Predictor Variable",
      y = "Odds Ratio (95% CI)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14),
      axis.text = element_text(size = 10)
    )
}

# Create the plot
forest_plot <- create_forest_plot(results)
print(forest_plot)

# 2. Distribution of Significant Predictors
create_significance_plot <- function(results) {
  sig_summary <- results %>%
    group_by(Significant) %>%
    summarise(Count = n())
  
  ggplot(sig_summary, aes(x = Significant, y = Count, fill = Significant)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = Count), vjust = -0.5, size = 5) +
    labs(
      title = "Number of Significant vs Non-Significant Predictors",
      x = "Significant at p < 0.05",
      y = "Count of Predictors"
    ) +
    theme_minimal() +
    scale_fill_manual(values = c("Yes" = "#2E86AB", "No" = "#D3D3D3"))
}

significance_plot <- create_significance_plot(results)
print(significance_plot)

# 3. Box plot for a significant continuous variable
create_boxplot <- function(data, predictor, outcome) {
  ggplot(data, aes(x = outcome, y = .data[[predictor]], fill = outcome)) +
    geom_boxplot(alpha = 0.7) +
    labs(
      title = paste("Distribution of", predictor, "by Outcome"),
      x = "Outcome",
      y = predictor
    ) +
    theme_minimal() +
    scale_fill_manual(values = c("#2E86AB", "#A23B72"))
}

# Example for a continuous variable (e.g., hormone1)
boxplot <- create_boxplot(data, "hormone1", "outcome")
print(boxplot)

# 4. Predicted Probabilities Plot
create_prediction_plot <- function(data, predictor, outcome) {
  model <- glm(as.formula(paste(outcome, "~", predictor)), 
               data = data, family = binomial())
  
  # Create prediction data
  pred_data <- data.frame(
    predictor = seq(min(data[[predictor]], na.rm = TRUE),
                    max(data[[predictor]], na.rm = TRUE),
                    length.out = 100)
  )
  names(pred_data) <- predictor
  
  # Predict
  pred_data$predicted <- predict(model, newdata = pred_data, type = "response")
  
  # Plot
  ggplot() +
    geom_point(data = data, aes(x = .data[[predictor]], 
                                y = as.numeric(outcome) - 1),
               alpha = 0.3, size = 1) +
    geom_line(data = pred_data, aes(x = .data[[predictor]], y = predicted),
              color = "red", size = 1.2) +
    labs(
      title = paste("Predicted Probability of Outcome by", predictor),
      x = predictor,
      y = "Predicted Probability"
    ) +
    theme_minimal()
}

# Example
pred_plot <- create_prediction_plot(data, "hormone1", "outcome")
print(pred_plot)

# Save plots
ggsave("forest_plot.png", forest_plot, width = 10, height = 8)
ggsave("significance_plot.png", significance_plot, width = 8, height = 6)
ggsave("boxplot.png", boxplot, width = 8, height = 6)
ggsave("prediction_plot.png", pred_plot, width = 8, height = 6)



# Export results to CSV
write.csv(results, "univariate_logistic_results.csv", row.names = FALSE)

# Export significant results
write.csv(significant_results, "significant_predictors.csv", row.names = FALSE)

# Create summary table for presentation
summary_table <- results %>%
  filter(P_Value < 0.05) %>%
  arrange(P_Value) %>%
  mutate(
    `95% CI` = paste0(CI_Lower, " - ", CI_Upper),
    `p-value` = format(P_Value, scientific = FALSE)
  ) %>%
  select(Variable, OR, `95% CI`, `p-value`)

write.csv(summary_table, "presentation_summary.csv", row.names = FALSE)




# Run these to get numbers for this slide:
nrow(data)  # Sample size
length(names(data))  # Number of variables
table(data$outcome)  # Outcome distribution

# Run this to get counts
sig_count <- sum(results$P_Value < 0.05, na.rm = TRUE)
total_count <- nrow(results)
non_sig_count <- total_count - sig_count

print(paste("Significant:", sig_count))
print(paste("Non-significant:", non_sig_count))

significance_plot <- create_significance_plot(results)
print(significance_plot)

forest_plot <- create_forest_plot(results)
print(forest_plot)


# Run this to create clean table
summary_table <- results %>%
  filter(P_Value < 0.05) %>%
  arrange(P_Value) %>%
  mutate(
    `95% CI` = paste0(CI_Lower, " - ", CI_Upper),
    `p-value` = format(P_Value, scientific = FALSE)
  ) %>%
  select(Variable, OR, `95% CI`, `p-value`)

# Use kable for nice formatting
kable(summary_table, caption = "Significant Predictors Table")


# Get top predictors
top_predictors <- results %>%
  filter(P_Value < 0.05) %>%
  arrange(P_Value) %>%
  head(5)

print(top_predictors$Variable)
print(top_predictors$OR)

final_model <- glm(outcome ~ lipids2 + hormone8 + lipids4 + 
                     antioxidant5 + antioxidant4, 
                   data = data, family = binomial())
coef(summary(final_model))


