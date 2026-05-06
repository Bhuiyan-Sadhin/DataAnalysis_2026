# Get current working directory
getwd()

#read dataset
data <- read.csv("/Users/apple/Downloads/INRTU courses/2nd semester/Data Analysis/Homework 3/data_for_analysis.csv", header=T)

install.packages("coin")
install.packages("pROC")
library(coin)
library(pROC)
data$outcome <- as.factor(data$outcome)
summary(data)


# QUESTION 1: Correlation Analysis Between Other Variables

# Define variable groups

lipid_vars <- c("lipids1", "lipids2", "lipids3", "lipids4", "lipids5")
hormone_vars <- c("hormone1", "hormone2", "hormone3", "hormone4", 
                  "hormone5", "hormone6", "hormone7", "hormone8", 
                  "hormone10_generated")
antioxidant_vars <- c("antioxidant1", "antioxidant2", "antioxidant3", 
                      "antioxidant4", "antioxidant5")
lipid_pero_vars <- c("lipid_pero1", "lipid_pero2", "lipid_pero3", 
                     "lipid_pero4", "lipid_pero5")

all_vars <- c(lipid_vars, hormone_vars, antioxidant_vars, lipid_pero_vars)

# Testing for normality of all variables
for(var in all_vars) {
  cat("\nNormality test for", var, "\n")
  print(shapiro.test(data[[var]]))
  
  hist(data[[var]], col = "lightblue", 
       main = paste("Histogram of", var))  
  qqnorm(data[[var]], main = paste("Q-Q Plot of", var))
  qqline(data[[var]], col = "red", lwd = 2)
}

# Spearman's correlation test between all pairs
cat("\nSpearman Correlation between all variables:\n")
for(i in 1:(length(all_vars)-1)) {
  for(j in (i+1):length(all_vars)) {
    result <- cor.test(data[[all_vars[i]]], data[[all_vars[j]]], method = "spearman")
    cat(sprintf("%s vs %s: rho = %.3f, p = %.4f\n", 
                all_vars[i], all_vars[j], result$estimate, result$p.value))
  }
}


# QUESTION 2: Table with Correlation Coefficients and Significance (Permutation Method)

# Data frame for results
results <- data.frame(
  var1 = character(),
  var2 = character(),
  spearman_corr = numeric(),
  s_p_value = numeric(),
  stringsAsFactors = FALSE
)

# Function for permutation-based Spearman correlation (replaces wPerm)
perm_spearman <- function(x, y, method = "spearman", R = 10000) {
  valid <- complete.cases(x, y)
  x_clean <- x[valid]
  y_clean <- y[valid]
  
  obs_cor <- cor(x_clean, y_clean, method = method)
  
  perm_cors <- numeric(R)
  for(i in 1:R) {
    perm_cors[i] <- cor(x_clean, sample(y_clean), method = method)
  }
  
  p_value <- mean(abs(perm_cors) >= abs(obs_cor))
  
  return(list(Observed = obs_cor, p.value = p_value))
}

# Main loop for all pairs
for(i in 1:(length(all_vars)-1)) {
  for(j in (i+1):length(all_vars)) {
    perm_result <- perm_spearman(
      x = data[[all_vars[i]]], 
      y = data[[all_vars[j]]],
      method = "spearman",
      R = 10000
    )
    
    results <- rbind(results, data.frame(
      var1 = all_vars[i],
      var2 = all_vars[j],
      spearman_corr = perm_result$Observed,
      s_p_value = perm_result$p.value
    ))
  }
}

# Output result
print("QUESTION 2: Correlation Coefficients with Permutation Significance (ALL VARIABLES)")
print(results)

# Filter significant results
sig_results <- results[results$s_p_value < 0.05, ]
print("Significant Correlations (p < 0.05):")
print(sig_results)


# QUESTION 3: Regression Analysis Between Other Variables

# Remove NAs first
df <- data[, c(x_var, y_var)]
df <- na.omit(df)
df <- df[order(df[[x_var]]), ]

# Linear regression
model_linear <- lm(as.formula(paste(y_var, "~", x_var)), data = df)

# Second degree polynomial
model_2 <- lm(as.formula(paste(y_var, "~ poly(", x_var, ", 2)")), data = df)

# Third degree polynomial
model_3 <- lm(as.formula(paste(y_var, "~ poly(", x_var, ", 3)")), data = df)

# Exponential dependence (y = a * e^(b*x))
# Correct: log(y) ~ x
model_exp <- lm(as.formula(paste("log(", y_var, ") ~", x_var)), data = df)

# Log dependence (y = a + b*log(x))
# Correct: y ~ log(x)
model_log <- lm(as.formula(paste(y_var, "~ log(", x_var, ")")), data = df)


# QUESTION 4: Select the Best Model (BIC)
rezult <- data.frame(
  model = c("model_linear", "model_2", "model_3", "model_exp", "model_log"), 
  BIC_value = c(BIC(model_linear), BIC(model_2), BIC(model_3), 
                BIC(model_exp), BIC(model_log))
)

rezult <- rezult[order(rezult$BIC_value), ]
print(rezult)

# CONCLUSION: Best Model Selection (BIC)

# Based on BIC comparison, model_linear has the lowest BIC value (-6964.082)
# indicating it is the best fitting model among all tested models.
# model_2 and model_3 have similar BIC values, suggesting polynomial models 
# do not substantially improve fit over the linear model.
# model_exp and model_log have much higher BIC values, indicating poor fit.

# The linear model is selected as the best model for describing the 
# relationship between the variables.


# QUESTION 5: Logistic Regression Models

# Fit logistic regression models using hormone variables to predict binary outcome,
# compare model performance via AIC/BIC, compute odds ratios, and summarize results.

# Check for NAs
sum(is.na(data$outcome))  
data <- data[!is.na(data$outcome), ]

# Create complete dataset (remove rows with any missing values in all variables)
data_complete <- data[complete.cases(data[, all_vars]), ]

# Model 1: Hormones only
model_logit_hormones <- glm(outcome ~ hormone1 + hormone2 + hormone3 + hormone4 + 
                              hormone5 + hormone6 + hormone7 + hormone8 + 
                              hormone10_generated, 
                            data = data_complete, family = binomial)
summary(model_logit_hormones)

# Model 2: Lipids only
model_logit_lipids <- glm(outcome ~ lipids1 + lipids2 + lipids3 + lipids4 + lipids5, 
                          data = data_complete, family = binomial)
summary(model_logit_lipids)

# Model 3: Antioxidants only
model_logit_antioxidants <- glm(outcome ~ antioxidant1 + antioxidant2 + antioxidant3 + 
                                  antioxidant4 + antioxidant5, 
                                data = data_complete, family = binomial)
summary(model_logit_antioxidants)

# Model 4: Lipid peroxidation only
model_logit_lipid_pero <- glm(outcome ~ lipid_pero1 + lipid_pero2 + lipid_pero3 + 
                                lipid_pero4 + lipid_pero5, 
                              data = data_complete, family = binomial)
summary(model_logit_lipid_pero)

# Model 5: All variables combined
model_logit_all <- glm(outcome ~ ., 
                       data = data_complete[, c("outcome", all_vars)], 
                       family = binomial)
summary(model_logit_all)

# Compare models via AIC/BIC
model_comparison <- data.frame(
  Model = c("Hormones", "Lipids", "Antioxidants", "Lipid_Peroxidation", "All_Variables"),
  AIC = c(AIC(model_logit_hormones), AIC(model_logit_lipids), 
          AIC(model_logit_antioxidants), AIC(model_logit_lipid_pero), 
          AIC(model_logit_all)),
  BIC = c(BIC(model_logit_hormones), BIC(model_logit_lipids), 
          BIC(model_logit_antioxidants), BIC(model_logit_lipid_pero), 
          BIC(model_logit_all))
)

print("Model Comparison (AIC/BIC):")
print(model_comparison)

# Stepwise variable selection on the full model
step_model <- step(model_logit_all, direction = "both", trace = 0)
summary(step_model)

# Select best model based on lowest AIC
best_model_name <- model_comparison$Model[which.min(model_comparison$AIC)]

# Predict using step model
data_complete$pred_prob <- predict(step_model, type = "response")
data_complete$pred_class <- ifelse(data_complete$pred_prob > 0.5, 1, 0)

# Confusion matrix
print("Confusion Matrix:")
confusion_mat <- table(Actual = data_complete$outcome, Predicted = data_complete$pred_class)
print(confusion_mat)

# Accuracy
accuracy <- sum(diag(confusion_mat)) / sum(confusion_mat)
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))

# ROC Curve and AUC
roc_curve <- roc(data_complete$outcome, data_complete$pred_prob)
plot(roc_curve, main = "ROC-Curve (Best Model)")
auc_value <- auc(roc_curve)
print(paste("AUC:", round(auc_value, 3)))

# Odds ratios for the best model (step_model)
print("Odds Ratios for Best Model:")
OR_table <- exp(cbind(OR = coef(step_model), confint(step_model)))
print(round(OR_table, 3))




