# SCRIPT PAPER: "Explainable Machine Learning Models for Age-at-Death Estimation Using Entheseal Changes: A Data-Driven Analysis of a Mediterranean Adult Sample."

# 1. Kappa de Cohen analysis

# Load necessary libraries
library(readxl)  # For reading Excel files
library(irr)     # For calculating inter-rater reliability statistics

# Define the sheet name for each insertion
sheet_name <- "subescapularis"  # Options include: "subescapularis", "supraspinatus", "brachial biceps", 
# "common flexor", "common extender", "gluteus medius", "gluteus minimus"

# Load the first dataset (named "first_obs")
# Prompt user to choose the file path for the first dataset
first_dataset_path <- file.choose()

# Read the first dataset from the specified Excel file and sheet
first_obs <- read_excel(first_dataset_path, sheet = sheet_name)

# Load the second dataset (named "second_obs")
# Prompt user to choose the file path for the second dataset
second_dataset_path <- file.choose(new = FALSE)

# Read the second dataset from the specified Excel file and sheet
second_obs <- read_excel(second_dataset_path, sheet = sheet_name)

# Select relevant columns for analysis
first_obs_column <- first_obs$SB_D_Z1_BF  # Column code for the first observation
second_obs_column <- second_obs$SB_D_Z1_BF  # Column code for the second observation

# Create a matrix from the selected columns for kappa calculation
obs_matrix <- matrix(c(first_obs_column, second_obs_column), ncol = 2)

# Calculate the Kappa coefficient to assess inter-rater reliability
kappa_result <- kappa2(obs_matrix)

# Print the Kappa result
print(kappa_result)

# 2. Exploratory data analysis (EAD)

# Load necessary libraries
library(readxl)   # For reading Excel files
library(dplyr)    # For data manipulation
library(ggpubr)   # For creating Q-Q plots and combining visualizations
library(nortest)  # For additional normality tests

# Define dataset path and initialize sheet name for analysis
dataset_path <- file.choose()  # Prompt user to select the dataset file
sheet_name <- "gluteus minimus"  # Example sheet: replace with desired insertion ("subescapularis", "supraspinatus", etc.)

# Load dataset
data_set <- read_excel(dataset_path, sheet = sheet_name)

# Descriptive Statistics - Total Score by Sex
total_score_by_sex <- data_set %>%
  group_by(sex) %>%
  summarise(
    Count = n(),
    Mean_total_score = mean(`total score`, na.rm = TRUE),
    SD_total_score = sd(`total score`, na.rm = TRUE),
    Max_total_score = max(`total score`, na.rm = TRUE),
    Min_total_score = min(`total score`, na.rm = TRUE)
  )
print(total_score_by_sex)

# Correlation Analysis: Shapiro-Wilk Test for Normality
# Remove NAs and check normality for 'age' and 'total score'
age_data <- na.omit(data_set$age)
shapiro_test_age <- shapiro.test(age_data)
print(shapiro_test_age)

total_score_data <- na.omit(data_set$`total score`)
shapiro_test_total_score <- shapiro.test(total_score_data)
print(shapiro_test_total_score)

# Q-Q Plots for Normality Visualization
qq_age <- ggplot(data_set, aes(sample = age)) +
  stat_qq(color = "#C1FFC1", shape = 20, size = 3, alpha = 0.7) +
  stat_qq_line() +
  theme_light(base_size = 10) +
  labs(title = "Q-Q Plot for Age",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles")

qq_total_score <- ggplot(data_set, aes(sample = `total score`)) +
  stat_qq(color = "#40E0D0", shape = 20, size = 3, alpha = 0.7) +
  stat_qq_line() +
  theme_light(base_size = 10) +
  labs(title = "Q-Q Plot for Total Score",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles")

# Combine Q-Q Plots
combined_qq_plot <- ggarrange(qq_age, qq_total_score, 
                              ncol = 2, nrow = 1, 
                              common.legend = TRUE, legend = "right")
print(combined_qq_plot)

# Correlation Analysis: Spearman Test
# Function to calculate Spearman correlation by sex
calculate_spearman <- function(data, variable_x, variable_y) {
  female_data <- data %>% filter(sex == "Female")
  male_data <- data %>% filter(sex == "Male")
  
  spearman_female <- cor.test(female_data[[variable_x]], female_data[[variable_y]], method = "spearman", exact = FALSE)
  spearman_male <- cor.test(male_data[[variable_x]], male_data[[variable_y]], method = "spearman", exact = FALSE)
  
  list("Spearman Correlation - Female" = spearman_female,
       "Spearman Correlation - Male" = spearman_male)
}

# Run Spearman Correlation for 'age' vs. 'total score'
spearman_results <- calculate_spearman(data_set, "age", "total score")
print(spearman_results)

# Required Libraries
install.packages("extrafont")
library(extrafont)
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)

# Import and Configure Fonts
font_import(prompt = FALSE)
loadfonts(device = "quartz")  # Use "win" for Windows
loadfonts(device = "postscript")

# Dataset Path and Sheet Selection
dataset_path <- file.choose()
sheet_name <- "supraspinatus"  # Change according to the muscle insertion being analyzed

# Load Data
data_set <- read_excel(dataset_path, sheet = sheet_name)

# Plot 1: Correlation Between Age and Total Score
ggplot(data_set, aes(x = age, y = `total score`)) +
  geom_point(aes(color = age), size = 3, alpha = 0.7) +
  scale_color_gradientn(colors = c("#C1FFC1", "#40E0D0"),
                        values = rescale(c(min(data_set$age), 50, max(data_set$age))),
                        limits = c(min(data_set$age), max(data_set$age))) +
  theme_light(base_family = "Times New Roman") +
  labs(title = "Supraspinatus: Correlation Between Age and Total Score",
       x = "Age",
       y = "Total Score",
       color = "Age") +
  theme(plot.title = element_text(hjust = 0.5))

# Plot 2: Correlation by Sex
filtered_df <- data_set %>%
  filter(sex %in% c("Female", "Male"))

scatter_plot <- ggplot(filtered_df, aes(x = age, y = `total score`, color = sex, shape = sex)) +
  geom_point(alpha = 0.7, size = 3) +
  scale_color_manual(values = c("Male" = "#E28394", "Female" = "#84D7E1")) +
  scale_shape_manual(values = c(19, 17)) +
  labs(title = "Correlation Between Age and Total Score by Sex",
       x = "Age",
       y = "Total Score",
       color = "Sex", shape = "Sex") +
  theme_light(base_size = 10) +
  theme(text = element_text(family = "Times New Roman"),
        legend.position = "right")

print(scatter_plot)

# Plot 3: Average Component Scores by Age
# Load All Sheets from the File
sheet_names <- excel_sheets(dataset_path)
all_data <- lapply(sheet_names, function(sheet) {
  read_excel(dataset_path, sheet = sheet)
}) %>% bind_rows()

# Consolidate Component Columns
components <- c("BF", "E", "TC", "FPO", "MPO", "CA")
for (component in components) {
  component_cols <- grep(component, names(all_data), value = TRUE)
  all_data[[paste0("mean_", component)]] <- rowMeans(all_data[, component_cols, drop = FALSE], na.rm = TRUE)
}

# Aggregate Data by Age
aggregated_data <- all_data %>%
  group_by(age) %>%
  summarise(across(starts_with("mean_"), mean, na.rm = TRUE))

# Component Plot
ggplot(aggregated_data, aes(x = age)) +
  geom_line(aes(y = mean_BF, color = "BF"), size = 1) +
  geom_line(aes(y = mean_E, color = "E"), size = 1) +
  geom_line(aes(y = mean_TC, color = "TC"), size = 1) +
  geom_line(aes(y = mean_FPO, color = "FPO"), size = 1) +
  geom_line(aes(y = mean_MPO, color = "MPO"), size = 1) +
  geom_line(aes(y = mean_CA, color = "CA"), size = 1) +
  labs(title = "Average Component Expression by Age",
       x = "Age",
       y = "Average Expression",
       color = "Component") +
  scale_color_manual(values = c("BF" = "#F992AD", "E" = "#87CEFF", 
                                "TC" = "#A480F2", "FPO" = "#20B2AA", 
                                "MPO" = "#96CDCD", "CA" = "#EEAEEE")) +
  theme_light() +
  theme(text = element_text(family = "Times New Roman", size = 10))


# 3. Percentile analysis

# Required Libraries
library(readxl)
library(dplyr)
library(boot)

# Load Excel File and Sheets
file_path <- file.choose()  # Prompt to choose the file
sheets <- excel_sheets(file_path)

# Load all sheets into a list of data frames and combine them into a single data frame
data_list <- lapply(sheets, function(sheet) {
  read_excel(file_path, sheet = sheet)
})
combined_data <- bind_rows(data_list)

# Ensure columns are named consistently
# Adjust names if necessary (e.g., handling inconsistent capitalization or extra spaces)
names(combined_data) <- tolower(gsub(" ", "_", names(combined_data)))  # Convert to lowercase and replace spaces with underscores

# Calculate Percentiles for Age and Total Score
age_percentiles <- quantile(combined_data$age, probs = seq(0, 1, by = 0.1), na.rm = TRUE)
total_score_percentiles <- quantile(combined_data$total_score, probs = seq(0, 1, by = 0.1), na.rm = TRUE)

# Print Percentile Results
cat("Percentiles for Age:\n")
print(age_percentiles)
cat("\nPercentiles for Total Score:\n")
print(total_score_percentiles)

# Function for Bootstrapped Confidence Intervals for Percentiles
bootstrap_percentiles <- function(data, probs, R = 1000, alpha = 0.05) {
  # Arguments:
  # data: Numeric vector of values
  # probs: Numeric vector of percentiles to calculate (e.g., 0.1, 0.5)
  # R: Number of bootstrap iterations
  # alpha: Significance level for confidence intervals
  
  n <- length(data)
  percentiles <- matrix(NA, nrow = R, ncol = length(probs))
  
  # Bootstrap iterations
  for (i in seq_len(R)) {
    sample <- sample(data, size = n, replace = TRUE)  # Resample with replacement
    percentiles[i, ] <- quantile(sample, probs = probs, na.rm = TRUE)
  }
  
  # Confidence Intervals
  CI_lower <- apply(percentiles, 2, quantile, probs = alpha / 2, na.rm = TRUE)
  CI_upper <- apply(percentiles, 2, quantile, probs = 1 - alpha / 2, na.rm = TRUE)
  
  return(list(lower = CI_lower, upper = CI_upper))
}

# Apply Bootstrapped CI Calculation
probs <- seq(0, 1, by = 0.1)  # Percentiles from 0% to 100%
age_CI <- bootstrap_percentiles(combined_data$age, probs, R = 1000, alpha = 0.05)
total_score_CI <- bootstrap_percentiles(combined_data$total_score, probs, R = 1000, alpha = 0.05)

# Print Confidence Interval Results
cat("\nConfidence Intervals for Age Percentiles:\n")
for (i in seq_along(probs)) {
  cat(sprintf("%dth Percentile: %.2f - %.2f\n", probs[i] * 100, age_CI$lower[i], age_CI$upper[i]))
}

cat("\nConfidence Intervals for Total Score Percentiles:\n")
for (i in seq_along(probs)) {
  cat(sprintf("%dth Percentile: %.2f - %.2f\n", probs[i] * 100, total_score_CI$lower[i], total_score_CI$upper[i]))
}


# 3. Decision rules system - leave one out cross validation

# Load necessary libraries
library(readxl)
library(dplyr)
library(caret)
library(ggplot2)
library(MLmetrics)

# --- PART 1: DATA LOADING AND PREPARATION ---
# Load all sheets from the Excel file and combine them into a single dataframe
file_path <- "/Users/noeaedonoa/Desktop/ENTHESEAL CHANGES | 2023 | GRANADA | DATASET OFFICIAL copy.xlsx"
sheets <- excel_sheets(file_path)
data_list <- lapply(sheets, function(sheet) read_excel(file_path, sheet = sheet))
dataset <- bind_rows(data_list)

# Display an initial summary of the data
print(head(dataset))
summary(dataset[, c("age", "total score")])

# Visualize distributions for "age" and "total score"
ggplot(dataset, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = 'lightblue', color = 'black', alpha = 0.7) +
  theme_minimal() +
  labs(title = "Age Distribution", x = "Age", y = "Frequency") +
  ggsave("age_distribution.png")

ggplot(dataset, aes(x = `total score`)) +
  geom_histogram(binwidth = 2, fill = 'lightgreen', color = 'black', alpha = 0.7) +
  theme_minimal() +
  labs(title = "Total Score Distribution", x = "Total Score", y = "Frequency") +
  ggsave("score_distribution.png")

# --- PART 2: RULE DEFINITIONS AND EVALUATION FUNCTION ---
# Confidence intervals (CI) for each rule based on predefined percentiles
ci_intervals <- list(
  rule_1 = list(total_score = c(9, 10), age = c(68, 68)),
  rule_2 = list(total_score = c(11, 14), age = c(69, 79)),
  rule_3 = list(total_score = c(14, 24), age = c(80, 96))
)

# Generalized function to evaluate if a data point matches the criteria of a rule
evaluate_rule <- function(total_score, age, ci_total, ci_age) {
  within_total <- total_score >= ci_total[1] & total_score <= ci_total[2]
  within_age <- age >= ci_age[1] & age <= ci_age[2]
  return(within_total & within_age)
}

# --- PART 3: LEAVE-ONE-OUT VALIDATION (LOOCV) ---
# Function to compute F1 Score
calculate_f1_score <- function(true_labels, predicted_labels) {
  precision <- ifelse(sum(predicted_labels) > 0, sum(true_labels & predicted_labels) / sum(predicted_labels), 0)
  recall <- sum(true_labels & predicted_labels) / sum(true_labels)
  if (precision + recall > 0) {
    return(2 * (precision * recall) / (precision + recall))
  } else {
    return(NA)
  }
}

# Perform LOOCV for a given rule
loo_validation <- function(data, ci_total, ci_age) {
  f1_scores <- numeric(nrow(data))
  
  for (i in 1:nrow(data)) {
    test_data <- data[i, ]
    train_data <- data[-i, ]
    
    true_label <- evaluate_rule(test_data$`total score`, test_data$age, ci_total, ci_age)
    predictions <- apply(train_data, 1, function(row) {
      evaluate_rule(as.numeric(row["total score"]), as.numeric(row["age"]), ci_total, ci_age)
    })
    
    f1_scores[i] <- calculate_f1_score(true_label, predictions)
  }
  
  return(mean(f1_scores, na.rm = TRUE))
}

# Compute F1 Scores for each rule
f1_scores <- sapply(names(ci_intervals), function(rule_name) {
  ci <- ci_intervals[[rule_name]]
  loo_validation(dataset, ci$total_score, ci$age)
})

# Display results
cat("Average F1 Scores for each rule:\n")
for (i in seq_along(f1_scores)) {
  cat(names(f1_scores)[i], ":", f1_scores[i], "\n")
}

# --- PART 4: CONFUSION MATRIX ANALYSIS ---
# Generate confusion matrices for each rule
confusion_matrices <- lapply(names(ci_intervals), function(rule_name) {
  ci <- ci_intervals[[rule_name]]
  predictions <- apply(dataset, 1, function(row) {
    evaluate_rule(as.numeric(row["total score"]), as.numeric(row["age"]), ci$total_score, ci$age)
  })
  true_labels <- dataset$age >= ci$age[1] & dataset$age <= ci$age[2]
  confusionMatrix(as.factor(predictions), as.factor(true_labels))
})

# Display confusion matrices
for (i in seq_along(confusion_matrices)) {
  cat("\nConfusion Matrix for", names(confusion_matrices)[i], ":\n")
  print(confusion_matrices[[i]])
}

# Function to plot confusion matrices
plot_confusion_matrix <- function(conf_matrix, title) {
  df <- as.data.frame(as.table(conf_matrix$table))
  ggplot(df, aes(Reference, Prediction, fill = Freq)) +
    geom_tile() +
    geom_text(aes(label = Freq), color = "black", size = 4) +
    scale_fill_gradient(low = "#cce5ff", high = "#ffb3b3") +
    theme_minimal() +
    labs(title = title, x = "True Labels", y = "Predicted Labels")
}

# Plot confusion matrices for each rule
for (i in seq_along(confusion_matrices)) {
  plot <- plot_confusion_matrix(confusion_matrices[[i]], paste("Confusion Matrix:", names(confusion_matrices)[i]))
  print(plot)
}

# Save confusion matrix plots
ggsave("conf_matrix_rule_1.png", plot_confusion_matrix(confusion_matrices[[1]], "Confusion Matrix Rule 1"))
ggsave("conf_matrix_rule_2.png", plot_confusion_matrix(confusion_matrices[[2]], "Confusion Matrix Rule 2"))
ggsave("conf_matrix_rule_3.png", plot_confusion_matrix(confusion_matrices[[3]], "Confusion Matrix Rule 3"))


# 4. Probabilistic analysis - Naïve Bayes algorithm

# --- PART 1: LIBRARIES AND DATA LOADING ---
# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)

# File path to the dataset
file_path <- "/Users/noeaedonoa/Desktop/ENTHESEAL CHANGES | 2023 | GRANADA | DATASET OFFICIAL copy.xlsx"
sheet_name <- "subescapular"  # Specify the sheet to load

# Load the specified sheet into a dataframe
data <- read_excel(file_path, sheet = sheet_name)

# --- PART 2: AGE INTERVALS AND FREQUENCIES ---
# Define age intervals
data <- data %>%
  mutate(age_interval = case_when(
    age >= 20 & age <= 29 ~ '20-29',
    age >= 30 & age <= 39 ~ '30-39',
    age >= 40 & age <= 49 ~ '40-49',
    age >= 50 & age <= 59 ~ '50-59',
    age >= 60 & age <= 69 ~ '60-69',
    age >= 70 & age <= 79 ~ '70-79',
    age >= 80 & age <= 89 ~ '80-89',
    age >= 90 & age <= 99 ~ '90-99',
    TRUE ~ 'Other'
  ))

# Calculate frequencies and prior probabilities
frequencies <- data %>%
  filter(age_interval != 'Other') %>%
  group_by(age_interval) %>%
  summarise(count = n()) %>%
  mutate(prior_probability = count / sum(count))

# Display the frequencies and prior probabilities
print(frequencies)

# --- PART 3: AGE CATEGORIES ---
# Define broader age categories
frequencies <- frequencies %>%
  mutate(age_category = case_when(
    age_interval %in% c('20-29', '30-39', '40-49') ~ 'Young',
    age_interval %in% c('50-59', '60-69') ~ 'Middle',
    age_interval %in% c('70-79', '80-89', '90-99') ~ 'Older'
  ))

# Aggregate data by age category
age_category_totals <- frequencies %>%
  group_by(age_category) %>%
  summarise(
    total_count = sum(count),
    total_prior_probability = sum(prior_probability)
  )

# Display totals by age category
print(age_category_totals)

# --- PART 4: VISUALIZATIONS ---
# Line chart for prior probabilities by age interval
line_chart <- ggplot(frequencies, aes(x = age_interval, y = prior_probability, group = 1)) +
  geom_line(color = '#81d6e3', size = 1) +
  geom_point(color = '#ffc1c1', size = 2) +
  geom_area(fill = '#81d6e3', alpha = 0.3) +
  theme_light(base_family = 'Times New Roman', base_size = 11) +
  labs(
    title = 'Prior Probabilities by Age Interval',
    x = 'Age Interval',
    y = 'Prior Probability'
  ) +
  theme(plot.title = element_text(hjust = 0.5))

# Display the line chart
print(line_chart)

# Bar chart for age categories
bar_chart <- ggplot(age_category_totals, aes(x = age_category, y = total_count, fill = age_category)) +
  geom_bar(stat = 'identity', color = 'black') +
  scale_fill_manual(values = c('Young' = '#81d6e3', 'Middle' = '#ffc1c1', 'Older' = '#81d6e3')) +
  theme_minimal(base_family = 'Times New Roman', base_size = 11) +
  labs(
    title = 'Total Individuals by Age Category',
    x = 'Age Category',
    y = 'Total Individuals'
  ) +
  theme(plot.title = element_text(hjust = 0.5))

# Display the bar chart
print(bar_chart)

# --- PART 5: OPTIONAL SAVING OF PLOTS ---
# Save plots as images for reporting or publication
ggsave("prior_probabilities_line_chart.png", line_chart, width = 8, height = 6)
ggsave("age_categories_bar_chart.png", bar_chart, width = 8, height = 6)

# Comprehensive Analysis for Entheseal Changes
# Required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  readxl,
  dplyr,
  tidyr,
  pROC,
  ggplot2,
  gridExtra
)

#' Load and Combine Excel Sheets
#' 
#' @param file_path Path to Excel file
#' @return Combined dataframe
load_excel_data <- function(file_path) {
  sheet_names <- excel_sheets(file_path)
  data_list <- lapply(sheet_names, function(sheet) {
    read_excel(file_path, sheet = sheet)
  })
  bind_rows(data_list)
}

#' Calculate Log Loss
#' 
#' @param y_true True labels
#' @param y_pred Predicted probabilities
#' @return Log loss value
calculate_log_loss <- function(y_true, y_pred) {
  epsilon <- 1e-15  # Small constant to avoid log(0)
  y_pred <- pmax(pmin(y_pred, 1 - epsilon), epsilon)
  -mean(y_true * log(y_pred) + (1 - y_true) * log(1 - y_pred))
}

# Define confusion matrices
confusion_matrices <- list(
  Young = list(TP = 55, FN = 9, FP = 9, TN = 66),
  Middle = list(TP = 28, FN = 23, FP = 13, TN = 75),
  Older = list(TP = 24, FN = 17, FP = 10, TN = 88)
)

# Calculate metrics for each age group
calculate_group_metrics <- function(cm) {
  list(
    sensitivity = cm$TP / (cm$TP + cm$FN),
    specificity = cm$TN / (cm$TN + cm$FP),
    ppv = cm$TP / (cm$TP + cm$FP),
    npv = cm$TN / (cm$TN + cm$FN),
    accuracy = (cm$TP + cm$TN) / (cm$TP + cm$TN + cm$FP + cm$FN),
    f1_score = 2 * (cm$TP / (cm$TP + cm$FP)) * (cm$TP / (cm$TP + cm$FN)) / 
      ((cm$TP / (cm$TP + cm$FP)) + (cm$TP / (cm$TP + cm$FN)))
  )
}

metrics <- lapply(confusion_matrices, calculate_group_metrics)

# Calculate prior probabilities
total_samples <- sum(sapply(confusion_matrices, function(x) x$TP + x$FN))
prior_prob <- sapply(confusion_matrices, function(x) (x$TP + x$FN) / total_samples)

# Calculate likelihood ratios
likelihood_ratios <- sapply(confusion_matrices, function(x) {
  sensitivity <- x$TP / (x$TP + x$FN)
  specificity <- x$TN / (x$TN + x$FP)
  sensitivity / (1 - specificity)
})

# Calculate posterior probabilities
posterior_prob <- prior_prob * likelihood_ratios
posterior_prob <- posterior_prob / sum(posterior_prob)

# Prepare data for density plots
create_prediction_data <- function(confusion_matrices, posterior_prob) {
  predictions <- data.frame()
  
  for (age in names(confusion_matrices)) {
    cm <- confusion_matrices[[age]]
    prob <- posterior_prob[age]
    
    # True positives
    tp_data <- data.frame(
      y_pred = rep(prob, cm$TP),
      y_true = rep(1, cm$TP),
      age = rep(age, cm$TP)
    )
    
    # False negatives
    fn_data <- data.frame(
      y_pred = rep(prob, cm$FN),
      y_true = rep(0, cm$FN),
      age = rep(age, cm$FN)
    )
    
    # False positives
    fp_data <- data.frame(
      y_pred = rep(1 - prob, cm$FP),
      y_true = rep(1, cm$FP),
      age = rep(age, cm$FP)
    )
    
    # True negatives
    tn_data <- data.frame(
      y_pred = rep(1 - prob, cm$TN),
      y_true = rep(0, cm$TN),
      age = rep(age, cm$TN)
    )
    
    predictions <- rbind(predictions, tp_data, fn_data, fp_data, tn_data)
  }
  
  predictions
}

# Create prediction data
predictions <- create_prediction_data(confusion_matrices, posterior_prob)

# Calculate log loss for each age group
log_loss_values <- sapply(names(confusion_matrices), function(age) {
  group_data <- subset(predictions, age == age)
  calculate_log_loss(group_data$y_true, group_data$y_pred)
})
names(log_loss_values) <- names(confusion_matrices)

# Create density plot
density_plot <- ggplot(predictions, aes(x = y_pred, fill = age)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ age) +
  labs(title = "Density of Predicted Probabilities by Age Group",
       x = "Predicted Probability",
       y = "Density") +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman", size = 11),
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  ) +
  scale_fill_manual(values = c("#81d6e3", "#81d6e3", "#81d6e3"))

# Display density plot
print(density_plot)

# Print results
print("Prior Probabilities:")
print(prior_prob)

print("\
Likelihood Ratios:")
print(likelihood_ratios)

print("\
Posterior Probabilities:")
print(posterior_prob)

print("\
Log Loss Values:")
print(log_loss_values)

# Create ROC curves
par(mfrow = c(1, 3))
for (age in names(confusion_matrices)) {
  cm <- confusion_matrices[[age]]
  y_true <- c(rep(1, cm$TP), rep(0, cm$FN), rep(0, cm$FP), rep(1, cm$TN))
  y_pred <- c(rep(posterior_prob[age], cm$TP), 
              rep(posterior_prob[age], cm$FN),
              rep(1 - posterior_prob[age], cm$FP),
              rep(1 - posterior_prob[age], cm$TN))
  
  roc_obj <- roc(y_true, y_pred)
  plot(roc_obj, 
       main = paste("ROC Curve -", age),
       col = "#81d6e3",
       cex.main = 1.1,
       family = "Times New Roman")
}

# Reset plot layout
par(mfrow = c(1, 1))

# Store all results
results <- list(
  prior_probabilities = prior_prob,
  likelihood_ratios = likelihood_ratios,
  posterior_probabilities = posterior_prob,
  metrics = metrics,
  log_loss_values = log_loss_values,
  predictions = predictions
)
