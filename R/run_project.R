# ============================================================
# RUN COMPLETE RTA ACCIDENT SEVERITY PROJECT
# ORIGINAL BASELINE VERSION
# ============================================================

cat("\n")
cat("============================================================\n")
cat(" RTA ACCIDENT SEVERITY PREDICTION PROJECT\n")
cat(" ORIGINAL BASELINE MODEL\n")
cat("============================================================\n")
cat("\n")


# ------------------------------------------------------------
# FILE 01: DATA LOADING
# ------------------------------------------------------------

cat("\n>>> Running File 01: Data Loading...\n")

source("R/01_data_loading.R")


# ------------------------------------------------------------
# FILE 02: DATA CLEANING
# ------------------------------------------------------------

cat("\n>>> Running File 02: Data Cleaning...\n")

source("R/02_data_cleaning.R")


# ------------------------------------------------------------
# FILE 03: EDA
# ------------------------------------------------------------

cat("\n>>> Running File 03: Exploratory Data Analysis...\n")

source("R/03_EDA.R")


# ------------------------------------------------------------
# FILE 04: TRAIN TEST SPLIT
# ------------------------------------------------------------

cat("\n>>> Running File 04: Train-Test Split...\n")

source("R/04_train_test_split.R")


# ------------------------------------------------------------
# FILE 05: DECISION TREE
# ------------------------------------------------------------

cat("\n>>> Running File 05: Decision Tree...\n")

source("R/05_decision_tree.R")


# ------------------------------------------------------------
# FILE 06: RANDOM FOREST
# ------------------------------------------------------------

cat("\n>>> Running File 06: Random Forest...\n")

source("R/06_random_forest.R")


# ------------------------------------------------------------
# FILE 07: LOGISTIC REGRESSION
# ------------------------------------------------------------

cat("\n>>> Running File 07: Logistic Regression...\n")

source("R/07_logistic_regression.R")


# ------------------------------------------------------------
# FILE 08: MODEL EVALUATION
# ------------------------------------------------------------

cat("\n>>> Running File 08: Model Evaluation...\n")

source("R/08_model_evaluation.R")


# ------------------------------------------------------------
# FILE 09: CLASSIFICATION METRICS
# ------------------------------------------------------------

cat("\n>>> Running File 09: Classification Metrics...\n")

source("R/09_classification_metrics.R")


# ------------------------------------------------------------
# FILE 10: MODEL COMPARISON
# ------------------------------------------------------------

cat("\n>>> Running File 10: Model Comparison...\n")

source("R/10_model_comparison.R")


# ------------------------------------------------------------
# FILE 11: MODEL COMPARISON PLOT
# ------------------------------------------------------------

cat("\n>>> Running File 11: Model Comparison Plot...\n")

source("R/11_model_comparison_plot.R")


# ------------------------------------------------------------
# FILE 12: FEATURE IMPORTANCE
# ------------------------------------------------------------

cat("\n>>> Running File 12: Feature Importance...\n")

source("R/12_feature_importance.R")


# ------------------------------------------------------------
# FILE 13: CONFUSION MATRICES
# ------------------------------------------------------------

cat("\n>>> Running File 13: Confusion Matrices...\n")

source("R/13_confusion_matrices.R")


# ------------------------------------------------------------
# FILE 14: SAVE RESULTS
# ------------------------------------------------------------

cat("\n>>> Running File 14: Save Results...\n")

source("R/14_save_results.R")


# ------------------------------------------------------------
# PROJECT COMPLETED
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" PROJECT COMPLETED SUCCESSFULLY\n")
cat("============================================================\n")
cat("\n")

cat("Original baseline pipeline completed.\n")
cat("Results have been saved in the 'results' folder.\n")

cat("\n")
cat("Models used:\n")
cat("1. Decision Tree\n")
cat("2. Random Forest\n")
cat("3. Multinomial Logistic Regression\n")

cat("\n")
cat("============================================================\n")