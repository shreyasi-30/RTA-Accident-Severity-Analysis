# ============================================
# 13 - CONFUSION MATRICES
# ============================================


# Decision Tree
cat("\n============================\n")
cat("DECISION TREE\n")
cat("============================\n")

print(
  table(
    Actual = test_data$Accident_severity,
    Predicted = tree_predictions
  )
)


# Random Forest
cat("\n============================\n")
cat("RANDOM FOREST\n")
cat("============================\n")

print(
  table(
    Actual = test_data$Accident_severity,
    Predicted = rf_predictions
  )
)


# Logistic Regression
cat("\n============================\n")
cat("LOGISTIC REGRESSION\n")
cat("============================\n")

print(
  table(
    Actual = test_data$Accident_severity,
    Predicted = logistic_predictions
  )
)