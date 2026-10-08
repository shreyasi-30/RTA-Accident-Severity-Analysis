# ============================================
# 08 - MODEL EVALUATION
# ============================================

# --------------------------------------------
# Accuracy Function
# --------------------------------------------

calculate_accuracy <- function(actual, predicted) {
  
  mean(actual == predicted)
}


# --------------------------------------------
# Decision Tree Accuracy
# --------------------------------------------

tree_accuracy <- calculate_accuracy(
  test_data$Accident_severity,
  tree_predictions
)


# --------------------------------------------
# Random Forest Accuracy
# --------------------------------------------

rf_accuracy <- calculate_accuracy(
  test_data$Accident_severity,
  rf_predictions
)


# --------------------------------------------
# Logistic Regression Accuracy
# --------------------------------------------

logistic_accuracy <- calculate_accuracy(
  test_data$Accident_severity,
  logistic_predictions
)


# --------------------------------------------
# Print Results
# --------------------------------------------

cat(
  "Decision Tree Accuracy:",
  round(tree_accuracy * 100, 2),
  "%\n"
)

cat(
  "Random Forest Accuracy:",
  round(rf_accuracy * 100, 2),
  "%\n"
)

cat(
  "Logistic Regression Accuracy:",
  round(logistic_accuracy * 100, 2),
  "%\n"
)