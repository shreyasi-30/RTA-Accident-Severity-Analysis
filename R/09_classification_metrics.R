# ============================================
# 09 - PRECISION, RECALL AND F1 SCORE
# ============================================


# --------------------------------------------
# Function to calculate metrics
# --------------------------------------------

calculate_metrics <- function(actual, predicted) {
  
  classes <- levels(actual)
  
  precision_values <- c()
  recall_values <- c()
  f1_values <- c()
  
  for (class in classes) {
    
    TP <- sum(actual == class & predicted == class)
    
    FP <- sum(actual != class & predicted == class)
    
    FN <- sum(actual == class & predicted != class)
    
    
    precision <- ifelse(
      (TP + FP) == 0,
      0,
      TP / (TP + FP)
    )
    
    recall <- ifelse(
      (TP + FN) == 0,
      0,
      TP / (TP + FN)
    )
    
    f1 <- ifelse(
      (precision + recall) == 0,
      0,
      2 * precision * recall /
        (precision + recall)
    )
    
    precision_values <- c(
      precision_values,
      precision
    )
    
    recall_values <- c(
      recall_values,
      recall
    )
    
    f1_values <- c(
      f1_values,
      f1
    )
  }
  
  # Macro averages
  data.frame(
    Precision = mean(precision_values),
    Recall = mean(recall_values),
    F1_Score = mean(f1_values)
  )
}


# --------------------------------------------
# Metrics for each model
# --------------------------------------------

tree_metrics <- calculate_metrics(
  test_data$Accident_severity,
  tree_predictions
)

rf_metrics <- calculate_metrics(
  test_data$Accident_severity,
  rf_predictions
)

logistic_metrics <- calculate_metrics(
  test_data$Accident_severity,
  logistic_predictions
)


# --------------------------------------------
# Display
# --------------------------------------------

print(tree_metrics)

print(rf_metrics)

print(logistic_metrics)