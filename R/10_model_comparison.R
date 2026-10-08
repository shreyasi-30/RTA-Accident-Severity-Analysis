# ============================================
# 10 - MODEL COMPARISON
# ============================================


# Create comparison table

model_comparison <- data.frame(
  
  Model = c(
    "Decision Tree",
    "Random Forest",
    "Multinomial Logistic Regression"
  ),
  
  Accuracy = c(
    tree_accuracy,
    rf_accuracy,
    logistic_accuracy
  ),
  
  Precision = c(
    tree_metrics$Precision,
    rf_metrics$Precision,
    logistic_metrics$Precision
  ),
  
  Recall = c(
    tree_metrics$Recall,
    rf_metrics$Recall,
    logistic_metrics$Recall
  ),
  
  F1_Score = c(
    tree_metrics$F1_Score,
    rf_metrics$F1_Score,
    logistic_metrics$F1_Score
  )
)


# Round values
model_comparison[, 2:5] <-
  round(model_comparison[, 2:5], 4)


# Display
print(model_comparison)


# --------------------------------------------
# Save results
# --------------------------------------------

write.csv(
  model_comparison,
  "results/model_comparison.csv",
  row.names = FALSE
)