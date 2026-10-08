# ============================================
# 14 - SAVE FINAL RESULTS
# ============================================


# Save model comparison

write.csv(
  model_comparison,
  "results/final_model_comparison.csv",
  row.names = FALSE
)


# Save confusion matrices

tree_confusion <- table(
  Actual = test_data$Accident_severity,
  Predicted = tree_predictions
)

rf_confusion <- table(
  Actual = test_data$Accident_severity,
  Predicted = rf_predictions
)

logistic_confusion <- table(
  Actual = test_data$Accident_severity,
  Predicted = logistic_predictions
)


write.csv(
  as.data.frame(tree_confusion),
  "results/decision_tree_confusion_matrix.csv",
  row.names = FALSE
)

write.csv(
  as.data.frame(rf_confusion),
  "results/random_forest_confusion_matrix.csv",
  row.names = FALSE
)

write.csv(
  as.data.frame(logistic_confusion),
  "results/logistic_confusion_matrix.csv",
  row.names = FALSE
)


cat(
  "\nAll results saved successfully.\n"
)