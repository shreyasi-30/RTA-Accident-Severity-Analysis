# ============================================================
# FILE 15: FINAL MODEL EVALUATION
# FINAL OPTIMIZED RANDOM FOREST
# ============================================================

cat("\n")
cat("============================================================\n")
cat(" FINAL MODEL EVALUATION\n")
cat(" OPTIMIZED RANDOM FOREST\n")
cat("============================================================\n\n")


# ------------------------------------------------------------
# 1. LOAD REQUIRED PACKAGES
# ------------------------------------------------------------

library(ggplot2)
library(randomForest)


# ------------------------------------------------------------
# 2. LOAD DATA
# ------------------------------------------------------------

cat("Loading data...\n")

source("R/01_data_loading.R")
source("R/02_data_cleaning.R")
source("R/04_train_test_split.R")


# ------------------------------------------------------------
# 3. LOAD FINAL OPTIMIZED RANDOM FOREST
# ------------------------------------------------------------

cat("\nLoading final optimized Random Forest...\n")

final_rf <- readRDS(
  "results/optimized_random_forest_model.rds"
)


# ------------------------------------------------------------
# 4. PREDICT TEST DATA
# ------------------------------------------------------------

cat("Generating predictions...\n")

final_predictions <- predict(
  final_rf,
  newdata = test_data
)


# ------------------------------------------------------------
# 5. CONFUSION MATRIX
# ------------------------------------------------------------

final_cm <- table(
  Actual = test_data$Accident_severity,
  Predicted = final_predictions
)


cat("\n")
cat("============================================================\n")
cat(" FINAL CONFUSION MATRIX\n")
cat("============================================================\n")

print(final_cm)


# ------------------------------------------------------------
# 6. OVERALL ACCURACY
# ------------------------------------------------------------

final_accuracy <- mean(
  final_predictions == test_data$Accident_severity
)


cat("\n")
cat("============================================================\n")
cat(" FINAL ACCURACY\n")
cat("============================================================\n")

cat(
  "Final Optimized Random Forest Accuracy:",
  round(final_accuracy * 100, 2),
  "%\n"
)


# ------------------------------------------------------------
# 7. PER-CLASS METRICS
# ------------------------------------------------------------

classes <- levels(test_data$Accident_severity)

metrics_list <- list()


for (class_name in classes) {
  
  TP <- final_cm[class_name, class_name]
  
  FP <- sum(final_cm[, class_name]) - TP
  
  FN <- sum(final_cm[class_name, ]) - TP
  
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
  
  metrics_list[[class_name]] <- data.frame(
    Class = class_name,
    Precision = precision,
    Recall = recall,
    F1_Score = f1
  )
}


class_metrics <- do.call(
  rbind,
  metrics_list
)

rownames(class_metrics) <- NULL


# ------------------------------------------------------------
# 8. MACRO AVERAGE
# ------------------------------------------------------------

macro_precision <- mean(
  class_metrics$Precision
)

macro_recall <- mean(
  class_metrics$Recall
)

macro_f1 <- mean(
  class_metrics$F1_Score
)


cat("\n")
cat("============================================================\n")
cat(" PER-CLASS PERFORMANCE\n")
cat("============================================================\n")

print(class_metrics)


cat("\n")
cat("============================================================\n")
cat(" MACRO AVERAGE\n")
cat("============================================================\n")

cat(
  "Macro Precision:",
  round(macro_precision, 4),
  "\n"
)

cat(
  "Macro Recall:",
  round(macro_recall, 4),
  "\n"
)

cat(
  "Macro F1:",
  round(macro_f1, 4),
  "\n"
)


# ------------------------------------------------------------
# 9. FEATURE IMPORTANCE
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" FEATURE IMPORTANCE\n")
cat("============================================================\n")


importance_matrix <- importance(final_rf)

feature_importance <- data.frame(
  Feature = rownames(importance_matrix),
  Importance = importance_matrix[, "MeanDecreaseGini"]
)

feature_importance <- feature_importance[
  order(-feature_importance$Importance),
]

print(feature_importance)


# ------------------------------------------------------------
# 10. CREATE FINAL DIRECTORIES
# ------------------------------------------------------------

if (!dir.exists("results/final")) {
  dir.create(
    "results/final",
    recursive = TRUE
  )
}

if (!dir.exists("plots/final")) {
  dir.create(
    "plots/final",
    recursive = TRUE
  )
}


# ------------------------------------------------------------
# 11. SAVE CONFUSION MATRIX
# ------------------------------------------------------------

write.csv(
  as.data.frame.matrix(final_cm),
  "results/final/final_confusion_matrix.csv"
)


# ------------------------------------------------------------
# 12. SAVE CLASS METRICS
# ------------------------------------------------------------

write.csv(
  class_metrics,
  "results/final/final_class_metrics.csv",
  row.names = FALSE
)


# ------------------------------------------------------------
# 13. SAVE OVERALL METRICS
# ------------------------------------------------------------

overall_metrics <- data.frame(
  Model = "Optimized Random Forest",
  Accuracy = final_accuracy,
  Macro_Precision = macro_precision,
  Macro_Recall = macro_recall,
  Macro_F1 = macro_f1
)


write.csv(
  overall_metrics,
  "results/final/final_overall_metrics.csv",
  row.names = FALSE
)


# ------------------------------------------------------------
# 14. SAVE FEATURE IMPORTANCE
# ------------------------------------------------------------

write.csv(
  feature_importance,
  "results/final/final_feature_importance.csv",
  row.names = FALSE
)


# ------------------------------------------------------------
# 15. CONFUSION MATRIX HEATMAP
# ------------------------------------------------------------

cm_plot_data <- as.data.frame(final_cm)

colnames(cm_plot_data) <- c(
  "Actual",
  "Predicted",
  "Count"
)


confusion_plot <- ggplot(
  cm_plot_data,
  aes(
    x = Predicted,
    y = Actual,
    fill = Count
  )
) +
  geom_tile(color = "white") +
  geom_text(
    aes(label = Count),
    size = 5
  ) +
  labs(
    title = "Final Random Forest Confusion Matrix",
    x = "Predicted Class",
    y = "Actual Class"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    ),
    axis.text.x = element_text(
      angle = 30,
      hjust = 1
    )
  )


ggsave(
  "plots/final/final_confusion_matrix.png",
  confusion_plot,
  width = 9,
  height = 7,
  dpi = 300
)


# ------------------------------------------------------------
# 16. FEATURE IMPORTANCE PLOT
# ------------------------------------------------------------

feature_plot <- ggplot(
  feature_importance,
  aes(
    x = reorder(Feature, Importance),
    y = Importance
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Final Random Forest Feature Importance",
    x = "Feature",
    y = "Mean Decrease in Gini"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    )
  )


ggsave(
  "plots/final/final_feature_importance.png",
  feature_plot,
  width = 10,
  height = 7,
  dpi = 300
)


# ------------------------------------------------------------
# 17. MODEL COMPARISON
# ------------------------------------------------------------

model_comparison <- data.frame(
  Model = c(
    "Decision Tree",
    "Random Forest",
    "Logistic Regression",
    "Optimized Random Forest"
  ),
  Accuracy = c(
    0.8466,
    0.8498,
    0.8466,
    final_accuracy
  )
)


write.csv(
  model_comparison,
  "results/final/final_model_comparison.csv",
  row.names = FALSE
)


# ------------------------------------------------------------
# 18. MODEL COMPARISON PLOT
# ------------------------------------------------------------

comparison_plot <- ggplot(
  model_comparison,
  aes(
    x = Model,
    y = Accuracy * 100
  )
) +
  geom_col() +
  geom_text(
    aes(
      label = paste0(
        round(Accuracy * 100, 2),
        "%"
      )
    ),
    vjust = -0.5
  ) +
  labs(
    title = "Model Accuracy Comparison",
    x = "Model",
    y = "Accuracy (%)"
  ) +
  ylim(
    0,
    max(model_comparison$Accuracy * 100) + 8
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    ),
    axis.text.x = element_text(
      angle = 20,
      hjust = 1
    )
  )


ggsave(
  "plots/final/final_model_comparison.png",
  comparison_plot,
  width = 10,
  height = 7,
  dpi = 300
)


# ------------------------------------------------------------
# 19. CLASSIFICATION METRICS PLOT
# ------------------------------------------------------------

metrics_long <- rbind(
  data.frame(
    Class = class_metrics$Class,
    Metric = "Precision",
    Value = class_metrics$Precision
  ),
  data.frame(
    Class = class_metrics$Class,
    Metric = "Recall",
    Value = class_metrics$Recall
  ),
  data.frame(
    Class = class_metrics$Class,
    Metric = "F1 Score",
    Value = class_metrics$F1_Score
  )
)


metrics_plot <- ggplot(
  metrics_long,
  aes(
    x = Class,
    y = Value * 100,
    fill = Metric
  )
) +
  geom_col(
    position = "dodge"
  ) +
  labs(
    title = "Final Random Forest Classification Metrics",
    x = "Accident Severity",
    y = "Score (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    ),
    axis.text.x = element_text(
      angle = 20,
      hjust = 1
    )
  )


ggsave(
  "plots/final/final_classification_metrics.png",
  metrics_plot,
  width = 10,
  height = 7,
  dpi = 300
)


# ------------------------------------------------------------
# 20. FINAL SUMMARY
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" FINAL MODEL SUMMARY\n")
cat("============================================================\n")

cat("Model: Optimized Random Forest\n")
cat("ntree: 700\n")
cat("mtry: 5\n")
cat("nodesize: 5\n")

cat(
  "Accuracy:",
  round(final_accuracy * 100, 2),
  "%\n"
)

cat(
  "Macro Precision:",
  round(macro_precision * 100, 2),
  "%\n"
)

cat(
  "Macro Recall:",
  round(macro_recall * 100, 2),
  "%\n"
)

cat(
  "Macro F1:",
  round(macro_f1 * 100, 2),
  "%\n"
)

cat("\n")
cat("Final results saved in:\n")
cat("results/final/\n")

cat("\n")
cat("Final charts saved in:\n")
cat("plots/final/\n")

cat("\n")
cat("============================================================\n")
cat(" FINAL EVALUATION COMPLETED SUCCESSFULLY\n")
cat("============================================================\n")