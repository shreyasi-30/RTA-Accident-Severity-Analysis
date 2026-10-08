# ============================================================
# FILE 06: RANDOM FOREST HYPERPARAMETER TUNING
# ORIGINAL DATA DISTRIBUTION
# ============================================================

cat("\n")
cat("============================================================\n")
cat(" RANDOM FOREST HYPERPARAMETER TUNING\n")
cat(" ORIGINAL DATA DISTRIBUTION\n")
cat("============================================================\n\n")


# ------------------------------------------------------------
# 1. CHECK REQUIRED DATA
# ------------------------------------------------------------

if (!exists("train_data")) {
  stop("train_data not found. Please run File 04 first.")
}

if (!exists("test_data")) {
  stop("test_data not found. Please run File 04 first.")
}


# ------------------------------------------------------------
# 2. LOAD PACKAGE
# ------------------------------------------------------------

library(randomForest)


# ------------------------------------------------------------
# 3. CREATE FORMULA
# ------------------------------------------------------------

rf_formula <- Accident_severity ~
  Day_of_week +
  Age_band_of_driver +
  Sex_of_driver +
  Driving_experience +
  Types_of_Junction +
  Road_surface_conditions +
  Light_conditions +
  Weather_conditions +
  Type_of_collision +
  Number_of_vehicles_involved +
  Number_of_casualties +
  Vehicle_movement +
  Cause_of_accident


# ------------------------------------------------------------
# 4. DISPLAY DATA DISTRIBUTION
# ------------------------------------------------------------

cat("============================================\n")
cat("TRAINING DATA DISTRIBUTION\n")
cat("============================================\n")

print(table(train_data$Accident_severity))

cat("\n")


# ------------------------------------------------------------
# 5. HYPERPARAMETER GRID
# ------------------------------------------------------------

ntree_values <- c(300, 500, 700)

mtry_values <- c(2, 3, 4, 5)

nodesize_values <- c(1, 3, 5)


# ------------------------------------------------------------
# 6. RESULTS STORAGE
# ------------------------------------------------------------

tuning_results <- data.frame(
  ntree = integer(),
  mtry = integer(),
  nodesize = integer(),
  OOB_Error = numeric(),
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------
# 7. HYPERPARAMETER TUNING
# ------------------------------------------------------------

total_models <- length(ntree_values) *
  length(mtry_values) *
  length(nodesize_values)

model_number <- 0


cat("============================================\n")
cat("STARTING HYPERPARAMETER TUNING\n")
cat("Total combinations:", total_models, "\n")
cat("============================================\n\n")


for (ntree_value in ntree_values) {
  
  for (mtry_value in mtry_values) {
    
    for (nodesize_value in nodesize_values) {
      
      model_number <- model_number + 1
      
      cat(
        "Testing model",
        model_number,
        "of",
        total_models,
        "\n"
      )
      
      rf_model <- randomForest(
        formula = rf_formula,
        data = train_data,
        ntree = ntree_value,
        mtry = mtry_value,
        nodesize = nodesize_value,
        importance = TRUE
      )
      
      oob_error <- rf_model$err.rate[
        nrow(rf_model$err.rate),
        "OOB"
      ]
      
      tuning_results <- rbind(
        tuning_results,
        data.frame(
          ntree = ntree_value,
          mtry = mtry_value,
          nodesize = nodesize_value,
          OOB_Error = oob_error
        )
      )
    }
  }
}


# ------------------------------------------------------------
# 8. SORT RESULTS
# ------------------------------------------------------------

tuning_results <- tuning_results[
  order(tuning_results$OOB_Error),
]


# ------------------------------------------------------------
# 9. DISPLAY BEST PARAMETERS
# ------------------------------------------------------------

cat("\n")
cat("============================================\n")
cat("BEST RANDOM FOREST PARAMETERS\n")
cat("============================================\n")

print(tuning_results[1, ])

cat("\n")


# ------------------------------------------------------------
# 10. TRAIN FINAL OPTIMIZED RANDOM FOREST
# ------------------------------------------------------------

best_ntree <- tuning_results$ntree[1]
best_mtry <- tuning_results$mtry[1]
best_nodesize <- tuning_results$nodesize[1]


cat("============================================\n")
cat("TRAINING FINAL OPTIMIZED RANDOM FOREST\n")
cat("============================================\n")

cat("ntree:", best_ntree, "\n")
cat("mtry:", best_mtry, "\n")
cat("nodesize:", best_nodesize, "\n\n")


optimized_rf <- randomForest(
  formula = rf_formula,
  data = train_data,
  ntree = best_ntree,
  mtry = best_mtry,
  nodesize = best_nodesize,
  importance = TRUE
)


# ------------------------------------------------------------
# 11. PREDICTION ON TEST DATA
# ------------------------------------------------------------

rf_predictions <- predict(
  optimized_rf,
  newdata = test_data
)


# ------------------------------------------------------------
# 12. CONFUSION MATRIX
# ------------------------------------------------------------

rf_confusion_matrix <- table(
  Actual = test_data$Accident_severity,
  Predicted = rf_predictions
)


cat("\n")
cat("============================================\n")
cat("OPTIMIZED RANDOM FOREST CONFUSION MATRIX\n")
cat("============================================\n")

print(rf_confusion_matrix)


# ------------------------------------------------------------
# 13. ACCURACY
# ------------------------------------------------------------

rf_accuracy <- mean(
  rf_predictions == test_data$Accident_severity
)


cat("\n")
cat("============================================\n")
cat("OPTIMIZED RANDOM FOREST ACCURACY\n")
cat("============================================\n")

cat(
  "Optimized Random Forest Accuracy:",
  round(rf_accuracy * 100, 2),
  "%\n"
)


# ------------------------------------------------------------
# 14. COMPARE WITH BASELINE
# ------------------------------------------------------------

baseline_accuracy <- 0.8498

cat("\n")
cat("============================================\n")
cat("BASELINE VS OPTIMIZED\n")
cat("============================================\n")

cat(
  "Baseline Random Forest Accuracy:",
  round(baseline_accuracy * 100, 2),
  "%\n"
)

cat(
  "Optimized Random Forest Accuracy:",
  round(rf_accuracy * 100, 2),
  "%\n"
)

difference <- (rf_accuracy - baseline_accuracy) * 100

cat(
  "Difference:",
  round(difference, 2),
  "percentage points\n"
)


# ------------------------------------------------------------
# 15. SAVE RESULTS
# ------------------------------------------------------------

if (!dir.exists("results")) {
  dir.create("results")
}


write.csv(
  tuning_results,
  "results/random_forest_tuning_results.csv",
  row.names = FALSE
)


write.csv(
  as.data.frame.matrix(rf_confusion_matrix),
  "results/optimized_random_forest_confusion_matrix.csv"
)


saveRDS(
  optimized_rf,
  "results/optimized_random_forest_model.rds"
)


# ------------------------------------------------------------
# 16. FEATURE IMPORTANCE
# ------------------------------------------------------------

importance_data <- data.frame(
  Feature = rownames(importance(optimized_rf)),
  Importance = importance(optimized_rf)[, "MeanDecreaseGini"]
)

importance_data <- importance_data[
  order(-importance_data$Importance),
]


write.csv(
  importance_data,
  "results/optimized_random_forest_feature_importance.csv",
  row.names = FALSE
)


cat("\n")
cat("============================================\n")
cat("RANDOM FOREST TUNING COMPLETED SUCCESSFULLY\n")
cat("============================================\n\n")