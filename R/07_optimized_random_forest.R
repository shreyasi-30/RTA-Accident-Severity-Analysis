# ============================================================
# FILE 07: OPTIMIZED RANDOM FOREST
# RTA ACCIDENT SEVERITY PREDICTION
#
# Method:
# 1. Stratified 5-Fold Cross-Validation
# 2. Moderate Class Balancing
# 3. Hyperparameter Optimization
# 4. Macro F1 Optimization
# 5. Final Evaluation on Untouched Test Set
# ============================================================

library(randomForest)

cat("\n========================================\n")
cat("OPTIMIZED RANDOM FOREST\n")
cat("========================================\n")


# ============================================================
# 1. LOAD BALANCED CROSS-VALIDATION FOLDS
# ============================================================

balanced_folds <- readRDS(
  
  "results/balanced_folds.rds"
  
)

cat(
  "\nBalanced folds loaded successfully.\n"
)


# ============================================================
# 2. MODEL FORMULA
# ============================================================

model_formula <- Accident_severity ~
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


# ============================================================
# 3. FUNCTION TO CALCULATE MACRO F1
# ============================================================

calculate_macro_f1 <- function(
    
  actual,
  
  predicted
  
) {
  
  actual <- as.factor(
    actual
  )
  
  predicted <- factor(
    
    predicted,
    
    levels =
      levels(actual)
  )
  
  classes <- levels(
    actual
  )
  
  f1_values <- c()
  
  
  for (
    class in classes
  ) {
    
    TP <- sum(
      
      predicted == class &
        actual == class,
      
      na.rm = TRUE
    )
    
    
    FP <- sum(
      
      predicted == class &
        actual != class,
      
      na.rm = TRUE
    )
    
    
    FN <- sum(
      
      predicted != class &
        actual == class,
      
      na.rm = TRUE
    )
    
    
    precision <- ifelse(
      
      TP + FP == 0,
      
      0,
      
      TP / (TP + FP)
    )
    
    
    recall <- ifelse(
      
      TP + FN == 0,
      
      0,
      
      TP / (TP + FN)
    )
    
    
    f1 <- ifelse(
      
      precision + recall == 0,
      
      0,
      
      2 * precision * recall /
        (precision + recall)
    )
    
    
    f1_values <- c(
      
      f1_values,
      
      f1
    )
  }
  
  
  return(
    mean(f1_values)
  )
}


# ============================================================
# 4. RANDOM FOREST HYPERPARAMETER GRID
# ============================================================

parameter_grid <- expand.grid(
  
  ntree = c(
    300,
    500
  ),
  
  mtry = c(
    3,
    5,
    7
  ),
  
  nodesize = c(
    1,
    3,
    5
  )
)


cat(
  
  "\nTotal Random Forest parameter combinations:",
  
  nrow(parameter_grid),
  
  "\n"
)


# ============================================================
# 5. HYPERPARAMETER OPTIMIZATION
# ============================================================

results <- data.frame()

set.seed(123)


for (
  p in 1:nrow(parameter_grid)
) {
  
  cat(
    
    "\nTesting Random Forest combination",
    
    p,
    
    "of",
    
    nrow(parameter_grid),
    
    "\n"
  )
  
  
  ntree_value <-
    parameter_grid$ntree[p]
  
  mtry_value <-
    parameter_grid$mtry[p]
  
  nodesize_value <-
    parameter_grid$nodesize[p]
  
  
  fold_scores <- c()
  
  
  # ----------------------------------------------------------
  # 5-FOLD CROSS-VALIDATION
  # ----------------------------------------------------------
  
  for (
    fold in 1:5
  ) {
    
    train_fold <-
      balanced_folds[[fold]]$train
    
    validation_fold <-
      balanced_folds[[fold]]$validation
    
    
    # --------------------------------------------------------
    # TRAIN RANDOM FOREST
    # --------------------------------------------------------
    
    model <- randomForest(
      
      model_formula,
      
      data = train_fold,
      
      ntree = ntree_value,
      
      mtry = mtry_value,
      
      nodesize = nodesize_value,
      
      importance = TRUE
    )
    
    
    # --------------------------------------------------------
    # VALIDATION PREDICTION
    # --------------------------------------------------------
    
    predictions <- predict(
      
      model,
      
      validation_fold
    )
    
    
    # --------------------------------------------------------
    # CALCULATE MACRO F1
    # --------------------------------------------------------
    
    macro_f1 <- calculate_macro_f1(
      
      validation_fold$Accident_severity,
      
      predictions
    )
    
    
    fold_scores <- c(
      
      fold_scores,
      
      macro_f1
    )
  }
  
  
  # ----------------------------------------------------------
  # STORE AVERAGE CV SCORE
  # ----------------------------------------------------------
  
  results <- rbind(
    
    results,
    
    data.frame(
      
      ntree =
        ntree_value,
      
      mtry =
        mtry_value,
      
      nodesize =
        nodesize_value,
      
      Macro_F1 =
        mean(fold_scores)
    )
  )
}


# ============================================================
# 6. SORT RESULTS
# ============================================================

results <- results[
  order(
    -results$Macro_F1
  ),
]


# ============================================================
# 7. BEST PARAMETERS
# ============================================================

best_parameters <- results[1, ]

cat("\n========================================\n")
cat("BEST RANDOM FOREST PARAMETERS\n")
cat("========================================\n")

print(
  best_parameters
)


# ============================================================
# 8. SAVE TUNING RESULTS
# ============================================================

write.csv(
  
  results,
  
  "results/random_forest_tuning.csv",
  
  row.names = FALSE
)


# ============================================================
# 9. LOAD ORIGINAL TRAIN AND TEST DATA
# ============================================================

train_data <- read.csv(
  
  "results/train_data.csv",
  
  stringsAsFactors = TRUE
)

test_data <- read.csv(
  
  "results/test_data.csv",
  
  stringsAsFactors = TRUE
)


train_data$Accident_severity <-
  as.factor(
    train_data$Accident_severity
  )

test_data$Accident_severity <-
  as.factor(
    test_data$Accident_severity
  )


# ============================================================
# 10. MODERATE CLASS BALANCING
#
# Fatal    -> 1500
# Serious  -> 3000
# Slight   -> ALL AVAILABLE
# ============================================================

balance_training_data <- function(
    data
) {
  
  data$Accident_severity <-
    as.factor(
      data$Accident_severity
    )
  
  classes <- levels(
    data$Accident_severity
  )
  
  
  output <- lapply(
    
    classes,
    
    function(class_name) {
      
      temp <- data[
        
        data$Accident_severity ==
          class_name,
        
      ]
      
      
      # --------------------------------------
      # FATAL INJURY
      # --------------------------------------
      
      if (
        
        class_name ==
        "Fatal injury"
        
      ) {
        
        target_size <- 1500
        
      }
      
      
      # --------------------------------------
      # SERIOUS INJURY
      # --------------------------------------
      
      else if (
        
        class_name ==
        "Serious Injury"
        
      ) {
        
        target_size <- 3000
        
      }
      
      
      # --------------------------------------
      # SLIGHT INJURY
      # KEEP ALL
      # --------------------------------------
      
      else {
        
        target_size <-
          nrow(temp)
        
      }
      
      
      # --------------------------------------
      # SAMPLING
      # --------------------------------------
      
      if (
        
        nrow(temp) >=
        target_size
        
      ) {
        
        temp[
          
          sample(
            
            nrow(temp),
            
            target_size,
            
            replace = FALSE
            
          ),
          
        ]
        
      }
      
      else {
        
        temp[
          
          sample(
            
            nrow(temp),
            
            target_size,
            
            replace = TRUE
            
          ),
          
        ]
      }
    }
  )
  
  
  # Combine classes
  
  output <- do.call(
    
    rbind,
    
    output
    
  )
  
  
  # Shuffle rows
  
  output <- output[
    
    sample(
      nrow(output)
    ),
    
  ]
  
  
  rownames(output) <- NULL
  
  return(output)
}


# ============================================================
# 11. CREATE FINAL BALANCED TRAINING DATA
# ============================================================

set.seed(123)

balanced_train <-
  balance_training_data(
    train_data
  )


cat("\n========================================\n")
cat("FINAL BALANCED TRAINING DATA\n")
cat("========================================\n")

cat(
  
  "Number of rows:",
  
  nrow(balanced_train),
  
  "\n"
)


cat(
  "\nClass distribution:\n"
)

print(
  
  table(
    
    balanced_train$Accident_severity
    
  )
  
)


# ============================================================
# 12. TRAIN FINAL OPTIMIZED RANDOM FOREST
# ============================================================

final_rf <- randomForest(
  
  model_formula,
  
  data = balanced_train,
  
  ntree =
    best_parameters$ntree,
  
  mtry =
    best_parameters$mtry,
  
  nodesize =
    best_parameters$nodesize,
  
  importance = TRUE
)


# ============================================================
# 13. PREDICT ON UNTOUCHED TEST DATA
# ============================================================

rf_predictions <- predict(
  
  final_rf,
  
  test_data
  
)


# ============================================================
# 14. CONFUSION MATRIX
# ============================================================

rf_confusion <- table(
  
  Actual =
    test_data$Accident_severity,
  
  Predicted =
    rf_predictions
  
)


cat("\n========================================\n")
cat("RANDOM FOREST CONFUSION MATRIX\n")
cat("========================================\n")

print(
  rf_confusion
)


# ============================================================
# 15. ACCURACY
# ============================================================

rf_accuracy <- mean(
  
  rf_predictions ==
    test_data$Accident_severity
  
)


cat(
  
  "\nOptimized Random Forest Accuracy:",
  
  round(
    
    rf_accuracy * 100,
    
    2
    
  ),
  
  "%\n"
)


# ============================================================
# 16. SAVE MODEL
# ============================================================

saveRDS(
  
  final_rf,
  
  "results/optimized_random_forest.rds"
  
)


# ============================================================
# 17. SAVE PREDICTIONS
# ============================================================

saveRDS(
  
  rf_predictions,
  
  "results/rf_predictions.rds"
  
)


# ============================================================
# 18. SAVE CONFUSION MATRIX
# ============================================================

write.csv(
  
  as.data.frame(
    rf_confusion
  ),
  
  "results/optimized_rf_confusion.csv",
  
  row.names = FALSE
  
)


# ============================================================
# 19. FEATURE IMPORTANCE
# ============================================================

importance_values <-
  importance(
    final_rf
  )


write.csv(
  
  importance_values,
  
  "results/optimized_feature_importance.csv"
  
)


# ============================================================
# 20. FINAL OUTPUT
# ============================================================

cat("\n========================================\n")
cat("RANDOM FOREST OPTIMIZATION COMPLETED\n")
cat("========================================\n")

cat(
  
  "\nBest CV Macro F1:",
  
  round(
    
    best_parameters$Macro_F1,
    
    4
    
  ),
  
  "\n"
)


cat(
  
  "Final Test Accuracy:",
  
  round(
    
    rf_accuracy * 100,
    
    2
    
  ),
  
  "%\n"
)


cat(
  "\nFiles saved successfully.\n"
)

cat(
  "========================================\n"
)