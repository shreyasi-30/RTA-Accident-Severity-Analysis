# ============================================================
# FILE 06: OPTIMIZED DECISION TREE
# RTA ACCIDENT SEVERITY PREDICTION
#
# Method:
# 1. Stratified 5-Fold Cross-Validation
# 2. Moderate Class Balancing
# 3. Hyperparameter Optimization
# 4. Macro F1 Optimization
# 5. Final Evaluation on Untouched Test Set
# ============================================================

library(rpart)
library(rpart.plot)

cat("\n========================================\n")
cat("OPTIMIZED DECISION TREE\n")
cat("========================================\n")


# ============================================================
# 1. LOAD BALANCED CROSS-VALIDATION FOLDS
# ============================================================

balanced_folds <- readRDS(
  "results/balanced_folds.rds"
)

cat("\nBalanced folds loaded successfully.\n")


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
  
  # Make sure both are factors
  actual <- as.factor(actual)
  predicted <- factor(
    predicted,
    levels = levels(actual)
  )
  
  classes <- levels(actual)
  
  f1_values <- c()
  
  for (class in classes) {
    
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
# 4. HYPERPARAMETER GRID
# ============================================================

parameter_grid <- expand.grid(
  
  cp = c(
    0.0005,
    0.001,
    0.005
  ),
  
  maxdepth = c(
    5,
    7,
    10
  ),
  
  minsplit = c(
    10,
    20,
    30
  )
)

cat(
  "\nTotal Decision Tree parameter combinations:",
  nrow(parameter_grid),
  "\n"
)


# ============================================================
# 5. HYPERPARAMETER OPTIMIZATION
# ============================================================

results <- data.frame()

set.seed(123)

for (p in 1:nrow(parameter_grid)) {
  
  cat(
    "\nTesting parameter combination",
    p,
    "of",
    nrow(parameter_grid),
    "\n"
  )
  
  cp_value <-
    parameter_grid$cp[p]
  
  depth_value <-
    parameter_grid$maxdepth[p]
  
  split_value <-
    parameter_grid$minsplit[p]
  
  fold_scores <- c()
  
  
  # ----------------------------------------------------------
  # 5-FOLD CROSS-VALIDATION
  # ----------------------------------------------------------
  
  for (fold in 1:5) {
    
    train_fold <-
      balanced_folds[[fold]]$train
    
    validation_fold <-
      balanced_folds[[fold]]$validation
    
    
    # --------------------------------------------------------
    # TRAIN DECISION TREE
    # --------------------------------------------------------
    
    model <- rpart(
      
      model_formula,
      
      data = train_fold,
      
      method = "class",
      
      control = rpart.control(
        
        cp = cp_value,
        
        maxdepth = depth_value,
        
        minsplit = split_value,
        
        minbucket = 10
      )
    )
    
    
    # --------------------------------------------------------
    # VALIDATION PREDICTION
    # --------------------------------------------------------
    
    predictions <- predict(
      
      model,
      
      validation_fold,
      
      type = "class"
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
      
      cp = cp_value,
      
      maxdepth = depth_value,
      
      minsplit = split_value,
      
      Macro_F1 = mean(
        fold_scores
      )
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
cat("BEST DECISION TREE PARAMETERS\n")
cat("========================================\n")

print(best_parameters)


# ============================================================
# 8. SAVE TUNING RESULTS
# ============================================================

write.csv(
  
  results,
  
  "results/decision_tree_tuning.csv",
  
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


# Make sure target is factor

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
        nrow(temp) >= target_size
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

cat("\nClass distribution:\n")

print(
  table(
    balanced_train$Accident_severity
  )
)


# ============================================================
# 12. TRAIN FINAL OPTIMIZED DECISION TREE
# ============================================================

final_tree <- rpart(
  
  model_formula,
  
  data = balanced_train,
  
  method = "class",
  
  control = rpart.control(
    
    cp =
      best_parameters$cp,
    
    maxdepth =
      best_parameters$maxdepth,
    
    minsplit =
      best_parameters$minsplit,
    
    minbucket = 10
  )
)


# ============================================================
# 13. PREDICT ON UNTOUCHED TEST DATA
# ============================================================

tree_predictions <- predict(
  
  final_tree,
  
  test_data,
  
  type = "class"
)


# ============================================================
# 14. CONFUSION MATRIX
# ============================================================

tree_confusion <- table(
  
  Actual =
    test_data$Accident_severity,
  
  Predicted =
    tree_predictions
)


cat("\n========================================\n")
cat("DECISION TREE CONFUSION MATRIX\n")
cat("========================================\n")

print(tree_confusion)


# ============================================================
# 15. ACCURACY
# ============================================================

tree_accuracy <- mean(
  
  tree_predictions ==
    test_data$Accident_severity
)

cat(
  
  "\nOptimized Decision Tree Accuracy:",
  
  round(
    tree_accuracy * 100,
    2
  ),
  
  "%\n"
)


# ============================================================
# 16. SAVE MODEL
# ============================================================

saveRDS(
  
  final_tree,
  
  "results/optimized_decision_tree.rds"
)


# ============================================================
# 17. SAVE PREDICTIONS
# ============================================================

saveRDS(
  
  tree_predictions,
  
  "results/tree_predictions.rds"
)


# ============================================================
# 18. SAVE CONFUSION MATRIX
# ============================================================

write.csv(
  
  as.data.frame(
    tree_confusion
  ),
  
  "results/optimized_tree_confusion.csv",
  
  row.names = FALSE
)


# ============================================================
# 19. SAVE TREE VISUALIZATION
# ============================================================

png(
  
  "results/optimized_decision_tree.png",
  
  width = 1600,
  
  height = 1000
)

rpart.plot(
  
  final_tree,
  
  type = 2,
  
  extra = 104,
  
  fallen.leaves = TRUE,
  
  box.palette = "Blues",
  
  shadow.col = "gray",
  
  cex = 0.6
)

dev.off()


# ============================================================
# 20. FINAL OUTPUT
# ============================================================

cat("\n========================================\n")
cat("DECISION TREE OPTIMIZATION COMPLETED\n")
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
    tree_accuracy * 100,
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