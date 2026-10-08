# ============================================
# 05 - DECISION TREE
# Road Traffic Accident Severity Prediction
# ============================================

library(rpart)
library(rpart.plot)

cat("\n")
cat("============================================\n")
cat("        DECISION TREE MODEL\n")
cat("============================================\n")


# ============================================
# 1. CHECK TRAINING AND TEST DATA
# ============================================

cat("Training rows:", nrow(train_data), "\n")
cat("Testing rows:", nrow(test_data), "\n")


# ============================================
# 2. TRAIN DECISION TREE
# ============================================

set.seed(123)

tree_model <- rpart(
  Accident_severity ~
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
    Cause_of_accident,
  
  data = train_data,
  
  method = "class",
  
  control = rpart.control(
    cp = 0.001,
    minsplit = 20,
    minbucket = 10,
    maxdepth = 10
  )
)


# ============================================
# 3. DISPLAY MODEL
# ============================================

cat("\nDecision Tree Model:\n")

print(tree_model)


# ============================================
# 4. DISPLAY TREE
# ============================================

# Use a valid named palette.
# The previous version caused the
# box.palette error.

rpart.plot(
  tree_model,
  main = "Decision Tree for Accident Severity",
  box.palette = "Blues",
  type = 2,
  extra = 104,
  fallen.leaves = TRUE,
  shadow.col = "gray"
)


# ============================================
# 5. SAVE TREE PLOT
# ============================================

png(
  "plots/decision_tree.png",
  width = 1200,
  height = 900
)

rpart.plot(
  tree_model,
  main = "Decision Tree for Accident Severity",
  box.palette = "Blues",
  type = 2,
  extra = 104,
  fallen.leaves = TRUE,
  shadow.col = "gray"
)

dev.off()


# ============================================
# 6. PREDICTIONS
# ============================================

tree_predictions <- predict(
  tree_model,
  newdata = test_data,
  type = "class"
)


# ============================================
# 7. CONFUSION MATRIX
# ============================================

tree_confusion <- table(
  Actual = test_data$Accident_severity,
  Predicted = tree_predictions
)

cat("\n")
cat("============================================\n")
cat("DECISION TREE CONFUSION MATRIX\n")
cat("============================================\n")

print(tree_confusion)


# ============================================
# 8. ACCURACY
# ============================================

tree_accuracy <- mean(
  tree_predictions ==
    test_data$Accident_severity
)

cat("\nDecision Tree Accuracy:",
    round(tree_accuracy * 100, 2),
    "%\n")


# ============================================
# 9. SAVE CONFUSION MATRIX
# ============================================

write.csv(
  as.data.frame(tree_confusion),
  "results/decision_tree_confusion_matrix.csv",
  row.names = FALSE
)


# ============================================
# 10. SAVE MODEL
# ============================================

saveRDS(
  tree_model,
  "results/decision_tree_model.rds"
)


cat("\n")
cat("============================================\n")
cat("  DECISION TREE COMPLETED SUCCESSFULLY\n")
cat("============================================\n")