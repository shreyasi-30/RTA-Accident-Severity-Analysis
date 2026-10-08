# ============================================
# 06 - RANDOM FOREST
# ============================================

library(randomForest)

set.seed(123)

rf_model <- randomForest(
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
  ntree = 300,
  importance = TRUE
)

print(rf_model)

# Predictions
rf_predictions <- predict(
  rf_model,
  test_data
)

# Confusion matrix
rf_cm <- table(
  Actual = test_data$Accident_severity,
  Predicted = rf_predictions
)

print(rf_cm)

# Variable importance
importance(rf_model)

varImpPlot(
  rf_model,
  main = "Random Forest Variable Importance"
)