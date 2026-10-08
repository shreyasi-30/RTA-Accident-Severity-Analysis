# ============================================
# 07 - MULTINOMIAL LOGISTIC REGRESSION
# ============================================

library(nnet)

# Make sure target is a factor
train_data$Accident_severity <- as.factor(
  train_data$Accident_severity
)

test_data$Accident_severity <- as.factor(
  test_data$Accident_severity
)

# Train multinomial logistic regression
logistic_model <- multinom(
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
  trace = FALSE
)

# Model summary
summary(logistic_model)

# Predict test data
logistic_predictions <- predict(
  logistic_model,
  newdata = test_data
)

# Confusion matrix
logistic_cm <- table(
  Actual = test_data$Accident_severity,
  Predicted = logistic_predictions
)

print(logistic_cm)