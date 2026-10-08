# ============================================
# 03 - EXPLORATORY DATA ANALYSIS
# Road Traffic Accident Severity Prediction
# ============================================

library(ggplot2)
library(dplyr)

cat("\n")
cat("============================================\n")
cat("     EXPLORATORY DATA ANALYSIS STARTED\n")
cat("============================================\n")


# ============================================
# 1. LOAD CLEANED DATA
# ============================================

data <- read.csv(
  "results/cleaned_RTA_Dataset.csv",
  stringsAsFactors = TRUE
)

cat("\nCleaned dataset loaded successfully.\n")


# ============================================
# 2. BASIC DATASET INFORMATION
# ============================================

cat("\n============================================\n")
cat("BASIC DATASET INFORMATION\n")
cat("============================================\n")

cat(
  "Number of rows:",
  nrow(data),
  "\n"
)

cat(
  "Number of columns:",
  ncol(data),
  "\n"
)


# ============================================
# 3. COLUMN NAMES
# ============================================

cat("\n============================================\n")
cat("COLUMN NAMES\n")
cat("============================================\n")

print(names(data))


# ============================================
# 4. DATA STRUCTURE
# ============================================

cat("\n============================================\n")
cat("DATA STRUCTURE\n")
cat("============================================\n")

str(data)



cat("\n============================================\n")
cat("SUMMARY STATISTICS\n")
cat("============================================\n")

print(summary(data))


cat("\n============================================\n")
cat("MISSING VALUE ANALYSIS\n")
cat("============================================\n")

missing_values <- colSums(
  is.na(data)
)

missing_values <- sort(
  missing_values,
  decreasing = TRUE
)

print(missing_values)


cat(
  "\nTotal missing values:",
  sum(missing_values),
  "\n"
)




missing_summary <- data.frame(
  Variable = names(data),
  Missing_Values = colSums(is.na(data))
)

missing_summary <- missing_summary[
  order(
    missing_summary$Missing_Values,
    decreasing = TRUE
  ),
]


# Save missing-value summary

write.csv(
  missing_summary,
  "results/missing_value_summary.csv",
  row.names = FALSE
)


# ============================================
# 7. DUPLICATE RECORD ANALYSIS
# ============================================

cat("\n============================================\n")
cat("DUPLICATE RECORD ANALYSIS\n")
cat("============================================\n")

duplicate_count <- sum(
  duplicated(data)
)

cat(
  "Number of duplicate rows:",
  duplicate_count,
  "\n"
)


# ============================================
# 8. TARGET VARIABLE ANALYSIS
# ============================================

cat("\n============================================\n")
cat("ACCIDENT SEVERITY ANALYSIS\n")
cat("============================================\n")


# Frequency

severity_count <- table(
  data$Accident_severity
)

cat("\nFrequency of accident severity:\n")

print(severity_count)


# Percentage

severity_percentage <- prop.table(
  severity_count
) * 100

cat("\nPercentage distribution:\n")

print(
  round(
    severity_percentage,
    2
  )
)


# Save target distribution

severity_summary <- data.frame(
  Accident_Severity =
    names(severity_count),
  Frequency =
    as.numeric(severity_count),
  Percentage =
    round(
      as.numeric(severity_percentage),
      2
    )
)

write.csv(
  severity_summary,
  "results/accident_severity_distribution.csv",
  row.names = FALSE
)


# ============================================
# 9. ACCIDENT SEVERITY PLOT
# ============================================

severity_plot <- ggplot(
  data,
  aes(
    x = Accident_severity
  )
) +
  geom_bar() +
  labs(
    title =
      "Distribution of Accident Severity",
    x =
      "Accident Severity",
    y =
      "Number of Accidents"
  ) +
  theme_minimal()

print(severity_plot)

ggsave(
  "plots/01_accident_severity.png",
  severity_plot,
  width = 8,
  height = 5
)


# ============================================
# 10. WEATHER CONDITIONS
# ============================================

cat("\n============================================\n")
cat("WEATHER CONDITIONS ANALYSIS\n")
cat("============================================\n")

print(
  table(data$Weather_conditions)
)


weather_plot <- ggplot(
  data,
  aes(
    x = Weather_conditions
  )
) +
  geom_bar() +
  labs(
    title =
      "Accidents by Weather Conditions",
    x =
      "Weather Conditions",
    y =
      "Number of Accidents"
  ) +
  theme_minimal() +
  theme(
    axis.text.x =
      element_text(
        angle = 45,
        hjust = 1
      )
  )

print(weather_plot)

ggsave(
  "plots/02_weather_conditions.png",
  weather_plot,
  width = 9,
  height = 5
)


# ============================================
# 11. LIGHT CONDITIONS
# ============================================

cat("\n============================================\n")
cat("LIGHT CONDITIONS ANALYSIS\n")
cat("============================================\n")

print(
  table(data$Light_conditions)
)


light_plot <- ggplot(
  data,
  aes(
    x = Light_conditions
  )
) +
  geom_bar() +
  labs(
    title =
      "Accidents by Light Conditions",
    x =
      "Light Conditions",
    y =
      "Number of Accidents"
  ) +
  theme_minimal() +
  theme(
    axis.text.x =
      element_text(
        angle = 45,
        hjust = 1
      )
  )

print(light_plot)

ggsave(
  "plots/03_light_conditions.png",
  light_plot,
  width = 9,
  height = 5
)


# ============================================
# 12. ROAD SURFACE CONDITIONS
# ============================================

cat("\n============================================\n")
cat("ROAD SURFACE CONDITIONS\n")
cat("============================================\n")

print(
  table(data$Road_surface_conditions)
)


road_surface_plot <- ggplot(
  data,
  aes(
    x = Road_surface_conditions
  )
) +
  geom_bar() +
  labs(
    title =
      "Accidents by Road Surface Conditions",
    x =
      "Road Surface Conditions",
    y =
      "Number of Accidents"
  ) +
  theme_minimal() +
  theme(
    axis.text.x =
      element_text(
        angle = 45,
        hjust = 1
      )
  )

print(road_surface_plot)

ggsave(
  "plots/04_road_surface_conditions.png",
  road_surface_plot,
  width = 9,
  height = 5
)


# ============================================
# 13. TYPE OF JUNCTION
# ============================================

cat("\n============================================\n")
cat("JUNCTION TYPE ANALYSIS\n")
cat("============================================\n")

print(
  table(data$Types_of_Junction)
)


junction_plot <- ggplot(
  data,
  aes(
    x = Types_of_Junction
  )
) +
  geom_bar() +
  labs(
    title =
      "Accidents by Type of Junction",
    x =
      "Type of Junction",
    y =
      "Number of Accidents"
  ) +
  theme_minimal() +
  theme(
    axis.text.x =
      element_text(
        angle = 45,
        hjust = 1
      )
  )

print(junction_plot)

ggsave(
  "plots/05_junction_type.png",
  junction_plot,
  width = 9,
  height = 5
)


# ============================================
# 14. NUMBER OF VEHICLES INVOLVED
# ============================================

cat("\n============================================\n")
cat("NUMBER OF VEHICLES INVOLVED\n")
cat("============================================\n")

print(
  table(
    data$Number_of_vehicles_involved
  )
)


vehicle_plot <- ggplot(
  data,
  aes(
    x = Number_of_vehicles_involved
  )
) +
  geom_histogram(
    bins = 10
  ) +
  labs(
    title =
      "Number of Vehicles Involved",
    x =
      "Number of Vehicles",
    y =
      "Frequency"
  ) +
  theme_minimal()

print(vehicle_plot)

ggsave(
  "plots/06_number_of_vehicles.png",
  vehicle_plot,
  width = 8,
  height = 5
)


# ============================================
# 15. NUMBER OF CASUALTIES
# ============================================

cat("\n============================================\n")
cat("NUMBER OF CASUALTIES\n")
cat("============================================\n")

print(
  table(
    data$Number_of_casualties
  )
)


casualty_plot <- ggplot(
  data,
  aes(
    x = Number_of_casualties
  )
) +
  geom_histogram(
    bins = 10
  ) +
  labs(
    title =
      "Number of Casualties",
    x =
      "Number of Casualties",
    y =
      "Frequency"
  ) +
  theme_minimal()

print(casualty_plot)

ggsave(
  "plots/07_number_of_casualties.png",
  casualty_plot,
  width = 8,
  height = 5
)


# ============================================
# 16. DRIVER AGE GROUP
# ============================================

cat("\n============================================\n")
cat("DRIVER AGE GROUP\n")
cat("============================================\n")

print(
  table(data$Age_band_of_driver)
)


driver_age_plot <- ggplot(
  data,
  aes(
    x = Age_band_of_driver
  )
) +
  geom_bar() +
  labs(
    title =
      "Accidents by Driver Age Group",
    x =
      "Driver Age Group",
    y =
      "Number of Accidents"
  ) +
  theme_minimal() +
  theme(
    axis.text.x =
      element_text(
        angle = 45,
        hjust = 1
      )
  )

print(driver_age_plot)

ggsave(
  "plots/08_driver_age_group.png",
  driver_age_plot,
  width = 9,
  height = 5
)


# ============================================
# 17. DRIVING EXPERIENCE
# ============================================

cat("\n============================================\n")
cat("DRIVING EXPERIENCE\n")
cat("============================================\n")

print(
  table(data$Driving_experience)
)


experience_plot <- ggplot(
  data,
  aes(
    x = Driving_experience
  )
) +
  geom_bar() +
  labs(
    title =
      "Accidents by Driving Experience",
    x =
      "Driving Experience",
    y =
      "Number of Accidents"
  ) +
  theme_minimal() +
  theme(
    axis.text.x =
      element_text(
        angle = 45,
        hjust = 1
      )
  )

print(experience_plot)

ggsave(
  "plots/09_driving_experience.png",
  experience_plot,
  width = 9,
  height = 5
)


# ============================================
# 18. TYPE OF COLLISION
# ============================================

cat("\n============================================\n")
cat("TYPE OF COLLISION\n")
cat("============================================\n")

print(
  table(data$Type_of_collision)
)


collision_plot <- ggplot(
  data,
  aes(
    x = Type_of_collision
  )
) +
  geom_bar() +
  labs(
    title =
      "Accidents by Type of Collision",
    x =
      "Type of Collision",
    y =
      "Number of Accidents"
  ) +
  theme_minimal() +
  theme(
    axis.text.x =
      element_text(
        angle = 45,
        hjust = 1
      )
  )

print(collision_plot)

ggsave(
  "plots/10_type_of_collision.png",
  collision_plot,
  width = 10,
  height = 5
)


# ============================================
# 19. CAUSE OF ACCIDENT
# ============================================

cat("\n============================================\n")
cat("CAUSE OF ACCIDENT\n")
cat("============================================\n")

cause_count <- sort(
  table(data$Cause_of_accident),
  decreasing = TRUE
)

print(cause_count)


cause_plot <- ggplot(
  data,
  aes(
    x = Cause_of_accident
  )
) +
  geom_bar() +
  labs(
    title =
      "Accidents by Cause",
    x =
      "Cause of Accident",
    y =
      "Number of Accidents"
  ) +
  theme_minimal() +
  theme(
    axis.text.x =
      element_text(
        angle = 90,
        hjust = 1
      )
  )

print(cause_plot)

ggsave(
  "plots/11_cause_of_accident.png",
  cause_plot,
  width = 12,
  height = 6
)


# ============================================
# 20. SEX OF DRIVER
# ============================================

cat("\n============================================\n")
cat("DRIVER GENDER DISTRIBUTION\n")
cat("============================================\n")

print(
  table(data$Sex_of_driver)
)


driver_gender_plot <- ggplot(
  data,
  aes(
    x = Sex_of_driver
  )
) +
  geom_bar() +
  labs(
    title =
      "Accidents by Driver Sex",
    x =
      "Driver Sex",
    y =
      "Number of Accidents"
  ) +
  theme_minimal()

print(driver_gender_plot)

ggsave(
  "plots/12_driver_sex.png",
  driver_gender_plot,
  width = 8,
  height = 5
)


# ============================================
# 21. CROSS-ANALYSIS:
# ACCIDENT SEVERITY vs WEATHER
# ============================================

severity_weather_plot <- ggplot(
  data,
  aes(
    x = Weather_conditions,
    fill = Accident_severity
  )
) +
  geom_bar(
    position = "dodge"
  ) +
  labs(
    title =
      "Accident Severity by Weather Conditions",
    x =
      "Weather Conditions",
    y =
      "Number of Accidents",
    fill =
      "Accident Severity"
  ) +
  theme_minimal() +
  theme(
    axis.text.x =
      element_text(
        angle = 45,
        hjust = 1
      )
  )

print(severity_weather_plot)

ggsave(
  "plots/13_severity_vs_weather.png",
  severity_weather_plot,
  width = 10,
  height = 6
)


# ============================================
# 22. CROSS-ANALYSIS:
# ACCIDENT SEVERITY vs LIGHT
# ============================================

severity_light_plot <- ggplot(
  data,
  aes(
    x = Light_conditions,
    fill = Accident_severity
  )
) +
  geom_bar(
    position = "dodge"
  ) +
  labs(
    title =
      "Accident Severity by Light Conditions",
    x =
      "Light Conditions",
    y =
      "Number of Accidents",
    fill =
      "Accident Severity"
  ) +
  theme_minimal() +
  theme(
    axis.text.x =
      element_text(
        angle = 45,
        hjust = 1
      )
  )

print(severity_light_plot)

ggsave(
  "plots/14_severity_vs_light.png",
  severity_light_plot,
  width = 10,
  height = 6
)


# ============================================
# 23. CROSS-ANALYSIS:
# ACCIDENT SEVERITY vs ROAD CONDITIONS
# ============================================

severity_road_plot <- ggplot(
  data,
  aes(
    x = Road_surface_conditions,
    fill = Accident_severity
  )
) +
  geom_bar(
    position = "dodge"
  ) +
  labs(
    title =
      "Accident Severity by Road Surface Conditions",
    x =
      "Road Surface Conditions",
    y =
      "Number of Accidents",
    fill =
      "Accident Severity"
  ) +
  theme_minimal()

print(severity_road_plot)

ggsave(
  "plots/15_severity_vs_road_conditions.png",
  severity_road_plot,
  width = 10,
  height = 6
)


# ============================================
# 24. CROSS-ANALYSIS:
# ACCIDENT SEVERITY vs COLLISION TYPE
# ============================================

severity_collision_plot <- ggplot(
  data,
  aes(
    x = Type_of_collision,
    fill = Accident_severity
  )
) +
  geom_bar(
    position = "dodge"
  ) +
  labs(
    title =
      "Accident Severity by Collision Type",
    x =
      "Type of Collision",
    y =
      "Number of Accidents",
    fill =
      "Accident Severity"
  ) +
  theme_minimal() +
  theme(
    axis.text.x =
      element_text(
        angle = 45,
        hjust = 1
      )
  )

print(severity_collision_plot)

ggsave(
  "plots/16_severity_vs_collision.png",
  severity_collision_plot,
  width = 11,
  height = 6
)


# ============================================
# 25. UNIQUE VALUE INSPECTION
# ============================================

cat("\n============================================\n")
cat("UNIQUE VALUE INSPECTION\n")
cat("============================================\n")

for (col in names(data)) {
  
  cat("\n----------------------------\n")
  
  cat(
    "Column:",
    col,
    "\n"
  )
  
  cat(
    "Number of unique values:",
    length(
      unique(
        data[[col]]
      )
    ),
    "\n"
  )
  
  print(
    head(
      unique(
        data[[col]]
      ),
      15
    )
  )
}


# ============================================
# 26. CREATE EDA SUMMARY
# ============================================

eda_summary <- data.frame(
  
  Variable =
    names(data),
  
  Data_Type =
    sapply(
      data,
      function(x)
        class(x)[1]
    ),
  
  Unique_Values =
    sapply(
      data,
      function(x)
        length(
          unique(x)
        )
    ),
  
  Missing_Values =
    sapply(
      data,
      function(x)
        sum(is.na(x))
    )
)


# Save EDA summary

write.csv(
  eda_summary,
  "results/eda_summary.csv",
  row.names = FALSE
)


# ============================================
# 27. FINAL EDA INFORMATION
# ============================================

cat("\n============================================\n")
cat("EDA SUMMARY\n")
cat("============================================\n")

cat(
  "Rows analyzed:",
  nrow(data),
  "\n"
)

cat(
  "Columns analyzed:",
  ncol(data),
  "\n"
)

cat(
  "Total missing values:",
  sum(
    is.na(data)
  ),
  "\n"
)

cat(
  "Duplicate rows:",
  sum(
    duplicated(data)
  ),
  "\n"
)


cat("\n============================================\n")
cat("     EDA COMPLETED SUCCESSFULLY\n")
cat("============================================\n")# ============================================
# 03 - EXPLORATORY DATA ANALYSIS
# Road Traffic Accident Severity Prediction
# ============================================

library(ggplot2)
library(dplyr)

cat("\n")
cat("============================================\n")
cat("     EXPLORATORY DATA ANALYSIS STARTED\n")
cat("============================================\n")


# ============================================
# 1. LOAD CLEANED DATA
# ============================================

data <- read.csv(
  "results/cleaned_RTA_Dataset.csv",
  stringsAsFactors = TRUE
)

cat("\nCleaned dataset loaded successfully.\n")


# ============================================
# 2. BASIC DATASET INFORMATION
# ============================================

cat("\n============================================\n")
cat("BASIC DATASET INFORMATION\n")
cat("============================================\n")

cat(
  "Number of rows:",
  nrow(data),
  "\n"
)

cat(
  "Number of columns:",
  ncol(data),
  "\n"
)


# ============================================
# 3. COLUMN NAMES
# ============================================

cat("\n============================================\n")
cat("COLUMN NAMES\n")
cat("============================================\n")

print(names(data))


# ============================================
# 4. DATA STRUCTURE
# ============================================

cat("\n============================================\n")
cat("DATA STRUCTURE\n")
cat("============================================\n")

str(data)


# ============================================
# 5. SUMMARY STATISTICS
# ============================================

cat("\n============================================\n")
cat("SUMMARY STATISTICS\n")
cat("============================================\n")

print(summary(data))


# ============================================
# 6. MISSING VALUE ANALYSIS
# ============================================

cat("\n============================================\n")
cat("MISSING VALUE ANALYSIS\n")
cat("============================================\n")

missing_values <- colSums(
  is.na(data)
)

missing_values <- sort(
  missing_values,
  decreasing = TRUE
)

print(missing_values)


cat(
  "\nTotal missing values:",
  sum(missing_values),
  "\n"
)


# Create missing-value summary

missing_summary <- data.frame(
  Variable = names(data),
  Missing_Values = colSums(is.na(data))
)

missing_summary <- missing_summary[
  order(
    missing_summary$Missing_Values,
    decreasing = TRUE
  ),
]


# Save missing-value summary

write.csv(
  missing_summary,
  "results/missing_value_summary.csv",
  row.names = FALSE
)


# ============================================
# 7. DUPLICATE RECORD ANALYSIS
# ============================================

cat("\n============================================\n")
cat("DUPLICATE RECORD ANALYSIS\n")
cat("============================================\n")

duplicate_count <- sum(
  duplicated(data)
)

cat(
  "Number of duplicate rows:",
  duplicate_count,
  "\n"
)


# ============================================
# 8. TARGET VARIABLE ANALYSIS
# ============================================

cat("\n============================================\n")
cat("ACCIDENT SEVERITY ANALYSIS\n")
cat("============================================\n")


# Frequency

severity_count <- table(
  data$Accident_severity
)

cat("\nFrequency of accident severity:\n")

print(severity_count)


# Percentage

severity_percentage <- prop.table(
  severity_count
) * 100

cat("\nPercentage distribution:\n")

print(
  round(
    severity_percentage,
    2
  )
)


# Save target distribution

severity_summary <- data.frame(
  Accident_Severity =
    names(severity_count),
  Frequency =
    as.numeric(severity_count),
  Percentage =
    round(
      as.numeric(severity_percentage),
      2
    )
)

write.csv(
  severity_summary,
  "results/accident_severity_distribution.csv",
  row.names = FALSE
)


# ============================================
# 9. ACCIDENT SEVERITY PLOT
# ============================================

severity_plot <- ggplot(
  data,
  aes(
    x = Accident_severity
  )
) +
  geom_bar() +
  labs(
    title =
      "Distribution of Accident Severity",
    x =
      "Accident Severity",
    y =
      "Number of Accidents"
  ) +
  theme_minimal()

print(severity_plot)

ggsave(
  "plots/01_accident_severity.png",
  severity_plot,
  width = 8,
  height = 5
)


# ============================================
# 10. WEATHER CONDITIONS
# ============================================

cat("\n============================================\n")
cat("WEATHER CONDITIONS ANALYSIS\n")
cat("============================================\n")

print(
  table(data$Weather_conditions)
)


weather_plot <- ggplot(
  data,
  aes(
    x = Weather_conditions
  )
) +
  geom_bar() +
  labs(
    title =
      "Accidents by Weather Conditions",
    x =
      "Weather Conditions",
    y =
      "Number of Accidents"
  ) +
  theme_minimal() +
  theme(
    axis.text.x =
      element_text(
        angle = 45,
        hjust = 1
      )
  )

print(weather_plot)

ggsave(
  "plots/02_weather_conditions.png",
  weather_plot,
  width = 9,
  height = 5
)


# ============================================
# 11. LIGHT CONDITIONS
# ============================================

cat("\n============================================\n")
cat("LIGHT CONDITIONS ANALYSIS\n")
cat("============================================\n")

print(
  table(data$Light_conditions)
)


light_plot <- ggplot(
  data,
  aes(
    x = Light_conditions
  )
) +
  geom_bar() +
  labs(
    title =
      "Accidents by Light Conditions",
    x =
      "Light Conditions",
    y =
      "Number of Accidents"
  ) +
  theme_minimal() +
  theme(
    axis.text.x =
      element_text(
        angle = 45,
        hjust = 1
      )
  )

print(light_plot)

ggsave(
  "plots/03_light_conditions.png",
  light_plot,
  width = 9,
  height = 5
)


# ============================================
# 12. ROAD SURFACE CONDITIONS
# ============================================

cat("\n============================================\n")
cat("ROAD SURFACE CONDITIONS\n")
cat("============================================\n")

print(
  table(data$Road_surface_conditions)
)


road_surface_plot <- ggplot(
  data,
  aes(
    x = Road_surface_conditions
  )
) +
  geom_bar() +
  labs(
    title =
      "Accidents by Road Surface Conditions",
    x =
      "Road Surface Conditions",
    y =
      "Number of Accidents"
  ) +
  theme_minimal() +
  theme(
    axis.text.x =
      element_text(
        angle = 45,
        hjust = 1
      )
  )

print(road_surface_plot)

ggsave(
  "plots/04_road_surface_conditions.png",
  road_surface_plot,
  width = 9,
  height = 5
)


# ============================================
# 13. TYPE OF JUNCTION
# ============================================

cat("\n============================================\n")
cat("JUNCTION TYPE ANALYSIS\n")
cat("============================================\n")

print(
  table(data$Types_of_Junction)
)


junction_plot <- ggplot(
  data,
  aes(
    x = Types_of_Junction
  )
) +
  geom_bar() +
  labs(
    title =
      "Accidents by Type of Junction",
    x =
      "Type of Junction",
    y =
      "Number of Accidents"
  ) +
  theme_minimal() +
  theme(
    axis.text.x =
      element_text(
        angle = 45,
        hjust = 1
      )
  )

print(junction_plot)

ggsave(
  "plots/05_junction_type.png",
  junction_plot,
  width = 9,
  height = 5
)


# ============================================
# 14. NUMBER OF VEHICLES INVOLVED
# ============================================

cat("\n============================================\n")
cat("NUMBER OF VEHICLES INVOLVED\n")
cat("============================================\n")

print(
  table(
    data$Number_of_vehicles_involved
  )
)


vehicle_plot <- ggplot(
  data,
  aes(
    x = Number_of_vehicles_involved
  )
) +
  geom_histogram(
    bins = 10
  ) +
  labs(
    title =
      "Number of Vehicles Involved",
    x =
      "Number of Vehicles",
    y =
      "Frequency"
  ) +
  theme_minimal()

print(vehicle_plot)

ggsave(
  "plots/06_number_of_vehicles.png",
  vehicle_plot,
  width = 8,
  height = 5
)


# ============================================
# 15. NUMBER OF CASUALTIES
# ============================================

cat("\n============================================\n")
cat("NUMBER OF CASUALTIES\n")
cat("============================================\n")

print(
  table(
    data$Number_of_casualties
  )
)


casualty_plot <- ggplot(
  data,
  aes(
    x = Number_of_casualties
  )
) +
  geom_histogram(
    bins = 10
  ) +
  labs(
    title =
      "Number of Casualties",
    x =
      "Number of Casualties",
    y =
      "Frequency"
  ) +
  theme_minimal()

print(casualty_plot)

ggsave(
  "plots/07_number_of_casualties.png",
  casualty_plot,
  width = 8,
  height = 5
)


# ============================================
# 16. DRIVER AGE GROUP
# ============================================

cat("\n============================================\n")
cat("DRIVER AGE GROUP\n")
cat("============================================\n")

print(
  table(data$Age_band_of_driver)
)


driver_age_plot <- ggplot(
  data,
  aes(
    x = Age_band_of_driver
  )
) +
  geom_bar() +
  labs(
    title =
      "Accidents by Driver Age Group",
    x =
      "Driver Age Group",
    y =
      "Number of Accidents"
  ) +
  theme_minimal() +
  theme(
    axis.text.x =
      element_text(
        angle = 45,
        hjust = 1
      )
  )

print(driver_age_plot)

ggsave(
  "plots/08_driver_age_group.png",
  driver_age_plot,
  width = 9,
  height = 5
)


# ============================================
# 17. DRIVING EXPERIENCE
# ============================================

cat("\n============================================\n")
cat("DRIVING EXPERIENCE\n")
cat("============================================\n")

print(
  table(data$Driving_experience)
)


experience_plot <- ggplot(
  data,
  aes(
    x = Driving_experience
  )
) +
  geom_bar() +
  labs(
    title =
      "Accidents by Driving Experience",
    x =
      "Driving Experience",
    y =
      "Number of Accidents"
  ) +
  theme_minimal() +
  theme(
    axis.text.x =
      element_text(
        angle = 45,
        hjust = 1
      )
  )

print(experience_plot)

ggsave(
  "plots/09_driving_experience.png",
  experience_plot,
  width = 9,
  height = 5
)


# ============================================
# 18. TYPE OF COLLISION
# ============================================

cat("\n============================================\n")
cat("TYPE OF COLLISION\n")
cat("============================================\n")

print(
  table(data$Type_of_collision)
)


collision_plot <- ggplot(
  data,
  aes(
    x = Type_of_collision
  )
) +
  geom_bar() +
  labs(
    title =
      "Accidents by Type of Collision",
    x =
      "Type of Collision",
    y =
      "Number of Accidents"
  ) +
  theme_minimal() +
  theme(
    axis.text.x =
      element_text(
        angle = 45,
        hjust = 1
      )
  )

print(collision_plot)

ggsave(
  "plots/10_type_of_collision.png",
  collision_plot,
  width = 10,
  height = 5
)


# ============================================
# 19. CAUSE OF ACCIDENT
# ============================================

cat("\n============================================\n")
cat("CAUSE OF ACCIDENT\n")
cat("============================================\n")

cause_count <- sort(
  table(data$Cause_of_accident),
  decreasing = TRUE
)

print(cause_count)


cause_plot <- ggplot(
  data,
  aes(
    x = Cause_of_accident
  )
) +
  geom_bar() +
  labs(
    title =
      "Accidents by Cause",
    x =
      "Cause of Accident",
    y =
      "Number of Accidents"
  ) +
  theme_minimal() +
  theme(
    axis.text.x =
      element_text(
        angle = 90,
        hjust = 1
      )
  )

print(cause_plot)

ggsave(
  "plots/11_cause_of_accident.png",
  cause_plot,
  width = 12,
  height = 6
)


# ============================================
# 20. SEX OF DRIVER
# ============================================

cat("\n============================================\n")
cat("DRIVER GENDER DISTRIBUTION\n")
cat("============================================\n")

print(
  table(data$Sex_of_driver)
)


driver_gender_plot <- ggplot(
  data,
  aes(
    x = Sex_of_driver
  )
) +
  geom_bar() +
  labs(
    title =
      "Accidents by Driver Sex",
    x =
      "Driver Sex",
    y =
      "Number of Accidents"
  ) +
  theme_minimal()

print(driver_gender_plot)

ggsave(
  "plots/12_driver_sex.png",
  driver_gender_plot,
  width = 8,
  height = 5
)


# ============================================
# 21. CROSS-ANALYSIS:
# ACCIDENT SEVERITY vs WEATHER
# ============================================

severity_weather_plot <- ggplot(
  data,
  aes(
    x = Weather_conditions,
    fill = Accident_severity
  )
) +
  geom_bar(
    position = "dodge"
  ) +
  labs(
    title =
      "Accident Severity by Weather Conditions",
    x =
      "Weather Conditions",
    y =
      "Number of Accidents",
    fill =
      "Accident Severity"
  ) +
  theme_minimal() +
  theme(
    axis.text.x =
      element_text(
        angle = 45,
        hjust = 1
      )
  )

print(severity_weather_plot)

ggsave(
  "plots/13_severity_vs_weather.png",
  severity_weather_plot,
  width = 10,
  height = 6
)


# ============================================
# 22. CROSS-ANALYSIS:
# ACCIDENT SEVERITY vs LIGHT
# ============================================

severity_light_plot <- ggplot(
  data,
  aes(
    x = Light_conditions,
    fill = Accident_severity
  )
) +
  geom_bar(
    position = "dodge"
  ) +
  labs(
    title =
      "Accident Severity by Light Conditions",
    x =
      "Light Conditions",
    y =
      "Number of Accidents",
    fill =
      "Accident Severity"
  ) +
  theme_minimal() +
  theme(
    axis.text.x =
      element_text(
        angle = 45,
        hjust = 1
      )
  )

print(severity_light_plot)

ggsave(
  "plots/14_severity_vs_light.png",
  severity_light_plot,
  width = 10,
  height = 6
)


# ============================================
# 23. CROSS-ANALYSIS:
# ACCIDENT SEVERITY vs ROAD CONDITIONS
# ============================================

severity_road_plot <- ggplot(
  data,
  aes(
    x = Road_surface_conditions,
    fill = Accident_severity
  )
) +
  geom_bar(
    position = "dodge"
  ) +
  labs(
    title =
      "Accident Severity by Road Surface Conditions",
    x =
      "Road Surface Conditions",
    y =
      "Number of Accidents",
    fill =
      "Accident Severity"
  ) +
  theme_minimal()

print(severity_road_plot)

ggsave(
  "plots/15_severity_vs_road_conditions.png",
  severity_road_plot,
  width = 10,
  height = 6
)


# ============================================
# 24. CROSS-ANALYSIS:
# ACCIDENT SEVERITY vs COLLISION TYPE
# ============================================

severity_collision_plot <- ggplot(
  data,
  aes(
    x = Type_of_collision,
    fill = Accident_severity
  )
) +
  geom_bar(
    position = "dodge"
  ) +
  labs(
    title =
      "Accident Severity by Collision Type",
    x =
      "Type of Collision",
    y =
      "Number of Accidents",
    fill =
      "Accident Severity"
  ) +
  theme_minimal() +
  theme(
    axis.text.x =
      element_text(
        angle = 45,
        hjust = 1
      )
  )

print(severity_collision_plot)

ggsave(
  "plots/16_severity_vs_collision.png",
  severity_collision_plot,
  width = 11,
  height = 6
)


# ============================================
# 25. UNIQUE VALUE INSPECTION
# ============================================

cat("\n============================================\n")
cat("UNIQUE VALUE INSPECTION\n")
cat("============================================\n")

for (col in names(data)) {

  cat("\n----------------------------\n")

  cat(
    "Column:",
    col,
    "\n"
  )

  cat(
    "Number of unique values:",
    length(
      unique(
        data[[col]]
      )
    ),
    "\n"
  )

  print(
    head(
      unique(
        data[[col]]
      ),
      15
    )
  )
}


# ============================================
# 26. CREATE EDA SUMMARY
# ============================================

eda_summary <- data.frame(

  Variable =
    names(data),

  Data_Type =
    sapply(
      data,
      function(x)
        class(x)[1]
    ),

  Unique_Values =
    sapply(
      data,
      function(x)
        length(
          unique(x)
        )
    ),

  Missing_Values =
    sapply(
      data,
      function(x)
        sum(is.na(x))
    )
)


# Save EDA summary

write.csv(
  eda_summary,
  "results/eda_summary.csv",
  row.names = FALSE
)


# ============================================
# 27. FINAL EDA INFORMATION
# ============================================

cat("\n============================================\n")
cat("EDA SUMMARY\n")
cat("============================================\n")

cat(
  "Rows analyzed:",
  nrow(data),
  "\n"
)

cat(
  "Columns analyzed:",
  ncol(data),
  "\n"
)

cat(
  "Total missing values:",
  sum(
    is.na(data)
  ),
  "\n"
)

cat(
  "Duplicate rows:",
  sum(
    duplicated(data)
  ),
  "\n"
)


cat("\n============================================\n")
cat("     EDA COMPLETED SUCCESSFULLY\n")
cat("============================================\n")