# ============================================
# 02 - DATA CLEANING
# Road Traffic Accident Severity Prediction
# ============================================

library(dplyr)

cat("\n")
cat("============================================\n")
cat("        DATA CLEANING STARTED\n")
cat("============================================\n")


# ============================================
# 1. LOAD RAW DATA
# ============================================

data <- read.csv(
  "data/RTA_Dataset.csv",
  stringsAsFactors = FALSE
)

cat("\nRaw dataset dimensions:\n")
cat("Rows:", nrow(data), "\n")
cat("Columns:", ncol(data), "\n")


# ============================================
# 2. CHECK ORIGINAL DATA
# ============================================

cat("\n============================================\n")
cat("ORIGINAL DATA INFORMATION\n")
cat("============================================\n")

cat("\nColumn names:\n")
print(names(data))

cat("\nDataset structure:\n")
str(data)


# ============================================
# 3. CHECK '?' VALUES
# ============================================

cat("\n============================================\n")
cat("CHECKING '?' VALUES\n")
cat("============================================\n")

question_mark_count <- sum(
  data == "?",
  na.rm = TRUE
)

cat(
  "Number of '?' values:",
  question_mark_count,
  "\n"
)


# Replace '?' with NA

data[data == "?"] <- NA


# ============================================
# 4. CHECK MISSING VALUES
# ============================================

cat("\n============================================\n")
cat("MISSING VALUE ANALYSIS\n")
cat("============================================\n")

missing_values <- colSums(is.na(data))

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


# ============================================
# 5. CHECK DUPLICATE RECORDS
# ============================================

cat("\n============================================\n")
cat("DUPLICATE RECORD ANALYSIS\n")
cat("============================================\n")

duplicate_count <- sum(
  duplicated(data)
)

cat(
  "Duplicate rows found:",
  duplicate_count,
  "\n"
)


# Remove duplicate rows

if (duplicate_count > 0) {
  
  data <- data[
    !duplicated(data),
  ]
  
  cat(
    "Duplicate rows removed.\n"
  )
  
} else {
  
  cat(
    "No duplicate rows found.\n"
  )
}


# ============================================
# 6. CHECK TARGET VARIABLE
# ============================================

cat("\n============================================\n")
cat("TARGET VARIABLE ANALYSIS\n")
cat("============================================\n")

cat(
  "Target variable: Accident_severity\n"
)

cat("\nTarget classes:\n")

print(
  table(data$Accident_severity)
)


cat("\nTarget percentages:\n")

severity_percentage <- prop.table(
  table(data$Accident_severity)
) * 100

print(
  round(severity_percentage, 2)
)


# ============================================
# 7. CONVERT CHARACTER VARIABLES TO FACTORS
# ============================================

cat("\n============================================\n")
cat("CONVERTING CATEGORICAL VARIABLES\n")
cat("============================================\n")

character_columns <- sapply(
  data,
  is.character
)

data[character_columns] <- lapply(
  data[character_columns],
  as.factor
)


# Make sure target is factor

data$Accident_severity <-
  as.factor(data$Accident_severity)


cat(
  "Categorical variables converted to factors.\n"
)


# ============================================
# 8. CHECK NUMERIC VARIABLES
# ============================================

cat("\n============================================\n")
cat("NUMERIC VARIABLES\n")
cat("============================================\n")

numeric_columns <- names(
  data
)[
  sapply(
    data,
    is.numeric
  )
]

print(numeric_columns)


# ============================================
# 9. MISSING VALUE SUMMARY AFTER CLEANING
# ============================================

cat("\n============================================\n")
cat("FINAL MISSING VALUE SUMMARY\n")
cat("============================================\n")

final_missing <- colSums(
  is.na(data)
)

print(
  sort(
    final_missing,
    decreasing = TRUE
  )
)


# ============================================
# 10. SAVE CLEANED DATA
# ============================================

write.csv(
  data,
  "results/cleaned_RTA_Dataset.csv",
  row.names = FALSE
)


# ============================================
# 11. SAVE CLEANING SUMMARY
# ============================================

cleaning_summary <- data.frame(
  
  Total_Rows = nrow(data),
  
  Total_Columns = ncol(data),
  
  Duplicate_Rows_Removed =
    duplicate_count,
  
  Total_Missing_Values =
    sum(is.na(data)),
  
  Fatal_Injury =
    sum(
      data$Accident_severity ==
        "Fatal injury",
      na.rm = TRUE
    ),
  
  Serious_Injury =
    sum(
      data$Accident_severity ==
        "Serious Injury",
      na.rm = TRUE
    ),
  
  Slight_Injury =
    sum(
      data$Accident_severity ==
        "Slight Injury",
      na.rm = TRUE
    )
)


write.csv(
  cleaning_summary,
  "results/cleaning_summary.csv",
  row.names = FALSE
)


# ============================================
# 12. FINAL DATASET INFORMATION
# ============================================

cat("\n============================================\n")
cat("FINAL CLEANED DATASET\n")
cat("============================================\n")

cat(
  "Rows:",
  nrow(data),
  "\n"
)

cat(
  "Columns:",
  ncol(data),
  "\n"
)

cat(
  "Missing values:",
  sum(is.na(data)),
  "\n"
)

cat(
  "Duplicates removed:",
  duplicate_count,
  "\n"
)


cat("\n============================================\n")
cat("       DATA CLEANING COMPLETED\n")
cat("============================================\n")