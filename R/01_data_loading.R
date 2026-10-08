# ============================================
# RTA ACCIDENT SEVERITY PREDICTION
# 01 - DATA LOADING
# ============================================

# Clear environment
rm(list = ls())

# Load dataset
data <- read.csv("data/RTA_Dataset.csv",
                 stringsAsFactors = FALSE)

# Basic information
cat("Number of rows:", nrow(data), "\n")
cat("Number of columns:", ncol(data), "\n")

# View structure
str(data)

# First 6 rows
head(data)

# Last 6 rows
tail(data)

# Column names
names(data)

# Summary
summary(data)

# Missing values
missing_values <- colSums(is.na(data))

print(missing_values)

# Duplicate rows
duplicates <- sum(duplicated(data))

cat("Number of duplicate rows:", duplicates, "\n")