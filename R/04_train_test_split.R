# ============================================
# 04 - TRAIN TEST SPLIT
# ============================================

set.seed(123)

# Load data
data <- read.csv(
  "data/RTA_Dataset.csv",
  stringsAsFactors = FALSE
)

# Replace ? with NA
data[data == "?"] <- NA

# Convert character variables
character_columns <- sapply(data, is.character)

data[character_columns] <- lapply(
  data[character_columns],
  as.factor
)

# Target
data$Accident_severity <-
  as.factor(data$Accident_severity)


# Randomly select 80% rows
train_index <- sample(
  seq_len(nrow(data)),
  size = 0.80 * nrow(data)
)

train_data <- data[train_index, ]

test_data <- data[-train_index, ]


# Check dimensions
cat("Training rows:",
    nrow(train_data), "\n")

cat("Testing rows:",
    nrow(test_data), "\n")


# Check target distribution
table(train_data$Accident_severity)

table(test_data$Accident_severity)