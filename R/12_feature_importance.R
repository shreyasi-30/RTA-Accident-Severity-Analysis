# ============================================
# 12 - FEATURE IMPORTANCE
# ============================================

library(randomForest)


# Get importance values

importance_values <- importance(
  rf_model
)


# Convert to data frame

importance_data <- data.frame(
  Feature = rownames(importance_values),
  Importance = importance_values[, 1]
)


# Sort

importance_data <-
  importance_data[
    order(
      importance_data$Importance,
      decreasing = TRUE
    ),
  ]


# Display top 10

top_features <-
  head(importance_data, 10)

print(top_features)


# --------------------------------------------
# Plot
# --------------------------------------------

library(ggplot2)

importance_plot <- ggplot(
  top_features,
  aes(
    x = reorder(
      Feature,
      Importance
    ),
    y = Importance
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title =
      "Top 10 Important Features",
    x = "Feature",
    y = "Importance"
  ) +
  theme_minimal()


print(importance_plot)


# Save

ggsave(
  "plots/top_10_feature_importance.png",
  importance_plot,
  width = 9,
  height = 6
)


# Save CSV

write.csv(
  importance_data,
  "results/feature_importance.csv",
  row.names = FALSE
)