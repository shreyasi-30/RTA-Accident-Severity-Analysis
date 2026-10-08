# ============================================
# 11 - MODEL COMPARISON PLOT
# ============================================

library(ggplot2)


# Convert accuracy to percentage
plot_data <- model_comparison

plot_data$Accuracy_Percent <-
  plot_data$Accuracy * 100


# Create plot

accuracy_plot <- ggplot(
  plot_data,
  aes(
    x = Model,
    y = Accuracy_Percent
  )
) +
  geom_col() +
  geom_text(
    aes(
      label = paste0(
        round(Accuracy_Percent, 2),
        "%"
      )
    ),
    vjust = -0.5
  ) +
  labs(
    title = "Comparison of Machine Learning Models",
    x = "Machine Learning Model",
    y = "Accuracy (%)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x =
      element_text(
        angle = 20,
        hjust = 1
      )
  )


print(accuracy_plot)


# Save plot

ggsave(
  "plots/model_accuracy_comparison.png",
  accuracy_plot,
  width = 9,
  height = 6
)