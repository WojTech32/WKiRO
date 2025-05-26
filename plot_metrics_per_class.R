plot_metrics_per_class <- function(results_df, model_name = "Model") {
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  
  # Zakładamy, że dane zawierają kolumny: Iteration, Class, Metric1, Metric2, ..., Model
  results_long <- results_df |>
    pivot_longer(cols = -c(Class, Iteration, Model), names_to = "Metric", values_to = "Value")
  
  ggplot(results_long, aes(x = Class, y = Value, fill = Model)) +
    geom_boxplot(outlier.shape = 21, outlier.size = 1.5, alpha = 0.8) +
    facet_wrap(~ Metric, scales = "free_y", ncol = 3) +
    theme_minimal(base_size = 12) +
    labs(
      title = paste("Rozkład metryk per klasa -", model_name),
      x = "Klasa",
      y = "Wartość metryki"
    ) +
    theme(
      legend.position = "bottom",
      strip.text = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}
