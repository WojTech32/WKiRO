plot_metrics_point_summary <- function(results_df, model_name = "Model") {
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  
  # Dane w formacie długim
  results_long <- results_df |>
    pivot_longer(cols = -c(Class, Iteration, Model),
                 names_to = "Metric", values_to = "Value")
  
  # Oblicz statystyki: średnia i SD
  summary_df <- results_long |>
    group_by(Class, Metric, Model) |>
    summarise(
      Mean = mean(Value, na.rm = TRUE),
      SD = sd(Value, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Wykres
  p1 <- ggplot(summary_df, aes(x = Class, y = Mean, color = Model, group = Model)) +
    geom_point(position = position_dodge(width = 0.5), size = 3) +
    geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                  width = 0.2, position = position_dodge(width = 0.5)) +
    geom_line(position = position_dodge(width = 0.5)) +
    facet_wrap(~ Metric, scales = "free_y", ncol = 3) +
    theme_minimal(base_size = 12) +
    labs(
      title = paste("Średnia wartość metryk ± SD –", model_name),
      x = "Klasa",
      y = "Średnia wartość"
    ) +
    theme(
      legend.position = "bottom",
      strip.text = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  return(list(panel_a = p1))
}