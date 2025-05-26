IQR_Median_plot <- function(results_df) {
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  
  # --- Przekształcenie danych do formatu długiego ---
  results_long <- results_df |>
    pivot_longer(cols = -c(Class, Iteration), names_to = "Metric", values_to = "Value")
  
  # --- Podsumowanie: mediany i IQR ---
  summary_df <- results_long |>
    group_by(Class, Metric) |>
    summarise(
      Median = median(Value, na.rm = TRUE),
      IQR = IQR(Value, na.rm = TRUE),
      .groups = "drop"
    )
  
  dodge <- position_dodge(width = 0.2)
  
  # --- Wykres A: Mediany ---
  p1 <- ggplot(summary_df, aes(x = Metric, y = Median, group = Class, color = Class)) +
    geom_point(position = dodge, size = 3) +
    geom_line(position = dodge) +
    geom_errorbar(aes(ymin = Median - IQR/2, ymax = Median + IQR/2), 
                  width = 0.2, position = dodge) +
    labs(title = "a) Median accuracy", y = "Median", x = "") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # --- Wykres B: IQR ---
  p2 <- ggplot(summary_df, aes(x = Metric, y = IQR, group = Class, color = Class)) +
    geom_point(position = dodge, size = 3) +
    geom_line(position = dodge, linetype = "dashed") +
    labs(title = "b) IQR of accuracies", y = "IQR", x = "") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # --- Wyświetlenie wykresów ---
  return(list(panel_a = p1, panel_b = p2))
}