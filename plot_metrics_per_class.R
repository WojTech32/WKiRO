plot_metrics_per_class <- function(true_label_col, model_cols, data, fraction = 0.7, n_reps = 10, seed = 123) {
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  
  data$true_label <- as.factor(data[[true_label_col]])
  
  all_results <- list()
  
  # Pętla po modelach
  for (model_col in model_cols) {
    prediction <- as.factor(data[[model_col]])
    
    eval_result <- run_repeated_evaluation(data$true_label, prediction, fraction, n_reps, seed)
    model_results <- eval_result$raw
    model_results$Model <- model_col
    
    all_results[[model_col]] <- model_results
  }
  
  # Łączenie wyników
  combined_results <- do.call(rbind, all_results)
  
  # Przekształcenie do długiego formatu
  results_long <- combined_results |>
    pivot_longer(cols = -c(Class, Iteration, Model), names_to = "Metric", values_to = "Value")
  
  # Rysowanie wykresu
  plot <- ggplot(results_long, aes(x = Class, y = Value, fill = Model)) +
    geom_boxplot(outlier.shape = 21, outlier.size = 1.5, alpha = 0.8) +
    facet_wrap(~ Metric, scales = "free_y", ncol = 2) +
    theme_minimal(base_size = 12) +
    labs(
      title = paste("Rozkład metryk per klasa -", paste(model_cols, collapse = " vs ")),
      x = "Klasa",
      y = "Wartość metryki"
    ) +
    theme(
      legend.position = "bottom",
      strip.text = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  return (plot);
}