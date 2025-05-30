plot_metric_heatmap <- function(true, pred,
                                metric = "Accuracy",
                                fractions = seq(0.4, 0.9, by = 0.1),
                                reps = seq(10, 100, by = 10)) {
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  source("run_repeated_evaluation.R")
  results <- data.frame()
  
  for (f in fractions) {
    for (r in reps) {
      eval <- run_repeated_evaluation(true, pred, fraction = f, n_reps = r)
      
      values <- eval$raw %>%
        pull(!!sym(metric))
      
      mean_val <- mean(values, na.rm = TRUE)
      
      results <- rbind(results, data.frame(
        Fraction = f,
        Repetitions = r,
        MetricValue = round(mean_val, 3)
      ))
    }
  }
  
  # Tworzenie heatmapy
  ggplot(results, aes(x = as.factor(Fraction), y = as.factor(Repetitions), fill = MetricValue)) +
    geom_tile() +
    geom_text(aes(label = MetricValue), color = "white", size = 3.5) +
    scale_fill_viridis_c(option = "viridis") +
    labs(title = paste("Średnia", metric, "dla różnych wartości 'fraction' i 'repetitions'"),
         x = "Fraction", y = "Repetitions") +
    theme_minimal()
}
