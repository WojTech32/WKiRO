plot_metric_heatmap_all_models <- function(true_label_col, model_cols,
                                           data,
                                           fractions = seq(0.4, 0.9, by = 0.1),
                                           reps = seq(10, 100, by = 10)) {
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  source("plots/run_repeated_evaluation.R")
  
  all_heatmaps <- list()
  
  true <- as.factor(data[[true_label_col]])
  
  for (model_col in model_cols) {
    pred <- as.factor(data[[model_col]])
    title <- model_col
    
    sample_eval <- run_repeated_evaluation(true, pred, fraction = 0.6, n_reps = 1)
    all_metrics <- colnames(sample_eval$raw)[!(colnames(sample_eval$raw) %in% c("Iteration", "Model", "Class"))]
    
    results <- data.frame()
    
    for (f in fractions) {
      for (r in reps) {
        eval <- run_repeated_evaluation(true, pred, fraction = f, n_reps = r)
        
        for (metric in all_metrics) {
          mean_val <- mean(eval$raw[[metric]], trim = 0.1, na.rm = TRUE)
          
          results <- rbind(results, data.frame(
            Metric = metric,
            Fraction = f,
            Repetitions = r,
            MetricValue = round(mean_val, 3)
          ))
        }
      }
    }
    
    plots <- list()
    for (metric in all_metrics) {
      metric_data <- filter(results, Metric == metric)
      
      p <- ggplot(metric_data, aes(x = as.factor(Fraction), y = as.factor(Repetitions), fill = MetricValue)) +
        geom_tile() +
        geom_text(aes(label = MetricValue), color = "white", size = 3.5) +
        scale_fill_viridis_c(option = "viridis") +
        labs(
          title = paste("Mean", metric, "for", title),
          x = "Fraction", y = "Repetitions"
        ) +
        theme_minimal()
      
      plots[[metric]] <- p
    }
    
    all_heatmaps[[model_col]] <- plots
  }
  
  return(all_heatmaps)
}
