IQR_Median_plot <- function(data, true_col, model_cols, fraction = 0.7, n_reps = 10) {
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  source("plots/run_repeated_evaluation.R")
  
  all_plots <- list()
  true_labels <- as.factor(data[[true_col]])
  
  for (model_col in model_cols) {
    prediction <- as.factor(data[[model_col]])
    eval_result <- run_repeated_evaluation(true_labels, prediction, fraction, n_reps)
    
    results_df <- eval_result$raw
    
    results_long <- results_df |>
      pivot_longer(cols = -c(Class, Iteration), names_to = "Metric", values_to = "Value")
    
    summary_df <- results_long |>
      group_by(Class, Metric) |>
      summarise(
        Median = median(Value, na.rm = TRUE),
        IQR = IQR(Value, na.rm = TRUE),
        .groups = "drop"
      )
    
    dodge <- position_dodge(width = 0.2)
    
    p1 <- ggplot(summary_df, aes(x = Metric, y = Median, group = Class, color = Class)) +
      geom_point(position = dodge, size = 3) +
      geom_line(position = dodge) +
      geom_errorbar(aes(ymin = Median - IQR/2, ymax = Median + IQR/2), 
                    width = 0.2, position = dodge) +
      labs(title = paste("a) Median accuracy -", model_col), y = "Median", x = "") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    p2 <- ggplot(summary_df, aes(x = Metric, y = IQR, group = Class, color = Class)) +
      geom_point(position = dodge, size = 3) +
      geom_line(position = dodge, linetype = "dashed") +
      labs(title = paste("b) IQR of accuracies -", model_col), y = "IQR", x = "") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    all_plots[[model_col]] <- list(median = p1, IQR = p2)
  }
  
  return(all_plots)
}
