plot_metric_correlation <- function(data, true_col, model_cols,
                                    fraction = 0.7, n_reps = 10) {
  library(ggcorrplot)
  library(Hmisc)
  library(ggplot2)
  library(dplyr)
  source("plots/run_repeated_evaluation.R")
  
  correlation_plots <- list()
  true_labels <- as.factor(data[[true_col]])
  
  for (model_col in model_cols) {
    prediction <- as.factor(data[[model_col]])
    
    eval_result <- run_repeated_evaluation(true_labels, prediction,
                                           fraction = fraction, n_reps = n_reps)
    
    # Usuń nienumeryczne kolumny i zbędne
    clean_df <- eval_result$raw %>%
      select(where(is.numeric)) %>%
      select(-Iteration, -contains("iteration"), -contains("class"))
    
    # Oblicz korelacje Spearmana
    corr_result <- rcorr(as.matrix(clean_df), type = "spearman")
    corr_mat <- corr_result$r
    p_mat <- corr_result$P
    
    # Wykres
    p <- ggcorrplot(corr_mat,
                    method = "square",
                    type = "upper",
                    lab = TRUE,
                    lab_size = 4,
                    p.mat = p_mat,
                    sig.level = 0.05,
                    insig = "pch",
                    colors = c("orange", "white", "darkgreen"),
                    ggtheme = theme_minimal()) +
      labs(title = paste("Korelacja metryk -", model_col)) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    correlation_plots[[model_col]] <- p
  }
  
  return(correlation_plots)
}
