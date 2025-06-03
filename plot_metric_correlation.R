plot_metric_correlation <- function(metrics_df, title = "Spearman Correlation of Metrics") {
  library(ggcorrplot)
  library(Hmisc)
  library(ggplot2)
  library(dplyr)
  
  # Usuń kolumny nienumeryczne i kolumny, które nie są metrykami
  clean_df <- metrics_df %>%
    select(where(is.numeric)) %>%
    select(-Iteration, -contains("iteration"), -contains("class"))
  
  # Korelacja Spearmana + p-wartości
  corr_result <- rcorr(as.matrix(clean_df), type = "spearman")
  corr_mat <- corr_result$r
  p_mat <- corr_result$P
  
  # Wykres
  ggcorrplot(corr_mat,
             method = "square",
             type = "upper",
             lab = TRUE,
             lab_size = 4,
             p.mat = p_mat,
             sig.level = 0.05,
             insig = "pch",  # krzyżyk dla nieistotnych
             colors = c("orange", "white", "darkgreen"),
             ggtheme = theme_minimal()) +
    labs(title = title) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}
