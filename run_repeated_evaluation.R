run_repeated_evaluation <- function(true, pred, fraction = 0.6, n_reps = 10) {
  stopifnot(length(true) == length(pred))
  
  all_results <- list()
  
  set.seed(123)
  
  for (i in 1:n_reps) {
    idx <- sample(seq_along(true), size = floor(length(true) * fraction), replace = FALSE)
    sub_true <- true[idx]
    sub_pred <- pred[idx]
    metrics <- calculate_metrics(sub_true, sub_pred)
    metrics$Iteration <- i
    all_results[[i]] <- metrics
  }
  
  # Połącz wszystko w jedną dużą ramkę danych
  results_df <- do.call(rbind, all_results)
  
  # Podsumowanie: statystyki dla każdej metryki i klasy
  summary_df <- results_df |>
    tidyr::pivot_longer(cols = -c(Class, Iteration), names_to = "Metric", values_to = "Value") |>
    dplyr::group_by(Class, Metric) |>
    dplyr::summarise(
      Mean = mean(Value, na.rm = TRUE),
      SD = sd(Value, na.rm = TRUE),
      Median = median(Value, na.rm = TRUE),
      IQR = IQR(Value, na.rm = TRUE),
      Min = min(Value, na.rm = TRUE),
      Max = max(Value, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(list(raw = results_df, summary = summary_df))
}
