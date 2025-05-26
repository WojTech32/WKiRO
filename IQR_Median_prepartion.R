IQR_median_preparation <- function(repeated) {
  library(dplyr)
  library(tidyr)
  
  # Dane "surowe" z wszystkich iteracji
  results_df <- repeated$raw
  
  # Przekształcenie do formatu długiego
  results_long <- results_df |>
    pivot_longer(cols = -c(Class, Iteration), names_to = "Metric", values_to = "Value")
  
  # Wyliczenie mediany i IQR dla każdej klasy i metryki
  summary_df <- results_long |>
    group_by(Class, Metric) |>
    summarise(
      Median = median(Value, na.rm = TRUE),
      IQR = IQR(Value, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(summary_df)
}