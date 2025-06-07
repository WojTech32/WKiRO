generate_all_multiclass_roc_plots <- function(true_label, df, n_reps = 10, seed = 123) {
  library(dplyr)
  source("plots/plot_multiclass_roc_median_sd.R")
  ROC_plots <- list()
  
  all_prob_cols <- grep("^prob_", names(df), value = TRUE)
  
  models <- unique(gsub("^prob_([^_]+)_.*$", "\\1", all_prob_cols))
  
  for (model in models) {
    model_cols <- grep(paste0("^prob_", model, "_"), names(df), value = TRUE)
    
    prob_matrix <- df[, model_cols]
    class_names <- gsub(paste0("^prob_", model, "_"), "", model_cols)
    colnames(prob_matrix) <- class_names
    
    ROC_plots[[model]] <- plot_multiclass_roc_median_sd(
      true_label = true_label,
      prob_matrix = prob_matrix,
      n_reps = n_reps,
      seed = seed,
      model_name = model
    )
  }
  
  return(ROC_plots)
}
