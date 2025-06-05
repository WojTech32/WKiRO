plot_multiclass_roc_median_sd <- function(true_label, prob_matrix, n_reps = 10, seed = 123, model_name = "Model") {
  library(pROC)
  library(dplyr)
  library(ggplot2)
  set.seed(seed)
  
  classes <- colnames(prob_matrix)
  fpr_grid <- seq(0, 1, length.out = 100)
  all_curves <- list()
  auc_info <- data.frame()
  
  for (cls in classes) {
    interp_matrix <- matrix(NA, nrow = n_reps, ncol = length(fpr_grid))
    aucs <- numeric(0)
    
    for (i in 1:n_reps) {
      idx <- sample(seq_along(true_label), size = floor(0.7 * length(true_label)), replace = FALSE)
      binary_true <- as.integer(true_label[idx] == cls)
      scores <- prob_matrix[idx, cls]
      
      # Pomijamy jeśli tylko jedna klasa (0 lub 1)
      if (length(unique(binary_true)) < 2) next
      
      roc_obj <- roc(binary_true, scores, quiet = TRUE)
        
      if (is.null(roc_obj)) next
      
      aucs <- c(aucs, as.numeric(auc(roc_obj)))
      
      interp_tpr <- approx(1 - roc_obj$specificities, roc_obj$sensitivities,
                           xout = fpr_grid, ties = mean, rule = 2)$y
      interp_matrix[i, ] <- interp_tpr
    }
    
    # Jeżeli nie ma żadnych poprawnych pomiarów – pomiń klasę
    if (length(aucs) == 0) next
    
    tpr_median <- apply(interp_matrix, 2, median, na.rm = TRUE)
    tpr_sd <- apply(interp_matrix, 2, sd, na.rm = TRUE)
    
    df <- data.frame(
      FPR = fpr_grid,
      TPR = tpr_median,
      SD = tpr_sd,
      Class = cls
    )
    
    all_curves[[cls]] <- df
    
    auc_info <- rbind(auc_info, data.frame(
      Class = cls,
      AUC = round(mean(aucs, na.rm = TRUE), 2),
      SD = round(sd(aucs, na.rm = TRUE), 2)
    ))
  }
  
  # Jeśli nadal brak danych – przerwij
  if (length(all_curves) == 0) stop("Brak danych do wygenerowania krzywej ROC")
  
  roc_df <- bind_rows(all_curves)
  
  p <- ggplot(roc_df, aes(x = FPR, y = TPR, color = Class, fill = Class)) +
    geom_line(linewidth = 1.2) +
    geom_ribbon(aes(ymin = TPR - SD, ymax = TPR + SD), alpha = 0.2, color = NA) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
    theme_minimal() +
    labs(title = paste0("ROC Curves with ±1 SD — ", model_name),
         x = "False Positive Rate", y = "True Positive Rate") +
    annotate("text", x = 0.6, y = seq(0.2, 0.05, length.out = nrow(auc_info)),
             label = paste0(auc_info$Class, ": AUC = ", auc_info$AUC, " ± ", auc_info$SD),
             hjust = 0) +
    theme(legend.position = "bottom")
  return(p)
}
