project_metrics <- function(model, pred, true_labels, name) {
  cat(sprintf("\n=== %s ===\n", name))
  
  cm <- confusionMatrix(pred, true_labels)
  print(cm)
  
  precision <- cm$byClass["Precision"]
  recall    <- cm$byClass["Recall"]
  f1_score <- 2 * (precision * recall) / (precision + recall)
  
  cat(sprintf("F1-score (%s): %.3f\n", name, f1_score))
}

