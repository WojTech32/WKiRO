calculate_metrics <- function(true, pred) {
  true <- as.factor(true)
  pred <- as.factor(pred)
  classes <- sort(unique(true))
  
  metrics_per_class <- data.frame()
  
  for (class in classes) {
    true_bin <- as.integer(true == class)
    pred_bin <- as.integer(pred == class)
    
    TP <- sum(true_bin == 1 & pred_bin == 1)
    TN <- sum(true_bin == 0 & pred_bin == 0)
    FP <- sum(true_bin == 0 & pred_bin == 1)
    FN <- sum(true_bin == 1 & pred_bin == 0)
    
    precision <- ifelse((TP + FP) == 0, NA, TP / (TP + FP))
    recall    <- ifelse((TP + FN) == 0, NA, TP / (TP + FN))  # Sensitivity
    specificity <- ifelse((TN + FP) == 0, NA, TN / (TN + FP))
    accuracy <- (TP + TN) / (TP + TN + FP + FN)
    f1 <- ifelse(is.na(precision) | is.na(recall) | (precision + recall == 0), NA,
                 2 * precision * recall / (precision + recall))
    iou <- ifelse((TP + FP + FN) == 0, NA, TP / (TP + FP + FN))
    
    denom <- sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
    mcc <- ifelse(denom == 0, NA, ((TP * TN) - (FP * FN)) / denom)
    
    class_metrics <- data.frame(
      Class = as.character(class),
      Precision = precision,
      Sensitivity = recall,
      Specificity = specificity,
      Accuracy = accuracy,
      MCC = mcc,
      F1_score = f1,
      IOU = iou
    )
    
    metrics_per_class <- rbind(metrics_per_class, class_metrics)
  }
  
  return(metrics_per_class)
}
