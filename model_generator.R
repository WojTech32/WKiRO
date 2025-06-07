# Wymagane biblioteki
library(e1071)    
library(caret) 

generate_models <- function(data ,data_name, seed = 13) {
  data$group <- factor(data$group)
  
  set.seed(seed)
  train_idx <- createDataPartition(data$group, p = 0.7, list = FALSE)
  train_data <- data[train_idx, ]
  test_data  <- data[-train_idx, ]
  
  
  ## SVM
  svm_model <- svm(group ~ ., data = train_data, kernel = "radial", probability = TRUE)
  svm_pred <- predict(svm_model, test_data, probability = TRUE)
  svm_probs <- attr(svm_pred, "probabilities")
  
  ## Naive Bayes
  nb_model <- naiveBayes(group ~ ., data = train_data)
  nb_pred <- predict(nb_model, test_data)
  nb_probs <- predict(nb_model, test_data, type = "raw")
  
  results_labels <- data.frame(
    label = test_data$group,
    svm = svm_pred,
    nb = nb_pred
  )
  
  saveRDS(results_labels, file = paste0(data_name,"_model_predictions.rds"))
  
  results_probs <- data.frame(label = test_data$group)
  
  for (cls in colnames(svm_probs)) {
    results_probs[[paste0("prob_svm_", cls)]] <- svm_probs[, cls]
  }
  
  for (cls in colnames(nb_probs)) {
    results_probs[[paste0("prob_nb_", cls)]] <- nb_probs[, cls]
  }
  
  saveRDS(results_probs, file = paste0(data_name,"_prob_predictions.rds"))
  
  return(list(
    results_labels = results_labels,
    results_probs = results_probs
  ))
  
  
  
}