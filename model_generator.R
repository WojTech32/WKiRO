# Wymagane biblioteki
library(e1071)    
library(caret) 

generate_models <-function(data,data_name){
  data$group <- factor(data$group)
  
  set.seed(13)
  train_idx <- createDataPartition(data$group, p = 0.7, list = FALSE)
  train_data <- data[train_idx, ]
  test_data  <- data[-train_idx, ]
  
  
  ## SVM
  svm_model <- svm(group ~ ., data = train_data, kernel = "radial")
  svm_pred <- predict(svm_model, test_data)
  
  ## Naive Bayes
  nb_model <- naiveBayes(group ~ ., data = train_data)
  nb_pred <- predict(nb_model, test_data)
  
  
  results <- data.frame(
    label = test_data$group,
    svm = svm_pred,
    nb = nb_pred
  )
  saveRDS(results, file = sprintf("\n=== %s ===\n", data_name))
  return(list(
    svm_model = svm_model,
    svm_pred = svm_pred,
    nb_model = nb_model,
    nb_pred = nb_pred,
    test_labels = test_data$group,
    results = results
  ))
  
}