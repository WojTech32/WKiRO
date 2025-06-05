# Wymagane biblioteki
library(e1071)    
library(caret)     

source("generate_poisson.R")
data <- generate_poisson_data(n = 200, lambda1 = c(5, 5), lambda2 = c(10, 10), range = 25)

# 2. Group jako kategorie
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

# SVM metryki
cat("\n=== SVM ===\n")
cm_svm <- confusionMatrix(svm_pred, test_data$group)
print(cm_svm)

precision_svm <- cm_svm$byClass["Precision"]
recall_svm    <- cm_svm$byClass["Recall"]
f1_svm <- 2 * (precision_svm * recall_svm) / (precision_svm + recall_svm)
cat(sprintf("F1-score (SVM): %.3f\n", f1_svm))

#Bayes metryki
cat("\n=== Naive Bayes ===\n")
cm_nb <- confusionMatrix(nb_pred, test_data$group)
print(cm_nb)

precision_nb <- cm_nb$byClass["Precision"]
recall_nb    <- cm_nb$byClass["Recall"]
f1_nb <- 2 * (precision_nb * recall_nb) / (precision_nb + recall_nb)
cat(sprintf("F1-score (Naive Bayes): %.3f\n", f1_nb))

saveRDS(svm_model, "svm_model_Poisson.rds")
saveRDS(nb_model, "nb_model_Poisson.rds")


