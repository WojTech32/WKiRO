# Wymagane pakiety
library(caret)
library(e1071)
library(MLmetrics)
library(yardstick)
library(dplyr)
library(ggplot2)
library(tidyr)
source("run_repeated_evaluation.R")
source("calculate_metrics.R")
source("plot_metrics_per_class.R")
source("IQR_Median_prepartion.R")
source("IQR_Median_plot.R")

data <- read.csv("acc_stability_multiclass_text.csv")

column_names <- colnames(data)
true_label_col <- column_names[1]
model_cols <- column_names[-1]

data$true_label <- as.factor(data[[true_label_col]])

# Lista na wyniki
all_results <- list()

# Iteracja po kolumnach modeli
for (model_col in model_cols) {
 prediction <- as.factor(data[[model_col]])
 
 # Uruchom ewaluację
 eval_result <- run_repeated_evaluation(data$true_label, prediction, fraction = 0.7, n_reps = 10)
 
 # Dodaj nazwę modelu do wyników
 model_results <- eval_result$raw
 model_results$Model <- model_col
 
 # Zbierz wyniki
 all_results[[model_col]] <- model_results
 
}

# Połącz wszystkie wyniki w jedną ramkę danych
combined_results <- do.call(rbind, all_results)

# Generuj zbiorczy wykres
plot_metrics_per_class(combined_results, model_name = paste(model_cols, collapse = " vs "))



