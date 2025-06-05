# Wymagane pakiety
library(caret)
library(e1071)
library(MLmetrics)
library(yardstick)
library(dplyr)
library(ggplot2)
library(tidyr)
source("run_repeated_evaluation.R")
source("plot_metrics_per_class.R")
source("IQR_Median_plot.R")
source("plot_metrics_point_summary.R")
source("plot_metric_heatmap.R")
source("plot_metric_correlation.R")
source("plot_multiclass_roc_median_sd.R")

data <- readRDS("poisson_model_predictions.rds")
colnames(data)

# Przykład użycia:
true_col <- colnames(data)[1]
model_cols <- colnames(data)[-1]

plot <- plot_metrics_per_class(
  true_label_col = true_col,
  model_cols = model_cols,
  data = data
)
print(plot)



heatmap_results <- plot_metric_heatmap_all_models(
  true_label_col = true_col,
  model_cols = model_cols,
  data = data
)
print(heatmap_results$svm$Precision)

IQR_Median_plots <- list()
correlation_plots <- list()


for (model_col in model_cols) {
  prediction <- as.factor(data[[model_col]])
  eval_result <- run_repeated_evaluation(data$label, prediction, fraction = 0.7, n_reps = 10)
  
  IQR_Median_plots[[model_col]] <- IQR_Median_plot(eval_result$raw)
  correlation_plots[[model_col]] <- plot_metric_correlation(eval_result$raw)
}




df <- read.csv("roc_multiclass_text_multimod.csv")

df$true_label <- as.factor(df$id)  # Zmień nazwę jeśli trzeba

all_prob_cols <- grep("^prob_", names(df), value = TRUE) #Wykrywanie modeli


models <- unique(gsub("^prob_([^_]+)_.*$", "\\1", all_prob_cols)) # Wyciągamy nazwy modeli np. "RF", "SVM"

ROC_plots <- list()

for (model in models) {
  # Wybierz kolumny danego modelu
  model_cols <- grep(paste0("^prob_", model, "_"), names(df), value = TRUE)
  
  # Tworzymy macierz z predykcjami
  prob_matrix <- df[, model_cols]
  class_names <- gsub(paste0("^prob_", model, "_"), "", model_cols)
  colnames(prob_matrix) <- class_names
  
  # Obliczamy krzywe ROC
  ROC_plots[[model]] <- plot_multiclass_roc_median_sd(true_label = df$id,
                                prob_matrix = prob_matrix,
                                n_reps = 10,
                                seed = 123,model)
  
}
print(ROC_plots$rf)

