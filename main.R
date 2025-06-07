# Wymagane pakiety
library(caret)
library(e1071)
library(MLmetrics)
library(yardstick)
library(dplyr)
library(ggplot2)
library(tidyr)
source("plots/run_repeated_evaluation.R")
source("plots/plot_metrics_per_class.R")
source("plots/IQR_Median_plot.R")
source("plots/plot_metrics_point_summary.R")
source("plots/plot_metric_heatmap.R")
source("plots/plot_metric_correlation.R")
source("plots/generate_all_multiclass_roc_plots.R")
source("plots/calculate_metrics.R")


data <- readRDS("data/poisson_model_predictions.rds")
colnames(data)

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

correlation_results <- plot_metric_correlation(data, true_col, model_cols)
print(correlation_results$nb)

iqr_plots <- IQR_Median_plot(data, true_col, model_cols)
print(iqr_plots$svm)

df <- readRDS("data/cauchy_prob_predictions.rds")

roc_plots <- generate_all_multiclass_roc_plots(true_label = df$label, df = df)

print(roc_plots$svm)


