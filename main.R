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
source("generate_poisson.R")
source("generate_couchy.R")
source("generate_gauss.R")

source("generate_multiclass_poisson.R")
source("generate_multiclass_cauchy.R")
source("generate_multiclass_gauss.R")

source("model_generator.R")


#data generate
generated_multiclass_poisson <- generate__multiclass_poisson_data(n = 200, list(c(-5, -5), c(5, 5)), range = 25)
generated_multiclass_cauchy <- generate_multiclass_cauchy_data(n = 200, list(c(-5, -5), c(5, 5), c(-5, -20)), range = 25)
genereted_bin_poisson <- generate_poisson_data(n = 200, lambda1 = c(5, 5), lambda2 = c(10, 10), range = 25)

#Prediction
poisson_prediction <- generate_models(genereted_bin_poisson, "poisson_binary")
cauchy_prediction <- generate_models(generated_multiclass_cauchy, "cauchy_multiclass")

#set to create plots
data <- cauchy_prediction$results_labels

#data <- readRDS("data/poisson_model_predictions.rds")

#set label and predictions names
true_col <- colnames(data)[1]
model_cols <- colnames(data)[-1]

#
plot <- plot_metrics_per_class(
  true_label_col = true_col,
  model_cols = model_cols,
  data = data,
  fraction = 0.7,
  n_reps = 10,
  seed = 123
  
)
print(plot)

#heatmap
heatmap_results <- plot_metric_heatmap_all_models(
  true_label_col = true_col,
  model_cols = model_cols,
  data = data,
  fractions = seq(0.4, 0.9, by = 0.1),
  reps = seq(10, 100, by = 10)
)
print(heatmap_results$svm$Accuracy)

correlation_results <- plot_metric_correlation(data, true_col, model_cols)
print(correlation_results$nb)

iqr_plots <- IQR_Median_plot(data, true_col, model_cols)
print(iqr_plots$svm)

#ROC plot
df <- poisson_prediction$results_probs

roc_plots <- generate_all_multiclass_roc_plots(true_label = df$label, df = df)

print(roc_plots$svm)


