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

data <- read.csv("acc_stability_multiclass_text.csv")

column_names <- colnames(data)
true_label_col <- column_names[1]
model_cols <- column_names[-1]

data$true_label <- as.factor(data[[true_label_col]])

# Lista na wyniki
all_results <- list()
heatmap_plots <- list()
IQR_Median_plots <- list()
plot_Median_plots <- list()
correlation_plots <- list()

for (model_col in model_cols) {
 prediction <- as.factor(data[[model_col]])

 eval_result <- run_repeated_evaluation(data$true_label, prediction, fraction = 0.7, n_reps = 10)
 
 model_results <- eval_result$raw
 model_results$Model <- model_col
 
 all_results[[model_col]] <- model_results
 IQR_Median_plots[[model_col]] <- IQR_Median_plot(eval_result$raw)
 correlation_plots[[model_col]] <- plot_metric_correlation(eval_result$raw)
}
print(correlation_plots$xgb)
print(IQR_Median_plots$rf$panel_a)

combined_results <- do.call(rbind, all_results)
plot_metrics_per_class(combined_results, model_name = paste(model_cols, collapse = " vs "))

for (model_col in model_cols) {
  prediction <- as.factor(data[[model_col]])
  
  p <- plot_metric_heatmap(data$true_label, prediction,model_col)
  heatmap_plots[[model_col]] <- p
}


print(heatmap_plots$xgb$F1)



