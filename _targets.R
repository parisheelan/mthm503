# _targets.R file
library(targets)

# Load Libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(skimr)
library(kableExtra)
library(vtable)
library(mgcv)
library(nnet)
library(caret)
library(testthat)
library(DBI)
library(RPostgres)
library(tidyr)

# Source the R functions script
tar_source("R/functions.R")
# This needs to be run by targets itself to make env vars available to targets' processes
readRenviron(".Renviron.R")


# Define the targets pipeline
list(
  # 1. Database Connection and Data Loading
  tar_target(raw_extrication_data, f_read_extrication_data()),
  
  # 2. Data Preparation and Preprocessing
  tar_target(cleaned_extrication_data, clean_data(raw_extrication_data)),
  
  # 3. Unit Tests for Data Preparation
  # This target will succeed only if all tests pass.
  tar_target(data_quality_tests, run_data_tests(cleaned_extrication_data)),
  
  # 4. Detailed EDA Plots
  # The plots will be printed to the console or saved if you modify the plotting functions
  tar_target(eda_plots, generate_eda_plots(cleaned_extrication_data)),
  
  # 5. Data Splitting
  # Returns a list, so we create separate targets for train_data and test_data
  tar_target(split_data_results, split_data(cleaned_extrication_data)),
  tar_target(train_data, split_data_results$train_data),
  tar_target(test_data, split_data_results$test_data),
  
  # 6. Model Building (Multinomial GLM)
  tar_target(glm_model, build_glm_model(train_data)),
  
  # 7. Model Evaluation (Multinomial GLM)
  # This target will print the summary and confusion matrix to the console
  tar_target(glm_evaluation_output, evaluate_model(glm_model, test_data, "Multinomial GLM (Sex-Age_Band Interaction)")),
  
  # 8. Visualize Confusion Matrix as a Heatmap
  # This target will print the heatmap plot
  tar_target(glm_confusion_heatmap_plot, plot_confusion_matrix_heatmap(glm_model, test_data))
)