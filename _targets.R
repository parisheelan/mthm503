# _targets.R

# Load the targets library
library(targets)

# Load other necessary packages that your functions will use.
# It's good practice to list all packages here.
library(dplyr)
library(skimr)
library(tidyr)
library(forcats)
library(DBI)
library(RPostgres)
library(tidymodels) # Loads parsnip, recipes, workflows, tune, yardstick
library(rsample)    # For initial_split
library(kknn)       # For KNN engine
library(ranger)     # For Random Forest engine
library(ggplot2)    # For plotting
library(scales)     # For scales::percent in plots

# Set target options (e.g., storage format, packages to load for each target)
tar_option_set(
  packages = c("dplyr", "skimr", "tidyr", "forcats", "DBI", "RPostgres",
               "tidymodels", "rsample", "kknn", "ranger", "ggplot2", "scales"),
  # Optional: For larger datasets, consider 'format = "qs"' for faster I/O
  # format = "qs"
)

# Source all R functions from the 'R' directory
tar_source("R/functions.R")

# Read .Renviron file to set environment variables for DB connection
# This needs to be run by targets itself to make env vars available to targets' processes
readRenviron(".Renviron.R")

# Define the pipeline
list(
  # --- Data Ingestion and Preparation ---
  tar_target(
    name = raw_data_list,
    command = f_read_raw_data()
  ),
  tar_target(
    name = pedestrian_data,
    command = f_prepare_pedestrian_data(raw_data_list)
  ),
  tar_target(
    name = pedestrian_clean,
    command = f_clean_pedestrian_data(pedestrian_data)
  ),
  
  # --- Data Splitting ---
  tar_target(
    name = split_data_list, # Will contain train and test dataframes
    command = f_split_data(pedestrian_clean)
  ),
  tar_target(
    name = train_data,
    command = split_data_list$train
  ),
  tar_target(
    name = test_data,
    command = split_data_list$test
  ),
  
  # --- Recipe Definition ---
  tar_target(
    name = pedestrian_recipe,
    command = f_define_recipe(train_data)
  ),
  
  # --- Model Fitting ---
  tar_target(
    name = tree_fit,
    command = f_fit_tree_model(train_data, pedestrian_recipe)
  ),
  tar_target(
    name = knn_fit,
    command = f_fit_knn_model(train_data, pedestrian_recipe)
  ),
  tar_target(
    name = rf_fit,
    command = f_fit_rf_model(train_data, pedestrian_recipe)
  ),
  
  # --- Predictions ---
  tar_target(
    name = tree_preds,
    command = f_predict_model(tree_fit, test_data)
  ),
  tar_target(
    name = knn_preds,
    command = f_predict_model(knn_fit, test_data)
  ),
  tar_target(
    name = rf_preds,
    command = f_predict_model(rf_fit, test_data)
  ),
  
  # --- Metrics Calculation ---
  tar_target(
    name = tree_class_metrics, # Class-based metrics
    command = f_calculate_class_metrics(tree_preds)
  ),
  tar_target(
    name = tree_roc_auc, # ROC AUC metric
    command = f_calculate_roc_auc(tree_preds)
  ),
  tar_target(
    name = tree_conf_mat_obj, # Confusion Matrix object
    command = f_get_conf_mat(tree_preds)
  ),
  
  tar_target(
    name = knn_class_metrics,
    command = f_calculate_class_metrics(knn_preds)
  ),
  tar_target(
    name = knn_roc_auc,
    command = f_calculate_roc_auc(knn_preds)
  ),
  tar_target(
    name = knn_conf_mat_obj,
    command = f_get_conf_mat(knn_preds)
  ),
  
  tar_target(
    name = rf_class_metrics,
    command = f_calculate_class_metrics(rf_preds)
  ),
  tar_target(
    name = rf_roc_auc,
    command = f_calculate_roc_auc(rf_preds)
  ),
  tar_target(
    name = rf_conf_mat_obj,
    command = f_get_conf_mat(rf_preds)
  ),
  
  # --- Plotting ---
  tar_target(
    name = tree_roc_plot,
    command = f_plot_roc(tree_preds, "Decision Tree")
  ),
  tar_target(
    name = tree_conf_mat_plot,
    command = f_plot_confusion_matrix(tree_conf_mat_obj, "Decision Tree") # Pass the object
  ),
  tar_target(
    name = knn_roc_plot,
    command = f_plot_roc(knn_preds, "KNN")
  ),
  tar_target(
    name = knn_conf_mat_plot,
    command = f_plot_confusion_matrix(knn_conf_mat_obj, "KNN") # Pass the object
  ),
  tar_target(
    name = rf_roc_plot,
    command = f_plot_roc(rf_preds, "Random Forest")
  ),
  tar_target(
    name = rf_conf_mat_plot,
    command = f_plot_confusion_matrix(rf_conf_mat_obj, "Random Forest") # Pass the object
  ),
  tar_target(
    name = sex_of_casualty_barchart,
    command = f_plot_sex_of_casualty(pedestrian_clean)
  ),
  tar_target(
    name = casualty_severity_barchart,
    command = f_plot_casualty_severity(pedestrian_clean)
  )
)