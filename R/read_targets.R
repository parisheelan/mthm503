# Access the cleaned pedestrian data
my_pedestrian_clean_data <- tar_read(pedestrian_clean)
glimpse(my_pedestrian_clean_data)
skimr::skim(my_pedestrian_clean_data)

# Access the fitted Random Forest model
my_rf_model <- tar_read(rf_fit)
print(my_rf_model)

# Access the predictions from the Random Forest model
my_rf_predictions <- tar_read(rf_preds)
glimpse(my_rf_predictions)

# Access the metrics for the Random Forest model
my_rf_class_metrics <- tar_read(rf_class_metrics)
print(my_rf_class_metrics)

my_rf_roc_auc <- tar_read(rf_roc_auc)
print(my_rf_roc_auc)

# Access the confusion matrix object for Random Forest
my_rf_conf_mat <- tar_read(rf_conf_mat_obj)
print(my_rf_conf_mat) # This will print the confusion matrix table

# Access and display the plots you generated
my_tree_roc_plot <- tar_read(tree_roc_plot)
print(my_tree_roc_plot)

my_knn_conf_mat_plot <- tar_read(knn_conf_mat_plot)
print(my_knn_conf_mat_plot)

my_sex_barchart <- tar_read(sex_of_casualty_barchart)
print(my_sex_barchart)

my_severity_barchart <- tar_read(casualty_severity_barchart)
print(my_severity_barchart)